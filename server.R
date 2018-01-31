
#===============  some customization functions =================
#
# customize a ggplot2 theme:
# http://joeystanley.com/blog/custom-themes-in-ggplot2
#
# can specify another base fond family: base_family = ""
# see package "extrafont", 'loadfonts()', ggsave(), etc.
# http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
# by Winston Chang
#
theme_hua <- function (base_size = 12, base_family = "",
                       
                       plot.title = element_text(hjust=0.5, size = 18, face = "bold", 
                                                 margin=margin(t = 20, b = 20, unit = "pt")),
                       legend.text = element_text(size = 10),
                       axis.text = element_text(size = 12),
                       axis.title = element_text(size = 14, face = "bold"),
                       
                       axis.line = element_line(color = 'black'), 
                       panel.border = element_blank(),
                       
                       #'gray96', i.e. #F4F4F4, is a comfortable color for background.
                       panel.background = element_rect(fill="gray96", colour=NA),
                       plot.background = element_rect(fill="gray96", colour=NA), 
                       legend.background = element_rect(fill="transparent", colour=NA),
                       
                       legend.key = element_rect(fill="transparent", colour=NA), ...
                       ) { 
  
  theme_bw(base_size, base_family) + 
    # "+" is to update target theme_bw w/ new specified changes
    # "%+replace%"  is to only use the following specified changes, 
    # all other unspecified params in the original theme are completely overriden.
    theme(
      # new changes 
      plot.title = plot.title,
      legend.text = legend.text,
      axis.text = axis.text,
      axis.title = axis.title,
      
      axis.line = axis.line, 
      panel.border = panel.border,
      
      panel.background = panel.background,
      plot.background = plot.background, 
      legend.background = legend.background,
      
      legend.key = legend.key, ...
    )
}

# fine control:
#"expand" param: default: data is placed some distance away from the axes

scale_y_continuous_hua <- function (expand = c(0, 0), 
                                    labels = scales::comma, ...) {
  scale_y_continuous(expand = expand, labels = labels, ...)
}

scale_x_continuous_hua <- function (expand = c(0, 0), 
                                    labels = scales::comma, ...) {
  scale_x_continuous(expand = expand, labels = labels, ...)
}

barWidth <- 0.6

function(input, output, session) {
  
  #===================================================
  #=========        Map View tab           =========== 
  #===================================================
  
  #--------- Render base map ----------
  
  output$myMap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #addProviderTiles("Esri.WorldStreetMap") %>%
      #addProviderTiles('CartoDB.DarkMatter') %>% # dark background map
      setView(lng = -73.935242, lat = 40.730610, zoom = 11)
  })
  
  
  #--------- Reactively update selected data info,
  #          when UI input data selection is changed. ---------
  mapData <- reactive({
    mvc %>%
      filter( 
        (input$mapBoro == 'All' | borough == input$mapBoro) &
          (input$mapYear == 'All' | year    == input$mapYear) &
          (month >= input$mapSliderMonth[1] & month <= input$mapSliderMonth[2]) &
          ((severity == 'nohurt'  & 'No hurt' %in% input$mapSeverity) | 
             (severity == 'injured' & 'Injured' %in% input$mapSeverity) | 
             (severity == 'lethal'  & 'Lethal'  %in% input$mapSeverity)) &
          (('Lethal'  %in% input$mapSeverity & #check lethal: 
              ((pedKill > 0 & 'Pedestrian' %in% input$mapVictim) |
                 (cycKill > 0 & 'Cyclist'    %in% input$mapVictim) |
                 (motKill > 0 & 'Motorist'   %in% input$mapVictim))) |
             ('Injured'  %in% input$mapSeverity & #check injured: 
                ((pedInj > 0 & 'Pedestrian' %in% input$mapVictim) |
                   (cycInj > 0 & 'Cyclist'    %in% input$mapVictim) |
                   (motInj > 0 & 'Motorist'   %in% input$mapVictim))) |
             ('No hurt'  %in% input$mapSeverity))
      )
  })
  
  #-------- instantly update map with updated data,
  #         when UI input data selection is changed. -----------
  observe({
    
    if (input$showHeatMap) {
      leafletProxy('myMap', data = mapData()) %>% #don't forget mapData()
        clearWebGLHeatmap() %>%
        addWebGLHeatmap(~long, ~lat, size = input$mapSliderSensitivity, units = 'p', opacity = 0.6)
    } else {
      leafletProxy('myMap', data = mapData()) %>% 
        clearWebGLHeatmap()
    }
    
    if (input$showClusterMap) {
      factpal <- colorFactor(c('green', 'blue', 'red'), Sevr)
      
      leafletProxy('myMap', data = mapData()) %>% 
        clearMarkerClusters() %>% 
        addCircleMarkers(lng = ~long, lat=~lat, radius = 3, stroke = F,
                         color = ~factpal(severity), fillOpacity = 0.2,
                         clusterOptions = markerClusterOptions())
    } else {
      leafletProxy('myMap', data = mapData()) %>% 
        clearMarkerClusters()     
    }
    
    if (input$mapMarkLethal) {
      
      x <- mapData() %>%
        filter(pedKill > 0 | cycKill > 0 | motKill > 0)
      
      leafletProxy('myMap', data = x) %>%
        clearMarkers() %>%
        addMarkers(lng = ~long, lat=~lat, 
                   popup = ~paste('<font color="Black"><b>','Collision Information','</b><br/>',
                                  'Date and time:', x$dtime,'<br/>',
                                  'Killed: ','ped ', x$pedKill, ', cyc ', x$cycKill, ', mot ', x$motKill, '<br/>',
                                  'Injured: ', 'ped ', x$pedInj, ', cyc ', x$cycInj, ', mot ', x$motInj, '<br/>',
                                  'Cause:', x$cFactor1, x$cFactor2, x$cFactor3, x$cFactor4, x$cFactor5, 
                                  '<br/></font>'))
    } else {
      leafletProxy('myMap', data = mapData()) %>%
        clearMarkers()
    }
    
  })
  
  #===================================================
  #=========     Time Factors tab          =========== 
  #===================================================
  
  
  #--------- bar char collisions by year and borough -------
  output$barPlotYearBorough = renderPlot({
    
    # result: obvious drop from 2015 to 2016, then gradual increase 2016, to 2017: 
    # why ? dig NYC past news ...
    
    ggplot(data=mvc) +
      geom_bar(aes(x=year,fill=borough), position='dodge', width=barWidth) +
      labs(title='Collisions by Borough over Years',
           x=NULL, 
           y='Collisions') +
      scale_fill_brewer(palette='Set1', name=NULL) +
      scale_y_continuous_hua(limits = c(0, 12000)) + 
      theme_hua()
  }) 
  
  #--------- Collisions by month ----------
  output$freqpolyPlotMonth = renderPlot({
    
    ggplot(data=mvc) +
      geom_freqpoly(aes(x=month, color=borough), bins=12) +
      labs(title='Collisions by Month', 
           x=NULL, 
           y='Collisions') + 
      scale_color_discrete(name=NULL, labels=Boro) +
      scale_x_continuous_hua(limits = c(1, 12), breaks = 1:12, 
                         labels=c("Jan","Feb","Mar","Apr","May","Jun",
                                  "Jul","Aug","Sep","Oct","Nov","Dec")) +
      scale_y_continuous_hua(limits=c(0, 6000)) +
      annotate("rect", xmin = 3, xmax = 7, ymin = 0, ymax = 6000,
               alpha = .05, fill='darkred') +
      annotate("rect", xmin = 10, xmax = 12, ymin = 0, ymax = 6000,
               alpha = .05, fill='darkred') +
      theme_hua()
  })
  
  #--------- Collisions by weekday ----------
  output$freqpolyPlotWeekday = renderPlot({
    
    ggplot(data=mvc) +
      geom_freqpoly(aes(x=weekday, color=borough), stat='count') +
      labs(title='Collisions by Day in a Week',
           x=NULL, 
           y='Collisions') + 
      scale_color_discrete(name=NULL, labels=Boro) +
      # use scale_x/y_continous(limits=..) to exactly control x,y lim:
      scale_x_continuous_hua(limits = c(1, 7),
                             breaks = 1:7, 
                             labels=c('Mon', 'Tue', 'Wed', 
                                      'Thu','Fri', 'Sat', 'Sun')) +
      geom_vline(xintercept = 5, colour='grey', linetype='longdash') +
      scale_y_continuous_hua(limits = c(0, 9000)) + 
      theme_hua()
  })
  
  #--------- Collisions by hour ----------
  output$freqpolyPlotHour = renderPlot({
    
    ggplot(data=mvc) +
      geom_freqpoly(aes(x=hour, color=borough), binwidth=1) +
      labs(title='Collisions by Hour of a Day',
           x='Hour of a Day',
           y='Collisions') + 
      scale_color_discrete(name=NULL, labels=Boro) +
      theme_hua() +
      # use scale_x/y_continous(limits=..) to exactly control x,y lim:
      scale_x_continuous_hua(limits=c(0, 23), breaks=c(0,5,7,10,15,17,20)) + 
      scale_y_continuous_hua(limits=c(0, 4500)) +
      annotate("text", x = c(8, 16), y = c(2800, 3700), 
               label = c('8 AM', '4 PM'), size=6) +
      annotate("rect", xmin = 7, xmax = 17, ymin = 0, ymax = 4500,
               alpha = .05, fill='darkred')
  })
  
  #===================================================
  #=========     Severity & Victims tab    =========== 
  #===================================================
  
  #-------- collisions by year and severity -------
  #         not quite interesting --> may use pie-chart or table...
  output$barPlotYearSeverity = renderPlot({
    
    # check related input:
    if (input$plotSeverityBoro != 'All') {
      df <- mvc %>%
        filter(borough == input$plotSeverityBoro)
    } else {
      df <- mvc
    }
    
    ggplot(data=df) +
      geom_bar(aes(x=year, fill=severity), position='dodge', width=barWidth) +
      labs(title='Collisions by Severity over Years',
           x=NULL, 
           y='Collisions') +
      scale_fill_manual(values=c("#00CC66", "#3399FF",'red'), name=NULL, 
                        labels=c('No hurt', 'Injured', 'Lethal')) + 
      scale_y_continuous_hua() +
      theme_hua()   
  })
  
  #---------- collisions by year and type of victims ------
  output$barPlotYearVictim = renderPlot({
    
    # check related input:
    if (input$plotVictimBoro != 'All') {
      
      df_victims <- mvc %>% 
        filter(borough == input$plotVictimBoro) %>%
        group_by(year) %>%
        summarise(ped = sum(pedInj) + sum(pedKill), 
                  cyc = sum(cycInj) + sum(cycKill),
                  mot = sum(motInj) + sum(motKill))
    } else {
      df_victims <- mvc %>% 
        group_by(year) %>%
        summarise(ped = sum(pedInj) + sum(pedKill), 
                  cyc = sum(cycInj) + sum(cycKill),
                  mot = sum(motInj) + sum(motKill))      
    }
    
    df_victims <- df_victims %>% 
      gather(key='key', value='value', c('ped','cyc','mot')) %>%
      mutate(key = factor(key, levels=c('ped', 'cyc', 'mot')))
    
    # result: if check by borough, will find Manhattan has a much higher
    # ratio of pedestrian victims than the other boroughs.
    
    #    year   ped   cyc   mot
    # 1  2013 10897  3728 27637
    # 2  2014  9967  3689 25167
    # 3  2015  9032  3876 25100
    # 4  2016  7148  2659 19805
    # 5  2017  8079  3312 23244 
    
    ggplot(df_victims, aes(x = year, y = value)) + 
      geom_bar(aes(fill = key), stat = "identity", position = "dodge", width=barWidth) + 
      labs(title='Victims of Different Types',
           x=NULL, 
           y='Victims') +
      scale_fill_brewer(palette='Set1', name=NULL, 
                        labels=Vict) +
      scale_y_continuous_hua() +
      theme_hua()
    
  })
  
  #------- Pie chart severity composition --------
  output$piePlotSeverity = renderPlot({
    
    # check related input:
    if (input$plotSeverityYear != 'All') {
      
      if (input$plotSeverityBoro != 'All') {
        
        df_severity <- mvc %>% 
          filter(year == input$plotSeverityYear, borough == input$plotSeverityBoro) %>% 
          group_by(severity) %>% 
          summarise(count = n())
      } else {
        df_severity <- mvc %>% 
          filter(year == input$plotSeverityYear) %>% 
          group_by(severity) %>% 
          summarise(count = n())       
      }
      
    } else {
      
      if (input$plotSeverityBoro != 'All') {
        
        df_severity <- mvc %>%
          filter(borough == input$plotSeverityBoro) %>%
          group_by(severity) %>% 
          summarise(count = n())     
      } else {
        df_severity <- mvc %>%
          group_by(severity) %>% 
          summarise(count = n())
      }
    }
    
    df_severity <- df_severity %>% 
      mutate(percent = 100* count / sum(count))
    
    # overall result
    #   severity count   percent
    # 1 nohurt   601416  81.4  
    # 2 injured  136423  18.5  
    # 3 lethal      747   0.101
    
    #position_stack: reverse is to let the smallest factor level 
    #show first on the bottom, and then going up. Instead of vice versa.
    #And thus, smallest level shows first on pie chart as well.
    ggplot(data=df_severity) +
      geom_bar(aes(x=1, y=percent, fill=severity), stat='identity', 
               position = position_stack(reverse=TRUE)) +  
      coord_polar(theta='y') +
      labs(title='Collision Severity Composition') +
      scale_fill_manual(values=c('#00CC66', '#3399FF','red'), name=NULL, 
                        labels=Sevr) +
      theme_hua(panel.border=element_blank(),
                axis.text  = element_blank(), 
                axis.title = element_blank(), 
                axis.line  = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank())
    
  })
  
  
  #---------- Pie chart victims composition -----------
  output$piePlotVictim = renderPlot({
    
    # check related input:
    if (input$plotVictimYear != 'All') {
      
      if (input$plotVictimBoro != 'All') {
        
        df_victims <- mvc %>% 
          filter(year == input$plotVictimYear, borough == input$plotVictimBoro) %>% 
          summarise(ped=sum(pedInj)+sum(pedKill), 
                    cyc=sum(cycInj)+sum(cycKill),
                    mot=sum(motInj)+sum(motKill))
      } else {
        
        df_victims <- mvc %>% 
          filter(year == input$plotVictimYear) %>%
          summarise(ped=sum(pedInj)+sum(pedKill), 
                    cyc=sum(cycInj)+sum(cycKill),
                    mot=sum(motInj)+sum(motKill))
      }
      
    } else {
      
      if (input$plotVictimBoro != 'All') {
        
        df_victims <- mvc %>% 
          filter(borough == input$plotVictimBoro) %>%
          summarise(ped=sum(pedInj)+sum(pedKill), 
                    cyc=sum(cycInj)+sum(cycKill),
                    mot=sum(motInj)+sum(motKill)) 
      } else {
        df_victims <- mvc %>% 
          summarise(ped=sum(pedInj)+sum(pedKill), 
                    cyc=sum(cycInj)+sum(cycKill),
                    mot=sum(motInj)+sum(motKill))        
      }
    }
    
    df_victims <- df_victims %>%
      gather(key='type', value='count', 1:3) %>%
      mutate(percent = 100*count / sum(count))
    
    # to make sure the order show up in pie chart:
    df_victims$type <- factor(df_victims$type, levels=c('ped', 'cyc', 'mot'))
    
    # overall result 
    #   type  count   percent
    # 1  ped  45123 24.611650
    # 2  cyc  17264  9.416385
    # 3  mot 120953 65.971965
    
    ggplot(data=df_victims) +
      geom_bar(aes(x=1, y=percent, fill=type), stat='identity', 
               position = position_stack(reverse=TRUE)) +
      coord_polar(theta='y') +
      labs(title='Collision Victims Composition') +
      scale_fill_discrete( name=NULL, labels=Vict) + 
      theme_hua(panel.border=element_blank(),
                axis.text = element_blank(), 
                axis.title = element_blank(), 
                axis.line  = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank())
  })
  
  
  #===================================================
  #=========     Causes & Vehicles tab     =========== 
  #===================================================
  
  # preprocess data for cause analysis:
  df_cause <- reactive({
    mvc %>% 
      select(borough, cFactor1, cFactor2, cFactor3, cFactor4, cFactor5) %>%
      gather(., key='key', value='cause', 
             cFactor1, cFactor2, cFactor3, cFactor4, cFactor5) %>% 
      # filter out missing and most common, general, uninformative records:
      filter(!(cause %in% c('', 
                            'Unspecified', 
                            'Driver Inattention/Distraction',
                            'Other Vehicular',
                            'Failure to Yield Right-of-Way'))) 
    
  })
  
  # find and select top 20 causes overall:
  df_cause_top <- reactive({
    df_cause() %>%
      group_by(cause) %>%
      summarise(count=n()) %>%
      top_n(20, count)
  }) 
  
  #-------- Collision causes bar chart ----------
  # 48 causes:
  output$flipbarPlotCauseOverall = renderPlot({
    
    # calculate z score to show flipped freq bar chart 
    # with above and below averge differentiation
    df <- df_cause_top() %>%
      mutate(cause_z = round((count - mean(count))/sd(count), 4),
             cause_type = ifelse(cause_z < 0, 'below', 'above')) 
    
    #---- plot frequency graph for top 20 causes:
    ggplot(data = df) +
      geom_bar(aes(x=reorder(cause, cause_z), y=cause_z, fill=cause_type), 
               width=0.5, stat='identity', show.legend = F) +
      coord_flip() +
      labs(title='Collisions by Causes',
           x='Top 20 Collision Causes',
           y='Total Count Z-Score') +
      guides(color = "none") +
      theme_hua()
  })
  
  #-------- plot top 20 causes freq graph for each borough -----------
  output$flipbarPlotCauseBorough = renderPlot({
    
    # use top 20 causes to filter original table with borough column: 
    df_top <- df_cause_top() %>% 
      arrange(count) 
    
    df <- df_cause() %>%
      # use top 20 overall result to filter the original table:
      semi_join(., df_top, by='cause') %>%
      group_by(borough, cause) %>%
      summarise(count=n()) %>%
      # reorder cause levels for desirable order in bar chart display:
      mutate(cause = factor(cause, levels=df_top$cause)) 
    
    df_boro_tot <- df %>%
      group_by(borough) %>%
      summarise(total = sum(count))
    
    # calculate ratio for percentage axis display:
    df <- df %>%
      left_join(., df_boro_tot, by='borough') %>%
      mutate(ratio = count / total) %>%
      select(-count, -total) 
    
    
    ggplot(data = df) +
      geom_bar(aes(x=cause, y=ratio, fill=borough), 
               width=0.5, stat='identity', show.legend = F) +
      coord_flip() +
      facet_grid(.~borough) +
      labs(title='Collisions by Causes',
           x='Top 20 Collision Causes',
           y=NULL) + 
      scale_fill_brewer(palette = 'Set1') +
      scale_y_continuous_hua(labels = scales::percent) +
      theme_hua() 
  })
  
  
  df_vehicle <- reactive({
    mvc %>% 
      select(borough, vType1, vType2, vType3, vType4, vType5) %>%
      gather(., key='key', value='vehicle', 
             vType1, vType2, vType3, vType4, vType5) %>%
      mutate(vehicle = tolower(vehicle)) %>%
      # some simple key word consolidation: later figure out a general func for it...
      mutate(vehicle = ifelse(vehicle == 'uliti', 'utili',         vehicle)) %>%
      mutate(vehicle = ifelse(vehicle == 'pk',    'pick-up truck', vehicle)) %>%
      mutate(vehicle = ifelse(vehicle == 'am',    'ambulance',     vehicle)) %>%
      mutate(vehicle = ifelse(vehicle == 'ambul', 'ambulance',     vehicle)) %>%
      mutate(vehicle = ifelse(vehicle == 'vn',    'van',           vehicle)) %>%
      mutate(vehicle = ifelse(vehicle == 'bu',    'bus',           vehicle)) %>%
      # filter out missing and most common, general, uninformative records:
      filter(! (vehicle %in% c('n/a', '',
                               'unknown','unkno','unk', 'other',
                               'passenger vehicle', 'sport utility / station wagon')))
  })
  
  
  # find and select top 20 causes overall:
  df_vehicle_top <- reactive({
    df_vehicle() %>%
      group_by(vehicle) %>%
      summarise(count=n()) %>%
      top_n(20, count)
  })
  
  #---------- Collision vehicle type bar chart ----------
  
  output$flipbarPlotVehicleOverall = renderPlot({
    
    # calculate z score to show flipped freq bar chart 
    # with above and below averge differentiation
    df_top <- df_vehicle_top() %>%
      mutate(vehicle_z = round((count - mean(count))/sd(count), 4),
             vehicle_type = ifelse(vehicle_z < 0, 'below', 'above')) %>%
      mutate(vehicle = tools::toTitleCase(vehicle))
    
    ggplot(data = df_top) +
      geom_bar(aes(x=reorder(vehicle, vehicle_z), y=vehicle_z,
                   fill=vehicle_type),
               width=0.5, stat='identity', show.legend = F) +
      coord_flip() +
      labs(title='Collisions by Types of Vehicles',
           x='Top 20 Collision Vehicles',
           y='Total Count Z-Score') + 
      theme_hua() 
      
  })
  
  #--------- plot top 20 vehicles freq graph for each borough -----------
  
  output$flipbarPlotVehicleBorough = renderPlot({
    
    # use top 20 vehicles to filter original table with borough column: 
    df_top <- df_vehicle_top() %>% 
      arrange(count) 
    
    df <- df_vehicle() %>%
      # use top 20 overall result to filter the original table:
      semi_join(., df_top, by='vehicle') %>%
      group_by(borough, vehicle) %>%
      summarise(count=n()) %>%
      mutate(vehicle = tools::toTitleCase(vehicle)) %>%
      # reorder vehicle types for desirable order in bar chart display:
      mutate(vehicle = factor(vehicle, levels=tools::toTitleCase(df_top$vehicle)))
    
    df_boro_tot <- df %>%
      group_by(borough) %>%
      summarise(total = sum(count)) 
    
    # calculate ratio for percentage axis display:
    df <- df %>%
      left_join(., df_boro_tot, by='borough') %>%
      mutate(ratio = count / total) %>%
      select(-count, -total) 
    
    
    ggplot(data = df) +
      geom_bar(aes(x=vehicle, y=ratio, fill=borough), 
               width=.5, stat='identity', show.legend = F) +
      coord_flip() +
      facet_grid(.~borough) +
      labs(title='Collisions by Types of Vehicles',
           x='Top 20 Collision Vehicles',
           y=NULL) +
      scale_y_continuous_hua(breaks=c(0, 0.1, 0.2, 0.3), labels = scales::percent) +
      theme_hua()
  })
  
}