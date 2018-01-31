#
# h6('Based on Jan 20, 2017 dataset from'),
# h6(a('NYC OpenData Motor Vehicle Collision', 
#      href = 'https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95', 
#      target='_blank'))
#
# could add an overview page, and end page ...
#
navbarPage('NYC Motor Vehicle Collision', theme = shinytheme("superhero"),

           #===================== Intro tab =====================
           tabPanel('Introduction',
                    div(
                        class='coverDiv',
                        tags$head(includeCSS('styles.css')), # custom styles
                        
                        absolutePanel(fixed = TRUE, draggable = FALSE, 
                                      top = 60, left = 50, right = 'auto', bottom = 'auto',
                                      width = 550, height = 'auto', 
                                    
                        div( class = 'coverTextDiv',
                        
                        h1('New York City Motor Vehicle Collision Data Visualization'),
                        h3('Everybody loves the city.'),
                        h3('Nobody likes car accidents.'),
                        h3('Why bother look at the motor vehicle collision data?'),
                        h3('Mainly, the interest here is to see what we can learn from the data to help better prevent and/or avoid collisions in the future.'),
                        h3('The work done so far includes an interactive map tool and some preliminary data analysis result, to start with...'),
                        h3('Thank you!'),
                        br()
                        )
                        
                        )
                        
                        # Another way to add image:
                        #img(src = "airbnb_overview.jpg", height = 600, weight =700, align="center")
                        #use HTML tag functions to center the image
                        #https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
                        #HTML('<center><img src="NYC_cover_image.jpg", height = 600, weight =700 ></center>')
                        
                        )
                    ),
         
           
           #===================== Map View tab =====================
           tabPanel('Map View', inputId = 'mapViewTab',
                    div(class='outer',
                         tags$head(includeCSS('styles.css') # custom styles
                        ),
                        leafletOutput('myMap', width = '100%', height = '100%')
                    ),
                    
                    # Panel options: 
                    absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE, draggable = TRUE, 
                                  top = 40, left = 40, right = 'auto', bottom = 'auto',
                                  width = 320, height = 'auto',
                                  
                                  #h3('Pick data & Set map'), 
                                  
                                  fluidRow(
                                    column(6,
                                           selectInput(inputId = 'mapYear',
                                                       label = h4('Year'),
                                                       choices = YearAll,
                                                       selected = 'All')),
                                    column(6,
                                           
                                           selectInput(inputId = 'mapBoro',
                                                       label = h4('Borough'),
                                                       choices = BoroAll,
                                                       selected = 'All'))
                                  ),
                                  
                                  sliderInput(inputId = 'mapSliderMonth', label = h4('Month range'),
                                              min = 1, max = 12, value = c(1, 12), step = 1, 
                                              animate = animationOptions(interval = 1000, loop = T)), 
                                  fluidRow(
                                    column(6,
                                           checkboxGroupInput(inputId = 'mapVictim', label = h4('Victim type'), 
                                                              choices = Vict, selected = Vict)),
                                    
                                    column(6,
                                           checkboxGroupInput(inputId = 'mapSeverity', label = h4('Severity level'), 
                                                              choices = Sevr, selected = Sevr))
                                  ),
                                  
                                  h3('Set map'),
                                  
                                  fluidRow(
                                    column(6,
                                           checkboxInput('showHeatMap', label = h4('Heat map'), value = FALSE)
                                    ),
                                    
                                    column(6,
                                           checkboxInput('showClusterMap', label = h4('Cluster map'), value = FALSE)
                                    )
                                  ),
                                  
                                  checkboxInput('mapMarkLethal', label = h4('Mark lethal collisions'), 
                                                value = FALSE),
                                  
                                  sliderInput(inputId = 'mapSliderSensitivity', label = h4('Heat map sensitivity'),
                                              min = 0, max = 600, value = 200)
                    )

           ),
           
           #===================== Time Factors tab =====================
           tabPanel('Time Factors', 
                    # If change background color, use:
                    # style = "background-color: #F4F4F4;", 
                    # this color code is for 'gray96', a comfortable background color.
                    wellPanel(
                      fluidRow(
                        column(6,
                               plotOutput(outputId = 'barPlotYearBorough')
                        ),
                        column(6,
                               plotOutput(outputId = 'freqpolyPlotMonth')
                        )
                      ),
                      br(),
                      fluidRow(
                        column(6,
                               plotOutput(outputId = 'freqpolyPlotWeekday')
                        ),
                        column(6,
                               plotOutput(outputId = 'freqpolyPlotHour')
                        )
                      ),
                      br()
                    ),
                    br()
           ),
           
           #===================== Severity & Victims tab =====================
           tabPanel('Severity & Victims', 
                    
                      fluidRow(
                        column(2,
                               h3('Severity data'),
                               selectInput(inputId = 'plotSeverityYear',
                                           label = h4('Year'),
                                           choices = YearAll,
                                           selected = 'All'),
                               
                               
                               selectInput(inputId = 'plotSeverityBoro',
                                           label = h4('Borough'),
                                           choices = BoroAll,
                                           selected = 'All')
                        ),
                        column(6,
                               plotOutput(outputId = 'barPlotYearSeverity')
                        ),
                        column(4,
                               plotOutput(outputId = 'piePlotSeverity')
                        )
                      ),
                      br(),
                      fluidRow(
                        column(2,
                               h3('Victim data'),
                               selectInput(inputId = 'plotVictimYear',
                                           label = h4('Year'),
                                           choices = YearAll,
                                           selected = 'All'),
                               
                               
                               selectInput(inputId = 'plotVictimBoro',
                                           label = h4('Borough'),
                                           choices = BoroAll,
                                           selected = 'All')
                        ),
                        column(6,
                               plotOutput(outputId = 'barPlotYearVictim')
                        ),
                        column(4,
                               plotOutput(outputId = 'piePlotVictim')
                        )
                        
                      ),
                      br(),
                    br()
           ),
           
           #===================== Causes & Vehicles tab =====================
           tabPanel('Causes & Vehicles', 
                    wellPanel(
                      fluidRow(
                        column(5,
                               plotOutput(outputId = 'flipbarPlotCauseOverall')
                        ),
                        column(7,
                               plotOutput(outputId = 'flipbarPlotCauseBorough')
                        )
                      ),
                      tags$small('Note: To be more informative,'),br(),
                      tags$small('excluded two most common top items:'), br(),
                      tags$small('"Driver Inattention/Distraction"'), br(),
                      tags$small('"Failure to Yield Right-of-Way".')
                    ),
                      br(),
                    wellPanel(
                      fluidRow(
                        column(5,
                               plotOutput(outputId = 'flipbarPlotVehicleOverall')
                        ),
                        column(7,
                               plotOutput(outputId = 'flipbarPlotVehicleBorough')
                        )
                      ),
                      tags$small('Note: To be more informative,'), br(),
                      tags$small('excluded two most common top items:'), br(),
                      tags$small('"Passenger Vehicle",'), br(),
                      tags$small('"Sport Utility / Station Wagon".'),
                      br()
                    ),
                    br()
           ),
           
           #===================== References tab =====================
           tabPanel("References",
 
                    h3("Detailed summary presentation of the work", a("Link", href="https://docs.google.com/presentation/d/1cKCIfQLJiUwfUUuLXcDBMbQyxzxN7h-fT-0xFjMEsuA/edit?usp=sharing")),
                  
                    h3("Source code on Github", a("Link", href="https://github.com/yanghua23/shinyapp_NYC_Motor_Vehicle_Collision")),
                    
                    h3("Original dataset from NYC OpenData", a("Link", href="https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95"))
           )
)