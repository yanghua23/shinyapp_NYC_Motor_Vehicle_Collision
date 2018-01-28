library(shiny)
library(leaflet)
library(leaflet.extras) #for demo heat map
library(ggplot2)
library(shinythemes)
library(dplyr)
library(tidyr)
library(data.table)

# variables
Boro <- c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island')
Vict <- c('Pedestrian', 'Cyclist', 'Motorist')
Sevr <- c('No hurt', 'Injured', 'Lethal')
Year <- c('2013', '2014', '2015', '2016', '2017')

BoroAll <- c('All', Boro)
YearAll <- c('All', Year)

mvc <- fread('NYPD_Motor_Vehicle_Collisions_processed.csv', sep=',', header=TRUE)

mvc$severity <- factor(mvc$severity, levels=c('nohurt', 'injured', 'lethal'))
mvc$borough  <- factor(mvc$borough,  levels=Boro)

# change Sunday from the 1st weekday to be the last weekday
# for easier bar chart demo of trends.
mvc$weekday <- factor(mvc$weekday, levels=c(2:7, 1)) # change order 
mvc$weekday <- as.numeric(mvc$weekday) # change back to number to show freq poly graph as continous variable.