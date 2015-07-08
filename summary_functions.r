################################################
# Script for conducting basic analyses based on 
# Caterpillars Count! data exported from the
# phpMyAdmin project site.

# Data include:
#
# tbl_surveys: date, site, time, and temperature of all survey events
#
# tbl_orders: arthropod observed, their length, and count associated with
#             each survey event

# Load required libraries
library(plyr)
library(dplyr)
library(lubridate)

# Read in data
surveys = read.csv('data/tbl_surveys.csv', header=F)
orders = read.csv('data/tbl_orders.csv', header=F)

names(surveys) = c('surveyID', 'site', 'userID', 'survey', 'circle', 'dateStart',
                   'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                   'herbivory', 'photo', 'isValid')
names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                  'count', 'photo', 'time', 'isValid')

# Merge orders and surveys table
orders2 = merge(surveys, orders, by = 'surveyID', all = T)

orders2$date = as.POSIXlt(orders2$dateStart, format = "%m/%d/%Y %H:%M")
orders2$julianday = yday(orders2$date)

orders3 = orders2[, c('surveyID', 'site', 'survey', 'circle', 'date','julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x')]

# temp fix of date class
orders3$date = as.character(orders3$date)

# add a year column
tempdate <- substring(orders3$date, 1, 4)
orders3$year = tempdate

# Calculate mean density per survey

meanDensityByDay = function(surveyData,            # merged dataframe of surveys and orders tables
                       ordersToInclude,       # which arthropod orders to calculate density for
                       byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                       minLength = 0,         # minimum arthropod size to include
                       inputYear,
                       inputSite)                  
  
  {
  dataYearSite = surveyData[surveyData$year == inputYear & surveyData$site == inputSite, ]
  
  temp = filter(dataYearSite, length >= minLength & arthropod %in% ordersToInclude)
  if (byTreeSpecies) {
    temp2 = ddply(temp, .(site, julianday, year, plantSp), summarize, 
                  meanDensity = sum(count)/length(unique(dataYearSite$surveyID)))
    
  } else {
    temp2 = ddply(temp, .(site, julianday, year), summarize, 
                  meanDensity = sum(count)/length(unique(dataYearSite$surveyID)))
  }
  
}

# Plotting
tempvarPR <- meanDensityByDay(surveyData = orders3, ordersToInclude = 'Caterpillars (Lepidoptera larvae)', 
                             inputYear = '2015', inputSite = 117) # for Prairie Ridge plot
plot(tempvarPR$julianday, tempvarPR$meanDensity, type = 'l', col = "blue")

tempvarBG <- meanDensityByDay(surveyData = orders3, ordersToInclude = 'Caterpillars (Lepidoptera larvae)', 
                            inputYear = '2015', inputSite = 8892356) # for Botanical Garden plot
plot(tempvarBG$julianday, tempvarBG$meanDensity, type = 'l', col = "blue")

# Not sure if these are correct, never a mean density of 0?
# Adding if else statements for if you don't want to input year and site?


