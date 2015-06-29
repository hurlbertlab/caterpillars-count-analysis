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


# Calculate mean density per survey

meanDensity = function(surveys, 
                       orders, 
                       ordersToInclude,
                       byTreeSpecies = FALSE,
                       minLength = 0) {
  
  
  
}