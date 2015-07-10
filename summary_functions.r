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
library(stringr)

# Read in data
surveys = read.csv('data/tbl_surveys.csv', header=F)
orders = read.csv('data/tbl_orders.csv', header=F)

names(surveys) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                   'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                   'herbivory', 'photo', 'isValid')
names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                  'count', 'photo', 'time', 'isValid')

# Convert 'survey' field to character from factor
surveys$survey = as.character(surveys$survey)


# Merge orders and surveys table
orders2 = merge(surveys, orders, by = 'surveyID', all.x = T)

orders2$date = as.POSIXlt(word(orders2$dateStart, 1, sep = " "), format = "%m/%d/%Y")
orders2$julianday = yday(orders2$date)

orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x')]

# temp fix of date class
orders3$date = as.character(orders3$date)

# add a year column
tempdate <- substring(orders3$date, 1, 4)
orders3$year = tempdate

# List of unique survey events
events = unique(orders3[, c("surveyID", "userID", "date", "year", "julianday", "site", "circle", "survey")])


# Function for checking survey events for a given site and year

surveyCount = function(events, site, year) {
  subevents = subset(events, site == site & year == year)
  #Count number of survey locations per date
  survcount1 = data.frame(table(subevents$date))
  names(survcount1) = c('date', 'numSurveys')                        
  survcount2 = data.frame(table(subevents[, c('date', 'circle', 'survey')]))
  zerocounts = survcount2[survcount2$Freq == 0,]
  zerocounts = zerocounts[order(zerocounts$date, zerocounts$circle, zerocounts$survey),]
  multicounts = survcount2[survcount2$Freq > 1,]
  multicounts = multicounts[order(multicounts$date, multicounts$circle, multicounts$survey),]
}





# Calculate mean density per survey

meanDensityByDay = function(surveyData,            # merged dataframe of surveys and orders tables
                       ordersToInclude,       # which arthropod orders to calculate density for
                       byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                       minLength = 0,         # minimum arthropod size to include
                       inputYear,
                       inputSite)                  
  
  {
  dataYearSite = surveyData[surveyData$year == inputYear & surveyData$site == inputSite, ]
  effortByDay = data.frame(table(unique(dataYearSite[, c('surveyID', 'julianday')])$julianday))
  names(effortByDay) = c('julianday', 'numSurveys')
  
  temp = filter(surveyData,
                surveyData$year == inputYear & 
                surveyData$site == inputSite &
                length >= minLength & 
                arthropod %in% ordersToInclude)
  
  
  if (byTreeSpecies) {
    temp2 = ddply(temp, .(site, julianday, year, plantSp), summarize, 
                  totalCount = sum(count))
    
  } else {
    temp2 = ddply(temp, .(site, julianday, year), summarize, 
                  totalCount = sum(count))
  }
  
  temp3 = merge(effortByDay, temp2[, c('julianday', 'totalCount')], by = 'julianday', all = T)
  temp3$totalCount[is.na(temp3$totalCount)] = 0
  temp3$meanDensity = temp3$totalCount/temp3$numSurveys
  temp3$julianday = as.numeric(as.character(temp3$julianday))
  return(temp3)
}


# Create different subsets of data for beat sheets vs visual surveys, 
# morning versus afternoon, etc. Also, try to pull out the number of leaves
# for the BEAT SHEETs.


visualsurvey = orders3[!grepl("BEAT SHEET", orders3$notes.x),]


beatsheet = orders3[grep("BEAT SHEET", orders3$notes.x),]
leavesNumTemp <- word(beatsheet$notes.x, -1, sep = "= ")
leavesNumTemp1 <- word(leavesNumTemp, 1, sep = "BEAT SHEET; Leaves  ")
leavesNumTemp2 <- word(leavesNumTemp1, 1, sep = "BEAT SHEET; Leaves=")
leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")

# Still errors in # leaves data


# Pulling out # Leaves, use strsplit(orders3$notes.x, "BEAT SHEET; Leaves = ")


splitbeat <- strsplit(as.character(beatsheet$notes.x), split = "BEAT SHEET; Leaves = ")
beatsheet$leavesBeatTemp = splitbeat

splitbeat1 <- strsplit(as.character(beatsheet$leavesBeatTemp), split = "BEAT SHEET; Leaves  = ")



# Plotting
tempvarPR <- meanDensityByDay(surveyData = orders3, ordersToInclude = 'Caterpillars (Lepidoptera larvae)', 
                             inputYear = '2015', inputSite = 117) # for Prairie Ridge plot
plot(tempvarPR$julianday, tempvarPR$meanDensity, type = 'l', col = "blue")

tempvarBG <- meanDensityByDay(surveyData = orders3, ordersToInclude = 'Caterpillars (Lepidoptera larvae)', 
                            inputYear = '2015', inputSite = 8892356) # for Botanical Garden plot
plot(tempvarBG$julianday, tempvarBG$meanDensity, type = 'l', col = "blue")

# Not sure if these are correct, never a mean density of 0?
# Adding if else statements for if you don't want to input year and site?


