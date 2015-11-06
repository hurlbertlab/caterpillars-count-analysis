################################################
# Script for reading in and cleaning data from
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

# Create effortByDay dataframe for use in summary functions
surveys$date = as.character(as.POSIXlt(word(surveys$dateStart, 1, sep = " "), format = "%Y-%m-%d"))
effortByDay = data.frame(table(surveys[, c('site', 'date')]))
names(effortByDay) = c('site', 'date', 'numSurveys')
effortByDay = effortByDay[effortByDay$numSurveys!=0, ]
effortByDay$date = as.POSIXlt(effortByDay$date, format = "%Y-%m-%d")
effortByDay$julianday = yday(effortByDay$date)
tempyear <- substring(effortByDay$date, 1, 4)
effortByDay$year = tempyear 

# Merge orders and surveys table
orders2 = merge(surveys, orders, by = 'surveyID', all.x = T)

orders2$date = as.POSIXlt(word(orders2$dateStart, 1, sep = " "), format = "%Y-%m-%d")
orders2$julianday = yday(orders2$date)

orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x')]

# Add column with arthopod order code
arthcodes = read.csv('arth_codes.csv', header=T)
arthcodes1 = arthcodes[, c('ArthCode', 'DataName')]
names(arthcodes1) = c('arthCode', 'arthropod')
cleandata <- merge(orders3, arthcodes1, all.x = TRUE, sort = FALSE)
cleandata <- cleandata[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                       'plantSp','herbivory','arthropod','arthCode','length',
                       'count','notes.y','notes.x')]
cleandata <- cleandata[order(cleandata$date),]

# Add a column indicating if the leaves were wet
cleandata$wetLeaves = c()
tempwet <- sort(c(grep("wet leaves", cleandata$notes.x), grep("Wet leaves", cleandata$notes.x), 
                  grep("very dewy", cleandata$notes.x), grep("Wet Leaves", cleandata$notes.x)))
cleandata$wetLeaves = rep('no', nrow(cleandata))
cleandata$wetLeaves[tempwet] = 'yes'

# temp fix of date class
cleandata$date = as.character(cleandata$date)

# add a year column
tempdate <- substring(cleandata$date, 1, 4)
cleandata$year = tempdate

# List of unique survey events
events = unique(cleandata[, c("surveyID", "userID", "date", "year", "julianday", "site", "circle", "survey")])

# Change arthCodes to 'NONE' that were previously NA
cleandata$arthCode[is.na(cleandata$arthCode)] = "NONE"

# Taking out large colonies completely:
cleandata <- cleandata[!(cleandata$arthCode == "LEPL" & cleandata$count > 10),]
# or
#cleandata$count[cleandata$arthCode == "LEPL" & cleandata$count > 5] = 5

# Cleaning beat sheets (PR and BG) and isolating # leaves into a new column
beatsheet = cleandata[grep("BEAT SHEET", cleandata$notes.x),]
leavesNumTemp0 <- word(beatsheet$notes.x, -1, sep = "BEAT SHEET; ")
leavesNumTemp <- word(leavesNumTemp0, -1, sep = "= ")
leavesNumTemp1 <- word(leavesNumTemp, -1, sep = "Leaves  ")
leavesNumTemp2 <- word(leavesNumTemp1, -1, sep = "Leaves=")
leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")
leavesNumTemp5 <- gsub(" ", "", leavesNumTemp4)
leavesNumTemp6 <- gsub("\n", "", leavesNumTemp5)
leavesNumTemp7 <- gsub("Unknown", NA, leavesNumTemp6)
leavesNumTemp8 <- gsub("unknown", NA, leavesNumTemp7)
beatsheet$leavesNum <- as.numeric(leavesNumTemp7)
cleandata = merge(cleandata, beatsheet, by = c("surveyID", "userID", "site", "survey", "circle", "date",
                                   "julianday", "plantSp", "herbivory", "arthropod", "arthCode",
                                   "length", "count", "notes.y", "notes.x", "wetLeaves", "year"), all = T)

# Not sure if beat sheet merging is appropriate
# Fixed but may still want to check - "NAs introduced by coercion" error even though only numbers show up in table


# Subsetting 2015 data for use in estimating_peaks.R, with a min length of 5 mm

cleandata.pr <- cleandata[cleandata$site == 117 & cleandata$year == 2015,]
cleandata.bg <- cleandata[cleandata$site == 8892356 & cleandata$year == 2015,]
# (Should I subset these later in estimating_peaks.R?)

amsurvey.pr <- surveySubset(cleandata.pr, subset = "visual am", minLength = 5)
pmsurvey.pr <- surveySubset(cleandata.pr, subset = "visual pm", minLength = 5)
beatsheet.pr <- surveySubset(cleandata.pr, subset = "beat sheet", minLength = 5)
volunteer.pr <- surveySubset(cleandata.pr, subset = "volunteer", minLength = 5)

amsurvey.bg <- surveySubset(cleandata.bg, subset = "visual am", minLength = 5)
beatsheet.bg <- surveySubset(cleandata.bg, subset = "beat sheet", minLength = 5)







