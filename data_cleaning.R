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
orders4 <- merge(orders3, arthcodes1, all.x = TRUE, sort = FALSE)
orders4 <- orders4[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                       'plantSp','herbivory','arthropod','arthCode','length',
                       'count','notes.y','notes.x')]
orders4 <- orders4[order(orders4$date),]

# Add a column indicating if the leaves were wet
orders4$wetLeaves = c()
tempwet <- sort(c(grep("wet leaves", orders4$notes.x), grep("Wet leaves", orders4$notes.x), 
                  grep("very dewy", orders4$notes.x), grep("Wet Leaves", orders4$notes.x)))
orders4$wetLeaves = rep('no', nrow(orders4))
orders4$wetLeaves[tempwet] = 'yes'

#tempwet$wetLeaves = 'yes'
#tempwet1 <- tempwet[, c('surveyID', 'wetLeaves')]
#orders5 <- merge(orders4, tempwet1, by = 'wetLeaves', all.x = T)
#orders5$wetLeaves[is.na(orders5$wetLeaves)] = 'no'


# temp fix of date class
orders4$date = as.character(orders4$date)

# add a year column
tempdate <- substring(orders4$date, 1, 4)
orders4$year = tempdate

# List of unique survey events
events = unique(orders4[, c("surveyID", "userID", "date", "year", "julianday", "site", "circle", "survey")])



# Create different subsets of data for beat sheets vs visual surveys, 
# morning versus afternoon, etc. Also, try to pull out the number of leaves
# for the BEAT SHEETs.

# Separating the data:

# All visual surveys
visualsurvey = orders4[!grepl("BEAT SHEET", orders4$notes.x),]

# Botanical Garden visual surveys
visualsurveybg = visualsurvey[visualsurvey$site == 8892356,]

# Prairie Ridge visual surveys
visualsurveypr = visualsurvey[visualsurvey$site == 117,]


# For separating the Prairie Ridge data:

# Our visual surveys from 2015 ONLY, not including repeat surveys
# Checked data and "REPEAT SURVEY" only format used so far to indicate
tempvis = visualsurveypr[!grepl("REPEAT SURVEY", visualsurveypr$notes.x),]
labsurvey = tempvis[tempvis$userID %in% c(69, 130, 131, 132) & tempvis$year == 2015, ]

# Our repeat surveys ONLY
repsurvey = visualsurveypr[grep("REPEAT SURVEY", visualsurveypr$notes.x),]

# Volunteer surveys
volsurvey = visualsurveypr[visualsurveypr$userID == 129,]


# Beat sheets (PR and BG) and isolating # leaves into a new column
beatsheet = orders4[grep("BEAT SHEET", orders4$notes.x),]
leavesNumTemp <- word(beatsheet$notes.x, -1, sep = "= ")
leavesNumTemp1 <- word(leavesNumTemp, -1, sep = "BEAT SHEET; Leaves  ")
leavesNumTemp2 <- word(leavesNumTemp1, -1, sep = "BEAT SHEET; Leaves=")
leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")
leavesNumTemp5 <- gsub(" ", "", leavesNumTemp4)
leavesNumTemp6 <- gsub("\n", "", leavesNumTemp5)
leavesNumTemp7 <- gsub("Unknown", "unknown", leavesNumTemp6)
beatsheet$leavesNum <- as.numeric(leavesNumTemp7)


# Pulling out # Leaves, use strsplit(orders4$notes.x, "BEAT SHEET; Leaves = ")
# splitbeat <- strsplit(as.character(beatsheet$notes.x), split = "BEAT SHEET; Leaves = ")
# beatsheet$leavesBeatTemp = splitbeat


# Change arthCodes to 'NONE' that were previously NA
# Make sure data entered correctly before doing this
labsurvey$arthCode[is.na(labsurvey$arthCode)] = "NONE"
beatsheet$arthCode[is.na(beatsheet$arthCode)] = "NONE"
repsurvey$arthCode[is.na(repsurvey$arthCode)] = "NONE"
volsurvey$arthCode[is.na(volsurvey$arthCode)] = "NONE"
visualsurveybg$arthCode[is.na(visualsurveybg$arthCode)] = "NONE"

# Two possibilities of dealing with caterpillar colony outliers (choose one)
# (Rerun the separating data section in between switching possibilities)

# #1. Reducing counts with more than 5 caterpillars to 5 caterpillars:
#labsurvey$count[labsurvey$arthCode == "LEPL" & labsurvey$count > 5] = 5
#beatsheet$count[beatsheet$arthCode == "LEPL" & beatsheet$count > 5] = 5
#repsurvey$count[repsurvey$arthCode == "LEPL" & repsurvey$count > 5] = 5
#volsurvey$count[volsurvey$arthCode == "LEPL" & volsurvey$count > 5] = 5
#visualsurveybg$count[visualsurveybg$arthCode == "LEPL" & visualsurveybg$count > 5] = 5

# #2. Taking out large colonies completely:
labsurvey <- labsurvey[!(labsurvey$arthCode == "LEPL" & labsurvey$count > 10),]
beatsheet <- beatsheet[!(beatsheet$arthCode == "LEPL" & beatsheet$count > 10),]
repsurvey <- repsurvey[!(repsurvey$arthCode == "LEPL" & repsurvey$count > 10),]
volsurvey <- volsurvey[!(volsurvey$arthCode == "LEPL" & volsurvey$count > 10),]
visualsurveybg <- visualsurveybg[!(visualsurveybg$arthCode == "LEPL" & visualsurveybg$count > 10),]


