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
                       ordersToInclude,       # which arthropod orders to calculate density for (codes)
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
                arthCode %in% ordersToInclude)
  
  
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


# Separating the data:

# For the word function:
library(stringr)

# All visual surveys
visualsurvey = orders4[!grepl("BEAT SHEET", orders4$notes.x),]

# Botanical Garden visual surveys
visualsurveybg = visualsurvey[visualsurvey$site == 8892356,]

# Prairie Ridge visual surveys
visualsurveypr = visualsurvey[visualsurvey$site == 117,]


# For separating the Prairie Ridge data:

# Our visual surveys ONLY, not including repeat surveys
# Checked data and "REPEAT SURVEY" only format used so far to indicate
tempvis = visualsurveypr[!grepl("REPEAT SURVEY", visualsurveypr$notes.x),]
labsurvey = tempvis[tempvis$userID %in% c(69, 130, 131, 132), ]

# Our repeat surveys ONLY
repsurvey = visualsurveypr[grep("REPEAT SURVEY", visualsurveypr$notes.x),]

# Volunteer surveys
volsurvey = visualsurveypr[visualsurveypr$userID == 129,]

# Beat sheets and isolating # leaves into a new column
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


# Plotting and calculations

# Prairie Ridge
# temporarily going to take out julian day 170, not a PR day and only one caterpillar seen on one survey
# (so I can start making peak calculations):
labsurvey1 <- labsurvey[labsurvey$julianday != 170, ]
twoorders <- c('LEPL', 'ORTH')
tempvarPR <- meanDensityByDay(surveyData = labsurvey, ordersToInclude = twoorders, 
                             inputYear = '2015', inputSite = 117)
plot(tempvarPR$julianday, tempvarPR$meanDensity, type = 'l', col = "blue")


# Botanical Garden
tempvarBG <- meanDensityByDay(surveyData = visualsurveybg, ordersToInclude = 'LEPL', 
                            inputYear = '2015', inputSite = 8892356) 
plot(tempvarBG$julianday, tempvarBG$meanDensity, type = 'l', col = "blue")




