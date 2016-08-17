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

# Set working directory
# setwd('c:/git/caterpillars-count-analysis')

# Read in data
tempsurveys = read.csv('data/tbl_surveys.csv', header=F, stringsAsFactors = F)
orders = read.csv('data/tbl_orders.csv', header=F, stringsAsFactors = F)

names(tempsurveys) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                   'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                   'herbivory', 'photo', 'isValid', 'status', 'surveyType',
                   'leafCount', 'source')
names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                  'count', 'photo', 'time', 'isValid')

# Only include valid entries in surveys
surveys = tempsurveys[tempsurveys$isValid == 1,]

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
orders2 = merge(surveys, orders, by = 'surveyID', sort = FALSE)

orders2$date = as.POSIXlt(word(orders2$dateStart, 1, sep = " "), format = "%Y-%m-%d")
orders2$julianday = yday(orders2$date)

orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x', 'surveyType', 'leafCount')]

# Clean arthropod names and then add column with arthopod order code
orders3$arthropod = gsub("Bees and Wasps (Hymenoptera excluding ants)",
                         "Bees and Wasps (Hymenoptera, excluding ants)", orders3$arthropod)
orders3$arthropod = gsub("Leaf Hoppers and Cicadas (Auchenorrhyncha)",
                         "Leaf hoppers and Cicadas (Auchenorrhyncha)", orders3$arthropod)
orders3$arthropod = gsub("Butterflies and Moths (Lepidoptera adult)",
                         "Moths, Butterflies (Lepidoptera)", orders3$arthropod)
orders3$arthropod = gsub("Other (describe in Notes)",
                         "OTHER (describe in Notes)", orders3$arthropod)
orders3$arthropod = gsub("Butterflies and Moths (Lepidoptera adult)",
                         "Moths, Butterflies (Lepidoptera)", orders3$arthropod)
orders3$arthropod = gsub("Unidentified",
                         "UNIDENTIFIED (describe in Notes)", orders3$arthropod)

# These aren't working for some reason...


arthcodes = read.csv('arth_codes.csv', header=T)
arthcodes1 = arthcodes[, c('ArthCode', 'DataName')]
names(arthcodes1) = c('arthCode', 'arthropod')
cleandata <- merge(orders3, arthcodes1, by = 'arthropod', all.x = TRUE, sort = FALSE)
cleandata <- cleandata[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                       'plantSp','herbivory','arthropod','arthCode','length',
                       'count','notes.y','notes.x', 'surveyType', 'leafCount')]
cleandata <- cleandata[order(cleandata$date),]

# Add a column indicating if the leaves were wet
cleandata$wetLeaves = c()
tempwet <- sort(c(grep("wet leaves", cleandata$notes.x), grep("Wet leaves", cleandata$notes.x), 
                  grep("very dewy", cleandata$notes.x), grep("Wet Leaves", cleandata$notes.x)))
cleandata$wetLeaves = rep('no', nrow(cleandata))
cleandata$wetLeaves[tempwet] = 'yes'

# Fix date class
cleandata$date = as.character(cleandata$date)

# Add a year column
cleandata$year = substring(cleandata$date, 1, 4)

# Create list of unique survey events
events = unique(cleandata[, c("surveyID", "userID", "date", "year", "julianday", "site", "circle", "survey")])

# Change arthCodes to 'NONE' that were previously NA
cleandata$arthCode[is.na(cleandata$arthCode)] = "NONE"

# Take out large caterpillar colonies
#cleandata <- cleandata[!(cleandata$arthCode == "LEPL" & cleandata$count > 10),]
# or
#cleandata$count[cleandata$arthCode == "LEPL" & cleandata$count > 5] = 5

# Cleaning beat sheets (PR and BG) and isolating # leaves into a new column

# Beat sheet designation was in notes field prior to 2016
beatsheet_pre2016 <- cleandata[grep("BEAT SHEET", cleandata$notes.x), ] 

# In 2016, surveyType was not getting recorded when entered from website, so 
# beat sheet is designated by either the surveyType field when entered by app or
# by a leaf count that differed from the 50 expected of a visual survey.
# (Participants were told to record 49 or 51 if the number of leaves in a beat
# sheet survey truly was 50.)
beatsheet_post2016 <- cleandata[((cleandata$leafCount != "50") & (cleandata$year>= "2016")) | (cleandata$surveyType=="Beat_Sheet"),] 

# Pull out the leafCount from the notes
leavesNumTemp0 <- word(beatsheet_pre2016$notes.x, -1, sep = "BEAT SHEET; ")
leavesNumTemp <- word(leavesNumTemp0, -1, sep = "= ")
leavesNumTemp1 <- word(leavesNumTemp, -1, sep = "Leaves  ")
leavesNumTemp2 <- word(leavesNumTemp1, -1, sep = "Leaves=")
leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")
leavesNumTemp5 <- gsub(" ", "", leavesNumTemp4)
leavesNumTemp6 <- gsub("\n", "", leavesNumTemp5)
leavesNumTemp7 <- gsub("Unknown", NA, leavesNumTemp6)
leavesNumTemp8 <- gsub("unknown", NA, leavesNumTemp7)
beatsheet_pre2016$leafCount <- as.numeric(leavesNumTemp8)
beatsheet<-rbind(beatsheet_pre2016, beatsheet_post2016)
beatsheet$surveyType <- "Beat_Sheet"
cleandata2 <- cleandata[!cleandata$surveyID %in% beatsheet$surveyID, ]
cleandata <- rbind(cleandata2, beatsheet)
cleandata$surveyType[cleandata$surveyType != "Beat_Sheet"] <- "Visual"




#-----------------------------------------------------------------------------------------------------------------

## Calculating biomass and adding this as a column to the cleandata

# Source summary_functions.r if have not already

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log(a)
reg.data.temp <- read.csv('arth_regression.csv', header = T, sep = ',')
# Calculate a (the coefficient)
reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)

# Create list of arthropod orders (by code)
arthlist <- arthcodes$ArthCode[arthcodes$ArthCode %in% reg.data.temp$arthCode]) # arthcodes from data_cleaning.R

# Merge reg.data.temp and arthlist so NAs will be calculated
reg.data <- merge(reg.data.temp, arthcodes, by.x = 'arthCode', by.y = 'ArthCode', all = T)

# Create empty biomass vector
cleandata$biomass = NA

# For loop for calculating biomass for each observation
for (ord in arthlist) {
  b = reg.data$slope[reg.data$arthCode == ord]
  a = reg.data$coefficient[reg.data$arthCode == ord]
  biomass = (a*(cleandata$length[cleandata$arthCode == ord])^b)*(cleandata$count[cleandata$arthCode == ord])
  cleandata$biomass[cleandata$arthCode == ord] <- biomass
}

# Orders with regression data:
regorders <- as.vector(reg.data.temp$arthCode)

#Removing exclosure trees (only 2016)    
cleandata <-filter(cleandata, !(grepl("EXCLOSURE", notes.x)))

# Subsetting cleandata now that it has the biomass column included
cleandata.pr <- cleandata[cleandata$site == 117 & cleandata$year == 2015,]
cleandata.bg <- cleandata[cleandata$site == 8892356 & cleandata$year == 2015,]

amsurvey.pr <- surveySubset(cleandata.pr, subset = "visual am", minLength = 5)
pmsurvey.pr <- surveySubset(cleandata.pr, subset = "visual pm", minLength = 5)
beatsheet.pr <- surveySubset(cleandata.pr, subset = "beat sheet", minLength = 5)
volunteer.pr <- surveySubset(cleandata.pr, subset = "volunteer", minLength = 5)

amsurvey.bg <- surveySubset(cleandata.bg, subset = "visual am", minLength = 5)
beatsheet.bg <- surveySubset(cleandata.bg, subset = "beat sheet", minLength = 5)

