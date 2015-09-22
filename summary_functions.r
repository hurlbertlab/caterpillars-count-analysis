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


#--------------------------------------------------------------------------------------------------
# FUNCTIONS

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


# Calculate mean density per survey per julian day

meanDensityByDay = function(surveyData,            # merged dataframe of surveys and orders tables
                       ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                       byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                       minLength = 0,         # minimum arthropod size to include
                       inputYear,
                       inputSite, 
                       plot = F,
                       plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys'
                       new = T,
                       color = 'black',
                       ...)                  
  
  {
  dataYearSite = surveyData[surveyData$year == inputYear & surveyData$site == inputSite, ]
  effortByDay = data.frame(table(unique(dataYearSite[, c('surveyID', 'julianday')])$julianday))
  names(effortByDay) = c('julianday', 'numSurveys')
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
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
                  totalCount = sum(count), numSurveysGTzero = length(unique(surveyID[count > 0])))
  }
  
  temp3 = merge(effortByDay, temp2[, c('julianday', 'totalCount', 'numSurveysGTzero')], 
                by = 'julianday', all = T)
  temp3$totalCount[is.na(temp3$totalCount)] = 0
  temp3$meanDensity = temp3$totalCount/temp3$numSurveys
  temp3$fracSurveys = temp3$numSurveysGTzero / temp3$numSurveys
  temp3$julianday = as.numeric(as.character(temp3$julianday))
  if (plot & new) {
    plot(temp3$julianday, temp3[, plotVar], type = 'l', 
         col = color, xlab = "Julian day", ylab = plotVar, ...)
  } else if (plot & new==F) {
    points(temp3$julianday, temp3[, plotVar], type = 'l', col = color, ...)
  }
  return(temp3)
}


# Calculate mean density per survey per week (to smooth data)

meanDensityByWeek = function(surveyData,            # merged dataframe of surveys and orders tables
                            ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                            byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                            minLength = 0,         # minimum arthropod size to include
                            inputYear,
                            inputSite, 
                            plot = F,
                            plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys'
                            new = T,
                            color = 'black',
                            ...)                  
  
{
  dataYearSite = surveyData[surveyData$year == inputYear & surveyData$site == inputSite, ]
  dataYearSite$week = floor(dataYearSite$julianday/7) + 1
  effortByWeek = data.frame(table(unique(dataYearSite[, c('surveyID', 'week')])$week))
  names(effortByWeek) = c('week', 'numSurveys')
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
  temp = subset(dataYearSite, length >= minLength & arthCode %in% ordersToInclude)

  if (byTreeSpecies) {
    temp2 = ddply(temp, .(site, week, year, plantSp), summarize, 
                  totalCount = sum(count))
    
  } else {
    temp2 = ddply(temp, .(site, week, year), summarize, 
                  totalCount = sum(count), numSurveysGTzero = length(unique(surveyID[count > 0])))
  }
  
  temp3 = merge(effortByWeek, temp2[, c('week', 'totalCount', 'numSurveysGTzero')], 
                by = 'week', all = T)
  temp3$totalCount[is.na(temp3$totalCount)] = 0
  temp3$meanDensity = temp3$totalCount/temp3$numSurveys
  temp3$fracSurveys = temp3$numSurveysGTzero / temp3$numSurveys
  temp3$week = as.numeric(as.character(temp3$week))
  if (plot & new) {
    plot(temp3$week, temp3[, plotVar], type = 'l', 
         col = color, xlab = "Week", ylab = plotVar, ...)
  } else if (plot & new==F) {
    points(temp3$week, temp3[, plotVar], type = 'l', col = color, ...)
  }
  return(temp3)
}


# Function for reading in frass data from GoogleDoc
# *if aim is to backup GoogleDoc and write to disk only, then open =F and write = T
# *if aim is to use data without writing to disk, then open = T and write = F
frassData = function(open = F, write = F) {
  require(gsheet)
  url = "https://docs.google.com/spreadsheets/d/1RwXzwhHUbP0m5gKSOVhnKZbS1C_NrbdfHLglIVCzyFc/edit#gid=1479231778"
  data = gsheet2tbl(url)
  
  if (write) {
    # Write a copy
    write.csv(data, paste('data/frass_', Sys.Date(), '.csv', sep = ''),
              row.names = F)
  }
  if (open) { return (data) }
}


# Function that takes a date field (formatted as %m/%d/%Y) and a time field
# (hh:mm in 24h time), converts the date to julian day and adds the fractional
# day represented by the hours and minutes
julianDayTime = function(date, hour_min) {
  require(lubridate)
  jday = yday(date)
  temp = sapply(strsplit(hour_min, ":"), function(x) {
    x = as.numeric(x)
    x[1] + x[2]/60
  })
  output = jday + temp/24
  return(output)
}


#--------------------------------------------------------------------------------

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

#################
# Prairie Ridge #
#################

# Change arthCodes to 'NONE' that were previously NA
# Make sure data entered correctly before doing this
labsurvey$arthCode[is.na(labsurvey$arthCode)] = "NONE"
beatsheet$arthCode[is.na(beatsheet$arthCode)] = "NONE"
repsurvey$arthCode[is.na(repsurvey$arthCode)] = "NONE"
volsurvey$arthCode[is.na(volsurvey$arthCode)] = "NONE"

# Two possibilities of dealing with caterpillar colony outliers (choose one)
# (Rerun the separating data section in between switching possibilities)

# #1. Reducing counts with more than 5 caterpillars to 5 caterpillars:
#labsurvey$count[labsurvey$arthCode == "LEPL" & labsurvey$count > 5] = 5
#beatsheet$count[beatsheet$arthCode == "LEPL" & beatsheet$count > 5] = 5
#labsurvey$count[labsurvey$arthCode == "LEPL" & labsurvey$count > 5] = 5
#labsurvey$count[labsurvey$arthCode == "LEPL" & labsurvey$count > 5] = 5

# #2. Taking out large colonies completely:
labsurvey <- labsurvey[!(labsurvey$arthCode == "LEPL" & labsurvey$count > 10),]
beatsheet <- beatsheet[!(beatsheet$arthCode == "LEPL" & beatsheet$count > 10),]
repsurvey <- repsurvey[!(repsurvey$arthCode == "LEPL" & repsurvey$count > 10),]
volsurvey <- volsurvey[!(volsurvey$arthCode == "LEPL" & volsurvey$count > 10),]

#-----------------------------------------------------------------------------------------------------------------
# Plot our morning surveys, our beat sheet surveys, our repeat surveys, and the volunteer surveys all on one graph
# Caterpillars only, mean density
PRam.lepl = meanDensityByDay(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs.lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm.lepl = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol.lepl = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Caterpillars only, fraction of surveys with at least one caterpillar
PRam.1lepl = meanDensityByDay(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = T, color = 'blue', xlim = c(135, 250))
PRbs.1lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'orange')
PRpm.1lepl = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red')
PRvol.1lepl = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
PRam.all = meanDensityByDay(labsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm.all = meanDensityByDay(repsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol.all = meanDensityByDay(volsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult = meanDensityByDay(labsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, 
                             xlim = c(135, 250), ylim = c(0.1, 1))
PRbs.mult = meanDensityByDay(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult = meanDensityByDay(repsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult = meanDensityByDay(volsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
#----------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
# Plots as above but averaged per week instead of by day

# Caterpillars only, mean density
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Caterpillars only, fraction of surveys with at least one caterpillar
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = T, color = 'blue', xlim = c(20, 35), ylim = c(0, 0.2))
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'orange')
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red')
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
PRam.all = meanDensityByWeek(labsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs.all = meanDensityByWeek(beatsheet, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm.all = meanDensityByWeek(repsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol.all = meanDensityByWeek(volsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult = meanDensityByWeek(labsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, xlim = c(20, 35),
                             ylim = c(0.1, 1))
PRbs.mult = meanDensityByWeek(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult = meanDensityByWeek(repsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult = meanDensityByWeek(volsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                              plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
#----------------------------------------------------------------------------------------------------------------------------


# Working with frass data
frass = frassData(open = T)

# Convert date class
frass$Date.Set = as.Date(frass$Date.Set, format = "%m/%d/%Y")
frass$Date.Collected = as.Date(frass$Date.Collected, format = "%m/%d/%Y")
frass$jday.Set = julianDayTime(frass$Date.Set, frass$Time.Set)
frass$jday.Collected = julianDayTime(frass$Date.Collected, frass$Time.Collected)

frass$frass.mg.d = frass$Frass.mass..mg./(frass$jday.Collected - frass$jday.Set)

frass$jday = floor(frass$jday.Collected)
frass$week = floor(frass$jday/7) + 1

meanFrassByDay = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$jday), function(x) mean(x, na.rm=T))
meanFrassByWeek = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$week), function(x) mean(x, na.rm=T))
names(meanFrassByWeek) = c('site', 'week', 'frass.mg.d')

PRfrassW = subset(meanFrassByWeek, site == "Prairie Ridge")

frassplot = function(site, frassdata, color = 'black', new = T) {
  temp = subset(frassdata, Group.1 == site)
  if (new) {
    plot(temp$Group.2, temp$x, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'b', col = color, ylim = range(frassdata$x), xlim = range(frassdata$Group.2))
  } else {
    points(temp$Group.2, temp$x, type = 'b', col = color)
  }
}




# Prairie Ridge fraction of surveys with caterpillars plot
pdf('plots/PR_LEPL_frac_by_week_wFrass.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2)
plot(c(20,35), c(0, 0.24), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

par(new = T)
plot(PRfrassW$week, PRfrassW$frass.mg.d, type = 'b', col = 'darkgreen', lwd = 1, xlim = c(20, 35), ylim = c(1,7),
     xlab = "", xaxt = "n", ylab = "", yaxt = "n")
axis(4, 1:7, cex.axis = 1.2)
mtext("Frass (mg/d)", 4, line = 2.5, cex = 1.5)
legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers', 'Frass'),
       lwd = c(3, 3, 3, 3, 1), lty = c(rep('solid', 3), 'dashed', 'solid'),
       col = c('blue', 'skyblue', 'red', 'red', 'darkgreen'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()

# As above but without frass
pdf('plots/PR_LEPL_frac_by_week.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2)
plot(c(20,35), c(0, 0.24), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers'),
       lwd = c(3, 3, 3, 3), lty = c(rep('solid', 3), 'dashed'),
       col = c('blue', 'skyblue', 'red', 'red'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()





# Merge each of the subsets above together
PRall1 = merge(PRam[,c('julianday','meanDensity')], PRbs[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall1) = c('julianday','density_am','density_bs')
PRall2 = merge(PRall1, PRpm[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall2)[4] = 'density_pm'
PRall = merge(PRall2, PRvol[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall)[5] = 'density_vol'

####################
# Botanical Garden #
####################

BG = meanDensityByDay(visualsurveybg, 'LEPL', inputYear = '2015', inputSite = 8892356,
                      plot = T, new = T, color = 'black') 




