################################################
# Script for reading in functions involving
# Caterpillars Count! data exported from the
# phpMyAdmin project site.

# Load required libraries
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)
library(gsheet)

labgroupusers = c(69, 130, 131, 132, 136, 158, 159, 189, 191) #needs to be edited as new lab members conduct surveys

#--------------------------------------------------------------------------------------------------
# FUNCTIONS


# Function for subsetting data based on survey type and time of survey

surveySubset = function(cleandata, subset = "visual am", minLength = 0) 
  
  {tempdata = cleandata[cleandata$length >= minLength | cleandata$arthCode == 'NONE',]
  
  if (subset == "visual am"){
    visualsurvey = tempdata[tempdata$surveyType=="Visual",]
    amsurvey = visualsurvey[!grepl("REPEAT SURVEY", visualsurvey$notes.x),]
    labsurvey = amsurvey[amsurvey$userID %in% labgroupusers, ] # make more general
    data.out = labsurvey
  } else if (subset == "beat sheet"){
    beatsheet = tempdata[tempdata$surveyType=="Beat_Sheet",]
    data.out = beatsheet
  } else if (subset == "visual pm"){
    repsurvey = tempdata[grep("REPEAT SURVEY", tempdata$notes.x),]
    data.out = repsurvey
  } else if (subset == "volunteer"){
    volsurvey = tempdata[tempdata$userID == 129,] # make more general
    data.out = volsurvey
  }
  return(data.out)
}


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


# Function for calculating mean density, mean biomass, or fraction of surveys 
# with a certain Order per Julian day

# Make sure datasets placed into function do not have minLength already subsetted out (?) need to check on this
meanDensityByDay = function(surveyData, # merged dataframe of surveys and orders tables
                       ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                       byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                       minLength = 0,         # minimum arthropod size to include 
                       inputSite,
                       inputYear,
                       plot = F,
                       plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                       new = T,
                       color = 'black',
                       ...)                  
{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
  temp = subset(surveyData,
                length >= minLength & 
                arthCode %in% ordersToInclude & 
                site %in% inputSite &
                year %in% inputYear)
  
  # Create subset of data to use for calculating effort by day
  pre_temp_foreffort = subset(surveyData,
                              site %in% inputSite &
                                year %in% inputYear)
  temp_foreffort <- unique(pre_temp_foreffort[,c('survey', 'circle', 'date', 'site')])
  
  # Create effort by day within function
  effortByDay = data.frame(table(temp_foreffort[, c('site', 'date')]))
  names(effortByDay) = c('site', 'date', 'numSurveys')
  effortByDay = effortByDay[effortByDay$numSurveys!=0, ]
  effortByDay$date = as.POSIXlt(effortByDay$date, format = "%Y-%m-%d")
  effortByDay$julianday = yday(effortByDay$date)
  tempyear <- substring(effortByDay$date, 1, 4)
  effortByDay$year = tempyear
  
  temp_effort = effortByDay
  
  # When no data is available for parameters set:
  if(nrow(temp) == 0 & !byTreeSpecies) {
    temp3 = temp_effort
    temp3$totalCount = 0
    temp3$numSurveysGTzero = 0
    temp3$totalBiomass = 0

  } else if (nrow(temp) > 0 & !byTreeSpecies) {
    temp2 = ddply(temp, .(site, julianday, year), summarize, 
                  totalCount = sum(count), numSurveysGTzero = length(unique(surveyID[count > 0])), 
                  totalBiomass = sum(biomass))
    temp3 = merge(temp_effort[,c('site','numSurveys','julianday','year')], temp2, 
                  by = c('julianday', 'site', 'year'), all.x = T) 

  } else if (nrow(temp) > 0 & byTreeSpecies) {
    temp2 = ddply(temp, .(site, julianday, year, clean_plantSp), summarize, 
                  totalCount = sum(count), numSurveysGTzero = length(unique(surveyID[count > 0])), 
                  totalBiomass = sum(biomass))
    temp3 = merge(temp_effort[,c('site','numSurveys','julianday','year')], temp2, 
                  by = c('julianday', 'site', 'year'), all.x = T)
  }  
  
  # Calculations shown in dataframe output:
  temp3$totalCount[is.na(temp3$totalCount)] = 0
  temp3$meanDensity = temp3$totalCount/temp3$numSurveys
  temp3$fracSurveys = temp3$numSurveysGTzero / temp3$numSurveys
  temp3$meanBiomass = temp3$totalBiomass/temp3$numSurveys
  temp3$julianday = as.numeric(as.character(temp3$julianday))
  
  # Plotting the chosen variable:
  if (plot & new) {
    plot(temp3$julianday, temp3[, plotVar], type = 'l', 
         col = color, xlab = "Julian day", ...)
    
  # Adding to an existing plot:
  } else if (plot & new==F) {
    points(temp3$julianday, temp3[, plotVar], type = 'l', col = color, ...)
  }
  return(temp3)
}



# ----------------------------------------------------------------------------------------


# Calculate mean density per survey per week (to smooth data)
# Need to include biomass

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
  dataYearSite = surveyData[surveyData$year %in% inputYear & 
                              surveyData$site %in% inputSite, ]
  dataYearSite$week = floor(dataYearSite$julianday/7) + 1
  effortByWeek = data.frame(table(unique(dataYearSite[, c('surveyID', 'week')])$week))
  names(effortByWeek) = c('week', 'numSurveys')
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
  temp = subset(dataYearSite, length >= minLength & arthCode %in% ordersToInclude)

  if (byTreeSpecies) {
    temp2 = ddply(temp, .(week, plantSp), summarize, 
                  totalCount = sum(count))
    
  } else {
    temp2 = ddply(temp, .(week), summarize, 
                  totalCount = sum(count), numSurveysGTzero = length(unique(surveyID[count > 0])))
                  #totalBiomass = sum(biomass))
  }
  
  temp3 = merge(effortByWeek, temp2[, c('week', 'totalCount', 'numSurveysGTzero')], 
                by = 'week', all = T)
  temp3$totalCount[is.na(temp3$totalCount)] = 0
  temp3$meanDensity = temp3$totalCount/temp3$numSurveys
  temp3$fracSurveys = temp3$numSurveysGTzero / temp3$numSurveys
  #temp3$meanBiomass = temp3$totalBiomass/temp3$numSurveys
  temp3$week = as.numeric(as.character(temp3$week))
  if (plot & new) {
    plot(temp3$week, temp3[, plotVar], type = 'l', 
         col = color, xlab = "Week", ...)
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






