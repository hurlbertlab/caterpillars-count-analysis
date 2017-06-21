################################################
# Script for reading in functions involving
# Caterpillars Count! data exported from the
# phpMyAdmin project site.

# Load required libraries
library(dplyr)
library(lubridate)
library(stringr)
library(gsheet)

#--------------------------------------------------------------------------------------------------
# FUNCTIONS

# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}



# Function for subsetting data based on survey type and time of survey

surveySubset = function(cleandata, subset = "visual am", minLength = 0) 
  
  {tempdata = cleandata[cleandata$length >= minLength | cleandata$arthCode == 'NONE',]
  
  if (subset == "visual am"){
    visualsurvey = tempdata[tempdata$surveyType=="Visual",]
    amsurvey = visualsurvey[!grepl("REPEAT SURVEY", visualsurvey$sitenotes),]
    labsurvey = amsurvey[amsurvey$userID %in% labgroupusers, ] # make more general
    data.out = labsurvey
  } else if (subset == "beat sheet"){
    beatsheet = tempdata[tempdata$surveyType=="Beat_Sheet",]
    data.out = beatsheet
  } else if (subset == "visual pm"){
    repsurvey = tempdata[grep("REPEAT SURVEY", tempdata$sitenotes),]
    data.out = repsurvey
  } else if (subset == "volunteer"){
    volsurvey = tempdata[!tempdata$userID %in% labgroupusers, ] # make more general
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


# Function for calculating mean density over a window of time (julian days and/or years)
# requires data_cleaning.R to have been run (specifically needs cleandata2 and labgroupusers)

meanDensity = function(siteID, years, lab = TRUE, ordersToInclude = 'LEPL', survType = 'Visual', 
                       minLength = 5, outlierDensity = 30, jdayRange = c(1, 365), circles = 1:12) {
  
  foo2 = cleandata2 %>% 
    filter(site == siteID,
           year %in% years,
           !grepl("REPEAT SURVEY", sitenotes), # For excluding afternoon repeat surveys by us in 2015
           surveyType == survType,
           count < outlierDensity,
           julianday >= min(jdayRange),
           julianday <= max(jdayRange),
           circle %in% circles)
  
  if(lab) {
    foo = filter(foo2, userID %in% labgroupusers)
  } else {
    foo = filter(foo2, !userID %in% labgroupusers)
  }
  
  num_surveys = length(unique(foo$surveyID))
  
  num_bugs = sum(foo$count[foo$arthCode %in% ordersToInclude & foo$length >= minLength])
  
  return(c(num_bugs, num_surveys, num_bugs / num_surveys))
  
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
                       jdRange = c(1,365),
                       outlierCount = 10000,
                       plot = F,
                       plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                       new = T,
                       color = 'black',
                                              ...)                  

{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
  firstFilter = surveyData %>%
    filter(site %in% inputSite, year %in% inputYear, 
           julianday >= jdRange[1], julianday <= jdRange[2])
  
  effortByDay = firstFilter %>%
    distinct(surveyID, julianday) %>%
    count(julianday)
  
  arthCount = firstFilter %>%
    filter(length >= minLength, 
           count < outlierCount, 
           arthCode %in% ordersToInclude) %>%
    group_by(julianday) %>%
    summarize(totalCount = sum(count, na.rm = T),
              totalBiomass = sum(biomass, na.rm = T),
              numSurveysGTzero = length(unique(surveyID[count > 0]))) %>% 
    right_join(effortByDay, by = 'julianday') %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, totalBiomass = 0, numSurveysGTzero = 0) %>%
    mutate(meanDensity = totalCount/n,
           meanBiomass = totalBiomass/n,
           fracSurveys = 100*numSurveysGTzero/n) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianday, arthCount[, plotVar], type = 'l', 
         col = color, xlab = "Julian day", ...)
  } else if (plot & new==F) {
    points(arthCount$julianday, arthCount[, plotVar], type = 'l', col = color, ...)
  }
  return(arthCount)
}

  

# ----------------------------------------------------------------------------------------


# Calculate mean density per survey per week (to smooth data)
# Need to include biomass

meanDensityByWeek = function(surveyData,            # merged dataframe of surveys and orders tables
                            ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                            minLength = 5,         # minimum arthropod size to include
                            inputYear,
                            inputSite, 
                            jdRange = c(1,365),
                            outlierCount = 10000,
                            plot = F,
                            plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys'
                            new = T,
                            color = 'black',
                            ...)                  
  
{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$arthCode)
  }
  
  firstFilter = surveyData %>%
    filter(site %in% inputSite, year %in% inputYear, 
           julianday >= jdRange[1], julianday <= jdRange[2]) %>%
    mutate(week = floor(julianday/7) + 1)
  
  effortByWeek = firstFilter %>%
    distinct(surveyID, week) %>%
    count(week)
  
  arthCount = firstFilter %>%
    filter(length >= minLength, 
           count < outlierCount, 
           arthCode %in% ordersToInclude) %>%
    group_by(week) %>%
    summarize(totalCount = sum(count, na.rm = T),
              totalBiomass = sum(biomass, na.rm = T),
              numSurveysGTzero = length(unique(surveyID[count > 0]))) %>% 
    right_join(effortByWeek, by = 'week') %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, totalBiomass = 0, numSurveysGTzero = 0) %>%
    mutate(meanDensity = totalCount/n,
           meanBiomass = totalBiomass/n,
           fracSurveys = 100*numSurveysGTzero/n,
           JDweek = (week - 1)*7 + 4) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$JDweek, arthCount[, plotVar], type = 'l', 
         col = color, xlab = "Week", ...)
  } else if (plot & new==F) {
    points(arthCount$JDweek, arthCount[, plotVar], type = 'l', col = color, ...)
  }
  return(arthCount)
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


# Function for plotting frass phenology

frassplot = function(frassdata, site, year, color = 'black', new = T, var = 'mass',...) {
  temp = filter(frassdata, Site == site, Year == year)
  if (new & var == 'mass') {
    plot(temp$jday, temp$mass, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'l', col = color, ...)
  } else if (!new & var == 'mass') {
    points(temp$jday, temp$mass, type = 'l', col = color, ...)
  } else if (new & var == 'density') {
    plot(temp$jday, temp$density, xlab = "Julian day", ylab = "Mean frass (no. / trap / day)",
         type = 'l', col = color, ...)
  } else if (!new & var == 'density') {
    points(temp$jday, temp$density, type = 'l', col = color, ...)
  }
}



# Function for fitting a Gaussian curve
#   mu:   mean
#   sig:  SD
#   scale: scale parameter
fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))
}



#--------------------------------------------------------------------------------






