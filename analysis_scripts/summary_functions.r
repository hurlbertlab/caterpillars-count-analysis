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
         col = color, ...)
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
    write.csv(data, paste('data/arthropods/frass_', Sys.Date(), '.csv', sep = ''),
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
#   minReliability is the minimum reliability score for including in the analysis.
#    3 - reliable, no obvious problems
#    2 - frass traps wet, or potential minor issues
#    1 - major problems, unreliable frass data


frassplot = function(frassdata, inputSite, year, color = 'black', new = T, 
                     var = 'mass', minReliability = 0, xlab = 'Julian day', ylab = '', ...) {
  
  temp = filter(frassdata, site == inputSite, Year == year, reliability >= minReliability) %>%
    data.frame()
  
  if (new) {
    plot(temp$jday, temp[, var], xlab = xlab, ylab = ylab,
         type = 'l', col = color, ...)
  } else {
    points(temp$jday, temp[, var], type = 'l', col = color, ...)
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


# Function for fitting a Gaussian curve to various subsets of the data, where one
# can vary the frequency of sampling as well as the number of survey circles per date.

effortAnalysis = function(surveyData, ordersToInclude = "LEPL", inputYear, inputSite,
                          jdRange = c(1,365), outlierCount = 10000, plotVar = 'fracSurveys',
                          
                          minFreq,          # minimum survey date frequency to subset, i.e., every
                          #                  "minFreq"-th survey date
                          numCircles,       # a vector of survey effort levels based on number of circles
                          #                  to subset for phenology estimation
                          plot = F,
                          
                          plotFilename = NULL, 
                          
                          seed = 1)
{
  
  set.seed(seed)
  
  colors = rainbow(minFreq)[minFreq:1]
  p = 0
  
  if (plot) {
    pdf(plotFilename, height = 10.5, width = 8)
    par(mfrow = c(5, 3), mar = c(4, 4, 2, 1), oma = c(4, 4, 0, 0), cex.main = .85)
  }
  
  output = data.frame(freq = NULL, n = NULL, start = NULL, circles = NULL, rep = NULL, maxJD = NULL,
                      mu = NULL, sig = NULL, scale = NULL, totalDensity = NULL, col = NULL)
  
  maxcirclenum = max(surveyData$circle)
  
  for (i in 1:minFreq) {
    
    # In order to represent the same number of phenology estimates per combination
    # of survey frequency and circle number (e.g. 60), the number of sampling events
    # per survey frequency/circle number/start date will need to decline with frequency.
    
    # freq 1 * 60 samples
    # freq 2 * 30
    # freq 3 * 20
    # freq 4 * 15
    # freq 5 * 12
    
    reps = 5*maxcirclenum/i
    
    for (j in 1:i) {
      
      for (cir in numCircles) {
        
        # All combinations of cir circles out of 12
        allcombos = combn(maxcirclenum, cir)
        
        if (ncol(allcombos) < reps) {
          combos = allcombos 
        } else {
          combos = allcombos[, sample(1:ncol(allcombos), reps)]
        }
        
        for (rep in 1:ncol(combos)) {
          
          circlesubset = combos[, rep]
          
          densByDay = meanDensityByDay(filter(surveyData, circle %in% circlesubset), 
                                       ordersToInclude = ordersToInclude, inputYear = inputYear, 
                                       inputSite = inputSite, jdRange = jdRange, 
                                       outlierCount = outlierCount, plot = F, plotVar = plotVar)
          
          numSurveyDates = length(densByDay$julianday[densByDay$julianday >= min(jdRange) & 
                                                        densByDay$julianday <= max(jdRange)])
          series = densByDay %>%  
            
            slice(seq(j, numSurveyDates, by = i)) %>%
            
            data.frame()
          
          # Fit Gaussian
          gfit = tryCatch({
            
            fitG(series$julianday, series[, plotVar], 
                 weighted.mean(series$julianday, series[, plotVar]),
                 14, 200)
            
          }, error = function(err) {
            
            gfitFail = list(par = c(NA, NA, NA))
            
            return(gfitFail)
            
          }) # end tryCatch
          
          
          if (is.na(gfit$par[1])) {
            R2 = NA
          } else {
            series$predval = gfit$par[3]*dnorm(series$julianday, gfit$par[1], gfit$par[2])
            lm1 = lm(series$predval ~ series[, plotVar])
            R2 = round(summary(lm1)$r.squared, 3)
          }
          
          
          # Calculate total number of arthropods in the subset of surveys
          totalDensity = surveyData %>% 
            
            filter(circle %in% circlesubset, year == inputYear,
                   site == inputSite, arthCode %in% ordersToInclude,
                   julianday >= min(jdRange), julianday <= max(jdRange),
                   count <= 30) %>%
            
            summarize(total = sum(count))
          
          # Results  
          temp = data.frame(freq = i,
                            n = nrow(series),
                            start = j,
                            circles = cir,
                            rep = rep,
                            maxJD = series$julianday[series[, plotVar] == max(series[, plotVar])],
                            mu = gfit$par[1],
                            sig = gfit$par[2],
                            scale = gfit$par[3],
                            R2 = R2,
                            totalDensity = totalDensity,
                            col = colors[i])
          
          output = rbind(output, temp)
          
          if(plot) {
            plot(series$julianday, series[, plotVar], pch = 16, cex = 2, 
                 xlim = jdRange, ylim = c(0, 1.3*max(series[, plotVar])), 
                 col = 'red', xlab = "", ylab = "", main = 
                   paste("freq=", i, ", start=", j, ", circles=", cir, ", rep=", rep, sep = ""))
            legend("topleft", paste("R2 =", R2), bty = 'n')
            
            p = p + 1
            
            if (!is.na(temp$mu)) {
              lines(jdRange[1]:jdRange[2], 
                    temp$scale*dnorm(jdRange[1]:jdRange[2], temp$mu, temp$sig), 
                    col = 'blue', lwd = 1)
              abline(v = temp$mu, lty = 'dotted')
            }
            if (p %% 15 == 0) {
              mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
              mtext(plotVar, 2, outer = T, line = 1, cex = 1.5)
            }
          } #end if plot
          
          
        } #end rep loop
      } #end cir loop
    } #end j loop
  } #end i loop
  
  if(plot) { dev.off() }
  
  return(output)
  
} #end function






