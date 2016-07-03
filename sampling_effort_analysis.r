source('summary_functions.r')
source('data_cleaning.r')

library(dplyr)
library(rasterImage)

# Get non-beat sheet effort by day
nonBSsurveys = surveys[-grep("BEAT SHEET", surveys$notes),]
visEffortByDay = data.frame(table(nonBSsurveys[, c('site', 'date')]))
names(visEffortByDay) = c('site', 'date', 'numSurveys')
visEffortByDay = visEffortByDay[visEffortByDay$numSurveys!=0, ]
visEffortByDay$date = as.POSIXlt(visEffortByDay$date, format = "%Y-%m-%d")
visEffortByDay$julianday = yday(visEffortByDay$date)
tempyear <- substring(visEffortByDay$date, 1, 4)
visEffortByDay$year = tempyear 

BGeffortByDay = subset(visEffortByDay, site == 8892356 & year == 2015)

amsurvey.BG = amsurvey.bg[-grep("BEAT SHEET", amsurvey.bg$notes.y),]

# Function for subsetting surveydata down to a specified number of count circles
#   surveyData: one of the cleaned datasets produced by data_cleaning.R
#   numCircles: number of survey circles to use in subset
#   circles: if specified, a vector of circles to use for subsetting; if none
#            is specified, then a random set of circles is chosen


dataSubset = function(surveyData, numCircles, circles = NA) {
  if(is.na(circles[1])) {
    circles = sample(1:max(surveyData$circle), numCircles, replace = FALSE)
  }
  outData = subset(surveyData, circle %in% circles)
}

# BG subsampling
BGvisleps = subset(amsurvey.BG, arthCode == 'LEPL')

output = data.frame(circles = NULL, rep = NULL, sampFreq = NULL, maxJD = NULL, JDmean = NULL)
for (cir in 1:8) {
  circCombs = combn(max(amsurvey.bg$circle), cir)
  for (d in 1:6) {
    #Starting on the 2nd sample date since the 1st one during training only has 10 surveys
    sampledates = as.character(fixedEffort$date[seq(2, nrow(fixedEffort), by = d)])
    
    effort = BGeffortByDay[BGeffortByDay$date %in% sampledates, ]
    
    for (i in 1:ncol(circCombs)) {
      circles = circCombs[,i]
      tmp = dataSubset(amsurvey.bg, cir, circles = circles)
      tmp2 = tmp[tmp$date %in% sampledates, ]
      meanByDay = meanDensityByDay(tmp2, effort = effort, minLength = 5,
                                   ordersToInclude = 'LEPL', inputSite = 8892356, inputYear = 2015)
      maxJD = meanByDay$julianday[meanByDay$meanDensity == max(meanByDay$meanDensity)]
      JDmean = sum(meanByDay$julianday * meanByDay$meanDensity) / sum(meanByDay$meanDensity)
      tmpout = data.frame(circles = cir, rep = i, sampFreq = d, maxJD = maxJD, JDmean = JDmean)
      output = rbind(output, tmpout)
    }  
  }
}

foo = output %>% group_by(circles, sampFreq) %>% 
  summarize(maxJD = mean(maxJD, na.rm = T), JDmean = mean(JDmean, na.rm = T))

maxJDmat = matrix(foo$maxJD, nrow = length(unique(foo$circles)), 
                  ncol = length(unique(foo$sampFreq)),
                  byrow = TRUE)
maxJDmat = maxJDmat[nrow(maxJDmat):1,]

JDmeanmat = matrix(foo$JDmean, nrow = length(unique(foo$circles)), 
                  ncol = length(unique(foo$sampFreq)),
                  byrow = TRUE)
JDmeanmat = JDmeanmat[nrow(JDmeanmat):1,]

# Plotting
col1 = 'blue'
col2 = 'red'
colfunc = colorRampPalette(c(col1, col2))
sampPlot = function(sampMatrix, col1, col2, title) {
  par(mgp = c(1.5, 0.3, 0), mar = c(3, 3, 2, 2))
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  image(sampMatrix, ylab = "Sampling frequency (lo -> hi)", 
        xlab = "Number of surveys", main = title, 
        xaxt = "n", yaxt = "n", col = colfunc(20))
  axis(2, seq(0, 1, length.out = 6), labels = 1:6, tck = -.01)
  axis(1, seq(0, 1, length.out = 8), labels = 5*(1:8), tck = -.01, las = 1)
  
  legend_image <- as.raster(matrix(colfunc(20), ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  mtext(seq(round(min(sampMatrix)), round(max(sampMatrix)), length.out=3), 4, 
        at = seq(0,1,l=3), las = 1, line = -1)
  rasterImage(legend_image, 0, 0, 1, 1)
}





foo2 = merge(foo, data.frame(circles = 1:8,
                             maxReps = sapply(1:8, function(x) max(output$rep[output$circles == x]))),
             all.x = T)
foo2$pct = foo2$Freq/foo2$maxReps
foo2$circles = as.numeric(as.character(foo2$circles))
foo2$

plot(output$circles, output$JDmean, pch = 16, xlab = "Number of survey circles")
points(foo2$circles, foo2$maxJD, cex = 3*foo2$pct, col = 'red', pch = 17)