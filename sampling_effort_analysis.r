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
PReffortByDay = subset(visEffortByDay, site == 117 & year == 2015)

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

# Function for subsetting survey data to specified levels of effort
# (both number of surveys, and frequency of sampling).
# It is assumed that surveyData and effortByDay have already been subsetted down to the 
# desired site and year.
effortSampling = function(surveyData, effortByDay, arthOrder, maxCircles, maxFreq) {
  
  output = data.frame(circles = NULL, rep = NULL, sampFreq = NULL, 
                      sampFreqRep = NULL, maxJD = NULL, JDmean = NULL)
  
  for (cir in 1:maxCircles) { # survey circle effort loop
    circCombs = combn(max(surveyData$circle), cir)

    for (d in 1:maxFreq) { # sampling frequency loop

      for (e in 1:(d-1)) { # alternative starting date for a given freq loop
        # Starting on the 2nd sample date since the 1st one at NCBG
        # during training only has 10 surveys
        dateseq = seq(e+1, nrow(effortByDay), by = d)
        sampledates = as.character(effortByDay$date[dateseq])
        
        effort = effortByDay[effortByDay$date %in% sampledates, ]
        
        for (i in 1:ncol(circCombs)) {
          circles = circCombs[,i]
          tmp = subset(surveyData, circles %in% circles)
          tmp2 = tmp[tmp$date %in% sampledates, ]
          meanByDay = meanDensityByDay(tmp2, effort = effort, minLength = 5,
                                       ordersToInclude = arthOrder, 
                                       inputSite = surveyData$site[1],
                                       inputYear = surveyData$year[1])
          maxJD = meanByDay$julianday[meanByDay$meanDensity == max(meanByDay$meanDensity)]
          JDmean = sum(meanByDay$julianday * meanByDay$meanDensity) / sum(meanByDay$meanDensity)
          tmpout = data.frame(circles = cir, 
                              rep = i, 
                              sampFreq = d, 
                              sampFreqRep = e,
                              maxJD = maxJD, 
                              JDmean = JDmean)
          output = rbind(output, tmpout)
        }  
      }  
    }
    
  }
  
  foo = output %>% group_by(circles, sampFreq) %>% 
    summarize(maxJD = mean(maxJD, na.rm = T), JDmean = mean(JDmean, na.rm = T))
  return(foo)
}

write.csv(foo, 'BG_sampling_effort_stats.csv', row.names = F)

maxJDmat = matrix(foo$maxJD, nrow = length(unique(foo$circles)), 
                  ncol = length(unique(foo$sampFreq)),
                  byrow = TRUE)

JDmeanmat = matrix(foo$JDmean, nrow = length(unique(foo$circles)), 
                  ncol = length(unique(foo$sampFreq)),
                  byrow = TRUE)

# Deviations from most frequent, intense sampling
maxJDmat1 = maxJDmat - maxJDmat[8, 6]     #check, is this the right value?
JDmeanmat1 = JDmeanmat - JDmeanmat[8, 6]     #check, is this the right value?

divergecol = function(values, bins, neg_col, pos_col) {
  col5 = colorRampPalette(c(neg_col, 'white', pos_col))
  color_levels = bins
  max_absolute_value = max(abs(values), na.rm = TRUE)
  color_sequence=seq(-max_absolute_value,max_absolute_value,length.out=color_levels+1)
}

# Plotting
pos_col = 'blue'
neg_col = 'red'
colRamp = colorRampPalette(c(neg_col, 'white', pos_col))
colfunc = colorRampPalette(c(neg_col, pos_col))

# Function for plotting matrix of raw values
sampPlot = function(sampMatrix, col1, col2, title) {
  par(mgp = c(1.5, 0.3, 0), mar = c(3, 3, 2, 2))
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  image(sampMatrix, ylab = "Sampling frequency (#/month)", 
        xlab = "Number of surveys", main = title, 
        xaxt = "n", yaxt = "n", col = colfunc(20))
  axis(2, seq(0, 1, length.out = 6), labels = round(8/(6:1), 1), tck = -.01)
  axis(1, seq(0, 1, length.out = 8), labels = 5*(1:8), tck = -.01, las = 1)
  
  legend_image <- as.raster(matrix(colfunc(20), ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  mtext(seq(round(max(sampMatrix)), round(min(sampMatrix)), length.out=3), 4, 
        at = seq(0,1,l=3), las = 1, line = -1)
  rasterImage(legend_image, 0, 0, 1, 1)
}

# Function for plotting matrix of deviations, with divergent color ramp from 0
sampPlot2 = function(sampMatrix, colRamp, bins, title, ...) {
  max_absolute_value = max(abs(sampMatrix), na.rm = TRUE)
  color_sequence=seq(-max_absolute_value,max_absolute_value, length.out=bins+1)
  
  n_in_class=hist(sampMatrix, breaks=color_sequence, plot=F)$counts>0
  col_to_include=min(which(n_in_class==T)):max(which(n_in_class==T))
  breaks_to_include=min(which(n_in_class==T)):(max(which(n_in_class==T))+1)
    
  par(mgp = c(2, 0.3, 0), mar = c(4, 4, 2, 2), ...)
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  image(sampMatrix, ylab = "Sampling frequency (#/month)", 
        xlab = "Number of surveys", main = title, 
        xaxt = "n", yaxt = "n", col = colRamp(bins)[col_to_include],
        breaks=color_sequence[breaks_to_include])
  axis(2, seq(0, 1, length.out = 6), labels = round(8/(6:1), 1), tck = -.01)
  axis(1, seq(0, 1, length.out = 8), labels = 5*(1:8), tck = -.01, las = 1)
  
  legend_image <- as.raster(matrix(colRamp(bins)[rev(col_to_include)], ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  
  if (max(sampMatrix) - min(sampMatrix) <= 6) {
    labels = round(min(sampMatrix)):round(max(sampMatrix))
  } else {
    labels = seq(round(min(sampMatrix)), round(max(sampMatrix)), by = 2)
  }
  mtext(labels, 4, 
        at = (labels - round(min(sampMatrix)))/
                (round(max(sampMatrix)) - round(min(sampMatrix))),
        las = 1, line = -1)
  rasterImage(legend_image, 0, 0, 1, 1)
}


pdf('sampling_effort.pdf', height = 5, width = 8)
par(mfrow = c(2,1))
sampPlot(JDmeanmat, col1, col2, "Abundance centroid date")
sampPlot(maxJDmat, col1, col2, "Max abundance date")
dev.off()

pdf('sampling_effort_deviations.pdf', height = 5, width = 8)
par(mfrow = c(2,1))
sampPlot2(JDmeanmat1, colRamp, 30, "Abundance centroid deviation", cex.axis = 1.25, cex.lab = 2)
sampPlot2(maxJDmat1, colRamp, 30, "Max abundance date deviation", cex.axis = 1.25, cex.lab = 2)
dev.off()


