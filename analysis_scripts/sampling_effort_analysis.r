source('analysis_scripts/summary_functions.r')
source('cleaning_scripts/data_cleaning.r')

library(dplyr)
library(rasterImage)


fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))
}



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
  
  for (i in 1:minFreq) {
    
    # In order to represent the same number of phenology estimates per combination
    # of survey frequency and circle number (e.g. 60), the number of sampling events
    # per survey frequency/circle number/start date will need to decline with frequency.
    
    # freq 1 * 60 samples
    # freq 2 * 30
    # freq 3 * 20
    # freq 4 * 15
    # freq 5 * 12
    
    reps = 60/i

    for (j in 1:i) {
      
      for (cir in numCircles) {
        
        # All combinations of cir circles out of 12
        allcombos = combn(12, cir)
        
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




beg_jd15 = 138
end_jd15 = 210
beg_jd16 = 130
end_jd16 = 205

##########
# STEP 1: Visualize finest grain phenology pattern using meanDensityByDay

PR.LEPL15.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 7, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '2015, Visual', col = 'blueviolet')
# upon inspection, use jdRange = c(135, 194)

PR.LEPL16.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 7, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '2016, Visual', col = 'blueviolet')
# upon inspection, use jdRange = c(132, 189)


PR.LEPL15.bsday = meanDensityByDay(beatsheet.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 7, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '2015, Beat sheet', col = 'blueviolet')
# upon inspection, use jdRange = c(138, 197)


PR.LEPL16.bsday = meanDensityByDay(beatsheet.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 7, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '2016, Beat sheet', col = 'blueviolet')
# upon inspection, use jdRange = c(137, 180); very non-Gaussian!

  
##########
# STEP 2: conduct full sampling effort analysis

lep15vis = effortAnalysis(amsurvey.pr, inputYear = 2015, inputSite = 117,
                       minFreq = 5, numCircles = c(12, 10, 8, 6, 4, 2), jdRange = c(135, 194),
                       plot = T, plotFilename = 'output/plots/lep15vis_effort.pdf', seed = 1)

lep16vis = effortAnalysis(amsurvey.pr, inputYear = 2016, inputSite = 117, 
                          minFreq = 5, numCircles = c(12, 10, 8, 6, 4, 2), jdRange = c(132, 189),
                          plot = T, plotFilename = 'output/plots/lep16vis_effort.pdf')

lep15bs = effortAnalysis(beatsheet.pr, inputYear = 2015, inputSite = 117, 
                          minFreq = 5, numCircles = c(12, 10, 8, 6, 4, 2), jdRange = c(138, 197),
                          plot = T, plotFilename = 'output/plots/lep15bs_effort.pdf')

lep16bs = effortAnalysis(beatsheet.pr, inputYear = 2016, inputSite = 117, 
                          minFreq = 5, numCircles = c(12, 10, 8, 6, 4, 2), jdRange = c(137, 180),
                          plot = T, plotFilename = 'output/plots/lep16bs_effort.pdf')

##########
# STEP 3: Summarize and visualize results

full.mu = lep15bs$mu[lep15bs$circles == 12 & lep15bs$freq == 1]
full.peak = lep15bs$maxJD[lep15bs$circles == 12 & lep15bs$freq == 1]
lep15bs$dev = lep15bs$mu - full.mu

lep15bs.sum = lep15bs %>%
  filter(mu > 100, mu < 200, R2 > 0.2) %>%
  group_by(freq, circles, col) %>% 
  summarize(mu.mean = mean(mu, na.rm = T),
            mu.sd = sd(mu, na.rm = T),
            sig = mean(sig, na.rm = T),
            scale = mean(scale, na.rm = T),
            R2 = mean(R2, na.rm = T),
            total = mean(total, na.rm = T),
            n = n(),
            mean.dev = mean(dev, na.rm = T),
            mean.abs.dev = mean(abs(dev), na.rm = T),
            dev.range = max(dev, na.rm = T) - min(dev, na.rm = T)) %>%
  rename(mu = mu.mean)


lep15bs2 = lep15bs %>%
  filter(mu > 100, mu < 200, R2 > 0.2)


pdf('output/plots/paper_plots/effort_analysis_Gausspeak_hists.pdf', height = 6, width = 7)
par(mfcol = c(5, 5), mar = c(2, 1, 1, 0.5), oma = c(3, 5, 5, 0), mgp = c(2, 0.7, 0))

numCircles = c(2,4,6,8,10)

for (f in 1:5) {
  for (c in numCircles[5:1]) {
    lepsub = filter(lep15bs2, freq == f, circles == c)
    hist(lepsub$mu, breaks = seq(100, 220, by = 2), xlim = c(140, 210), 
         main = '', xlab = '', ylab = '', yaxt = 'n', col = 'black', tck = -.1)
    abline(v = full.mu, col = 'red', lwd = 2)
  }
}
mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
mtext("Sampling interval (days)", 3, outer = T, line = 3, cex = 1.5)
mtext(round(3.5*(1:5), 0), 3, outer = T, at = seq(0.1, 0.9, length.out=5), cex = 1.5)
mtext("Number of surveys", 2, outer = T, line = 3, cex = 1.5)
mtext(5*numCircles, 2, outer = T, at = seq(0.13, .93, length.out = 5), cex = 1.5, las = 1)
dev.off()



pdf('output/plots/paper_plots/effort_analysis_peakdate_hists.pdf', height = 6, width = 7)
par(mfcol = c(5, 5), mar = c(2, 1, 1, 0.5), oma = c(3, 5, 5, 0), mgp = c(2, 0.7, 0))

numCircles = c(2,4,6,8,10)

for (f in 1:5) {
  for (c in numCircles[5:1]) {
    lepsub = filter(lep15bs2, freq == f, circles == c)
    hist(lepsub$maxJD, breaks = seq(100, 220, by = 2), xlim = c(140, 210), 
         main = '', xlab = '', ylab = '', yaxt = 'n', col = 'black', tck = -.1)
    abline(v = full.peak, col = 'red', lwd = 2)
  }
}
mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
mtext("Sampling interval (days)", 3, outer = T, line = 3, cex = 1.5)
mtext(round(3.5*(1:5), 0), 3, outer = T, at = seq(0.1, 0.9, length.out=5), cex = 1.5)
mtext("Number of surveys", 2, outer = T, line = 3, cex = 1.5)
mtext(5*numCircles, 2, outer = T, at = seq(0.13, .93, length.out = 5), cex = 1.5, las = 1)
dev.off()

















####

PR.LEPL15.bsday = meanDensityByDay(beatsheet.pr, 
                                   ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                   jdRange = c(130,210), outlierCount = 10000, plot = T, 
                                   plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, las = 1,
                                   xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillars", 
                                   main = '2015, Beat sheet', col = 'gray80')

PR.LEPL15.bsday = meanDensityByDay(beatsheet.pr, 
                                   ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                   jdRange = c(138,197), outlierCount = 10000, plot = T, 
                                   plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 7, las = 1,
                                   xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillars", 
                                   main = '2015, Beat sheet', col = 'blueviolet')

# Example: plotting fitted Gaussians where circles == 4 and freq == 4

lines(130:210, full$scale*dnorm(130:210, full$mu, full$sig), lwd = 3, col = 'blueviolet', lty = 'dashed')

example = lep15bs %>% 
  filter(mu > 100, mu < 200, R2 > 0.2, circles == 4, freq == 3) %>%
  group_by(start) %>%
  top_n(2)

sapply(1:nrow(example), function(x)
  lines(130:210, example$scale[x]*dnorm(130:210, example$mu[x], example$sig[x]),
        col = 'gray60'))
###

# Estimated peak
mu.mat = matrix(lep15bs.sum$mu, nrow = length(unique(lep15bs.sum$circles)), 
                 ncol = length(unique(lep15bs.sum$freq)), byrow = F)

# Mean absolute deviations
mad.mat = matrix(lep15bs.sum$mean.abs.dev, nrow = length(unique(lep15bs.sum$circles)), 
                  ncol = length(unique(lep15bs.sum$freq)), byrow = F)

# Mean deviations
md.mat = matrix(lep15bs.sum$mean.dev, nrow = length(unique(lep15bs.sum$circles)), 
               ncol = length(unique(lep15bs.sum$freq)), byrow = F)




# Plotting
pos_col = 'blue'
neg_col = 'red'
colRamp = colorRampPalette(c(neg_col, 'white', pos_col))
colfunc = colorRampPalette(c(neg_col, pos_col))

# Function for plotting matrix of raw values
sampPlot = function(sampMatrix, col1, col2, title, numCircles = c(2, 4, 6, 8, 10, 12)) {
  colfunc = colorRampPalette(c(col1, col2))
  par(mgp = c(1.5, 0.3, 0), mar = c(3, 3, 2, 2))
  layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
  image(sampMatrix, ylab = "Sampling interval (days))", 
        xlab = "Number of surveys", main = title, 
        xaxt = "n", yaxt = "n", col = colfunc(20))
  axis(2, seq(0, 1, length.out = ncol(sampMatrix)), 
       labels = round(3.5*(1:ncol(sampMatrix)), 0), tck = -.01, las = 1)
  axis(1, seq(0, 1, length.out = nrow(sampMatrix)), labels = 5*numCircles, tck = -.01, las = 1)
  
  legend_image <- as.raster(matrix(colfunc(20)[20:1], ncol=1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  mtext(seq(round(min(sampMatrix)), round(max(sampMatrix)), length.out=5), 4, 
        at = seq(0,1,l=5), las = 1, line = -1)
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
        las = 1, line = -1, cex = 1.5)
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











# Get non-beat sheet effort by day
nonBSsurveys = filter(labdata, surveyType == 'Visual')
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
        las = 1, line = -1, cex = 1.5)
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


