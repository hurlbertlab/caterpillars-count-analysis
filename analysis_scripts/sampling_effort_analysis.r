source('analysis_scripts/summary_functions.r')
source('cleaning_scripts/data_cleaning.r')

library(dplyr)
library(rasterImage)

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

lep15bs.bg = effortAnalysis(beatsheet.bg, inputYear = 2015, inputSite = 8892356, 
                         minFreq = 5, numCircles = c(8, 6, 4, 2), jdRange = c(138, 196),
                         plot = T, plotFilename = 'output/plots/lep15bs_BG_effort.pdf')


##########
# STEP 3: Summarize and visualize results

# minFreq = the minimum sampling frequency, e.g., every 'nth' sampling date
# maxcircles = the maximum number of survey circles to subsample

plotGaussPeakHists = function(effortsummary, write = F, filename = NULL, minFreq = 5, maxcircles = NULL) {
  
  full.mu = effortsummary$mu[effortsummary$circles == max(effortsummary$circles) & 
                               effortsummary$freq == 1]
  
  if (is.null(maxcircles)) {
    maxcircles = max(effortsummary$circles)
  }
  
  effortsummary2 = effortsummary %>%
    filter(mu > 100, mu < 200, R2 > 0.2, circles <= maxcircles)
  
  CIs = effortsummary2 %>% 
    group_by(freq, circles, col) %>% 
    summarize(l95 = quantile(mu, .05), u95 = quantile(mu, .95), range = u95-l95) 
  #%>% left_join(freqcols, by = 'freq')
  
  if (write) {
    pdf(paste('output/plots/', filename, sep = ''), height = 6, width = 7)
  }
  
  numCircles = unique(effortsummary2$circles) %>% sort(decreasing = T)
  
  # Plot of histograms of estimated peak dates based on subsampling intensity/frequency
  par(mfcol = c(length(numCircles), minFreq), mar = c(2, 1, 1, 0.5), oma = c(3, 5, 5, 0), mgp = c(2, 0.7, 0))
  
  
  for (f in 1:minFreq) {
    for (c in numCircles) {
      lepsub = filter(effortsummary2, freq == f, circles == c)
      CI = filter(CIs, freq == f, circles == c)
      h = hist(lepsub$mu, breaks = seq(100, 220, by = 2), xlim = c(140, 210), 
               main = '', xlab = '', ylab = '', yaxt = 'n', col = 'black', tck = -.1)
      #mtext(paste("[", round(CI$l95, 0), ", ", round(CI$u95, 0), "]", sep = ""), 
      #      3, adj = 1, line = -0.45, cex = .6)
      legend("topright", legend = round(CI$u95 - CI$l95, 0), bty = 'n', cex = 1.5)
      abline(v = full.mu, col = 'red', lwd = 2)
    }
  }
  mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
  mtext("Sampling interval (days)", 3, outer = T, line = 3, cex = 1.5)
  mtext(3.5*(1:minFreq), 3, outer = T, at = seq(0.1, 0.9, length.out=5), cex = 1.5)
  mtext("Number of surveys", 2, outer = T, line = 3, cex = 1.5)
  mtext(5*numCircles, 2, outer = T, at = seq(0.93, .13, length.out = length(numCircles)), cex = 1.5, las = 1)
  
  if (write) {
    dev.off()
  }
}


plotGaussPeakCIs = function(effortsummary, write = F, filename = NULL, minFreq = 5,
                            arrow = F, arrowX = NULL, arrowY = NULL) {
  
  effortsummary2 = effortsummary %>%
    filter(mu > 100, mu < 200, R2 > 0.2)
  
  CIs = effortsummary2 %>% 
    group_by(freq, circles, col) %>% 
    summarize(l95 = quantile(mu, .05), 
              u95 = quantile(mu, .95), 
              range = u95-l95,
              mean = mean(mu, na.rm = T))
  
  CIs$col = as.character(CIs$col)
  
  if (write) {
    pdf(paste('output/plots/', filename, sep = ''), height = 6, width = 7)
  }
  
  numCircles = unique(effortsummary$circles) %>% sort(decreasing = T)
  
  # Plot of CI width as a function of subsampling intensity/frequency
  par(mar = c(6, 6, 1, 1), cex.lab = 2, cex.axis = 1.5, mgp = c(4, 1, 0))
  plot(effortsummary2$circles, effortsummary2$mu, xaxt = 'n', 
       xlab = 'Number of surveys', ylab = 'Peak caterpillar date (Julian day)',
       las = 1, pch = 16, type = 'n', ylim = c(150, 195))
  axis(1, at = unique(effortsummary2$circles), labels = 5*unique(effortsummary2$circles))
  
  graycols = c('gray5', 'gray25', 'gray45', 'gray65', 'gray85')
  
  for (f in 5:1) {
    filt = filter(CIs, freq == f)
    #rgbcol = col2rgb(as.character(filt$col[1]))/255
    rgbcol = col2rgb(graycols[f])/255
    polygon(c(filt$circles, rev(filt$circles)), c(filt$l95, rev(filt$u95)), 
            col = rgb(rgbcol[1,1], rgbcol[2,1], rgbcol[3,1], 1), border = NA)
  }
  
  abline(h = full.mu, lwd = 4, col = 'white')
  abline(h = full.mu - 7, lty = 'dotted', lwd = 3, col = 'white')
  abline(h = full.mu + 7, lty = 'dotted', lwd = 3, col = 'white')
  segments(8, full.mu - 7, 12, full.mu - 7, lty = 'dotted', lwd = 3)
  
  legend("bottomright", legend = c(4, 7, 10, 14, 18), pch = 15, col = graycols, 
         cex = 1.5, ncol = 3, title = 'Sampling interval (days)')
  
  if (arrow) {
    arrows(arrowX, arrowY-5, arrowX, arrowY, lwd = 5)
  }
  
  if (write) {
    dev.off()
  }
}

plotGaussPeakCIs(lep15bs, write = T, filename = 'paper_plots/PR_effort_analysis_CI_polygons.pdf', 
                 arrow = T, arrowX = 6, arrowY = 166)


plotGaussPeakHists(lep15bs, write = T, filename = 'paper_plots/PR_effort_analysis_hists_labels.pdf',
                   maxcircles = 10)
