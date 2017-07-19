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

plotGaussPeakHists = function(effortsummary, write = F, filename = NULL, minFreq = 5) {
  
  full.mu = effortsummary$mu[effortsummary$circles == max(effortsummary$circles) & 
                               effortsummary$freq == 1]
  
  effortsummary2 = effortsummary %>%
    filter(mu > 100, mu < 200, R2 > 0.2)
  
  CIs = effortsummary2 %>% 
    group_by(freq, circles, col) %>% 
    summarize(l95 = quantile(mu, .05), u95 = quantile(mu, .95), range = u95-l95) 
  #%>% left_join(freqcols, by = 'freq')
  
  if (write) {
    pdf(paste('output/plots/', filename, sep = ''), height = 6, width = 7)
  }
  
  numCircles = unique(effortsummary$circles) %>% sort(decreasing = T)
  
  # Plot of histograms of estimated peak dates based on subsampling intensity/frequency
  par(mfcol = c(length(numCircles), minFreq), mar = c(2, 1, 1, 0.5), oma = c(3, 5, 5, 0), mgp = c(2, 0.7, 0))
  
  
  for (f in 1:minFreq) {
    for (c in numCircles) {
      lepsub = filter(effortsummary2, freq == f, circles == c)
      CI = filter(CIs, freq == f, circles == c)
      h = hist(lepsub$mu, breaks = seq(100, 220, by = 2), xlim = c(140, 210), 
               main = '', xlab = '', ylab = '', yaxt = 'n', col = 'black', tck = -.1)
      mtext(paste("[", round(CI$l95, 0), ", ", round(CI$u95, 0), "]", sep = ""), 
            3, adj = 1, line = -0.45, cex = .6)
      abline(v = full.mu, col = 'red', lwd = 2)
    }
  }
  mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
  mtext("Sampling interval (days)", 3, outer = T, line = 3, cex = 1.5)
  mtext(round(3.5*(1:minFreq), 0), 3, outer = T, at = seq(0.1, 0.9, length.out=5), cex = 1.5)
  mtext("Number of surveys", 2, outer = T, line = 3, cex = 1.5)
  mtext(5*numCircles, 2, outer = T, at = seq(0.93, .13, length.out = length(numCircles)), cex = 1.5, las = 1)
  
  if (write) {
    dev.off()
  }
}



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

cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))
mypalette <- colorRampPalette(cols)(5)
freqcols = data.frame(freq = 1:5, col = mypalette)

CIs = lep15bs2 %>% group_by(freq, circles, col) %>% summarize(l95 = quantile(mu, .05), u95 = quantile(mu, .95), range = u95-l95) #%>% left_join(freqcols, by = 'freq')

# Plot of histograms of estimated peak dates based on subsampling intensity/frequency
pdf('output/plots/paper_plots/effort_analysis_Gausspeak_hists.pdf', height = 6, width = 7)
par(mfcol = c(5, 5), mar = c(2, 1, 1, 0.5), oma = c(3, 5, 5, 0), mgp = c(2, 0.7, 0))

numCircles = c(2,4,6,8,10)

for (f in 1:5) {
  for (c in numCircles[5:1]) {
    lepsub = filter(lep15bs2, freq == f, circles == c)
    CI = filter(CIs, freq == f, circles == c)
    h = hist(lepsub$mu, breaks = seq(100, 220, by = 2), xlim = c(140, 210), 
         main = '', xlab = '', ylab = '', yaxt = 'n', col = 'black', tck = -.1)
    mtext(paste("[", round(CI$l95, 0), ", ", round(CI$u95, 0), "]", sep = ""), 
           3, adj = 1, line = -0.45, cex = .6)
    abline(v = full.mu, col = 'red', lwd = 2)
  }
}
mtext("Julian day", 1, outer = T, line = 1, cex = 1.5)
mtext("Sampling interval (days)", 3, outer = T, line = 3, cex = 1.5)
mtext(round(3.5*(1:5), 0), 3, outer = T, at = seq(0.1, 0.9, length.out=5), cex = 1.5)
mtext("Number of surveys", 2, outer = T, line = 3, cex = 1.5)
mtext(5*numCircles, 2, outer = T, at = seq(0.13, .93, length.out = 5), cex = 1.5, las = 1)
dev.off()

# Plot of 95% confidence interval width of peak date estimates as fcn of sampling intensity/frequency
pdf('output/plots/paper_plots/effort_analysis_Gausspeak_CIs.pdf', height = 6, width = 7)
par(mar = c(5, 5, 1, 1), oma = c(0, 0, 0, 0))
plot(CIs$circles, CIs$range, pch = 16, cex = 3, col = CIs$col, las = 1, xaxt = "n", xlab = "", ylab = "")
axis(1, at = c(numCircles, 12), labels = 5*c(numCircles, 12))
mtext("Number of surveys", 1, cex = 1.5, line = 3)
mtext("95% CI width (Julian days)", 2, cex = 1.5, line = 3)
legend("topright", legend = c(4, 7, 10, 14, 18), pch = 16, col = unique(CIs$col), 
       cex = 1.5, ncol = 3, title = 'Sampling frequency')
abline(h = 14, lty = 'dotted', lwd = 2)
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







# ------- OLD --------------------

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











