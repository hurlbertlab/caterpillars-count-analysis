# Phenology plots - comparing cit sci to trained sci [for overall paper], only Prairie Ridge
#
# Note that only circles 1:8 are being used for this comparison
# (citizen scientists did not survey circles 9:12)

# Note that for occurrence plots, there's no need to exclude outliers, so outlierCount
# is set at the default (10000) is used.



source("cleaning_scripts/data_cleaning.r")

multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HETE', 'AUCH')

meanfrass = read.csv("data/arthropods/frass_by_day_2015-2017.csv", header = T)


#----PHENOLOGY PLOT by occurrence (%) by week----

pdf('output/plots/paper_plots/phenology_trained_v_vols_pct.pdf', width = 7.5, height = 9)

par(mfrow = c(3,2), mar = c(2,5,3,3), oma = c(4,4,1,1), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.75)

# Restrict comparisons to the window of julian days when both groups were conducting surveys
beg_jd15 = 136
end_jd15 = 206
beg_jd16 = 147
end_jd16 = 194


linewidth = 3
cex.corr = 1.5 # font size of correlation coefficients

col1 = 'black'
col2 = 'gray70'
col3 = 'gray30'

# First panel
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,15), ylab = "Caterpillars", xaxt = "n",
                                 main = '2015, Visual', col = col1)
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, xaxt = "n",
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)
#PR.LEPL15.cs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,],  
#                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
#                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, xaxt = "n",
#                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, las = 1, 
#                                 lty = 'dashed', col = col3)



par(new=T)
frassplot(meanfrass, inputSite = 117, 2015, col1, new = T, var = 'mass', xlim = c(beg_jd15, end_jd15),
          lwd = 2, minReliability = 3, lty = 'dotted', ylim = c(1,8), yaxt = "n", xaxt = "n")

axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("5/15", "6/1", "6/15", "7/1", "7/15"), at = c(136, 153, 167, 183, 197), side = 1, line = 1, cex = 1)

axis(4, at = 1:7, labels = 1:7, tck = -.02, las = 1)
mtext("Frass (mg / trap / day)", 4, at = 4, line = 2.3)

legend("topleft", c('trained', 'citizen', 'frass'), lwd = c(linewidth, linewidth, 2), 
       lty = c('solid', 'solid', 'dotted'), col = c(col1, col2, col1), cex = 1.2)
LEPL15 = merge(PR.LEPL15.sci[,c('fracSurveys', 'week')], 
               PR.LEPL15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL15$fracSurveys.x, LEPL15$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("A", 3, adj=-.2, line=1.25, cex = cex.corr)


# Second panel
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,20), ylab = "", xaxt = "n",
                                 main = '2016, Beat Sheet', col = col1)
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)

LEPL16 = merge(PR.LEPL16.sci[,c('fracSurveys', 'week')], PR.LEPL16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL16$fracSurveys.x, LEPL16$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("B", 3, adj=-.2, line=1.25, cex = cex.corr)

axis(1, at = c(153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("6/1", "6/15", "7/1", "7/15"), at = c(153, 167, 183, 197), side = 1, line = 1, cex = 1)



# Third panel
PR.ORTH15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,23), ylab = "Orthopterans", xaxt = "n",
                                 main = '', col = col1)
PR.ORTH15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)
ORTH15 = merge(PR.ORTH15.sci[,c('fracSurveys', 'week')], PR.ORTH15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH15$fracSurveys.x, ORTH15$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("C", 3, adj=-.2, line=1.25, cex = cex.corr)

axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("5/15", "6/1", "6/15", "7/1", "7/15"), at = c(136, 153, 167, 183, 197), side = 1, line = 1, cex = 1)


# Fourth panel
PR.ORTH16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,40), ylab = "", xaxt = "n",
                                 main = '', col = col1)
PR.ORTH16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)
ORTH16 = merge(PR.ORTH16.sci[,c('fracSurveys', 'week')], PR.ORTH16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH16$fracSurveys.x, ORTH16$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("D", 3, adj=-.2, line=1.25, cex = cex.corr)

axis(1, at = c(153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("6/1", "6/15", "7/1", "7/15"), at = c(153, 167, 183, 197), side = 1, line = 1, cex = 1)


# Fifth panel
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1, xaxt = "n",
                                 xlim = c(beg_jd15, end_jd15), ylim = c(10,70), ylab = "Foliage arthropods", 
                                 main = '', col = col1)
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 10000, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)

BIRD15 = merge(PR.BIRD15.sci[,c('fracSurveys', 'week')], PR.BIRD15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD15$fracSurveys.x, BIRD15$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("E", 3, adj=-.2, line=1.25, cex = cex.corr)

axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("5/15", "6/1", "6/15", "7/1", "7/15"), at = c(136, 153, 167, 183, 197), side = 1, line = 1, cex = 1)


# Sixth panel
PR.BIRD16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                  jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd16, end_jd16), ylim = c(10,70), ylab = "", xaxt = "n",
                                  main = '', col = col1)
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd16, end_jd16), outlierCount = 10000, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = col2)

BIRD16 = merge(PR.BIRD16.sci[,c('fracSurveys', 'week')], PR.BIRD16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD16$fracSurveys.x, BIRD16$fracSurveys.y), 2))), bty="n", cex = cex.corr)
mtext("F", 3, adj=-.2, line=1.25, cex = cex.corr)

axis(1, at = c(153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("6/1", "6/15", "7/1", "7/15"), at = c(153, 167, 183, 197), side = 1, line = 1, cex = 1)


mtext("Occurrence (% of surveys)", side = 2, outer = TRUE, line = 1.75, cex = 1.25)
mtext("Date", side = 1, outer = TRUE, line = 2, cex = 1.25)

dev.off()










#----PHENOLOGY PLOT by density by week----

pdf('output/plots/paper_plots/phenology_trained_v_vols_den.pdf', width = 6, height = 7)

par(mfrow = c(3,2), mar = c(2,5,3,1), oma = c(4,4,1,1), cex.lab = 1.5, cex.axis = 1.25, mgp = c(3.5, 1, 0))

# Restrict comparisons to the window of julian days when both groups were conducting surveys
beg_jd15 = 138
end_jd15 = 206
beg_jd16 = 147
end_jd16 = 194

linewidth = 3

# First panel
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                  jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd15, end_jd15), ylim = c(0,.15), ylab = "Caterpillars", 
                                  main = '2015, Visual', col = 'blueviolet')
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')

legend("topleft", c('trained', 'citizen'), lwd = linewidth, lty = 1, col = c('blueviolet', 'darkgoldenrod3'))
LEPL15 = merge(PR.LEPL15.sci[,c('meanDensity', 'week')], 
               PR.LEPL15.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL15$meanDensity.x, LEPL15$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("A", 3, adj=-.2, line=1.25, cex = 1.3)


# Second panel
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                  ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                  jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd16, end_jd16), ylim = c(0,.2), ylab = "", 
                                  main = '2016, Beat Sheet', col = 'blueviolet')
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')

LEPL16 = merge(PR.LEPL16.sci[,c('meanDensity', 'week')], PR.LEPL16.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL16$meanDensity.x, LEPL16$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("B", 3, adj=-.2, line=1.25, cex = 1.3)


# Third panel
PR.ORTH15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                  ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                  jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd15, end_jd15), ylim = c(0,.25), ylab = "Orthopterans", 
                                  main = '', col = 'blueviolet')
PR.ORTH15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')
ORTH15 = merge(PR.ORTH15.sci[,c('meanDensity', 'week')], PR.ORTH15.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH15$meanDensity.x, ORTH15$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("C", 3, adj=-.2, line=1.25, cex = 1.3)


# Fourth panel
PR.ORTH16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                  ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                  jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd16, end_jd16), ylim = c(0,.43), ylab = "", 
                                  main = '', col = 'blueviolet')
PR.ORTH16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')
ORTH16 = merge(PR.ORTH16.sci[,c('meanDensity', 'week')], PR.ORTH16.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH16$meanDensity.x, ORTH16$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("D", 3, adj=-.2, line=1.25, cex = 1.3)


# Fifth panel
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$circle %in% 1:8,], 
                                  ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                  jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd15, end_jd15), ylim = c(0,1.2), ylab = "Foliage arthropods", 
                                  main = '', col = 'blueviolet')
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')

BIRD15 = merge(PR.BIRD15.sci[,c('meanDensity', 'week')], PR.BIRD15.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD15$meanDensity.x, BIRD15$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("E", 3, adj=-.2, line=1.25, cex = 1.3)


# Sixth panel
PR.BIRD16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$circle %in% 1:8,], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                  jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd16, end_jd16), ylim = c(0,1.2), ylab = "", 
                                  main = '', col = 'blueviolet')
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$circle %in% 1:8,],  
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                 plotVar = 'meanDensity', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                 lty = 1, col = 'darkgoldenrod3')

BIRD16 = merge(PR.BIRD16.sci[,c('meanDensity', 'week')], PR.BIRD16.cs[,c('meanDensity', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD16$meanDensity.x, BIRD16$meanDensity.y), 2))), bty="n", cex = 1.3)
mtext("F", 3, adj=-.2, line=1.25, cex = 1.3)


mtext("Density (# / survey)", side = 2, outer = TRUE, line = 1.75, cex = 1.25)
mtext("Julian day", side = 1, outer = TRUE, line = 2, cex = 1.25)

dev.off()

