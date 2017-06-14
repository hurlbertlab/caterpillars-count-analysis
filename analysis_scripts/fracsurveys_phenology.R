# Phenology plots - comparing cit sci to trained sci [for overall paper], only Prairie Ridge
# Included in 495 paper (Tracie Fall 2016)
# This script is a mess- ORGANIZE
# DO BY WEEK

source("cleaning_scripts/data_cleaning.r")

pdf('output/plots/paper_plots/phenology_trained_v_vols.pdf', width = 6, height = 7)

par(mfrow = c(3,2), mar = c(2,5,3,1), oma = c(4,4,1,1), cex.lab = 1.5, cex.axis = 1.25)
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HETE')

# Restrict comparisons to the window of julian days when both groups were conducting surveys
beg_jd15 = 138
end_jd15 = 204
beg_jd16 = 149
end_jd16 = 203


#----PHENOLOGY PLOT for paper, by week----

linewidth = 3

# First panel
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,15), ylab = "Caterpillars", 
                                 main = '2015, Visual', col = 'blueviolet')
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr,  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')

legend("topleft", c('trained', 'citizen'), lwd = linewidth, lty = 1, col = c('blueviolet', 'darkgoldenrod3'))
LEPL15 = merge(PR.LEPL15.sci[,c('fracSurveys', 'week')], 
               PR.LEPL15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL15$fracSurveys.x, LEPL15$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("A", 3, adj=-.2, line=1.25, cex = 1.3)


# Second panel
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet',], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,15), ylab = "", 
                                 main = '2016, Beat Sheet', col = 'blueviolet')
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr,  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')

LEPL16 = merge(PR.LEPL16.sci[,c('fracSurveys', 'week')], PR.LEPL16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(LEPL16$fracSurveys.x, LEPL16$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("B", 3, adj=-.2, line=1.25, cex = 1.3)


# Third panel
PR.ORTH15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,20), ylab = "Orthopterans", 
                                 main = '', col = 'blueviolet')
PR.ORTH15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')
ORTH15 = merge(PR.ORTH15.sci[,c('fracSurveys', 'week')], PR.ORTH15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH15$fracSurveys.x, ORTH15$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("C", 3, adj=-.2, line=1.25, cex = 1.3)


# Fourth panel
PR.ORTH16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet',], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "", 
                                 main = '', col = 'blueviolet')
PR.ORTH16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')
ORTH16 = merge(PR.ORTH16.sci[,c('fracSurveys', 'week')], PR.ORTH16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(ORTH16$fracSurveys.x, ORTH16$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("D", 3, adj=-.2, line=1.25, cex = 1.3)


# Fifth panel
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,50), ylab = "Bird food", 
                                 main = '', col = 'blueviolet')
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                jdRange = c(beg_jd15, end_jd15), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')

BIRD15 = merge(PR.BIRD15.sci[,c('fracSurveys', 'week')], PR.BIRD15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD15$fracSurveys.x, BIRD15$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("E", 3, adj=-.2, line=1.25, cex = 1.3)


# Sixth panel
PR.BIRD16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet',], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                  jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = linewidth, las = 1,
                                  xlim = c(beg_jd16, end_jd16), ylim = c(0,60), ylab = "", 
                                  main = '', col = 'blueviolet')
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, 
                                jdRange = c(beg_jd16, end_jd16), outlierCount = 30, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = linewidth, las = 1, 
                                lty = 1, col = 'darkgoldenrod3')

BIRD16 = merge(PR.BIRD16.sci[,c('fracSurveys', 'week')], PR.BIRD16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', c(paste("r =", round(cor(BIRD16$fracSurveys.x, BIRD16$fracSurveys.y), 2))), bty="n", cex = 1.3)
mtext("F", 3, adj=-.2, line=1.25, cex = 1.3)


mtext("Occurrence (% of surveys)", side = 2, outer = TRUE, line = 1.75, cex = 1.25)
mtext("Julian day", side = 1, outer = TRUE, line = 2, cex = 1.25)

dev.off()

