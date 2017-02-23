# Phenology plots for honors thesis - comparing beat sheet to visual each year
# Both Prairie Ridge and Bot Garden
# This script is a mess- ORGANIZE

setwd('c:/git/caterpillars-count-analysis')
source("data_cleaning.r")

multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

if (0) { # to not run by day plots

#----PR by day phenology plots----
par(mfrow = c(3,2), mar = c(2,3,2,2), oma = c(5,5,3,3))

# First panel
PR.LEPL15.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Caterpillars", main = '2015', col = 'forestgreen')
PR.LEPL15.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topright", c('visual', 'beat sheet'), lwd = 2, lty = 1, col = c('forestgreen', 'sienna3'))
legend("topleft", "A", bty="n")


# Second panel
PR.LEPL16.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", main = '2016', col = 'forestgreen')
PR.LEPL16.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "B", bty="n")


# Third panel
PR.ORTH15.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Orthopterans", col = 'forestgreen')
PR.ORTH15.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "C", bty="n")


# Fourth panel
PR.ORTH16.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", col = 'forestgreen')
PR.ORTH16.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "D", bty="n")


# Fifth panel
PR.BIRD15.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "Bird food", col = 'forestgreen')
PR.BIRD15.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "E", bty="n")


# Sixth panel
PR.BIRD16.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", col = 'forestgreen')
PR.BIRD16.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'sienna3',
                                xlim = c(130,207), ylab = "")
legend("topleft", "F", bty="n")

mtext("Fraction of surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Julian day", side = 1, outer = TRUE, line = 2)
mtext('Prairie Ridge', side = 3, outer = T, line = 1)

#----BG by day phenology plots----
par(mfrow = c(3,2), mar = c(2,3,2,2), oma = c(5,5,3,3))

# First panel
BG.LEPL15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.33), ylab = "Caterpillars", main = '2015', col = 'forestgreen')
BG.LEPL15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topright", c('visual', 'beat sheet'), lwd = 2, lty = 1, col = c('forestgreen', 'sienna3'))
legend("topleft", "A", bty="n")


# Second panel
BG.LEPL16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.33), ylab = "", main = '2016', col = 'forestgreen')
BG.LEPL16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "B", bty="n")


# Third panel
BG.ORTH15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Orthopterans", col = 'forestgreen')
BG.ORTH15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "C", bty="n")


# Fourth panel
BG.ORTH16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", col = 'forestgreen')
BG.ORTH16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "D", bty="n")


# Fifth panel
BG.BIRD15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "Bird food", col = 'forestgreen')
BG.BIRD15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "E", bty="n")


# Sixth panel
BG.BIRD16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", col = 'forestgreen')
BG.BIRD16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'sienna3',
                                xlim = c(130,207), ylab = "")
legend("topleft", "F", bty="n")

mtext("Fraction of surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Julian day", side = 1, outer = TRUE, line = 2)
mtext('Botanical Garden', side = 3, outer = T, line = 1)

} # end if (0)

#----PR by week phenology plots----
par(mfrow = c(3,2), mar = c(2,4,2,2), oma = c(5,5,3,3))

# First panel
PR.LEPL15.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "Caterpillars", main = '2015', col = 'forestgreen')
PR.LEPL15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend('topleft', c('visual', 'beat'), lwd = 2, lty = 1, col = c('forestgreen', 'sienna3'))
mtext('A', side = 2, line = 1)
LEPL15 = merge(PR.LEPL15.vis[,c('fracSurveys', 'week')], PR.LEPL15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL15$fracSurveys.x, LEPL15$fracSurveys.y), 2))), bty="n")


# Second panel
PR.LEPL16.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "", main = '2016', col = 'forestgreen')
PR.LEPL16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(20,30), ylab = "", col = 'sienna3')
legend("topleft", "B", bty="n")
LEPL16 = merge(PR.LEPL16.vis[,c('fracSurveys', 'week')], PR.LEPL16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL16$fracSurveys.x, LEPL16$fracSurveys.y), 2))), bty="n")

# Third panel
PR.ORTH15.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "Orthopterans", col = 'forestgreen')
PR.ORTH15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "C", bty="n")
ORTH15 = merge(PR.ORTH15.vis[,c('fracSurveys', 'week')], PR.ORTH15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH15$fracSurveys.x, ORTH15$fracSurveys.y), 2))), bty="n")

# Fourth panel
PR.ORTH16.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "", col = 'forestgreen')
PR.ORTH16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(20,30), ylab = "", col = 'sienna3')
legend("topleft", "D", bty="n")
ORTH16 = merge(PR.ORTH16.vis[,c('fracSurveys', 'week')], PR.ORTH16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH16$fracSurveys.x, ORTH16$fracSurveys.y), 2))), bty="n")

# Fifth panel
PR.BIRD15.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.7), ylab = "Bird food", col = 'forestgreen')
PR.BIRD15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "E", bty="n")
BIRD15 = merge(PR.BIRD15.vis[,c('fracSurveys', 'week')], PR.BIRD15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD15$fracSurveys.x, BIRD15$fracSurveys.y), 2))), bty="n")

# Sixth panel
PR.BIRD16.vis = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.7), ylab = "", col = 'forestgreen')
PR.BIRD16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'sienna3',
                                xlim = c(20,30), ylab = "")
legend("topleft", "F", bty="n")
BIRD16 = merge(PR.BIRD16.vis[,c('fracSurveys', 'week')], PR.BIRD16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD16$fracSurveys.x, BIRD16$fracSurveys.y), 2))), bty="n")

mtext("Fraction of surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Week", side = 1, outer = TRUE, line = 2)
mtext('Prairie Ridge', side = 3, outer = T, line = 1)

#----BG by week phenology plots----
par(mfrow = c(3,2), mar = c(2,4,2,2), oma = c(5,5,3,3))

# First panel
BG.LEPL15.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.3), ylab = "Caterpillars", main = '2015', col = 'forestgreen')
BG.LEPL15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend(26, .25, c('visual', 'beat'), lwd = 2, lty = 1, col = c('forestgreen', 'sienna3'))
legend("topleft", "A", bty="n")
LEPL15 = merge(BG.LEPL15.vis[,c('fracSurveys', 'week')], BG.LEPL15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL15$fracSurveys.x, LEPL15$fracSurveys.y), 2))), bty="n")


# Second panel
BG.LEPL16.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.3), ylab = "", main = '2016', col = 'forestgreen')
BG.LEPL16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(20,30), ylab = "", col = 'sienna3')
legend("topleft", "B", bty="n")
LEPL16 = merge(BG.LEPL16.vis[,c('fracSurveys', 'week')], BG.LEPL16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL16$fracSurveys.x, LEPL16$fracSurveys.y), 2))), bty="n")


# Third panel
BG.ORTH15.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "Orthopterans", col = 'forestgreen')
BG.ORTH15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "C", bty="n")
ORTH15 = merge(BG.ORTH15.vis[,c('fracSurveys', 'week')], BG.ORTH15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH15$fracSurveys.x, ORTH15$fracSurveys.y), 2))), bty="n")


# Fourth panel
BG.ORTH16.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.25), ylab = "", col = 'forestgreen')
BG.ORTH16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(20,30), ylab = "", col = 'sienna3')
legend("topleft", "D", bty="n")
ORTH16 = merge(BG.ORTH16.vis[,c('fracSurveys', 'week')], BG.ORTH16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH16$fracSurveys.x, ORTH16$fracSurveys.y), 2))), bty="n")


# Fifth panel
BG.BIRD15.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.7), ylab = "Bird food", col = 'forestgreen')
BG.BIRD15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "E", bty="n")
BIRD15 = merge(BG.BIRD15.vis[,c('fracSurveys', 'week')], BG.BIRD15.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD15$fracSurveys.x, BIRD15$fracSurveys.y), 2))), bty="n")


# Sixth panel
BG.BIRD16.vis = meanDensityByWeek(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(20,30), ylim = c(0,.7), ylab = "", col = 'forestgreen')
BG.BIRD16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'sienna3',
                                xlim = c(20,30), ylab = "")
legend("topleft", "F", bty="n")
BIRD16 = merge(BG.BIRD16.vis[,c('fracSurveys', 'week')], BG.BIRD16.bs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD16$fracSurveys.x, BIRD16$fracSurveys.y), 2))), bty="n")


mtext("Fraction of surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Week", side = 1, outer = TRUE, line = 2)
mtext('Botanical Garden', side = 3, outer = T, line = 1)