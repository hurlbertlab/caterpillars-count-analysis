# Phenology plots for honors thesis - comparing beat sheet to visual each year
# Both Prairie Ridge and Bot Garden
# This script is a mess- ORGANIZE

setwd('c:/git/caterpillars-count-analysis')
source("data_cleaning.r")

par(mfrow = c(3,2), mar = c(2,3,2,2), oma = c(5,5,3,3))
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

#----PR by day phenology plots----

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

# First panel
BG.LEPL15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Caterpillars", main = '2015', col = 'forestgreen')
BG.LEPL15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topright", c('visual', 'beat sheet'), lwd = 2, lty = 1, col = c('forestgreen', 'sienna3'))
legend("topleft", "A", bty="n")


# Second panel
BG.LEPL16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", main = '2016', col = 'forestgreen')
BG.LEPL16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "B", bty="n")


# Third panel
BG.ORTH15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Orthopterans", col = 'forestgreen')
BG.ORTH15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "C", bty="n")


# Fourth panel
BG.ORTH16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", col = 'forestgreen')
BG.ORTH16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                xlim = c(130,207), ylab = "", col = 'sienna3')
legend("topleft", "D", bty="n")


# Fifth panel
BG.BIRD15.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "Bird food", col = 'forestgreen')
BG.BIRD15.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'sienna3')
legend("topleft", "E", bty="n")


# Sixth panel
BG.BIRD16.vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual' & amsurvey.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", col = 'forestgreen')
BG.BIRD16.bs = meanDensityByDay(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'sienna3',
                                xlim = c(130,207), ylab = "")
legend("topleft", "F", bty="n")

mtext("Fraction of surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Julian day", side = 1, outer = TRUE, line = 2)
mtext('Botanical Garden', side = 3, outer = T, line = 1)