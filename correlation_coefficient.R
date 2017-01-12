# Correlation coefficients between trained and citizen scientist estimates over the year
# Weekly average values

setwd('C:/git/caterpillars-count-analysis')
source('summary_functions.r')
source('data_cleaning.R')

# 2015 Visual LEPL
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(20,30), ylim = c(0,.15), ylab = "Mean density of caterpillars", main = '2015 Visual')
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)


PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(20,30), ylim = c(0,.25), ylab = "", main = '2016 Beat Sheet')
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(20,30), ylim = c(0,.4), ylab = "Mean density of orthopterans", main = '')
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(20,30), ylim = c(0,.4), ylab = "", main = '')
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(20,30), ylim = c(0,.6), ylab = "Mean density of bird food", main = '')
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.BIRDL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                   ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                                   plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                   xlim = c(20,30), ylim = c(0,.6), ylab = "", main = '')
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)