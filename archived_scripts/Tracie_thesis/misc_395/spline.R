# Spline fits

par(mfrow = c(1,1))

PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Caterpillars", main = '2015 Visual')
smooth.spline(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys, spar = .4), col = 'red')

PR.LEPL16.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", main = '2016 Visual')
smooth.spline(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys, spar = 0.3), col = 'red')



# Beat Sheet
PR.LEPL15.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Caterpillars", main = '2015 Beat Sheet')
smooth.spline(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys, spar = 0.4), col = 'red')

PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", main = '2016 Beat Sheet')
smooth.spline(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys, spar = 0.3), col = 'red')


# By week BEAT SHEETS
PR.LEPL15.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  ylim = c(0,.3), ylab = "Caterpillars", main = '2015 Beat Sheet')
smooth.spline(PR.LEPL15.sci$week, PR.LEPL15.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL15.sci$week, PR.LEPL15.sci$fracSurveys, spar = 0.4), col = 'red')

PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  ylim = c(0,.15), ylab = "Caterpillars", main = '2016 Beat Sheet')
smooth.spline(PR.LEPL16.sci$week, PR.LEPL16.sci$fracSurveys, spar = 0.4)
lines(smooth.spline(PR.LEPL16.sci$week, PR.LEPL16.sci$fracSurveys, spar = 0.4), col = 'red')
