# Phenology plot for paper
# Need volunteer beat sheet data from 2016

par(mfrow = c(2,2))
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

# First panel
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,208), ylim = c(0,.33), ylab = "Mean density of caterpillars", main = '2015 Visual')
PR.LEPL15.cs = meanDensityByDay(volunteer.pr,  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Second panel
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet',], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,208), ylim = c(0,.33), ylab = "Mean density of caterpillars", main = '2016 Beat Sheet')
#PR.LEPL16.cs = meanDensityByDay(volunteer.pr,  
#                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
#                             plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))

# Third panel
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,208), ylim = c(0,1.5), ylab = "Mean density of bird food", main = '')
PR.BIRD15.cs = meanDensityByDay(volunteer.pr,  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Fourth panel
PR.BIRDL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet',], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                  xlim = c(130,208), ylim = c(0,1.5), ylab = "Mean density of bird food", main = '')
#PR.BIRD16.cs = meanDensityByDay(volunteer.pr,  
#                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
#                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))



# bird food orders
# caterpillar colonies