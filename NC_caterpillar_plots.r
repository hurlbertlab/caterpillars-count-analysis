################################################
# Script for general plotting of data from
# Caterpillars Count! data exported from the
# phpMyAdmin project site.
# ****************Need to go through and add effortByDay argument**************

# Run summary_functions.r and data_cleaning.R
source('summary_functions.r')
source('data_cleaning.R')


# Plotting mean density
plotting = 'meanDensity'
PRcol = 'cornflowerblue'
BGcol = 'limegreen'
linewidth = 5


PRlepl16 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'cornflowerblue', minLength = 5, lty = 'dashed', lwd = 3, ylim = c(0, 0.4), ylab = 'Caterpillar density')
PRlepl15 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'cornflowerblue', minLength = 5, lwd = 3)
BGlepl15 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3)
BGlepl16 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3, lty = 'dashed')
lepl15 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = c(117,8892356), plot = T, plotVar = 'meanDensity', new = F, color = 'salmon', minLength = 5, lwd = 3)
lepl16 = meanDensityByDay(labdata, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = c(117,8892356), plot = T, plotVar = 'meanDensity', new = F, color = 'salmon', minLength = 5, lwd = 3, lty = 'dashed')


# Caterpillars only, mean density
PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.lepl = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.lepl = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

