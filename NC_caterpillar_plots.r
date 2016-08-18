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

# Plotting by day
PRlepl16 = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'cornflowerblue', minLength = 5, lty = 'dashed', lwd = 3, ylim = c(0, 0.25), ylab = 'Caterpillar density')
PRlepl15 = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'cornflowerblue', minLength = 5, lwd = 3)
BGlepl15 = meanDensityByDay(amsurvey.bg, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3)
BGlepl16 = meanDensityByDay(amsurvey.bg, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3, lty = 'dashed')


# Plotting by week
datelabs = c("2016-05-15", "2016-06-01", "2016-06-15", "2016-07-01", '2016-07-15')
jds = yday(as.Date(datelabs, format = "%Y-%m-%d"))

jdwks = jds/7 + 1

PRlepl16wk = meanDensityByWeek(amsurvey.pr, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'cornflowerblue', minLength = 5, lty = 'dashed', lwd = 3, ylim = c(0, 0.4), ylab = 'Caterpillar density')
PRlepl15wk = meanDensityByWeek(amsurvey.pr, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'cornflowerblue', minLength = 5, lwd = 3)
BGlepl15wk = meanDensityByWeek(amsurvey.bg, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3)
BGlepl16wk = meanDensityByWeek(amsurvey.bg, ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'limegreen', minLength = 5, lwd = 3, lty = 'dashed')

# Average phenology of two sites
amsurvey = rbind(amsurvey.pr, amsurvey.bg)
pdf('plots/NC_caterpillars.pdf', height = 6, width = 8)
lepl15wk = meanDensityByWeek(amsurvey, ordersToInclude = "LEPL", inputYear = 2015, inputSite = c(117,8892356), plot = T, plotVar = 'meanDensity', new = T, color = 'salmon', minLength = 5, lwd = 3, ylab = 'Caterpillar density', xaxt = "n")
lepl16wk = meanDensityByWeek(amsurvey, ordersToInclude = "LEPL", inputYear = 2016, inputSite = c(117,8892356), plot = T, plotVar = 'meanDensity', new = F, color = 'salmon', minLength = 5, lwd = 3, lty = 'dashed')
legend("topleft", legend = c(2015, 2016), col = 'salmon', lty = c('solid', 'dashed'), lwd = 3)
axis(1, at = jdwks, labels = F)
mtext(c('May-15', 'Jun-1', 'Jun-15', 'Jul-1', 'Jul-15'), 1, at = jdwks, line = 1)
dev.off()

