################################################
# Script for general plotting of data from
# Caterpillars Count! data exported from the
# phpMyAdmin project site.
# ****************Need to go through and add effortByDay argument**************

# Run summary_functions.r and data_cleaning.R
source('summary_functions.r')
source('data_cleaning.R')

#################
# Prairie Ridge #
#################

dev.off()

# Caterpillars only, mean density
PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.lepl = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.lepl = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#PRam.1lepl = meanDensityByDay(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#PRbs.1lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#PRpm.1lepl = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', minLength = 5)
#PRvol.1lepl = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
#PRam.all = meanDensityByDay(labsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#PRbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#PRpm.all = meanDensityByDay(repsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
#PRvol.all = meanDensityByDay(volsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult = meanDensityByDay(amsurvey.pr, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, 
                             xlim = c(135, 250), ylim = c(0, .8))
PRbs.mult = meanDensityByDay(beatsheet.pr, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult = meanDensityByDay(pmsurvey.pr, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult = meanDensityByDay(volunteer.pr, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                              plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))


#----------------------------------------------------------------------------------------------
# Plots as above but averaged per week instead of by day

# Caterpillars only, mean density
PRam.lepl.wk = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.lepl.wk = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.lepl.wk = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Caterpillars only, fraction of surveys with at least one caterpillar
PRam.1lepl.wk = meanDensityByWeek(amsurvey.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(20, 35), ylim = c(0, 0.3))
PRbs.1lepl.wk = meanDensityByWeek(beatsheet.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
PRpm.1lepl.wk = meanDensityByWeek(pmsurvey.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', minLength = 5)
PRvol.1lepl.wk = meanDensityByWeek(volunteer.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
#PRam.all.wk = meanDensityByWeek(labsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#PRbs.all.wk = meanDensityByWeek(beatsheet, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#PRpm.all.wk = meanDensityByWeek(repsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
#PRvol.all.wk = meanDensityByWeek(volsurvey, "All", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult.wk = meanDensityByWeek(labsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, xlim = c(20, 35),
                                 ylim = c(0.1, 1))
PRbs.mult.wk = meanDensityByWeek(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult.wk = meanDensityByWeek(repsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                 plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult.wk = meanDensityByWeek(volsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                                  plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
#----------------------------------------------------------------------------------------------------------------------------


####################
# Botanical Garden #
####################

# Taking out julian day 259 for the botanical garden data (in visualsurveybg),
# don't know where this survey came from
visualsurveybg = visualsurveybg[visualsurveybg$julianday != 259,]

# Plot our morning surveys and our beat sheet surveys for the botanical garden
# Caterpillars only, mean density
BGam.lepl = meanDensityByDay(visualsurveybg, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#BGam.1lepl = meanDensityByDay(visualsurveybg, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#BGbs.1lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# All orders, mean density
#BGam.all = meanDensityByDay(visualsurveybg, "All", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#BGbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
BGam.mult = meanDensityByDay(visualsurveybg, ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.mult = meanDensityByDay(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

#----------------------------------------------------------------------------------------------------------------------------
# Plots as above but averaged per week instead of by day

# Plot our morning surveys and our beat sheet surveys for the botanical garden
# Caterpillars only, mean density
BGam.lepl.wk = meanDensityByWeek(visualsurveybg, "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

# Caterpillars only, fraction of surveys with at least one caterpillar
#BGam.1lepl.wk = meanDensityByWeek(visualsurveybg, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(135, 250))
#BGbs.1lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# All orders, mean density
#BGam.all.wk = meanDensityByWeek(visualsurveybg, "All", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
#BGbs.all.wk = meanDensityByWeek(beatsheet, "All", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
BGam.mult.wk = meanDensityByWeek(visualsurveybg, ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.mult.wk = meanDensityByWeek(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, 
                             plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange'))

#----------------------------------------------------------------------------------------------------------------------------

#########
# Frass #
#########

# Working with frass data
frass = frassData(open = T)

# If does not open, read from git
frass = read.csv('data/frass.csv', header=T)

# Convert date class
frass$Date.Set = as.Date(frass$Date.Set, format = "%m/%d/%Y")
frass$Date.Collected = as.Date(frass$Date.Collected, format = "%m/%d/%Y")
frass$Time.Set = as.character(frass$Time.Set)
frass$jday.Set = julianDayTime(frass$Date.Set, frass$Time.Set)
frass$Time.Collected = as.character(frass$Time.Collected)
frass$jday.Collected = julianDayTime(frass$Date.Collected, frass$Time.Collected)

frass$frass.mg.d = frass$Frass.mass..mg./(frass$jday.Collected - frass$jday.Set)

frass$jday = floor(frass$jday.Collected)
frass$week = floor(frass$jday/7) + 1

meanFrassByDay = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$jday), function(x) mean(x, na.rm=T))
meanFrassByWeek = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$week), function(x) mean(x, na.rm=T))
names(meanFrassByWeek) = c('site', 'week', 'frass.mg.d')
names(meanFrassByDay) = c('site', 'julianday', 'frass.mg.d')

PRfrassD = subset(meanFrassByDay, site == 'Prairie Ridge')
PRfrassW = subset(meanFrassByWeek, site == 'Prairie Ridge')

frassplot = function(site, frassdata, color = 'black', new = T) {
  temp = subset(frassdata, site == site)
  if (new) {
    plot(temp$julianday, temp$frass.mg.d, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'b', col = color, ylim = range(frassdata$frass.mg.d), xlim = range(frassdata$julianday))
  } else {
    points(temp$julianday, temp$frass.mg.d, type = 'b', col = color)
  }
}





# Prairie Ridge fraction of surveys with caterpillars plot
#pdf('plots/PR_LEPL_frac_by_week_wFrass.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2, mfrow = c(1,1))
plot(c(20,35), c(0, 0.25), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(amsurvey.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(pmsurvey.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volunteer.pr, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

par(new = T)
plot(PRfrassW$week, PRfrassW$frass.mg.d, type = 'b', col = 'darkgreen', lwd = 1, xlim = c(20, 35), ylim = c(1,7),
     xlab = "", xaxt = "n", ylab = "", yaxt = "n")
axis(4, 1:7, cex.axis = 1.2)
mtext("Frass (mg/d)", 4, line = 2.5, cex = 1.5)
legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers', 'Frass'),
       lwd = c(3, 3, 3, 3, 1), lty = c(rep('solid', 3), 'dashed', 'solid'),
       col = c('blue', 'skyblue', 'red', 'red', 'darkgreen'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()

# As above but without frass
pdf('plots/PR_LEPL_frac_by_week.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2)
plot(c(20,35), c(0, 0.24), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

legend("topleft", c('am Visual', 'am Beat sheet', 'pm Visual', 'pm Volunteers'),
       lwd = c(3, 3, 3, 3), lty = c(rep('solid', 3), 'dashed'),
       col = c('blue', 'skyblue', 'red', 'red'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds/7, line = 1, cex = 1.5)
dev.off()

#-------------------------------------------------------------------------------------------------------

## Merge mean densities into one dataframe

# Just caterpillars:
# Prairie Ridge
PRall.lepl1 = merge(PRam.lepl[,c('julianday','meanDensity')], PRbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.lepl1) = c('julianday','density_am','density_bs')
PRall.lepl2 = merge(PRall.lepl1, PRpm.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl2)[4] = 'density_pm'
PRall.lepl = merge(PRall.lepl2, PRvol.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl)[5] = 'density_vol'
# Botanical Garden
BGall.lepl = merge(BGam.lepl[,c('julianday','meanDensity')], BGbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.lepl) = c('julianday','density_am','density_bs')

# Selected orders:
# Prairie Ridge
PRall.mult1 = merge(PRam.mult[,c('julianday','meanDensity')], PRbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.mult1) = c('julianday','density_am','density_bs')
PRall.mult2 = merge(PRall.mult1, PRpm.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult2)[4] = 'density_pm'
PRall.mult = merge(PRall.mult2, PRvol.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult)[5] = 'density_vol'
# Botanical Garden
BGall.mult = merge(BGam.mult[,c('julianday','meanDensity')], BGbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.mult) = c('julianday','density_am','density_bs')





#-----------------------------------------------------------------------------------------------------
# Plotting for powerpoint for Chris Goforth

par(mfrow = c(1,1), mar = c(4,4,3,2), oma = c(1,1,0,0))

# Mean density

PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             ylim = c(0,.15), xaxt='n', ann=FALSE)
PRbs.lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.lepl = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.lepl = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Density')

# Temporary fix of caterpillar colony data classified as "OTHER":
volunteer.pr <- volunteer.pr[!(volunteer.pr$arthCode == "NONE" & volunteer.pr$count > 10),]

PRam.all = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = 'All', inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2, 
                            ylim = c(0.05,.9), xaxt='n', ann=FALSE)
PRbs.all = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.all = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.all = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean density", 2, line = 2.5, cex = 1.5)
title(main = 'All Arthropods - Mean Density')


# Biomass

PRam.leplbm = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                             ylim = c(0,.6), xaxt='n', ann=FALSE)
PRbs.leplbm = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.leplbm = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.leplbm = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Caterpillars - Mean Biomass')

PRam.allbm = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = T, color = 'blue', minLength = 5, lwd = 2, 
                            ylim = c(0,16), xaxt='n', ann=FALSE)
PRbs.allbm = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'orange', minLength = 5, lwd = 2)
PRpm.allbm = meanDensityByDay(pmsurvey.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'red', minLength = 5, lwd = 2)
PRvol.allbm = meanDensityByDay(volunteer.pr, effort = effortByDay, ordersToInclude = regorders, inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanBiomass', new = F, color = 'green', minLength = 5, lwd = 2)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
jds = c(140, 171, 201, 232)
mtext(c("May 20", "Jun 20", "Jul 20", "Aug 20"), 1, at = jds, line = 1.5, cex = 1.5)
mtext("Mean biomass", 2, line = 2.5, cex = 1.5)
title(main = 'Multiple Orders - Mean Biomass')



# Ellipse chart, bivariates, correlation matrices

## Make a giant dataset with all mean densities, biomasses, and frass
# (for standard julian days from core morning survey days)
everything1 = merge(PRam.all[, c(1,8)], PRbs.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything2 = merge(everything1, PRpm.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything2) = c('julianday', 'am all density', 'bs all density', 'pm all density')
everything3 = merge(everything2, PRvol.all[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything4 = merge(everything3, PRam.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything4) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density')
everything5 = merge(everything4, PRbs.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything6 = merge(everything5, PRpm.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
names(everything6) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density')
everything7 = merge(everything6, PRvol.lepl[,c(1,8)], by = 'julianday', all.x = T, all.y = F)
everything8 = merge(everything7, PRam.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything8) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                       'vol lepl density', 'am selected bm')
everything9 = merge(everything8, PRbs.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything10 = merge(everything9, PRpm.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything10) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                       'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                       'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm')
everything11 = merge(everything10, PRvol.allbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything12 = merge(everything11, PRam.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything12) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm')
everything13 = merge(everything12, PRbs.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything14 = merge(everything13, PRpm.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
names(everything14) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm', 'bs lepl bm', 'pm lepl bm')
everything15 = merge(everything14, PRvol.leplbm[,c(1,10)], by = 'julianday', all.x = T, all.y = F)
everything16 = merge(everything15, PRfrassD[,c(3,2)], by = 'julianday', all.x = T, all.y = F) 
names(everything16) = c('julianday', 'am all density', 'bs all density', 'pm all density', 
                        'vol all density', 'am lepl density', 'bs lepl density', 'pm lepl density',
                        'vol lepl density', 'am selected bm', 'bs selected bm', 'pm selected bm',
                        'vol selected bm', 'am lepl bm', 'bs lepl bm', 'pm lepl bm', 
                        'vol lepl bm', 'frass')
corevery = everything16

## Make a correlation chart
round(cor(corevery[,c(2:18)], use = 'pairwise.complete.obs'),2) # is that use argument ok?

# Smaller, more readable subsets of that correlation chart
# Just all arthropods mean density
all.dens = round(cor(corevery[,c(2:5,18)], use = 'pairwise.complete.obs'),2)
write.csv(all.dens, 'all.dens.csv')
# Just caterpillar mean density
lepl.dens = round(cor(corevery[,c(6:9,18)], use = 'pairwise.complete.obs'),2)
write.csv(lepl.dens, 'lepl.dens.csv')
# Just selected arthropods biomass
arth.bm = round(cor(corevery[,c(10:13,18)], use = 'pairwise.complete.obs'),2)
write.csv(arth.bm, 'arth.bm.csv')
# Just caterpillar biomass
lepl.bm = round(cor(corevery[,c(14:17,18)], use = 'pairwise.complete.obs'),2)
write.csv(lepl.bm, 'lepl.bm.csv')


## Make an ellipse chart

# Libraries
library(ellipse)
library(RColorBrewer)

# Use of the mtcars data proposed by R
ellipsedata=round(cor(corevery[,c(2:17)], use = 'pairwise.complete.obs'),2)

# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "PiYG")
my_colors=colorRampPalette(my_colors)(101)

# Plot ellipse chart
plotcorr(ellipsedata, col=my_colors[ellipsedata*50+51] , mar=c(1,1,1,1), cex.lab = 0.75)


## Bivariate plots

plot(corevery$`pm lepl density`, corevery$`vol lepl density`)




# Pie charts
par(mfrow = c(2,2), mar = c(1,1,1,1))

# Volunteers (need to fix NAs)
slices <- c(sum(volunteer.pr$count[volunteer.pr$arthCode == 'ARAN'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'AUCH'], na.rm = T), 
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'COLE'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'DIPT'], na.rm = T),
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'LEPL'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'OPIL'], na.rm = T),
            sum(volunteer.pr$count[volunteer.pr$arthCode == 'ORTH'], na.rm = T), sum(volunteer.pr$count[volunteer.pr$arthCode == 'HEMI'], na.rm = T))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Volunteer Counts", 
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Lab pm
slices <- c(sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'ARAN']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'AUCH']), 
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'COLE']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'DIPT']),
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'LEPL']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'OPIL']),
            sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'ORTH']), sum(pmsurvey.pr$count[pmsurvey.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Lab PM Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Lab am
slices <- c(sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'ARAN']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'AUCH']), 
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'COLE']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'DIPT']),
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'LEPL']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'OPIL']),
            sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'ORTH']), sum(amsurvey.pr$count[amsurvey.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Lab AM Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))

# Beat Sheet
slices <- c(sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'ARAN']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'AUCH']), 
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'COLE']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'DIPT']),
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'LEPL']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'OPIL']),
            sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'ORTH']), sum(beatsheet.pr$count[beatsheet.pr$arthCode == 'HEMI']))
lbls <- c("ARAN", "AUCH", "COLE", "DIPT", "LEPL", "OPIL", "ORTH", "HEMI")
pie(slices, labels = lbls, main="Beat Sheet Counts",
    col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))


