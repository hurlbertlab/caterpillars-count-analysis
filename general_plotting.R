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
PRam.lepl = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
PRbs.lepl = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)s
PRpm.lepl = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.lepl = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
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
PRam.mult = meanDensityByDay(labsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, 
                             xlim = c(135, 250), ylim = c(0.1, 1))
PRbs.mult = meanDensityByDay(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult = meanDensityByDay(repsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = F, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult = meanDensityByDay(volsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                              plot = F, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
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
#PRam.1lepl.wk = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = T, color = 'blue', minLength = 5, xlim = c(20, 35), ylim = c(0, 0.2))
#PRbs.1lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'orange', minLength = 5)
#PRpm.1lepl.wk = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', minLength = 5)
#PRvol.1lepl.wk = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'green', minLength = 5)
#legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
#       col = c('blue', 'orange', 'red', 'green'))

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
BGam.lepl.wk = meanDensityByWeek(visualsurveybg, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5)
BGbs.lepl.wk = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
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

# Convert date class
frass$Date.Set = as.Date(frass$Date.Set, format = "%m/%d/%Y")
frass$Date.Collected = as.Date(frass$Date.Collected, format = "%m/%d/%Y")
frass$jday.Set = julianDayTime(frass$Date.Set, frass$Time.Set)
frass$jday.Collected = julianDayTime(frass$Date.Collected, frass$Time.Collected)

frass$frass.mg.d = frass$Frass.mass..mg./(frass$jday.Collected - frass$jday.Set)

frass$jday = floor(frass$jday.Collected)
frass$week = floor(frass$jday/7) + 1

meanFrassByDay = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$jday), function(x) mean(x, na.rm=T))
meanFrassByWeek = aggregate(frass$frass.mg.d, by = list(frass$Site, frass$week), function(x) mean(x, na.rm=T))
names(meanFrassByWeek) = c('site', 'week', 'frass.mg.d')

PRfrassW = subset(meanFrassByWeek, site == "Prairie Ridge")

frassplot = function(site, frassdata, color = 'black', new = T) {
  temp = subset(frassdata, Group.1 == site)
  if (new) {
    plot(temp$Group.2, temp$x, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'b', col = color, ylim = range(frassdata$x), xlim = range(frassdata$Group.2))
  } else {
    points(temp$Group.2, temp$x, type = 'b', col = color)
  }
}





# Prairie Ridge fraction of surveys with caterpillars plot
pdf('plots/PR_LEPL_frac_by_week_wFrass.pdf', height = 6, width = 8)
par(mgp = c(3, 1, 0), mar = c(3, 5, 1, 4), cex.lab = 1.5, cex.axis = 1.2)
plot(c(20,35), c(0, 0.24), type = "n", xlab = "", xaxt = "n", ylab = "Fraction of surveys")
PRam = meanDensityByWeek(labsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'blue', lwd = 3)
PRbs = meanDensityByWeek(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'skyblue', lwd = 3)
PRpm = meanDensityByWeek(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3)
PRvol = meanDensityByWeek(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = F, plotVar = 'fracSurveys', new = F, color = 'red', lwd = 3, lty = 'dashed')

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
