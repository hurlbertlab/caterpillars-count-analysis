######################
# Arthropod Analyses #
######################


# By order, AM surveys

arthplot = function(cleandata, ebd = effortByDay, site, year) {

par(mfrow = c(3, 2), mar = c(2, 1, 3, 3), oma = c(4, 4, 3, 1))

lepl = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'LEPL', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'LEPL')
leplquad = lm(meanDensity ~ julianday + I(julianday^2), data = lepl)
R2.quad = summary(leplquad)$r.squared
p.quad = summary(leplquad)$coefficients[2,4]
p.quad2 = summary(leplquad)$coefficients[3,4]
#text('topleft', bquote(R^2 == .(round(R2.quad, 2))), cex = r2p.cex)
#text('topleft',, bquote(p == .(round(p.quad, 2))), cex = r2p.cex)
jd = c(min(lepl$julianday):max(lepl$julianday))
leplquad.predict = leplquad$coefficients[1] + leplquad$coefficients[2]*jd + leplquad$coefficients[3]*jd^2
points(jd, leplquad.predict, type = 'l')
max.jd.leplquad = jd[leplquad.predict == max(leplquad.predict)]

orth = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'ORTH', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'ORTH')
orthquad = lm(meanDensity ~ julianday + I(julianday^2), data = orth)
R2.quad = summary(orthquad)$r.squared
p.quad = summary(orthquad)$coefficients[2,4]
p.quad2 = summary(orthquad)$coefficients[3,4]
jd = c(min(orth$julianday):max(orth$julianday))
orthquad.predict = orthquad$coefficients[1] + orthquad$coefficients[2]*jd + orthquad$coefficients[3]*jd^2
points(jd, orthquad.predict, type = 'l')
max.jd.orthquad = jd[orthquad.predict == max(orthquad.predict)]

cole = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'COLE', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'COLE')
colequad = lm(meanDensity ~ julianday + I(julianday^2), data = cole)
R2.quad = summary(colequad)$r.squared
p.quad = summary(colequad)$coefficients[2,4]
p.quad2 = summary(colequad)$coefficients[3,4]
jd = c(min(cole$julianday):max(cole$julianday))
colequad.predict = colequad$coefficients[1] + colequad$coefficients[2]*jd + colequad$coefficients[3]*jd^2
points(jd, colequad.predict, type = 'l')
max.jd.colequad = jd[colequad.predict == max(colequad.predict)]

hemi = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'HEMI', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'HEMI')
hemiquad = lm(meanDensity ~ julianday + I(julianday^2), data = hemi)
R2.quad = summary(hemiquad)$r.squared
p.quad = summary(hemiquad)$coefficients[2,4]
p.quad2 = summary(hemiquad)$coefficients[3,4]
jd = c(min(hemi$julianday):max(hemi$julianday))
hemiquad.predict = hemiquad$coefficients[1] + hemiquad$coefficients[2]*jd + hemiquad$coefficients[3]*jd^2
points(jd, hemiquad.predict, type = 'l')
max.jd.hemiquad = jd[hemiquad.predict == max(hemiquad.predict)]

aran = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'ARAN', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'ARAN')
aranquad = lm(meanDensity ~ julianday + I(julianday^2), data = aran)
R2.quad = summary(aranquad)$r.squared
p.quad = summary(aranquad)$coefficients[2,4]
p.quad2 = summary(aranquad)$coefficients[3,4]
jd = c(min(aran$julianday):max(aran$julianday))
aranquad.predict = aranquad$coefficients[1] + aranquad$coefficients[2]*jd + aranquad$coefficients[3]*jd^2
points(jd, aranquad.predict, type = 'l')
max.jd.aranquad = jd[aranquad.predict == max(aranquad.predict)]

auch = meanDensityByDay(cleandata, effort = ebd, ordersToInclude = 'AUCH', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'AUCH')
auchquad = lm(meanDensity ~ julianday + I(julianday^2), data = auch)
R2.quad = summary(auchquad)$r.squared
p.quad = summary(auchquad)$coefficients[2,4]
p.quad2 = summary(auchquad)$coefficients[3,4]
jd = c(min(auch$julianday):max(auch$julianday))
auchquad.predict = auchquad$coefficients[1] + auchquad$coefficients[2]*jd + auchquad$coefficients[3]*jd^2
points(jd, auchquad.predict, type = 'l')
max.jd.auchquad = jd[auchquad.predict == max(auchquad.predict)]

title(xlab = "Julian day",
      ylab = "Mean density",
      outer = TRUE, line = 2, cex.main = 2)

}


pdf('amsurvey.pr.pdf', width = 7, height = 9)
dev.new()
arthplot(amsurvey.pr, site = 117, year = 2015)
title(main = 'Prairie Ridge am survey', outer = T, cex.main = 2)
dev.off()

arthplot(beatsheet.pr, site = 117, year = 2015)
title(main = 'Prairie Ridge beat sheet', outer = T, cex.main = 2)

arthplot(amsurvey.bg, site = 8892356, year = 2015)
title(main = 'NCBG am survey', outer = T, cex.main = 2)

arthplot(beatsheet.bg, site = 8892356, year = 2015)
title(main = 'NCBG beat sheet', outer = T, cex.main = 2)








# By order, beat sheet surveys

par(mfrow = c(3, 2), mar = c(5, 5, 2, 2))

lepl = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'LEPL', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'LEPL')
leplquad = lm(meanDensity ~ julianday + I(julianday^2), data = lepl)
R2.quad = summary(leplquad)$r.squared
p.quad = summary(leplquad)$coefficients[2,4]
p.quad2 = summary(leplquad)$coefficients[3,4]
jd = c(min(lepl$julianday):max(lepl$julianday))
leplquad.predict = leplquad$coefficients[1] + leplquad$coefficients[2]*jd + leplquad$coefficients[3]*jd^2
points(jd, leplquad.predict, type = 'l')
max.jd.leplquad = jd[leplquad.predict == max(leplquad.predict)]

orth = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'ORTH', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'ORTH')
orthquad = lm(meanDensity ~ julianday + I(julianday^2), data = orth)
R2.quad = summary(orthquad)$r.squared
p.quad = summary(orthquad)$coefficients[2,4]
p.quad2 = summary(orthquad)$coefficients[3,4]
jd = c(min(orth$julianday):max(orth$julianday))
orthquad.predict = orthquad$coefficients[1] + orthquad$coefficients[2]*jd + orthquad$coefficients[3]*jd^2
points(jd, orthquad.predict, type = 'l')
max.jd.orthquad = jd[orthquad.predict == max(orthquad.predict)]

cole = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'COLE', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'COLE')
colequad = lm(meanDensity ~ julianday + I(julianday^2), data = cole)
R2.quad = summary(colequad)$r.squared
p.quad = summary(colequad)$coefficients[2,4]
p.quad2 = summary(colequad)$coefficients[3,4]
jd = c(min(cole$julianday):max(cole$julianday))
colequad.predict = colequad$coefficients[1] + colequad$coefficients[2]*jd + colequad$coefficients[3]*jd^2
points(jd, colequad.predict, type = 'l')
max.jd.colequad = jd[colequad.predict == max(colequad.predict)]

hemi = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'HEMI', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'HEMI')
hemiquad = lm(meanDensity ~ julianday + I(julianday^2), data = hemi)
R2.quad = summary(hemiquad)$r.squared
p.quad = summary(hemiquad)$coefficients[2,4]
p.quad2 = summary(hemiquad)$coefficients[3,4]
jd = c(min(hemi$julianday):max(hemi$julianday))
hemiquad.predict = hemiquad$coefficients[1] + hemiquad$coefficients[2]*jd + hemiquad$coefficients[3]*jd^2
points(jd, hemiquad.predict, type = 'l')
max.jd.hemiquad = jd[hemiquad.predict == max(hemiquad.predict)]

aran = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'ARAN', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'ARAN')
aranquad = lm(meanDensity ~ julianday + I(julianday^2), data = aran)
R2.quad = summary(aranquad)$r.squared
p.quad = summary(aranquad)$coefficients[2,4]
p.quad2 = summary(aranquad)$coefficients[3,4]
jd = c(min(aran$julianday):max(aran$julianday))
aranquad.predict = aranquad$coefficients[1] + aranquad$coefficients[2]*jd + aranquad$coefficients[3]*jd^2
points(jd, aranquad.predict, type = 'l')
max.jd.aranquad = jd[aranquad.predict == max(aranquad.predict)]

auch = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'AUCH', inputYear = '2015', inputSite = 117, plot = T, new = T, lwd = 2, main = 'AUCH')
auchquad = lm(meanDensity ~ julianday + I(julianday^2), data = auch)
R2.quad = summary(auchquad)$r.squared
p.quad = summary(auchquad)$coefficients[2,4]
p.quad2 = summary(auchquad)$coefficients[3,4]
jd = c(min(auch$julianday):max(auch$julianday))
auchquad.predict = auchquad$coefficients[1] + auchquad$coefficients[2]*jd + auchquad$coefficients[3]*jd^2
points(jd, auchquad.predict, type = 'l')
max.jd.auchquad = jd[auchquad.predict == max(auchquad.predict)]














multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database

library(zoo)

PRam = meanDensityByDay(labsurvey1, multorders, inputYear = 2015, inputSite = 117, plot = T, 
                        plotVar = 'meanDensity', new = T, color = 'blue')
BG = meanDensityByDay(visualsurveybg, multorders, inputYear = '2015', inputSite = 8892356,
                      plot = T, new = F, color = 'black') 


# Getting a better comparison with mismatching Julian Days
prday <- unique(PRam$julianday)
bgday <- unique(BG$julianday)
julianall1 <- c(prday, bgday)
julianall2 <- julianall1[order(julianall1)]
julianall <- data.frame(julianall2)

prFull1 <- merge(PRam, julianall, by.x = 'julianday', by.y = 'julianall2', all = T)
prFull2 <- prFull1[,c(1,5)]
prFull <- na.approx(prFull2)

bgFull1 <- merge(BG, julianall, by.x = 'julianday', by.y = 'julianall2', all = T)
bgFull2 <- bgFull1[,c(1,5)]
bgFull <- na.approx(bgFull2)

compbug <- merge(prFull, bgFull, by ='julianday', all = T)
names(compbug) <- c('julianday', 'prDensity', 'bgDensity')

plot(compbug$prDensity, compbug$bgDensity)










# Plotting and calculations (from summary_functions.r)

#################
# Prairie Ridge #
#################

# Temporarily going to take out julian day 170, not a PR day and only one caterpillar seen on one survey
# Also going to take out julian day 138, num surveys too high for Prairie Ridge
labsurvey1 <- labsurvey[labsurvey$julianday != 170, ]
labsurvey1 <- labsurvey[labsurvey$julianday != 138, ]

# Change arthCodes to 'NONE' that were previously NA
# Make sure data entered correctly before doing this
labsurvey1$arthCode[is.na(labsurvey1$arthCode)] = "NONE"
beatsheet$arthCode[is.na(beatsheet$arthCode)] = "NONE"
repsurvey$arthCode[is.na(repsurvey$arthCode)] = "NONE"
volsurvey$arthCode[is.na(volsurvey$arthCode)] = "NONE"

# Two possibilities of dealing with caterpillar colony outliers (choose one)
# (Rerun the separating data section in between switching possibilities)
# #1. Reducing counts with more than 5 caterpillars to 5 caterpillars:
labsurvey1$count[labsurvey1$arthCode == "LEPL" & labsurvey1$count > 5] = 5
beatsheet$count[beatsheet$arthCode == "LEPL" & beatsheet$count > 5] = 5
labsurvey1$count[labsurvey1$arthCode == "LEPL" & labsurvey1$count > 5] = 5
labsurvey1$count[labsurvey1$arthCode == "LEPL" & labsurvey1$count > 5] = 5
# #2. Taking out large colonies completely:
labsurvey1 <- labsurvey1[!(labsurvey1$arthCode == "LEPL" & labsurvey1$count > 10),]
beatsheet <- beatsheet[!(beatsheet$arthCode == "LEPL" & beatsheet$count > 10),]
repsurvey <- repsurvey[!(repsurvey$arthCode == "LEPL" & repsurvey$count > 10),]
volsurvey <- volsurvey[!(volsurvey$arthCode == "LEPL" & volsurvey$count > 10),]

# Plot our morning surveys, our beat sheet surveys, our repeat surveys, and the volunteer surveys all on one graph

# Caterpillars only, mean density
PRam = meanDensityByDay(labsurvey1, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Caterpillars only, fraction of surveys with at least one caterpillar
PRam = meanDensityByDay(labsurvey1, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = T, color = 'blue')
PRbs = meanDensityByDay(beatsheet, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'orange')
PRpm = meanDensityByDay(repsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'red')
PRvol = meanDensityByDay(volsurvey, "LEPL", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'fracSurveys', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# All orders, mean density
PRam.all = meanDensityByDay(labsurvey1, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = T, color = 'blue')
PRbs.all = meanDensityByDay(beatsheet, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'orange')
PRpm.all = meanDensityByDay(repsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'red')
PRvol.all = meanDensityByDay(volsurvey, "All", inputYear = 2015, inputSite = 117, plot = T, plotVar = 'meanDensity', new = F, color = 'green')
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))

# Selected orders, mean density
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI') # based on Birds of North America online, fledgling diet preferences, and the Avian Diet Database
PRam.mult = meanDensityByDay(labsurvey1, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, 
                             ylim = c(0,1.4))
PRbs.mult = meanDensityByDay(beatsheet, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'orange', minLength = 5)
PRpm.mult = meanDensityByDay(repsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                             plot = T, plotVar = 'meanDensity', new = F, color = 'red', minLength = 5)
PRvol.mult = meanDensityByDay(volsurvey, ordersToInclude = multorders, inputYear = 2015, inputSite = 117, 
                              plot = T, plotVar = 'meanDensity', new = F, color = 'green', minLength = 5)
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))





# Merge each of the subsets above together
PRall1 = merge(PRam[,c('julianday','meanDensity')], PRbs[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall1) = c('julianday','density_am','density_bs')
PRall2 = merge(PRall1, PRpm[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall2)[4] = 'density_pm'
PRall = merge(PRall2, PRvol[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall)[5] = 'density_vol'

####################
# Botanical Garden #
####################

BG = meanDensityByDay(visualsurveybg, 'LEPL', inputYear = '2015', inputSite = 8892356,
                      plot = T, new = T, color = 'black') 


