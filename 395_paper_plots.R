# Script for creating plots for 395 paper - Fall 2015
# Tracie Hayes

# Set working directory for where you want plot pdfs to appear
#setwd('c:/users/hayeste/my documents/395/395 paper figures')
# (Take out # on pdf lines and dev.off() lines to actually create pdfs)

# Numbers based on what is used in paper


#----Figure 2----

#pdf('figure_2.pdf', width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# Set Julian day range
jd = c(134:204)

# Plot LEPL beat sheet mean density data
lepl = meanDensityByDay(beatsheet.pr, ordersToInclude = 'LEPL', inputYear = 2015, inputSite = 117, plot = T, new = T, lwd = 2, main = 'LEPL Beat Sheet Surveys')

# Fit a quadratic to LEPL beat sheet data
leplquad = lm(meanDensity ~ julianday + I(julianday^2), data = lepl)

# Find R squared and p-values
R2.quad = summary(leplquad)$r.squared
p.quad = summary(leplquad)$coefficients[2,4]
p.quad2 = summary(leplquad)$coefficients[3,4]

# Plot quadratic fit
jd = c(min(lepl$julianday):max(lepl$julianday))
leplquad.predict = leplquad$coefficients[1] + leplquad$coefficients[2]*jd + leplquad$coefficients[3]*jd^2
points(jd, leplquad.predict, type = 'l')
max.jd.leplquad = jd[leplquad.predict == max(leplquad.predict)]

# Plot ORTH beat sheet mean density data
orth = meanDensityByDay(beatsheet.pr, ordersToInclude = 'ORTH', inputYear = 2015, inputSite = 117, plot = T, new = T, lwd = 2, main = 'ORTH Beat Sheet Surveys')

# Fit a quadratic to ORTH beat sheet data
orthquad = lm(meanDensity ~ julianday + I(julianday^2), data = orth)

# Find R squared and p-values
R2.quad = summary(orthquad)$r.squared
p.quad = summary(orthquad)$coefficients[2,4]
p.quad2 = summary(orthquad)$coefficients[3,4]

# Plot quadratic fit
jd = c(min(orth$julianday):max(orth$julianday))
orthquad.predict = orthquad$coefficients[1] + orthquad$coefficients[2]*jd + orthquad$coefficients[3]*jd^2
points(jd, orthquad.predict, type = 'l')
max.jd.orthquad = jd[orthquad.predict == max(orthquad.predict)]

# Add x and y labels to both plots
title(xlab = "Julian day",
      ylab = "Mean density",
      outer = TRUE, line = 1, cex.lab = 1)

#dev.off()


#----Figure 3----

#pdf('figure_3.pdf', width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# Plotting mean density and mean biomass

## All arthropod groups - visual surveys

# Get mean density and mean biomass data from function
compam.pr <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.pr$julianday, compam.pr$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0.15, 0.7))
par(new = T)
plot(compam.pr$julianday, compam.pr$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.6))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('darkorange2', 'darkslategray3'))
title("Visual Surveys", line = 1)

## All arthropod groups - beat sheet surveys

# Get mean density and mean biomass data from function
compbs.pr <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.pr$julianday, compbs.pr$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.64))
par(new = T)
plot(compbs.pr$julianday, compbs.pr$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 4.3))
axis(side=4)
title("Beat Sheet Surveys", line = 1)

mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)

#dev.off()



#----Figure 4a----

# All figure 4s: plotting mean density and mean biomass for certain arthropod groups

#pdf('figure_4a.pdf', width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# LEPL visual survey

# Get mean density and mean biomass data from function
compam.prlepl <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'LEPL', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prlepl$julianday, compam.prlepl$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.125))
par(new = T)
plot(compam.prlepl$julianday, compam.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.4))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('darkorange2', 'darkslategray3'))
title("LEPL Visual Surveys", line = 1)

# LEPL beat sheet

# Get mean density and mean biomass data from function
compbs.prlepl <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'LEPL', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prlepl$julianday, compbs.prlepl$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.prlepl$julianday, compbs.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("LEPL Beat Sheet Surveys", line = 1)

mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)

#dev.off()

#----Figure 4b----

#pdf('figure_4b.pdf', width = 8, height = 4)
#par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# ORTH visual survey

# Get mean density and mean biomass data from function
compam.prorth <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prorth$julianday, compam.prorth$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.105))
par(new = T)
plot(compam.prorth$julianday, compam.prorth$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("ORTH Visual Surveys", line = 1)

# ORTH beat sheet

# Get mean density and mean biomass data from function
compbs.prorth <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prorth$julianday, compbs.prorth$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.prorth$julianday, compbs.prorth$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.35))
axis(side=4)
title("ORTH Beat Sheet Surveys", line = 1)

mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)

#dev.off()

#----Figure 4c----

#pdf('figure_4c.pdf', width = 8, height = 4)
#par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# ARAN visual survey

# Get mean density and mean biomass data from function
compam.praran <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.praran$julianday, compam.praran$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.09))
par(new = T)
plot(compam.praran$julianday, compam.praran$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.6))
axis(side=4)
title("ARAN Visual Surveys", line = 1)

# ARAN beat sheet

# Get mean density and mean biomass data from function
compbs.praran <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.praran$julianday, compbs.praran$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.06))
par(new = T)
plot(compbs.praran$julianday, compbs.praran$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.45))
axis(side=4)
title("ARAN Beat Sheet Surveys", line = 1)

mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)

#dev.off()



#----Figure 5----

#pdf('figure_5.pdf', width = 8, height = 4)
par(mfrow = c(1, 2), mar = c(2, 2.5, 2, 2.5), oma = c(2, 2, 2, 2))

# Get mean density information for all arthropod groups
am.meanden <- meanDensityByDay(surveyData = amsurvey.pr, minLength = 5, inputSite = 117, 
                               inputYear = 2015, plot = F)
bs.meanden <- meanDensityByDay(surveyData = beatsheet.pr, minLength = 5, inputSite = 117, 
                               inputYear = 2015, plot = F)

# Plot beat sheet mean density vs. visual survey mean density for all arthropod groups
plot(am.meanden$meanDensity, bs.meanden$meanDensity, main = 'All Orders', xlim = c(0, 0.61), ylim = c(0, 0.61))
# Add a linear regression line
fig4all = lm(bs.meanden$meanDensity ~ am.meanden$meanDensity)
abline(fig4all)
# Add a 1:1 line
abline(a = 0, b = 1, lty = 2)

# Plot beat sheet mean density vs. visual survey mean density for all arthropod groups
leplboth = merge(compam.prlepl, compbs.prlepl, by = 'julianday', all = F)
plot(leplboth$meanDensity.x, leplboth$meanDensity.y, main = 'Lepidoptera Larvae', xlim = c(0, 0.14), ylim = c(0, 0.14))
# Add a linear regression line
fig4lepl = lm(leplboth$meanDensity.y ~ leplboth$meanDensity.x)
abline(fig4lepl)
# Add a 1:1 line
abline(a = 0, b = 1, lty = 2)

# Add x and y labels
title(xlab = "Visual surveys mean density",
      ylab = "Beat sheet surveys mean density",
      outer = TRUE, line = 1, cex.lab = 1)

#dev.off()



#---- Table 1 ----

## FINDING PEAKS

# All arthropod groups:

# Find maximum mean density and maximum biomass for regorders arthropods
compbs.pr$julianday[compbs.pr$meanDensity == max(compbs.pr$meanDensity)] #mean density peak
compbs.pr$julianday[compbs.pr$meanBiomass == max(compbs.pr$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for regorders arthropods quadratic fits
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.pr)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quadratic peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.pr)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quadratic peak

# LEPL:

# Find maximum mean density and maximum biomass for LEPL
compbs.prlepl$julianday[compbs.prlepl$meanDensity == max(compbs.prlepl$meanDensity)] #mean density peak
compbs.prlepl$julianday[compbs.prlepl$meanBiomass == max(compbs.prlepl$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for LEPL
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.prlepl)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quad peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.prlepl)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quad peak

# ORTH:

# Find maximum mean density and maximum biomass for ORTH
compbs.prorth$julianday[compbs.prorth$meanDensity == max(compbs.prorth$meanDensity)] #mean density peak
compbs.prorth$julianday[compbs.prorth$meanBiomass == max(compbs.prorth$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for ORTH
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.prorth)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quad peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.prorth)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quad peak

# AUCH
compbs.prauch <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Find maximum mean density and maximum biomass for AUCH
compbs.prauch$julianday[compbs.prauch$meanDensity == max(compbs.prauch$meanDensity)] #mean density peak
compbs.prauch$julianday[compbs.prauch$meanBiomass == max(compbs.prauch$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for AUCH
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.prauch)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quad peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.prauch)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quad peak

# ARAN

# Find maximum mean density and maximum biomass for ARAN
compbs.praran$julianday[compbs.praran$meanDensity == max(compbs.praran$meanDensity)] #mean density peak
compbs.praran$julianday[compbs.praran$meanBiomass == max(compbs.praran$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for ARAN
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.praran)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quad peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.praran)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quad peak


# COLE
compbs.prcole <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Find maximum mean density and maximum biomass for COLE
compbs.prcole$julianday[compbs.prcole$meanDensity == max(compbs.prcole$meanDensity)] #mean density peak
compbs.prcole$julianday[compbs.prcole$meanBiomass == max(compbs.prcole$meanBiomass)] #mean biomass peak

# Find maximum mean density and maximum biomass for COLE
jd = c(134:204)
quaddensity = lm(meanDensity ~ julianday + I(julianday^2), data = compbs.prcole)
quaddensity.predict = quaddensity$coefficients[1] + quaddensity$coefficients[2]*jd + quaddensity$coefficients[3]*jd^2
jd[quaddensity.predict == max(quaddensity.predict)] #mean density quad peak
quadbiomass = lm(meanBiomass ~ julianday + I(julianday^2), data = compbs.prcole)
quadbiomass.predict = quadbiomass$coefficients[1] + quadbiomass$coefficients[2]*jd + quadbiomass$coefficients[3]*jd^2
jd[quadbiomass.predict == max(quadbiomass.predict)] #mean biomass quad peak




