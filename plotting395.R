# Final plotting for 395
# Only using Prairie Ridge data
# Tracie Hayes

par(mfrow = c(3, 2), mar = c(3, 2.5, 3, 2.5), oma = c(4, 4, 3, 4))
#pdf('density_biomass.pdf', width = 7, height = 9)

#----Visual Surveys----

# Get mean density and mean biomass data from function
compam.pr <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.pr$julianday, compam.pr$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0.15, 0.7))
par(new = T)
plot(compam.pr$julianday, compam.pr$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.6))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("Visual Surveys", line = 1)


#----Beat Sheet Surveys----

# Get mean density and mean biomass data from function
compbs.pr <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.pr$julianday, compbs.pr$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0.15, 0.64))
par(new = T)
plot(compbs.pr$julianday, compbs.pr$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0.5, 4.3))
axis(side=4)
title("Beat Sheet Surveys", line = 1)


#----Caterpillars Visual Surveys----

# Get mean density and mean biomass data from function
compam.prlepl <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'LEPL', 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prlepl$julianday, compam.prlepl$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.125))
par(new = T)
plot(compam.prlepl$julianday, compam.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.9))
axis(side=4)
title("LEPL Visual Surveys", line = 1)


#----Caterpillar Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prlepl <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'LEPL', 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prlepl$julianday, compbs.prlepl$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.prlepl$julianday, compbs.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("LEPL Beat Sheet Surveys", line = 1)


#----Orthoptera Visual Surveys----

# Get mean density and mean biomass data from function
compam.prorth <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prorth$julianday, compam.prorth$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.105))
par(new = T)
plot(compam.prorth$julianday, compam.prorth$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("ORTH Visual Surveys", line = 1)


#----Orthoptera Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prorth <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prorth$julianday, compbs.prorth$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.prorth$julianday, compbs.prorth$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.35))
axis(side=4)
title("ORTH Beat Sheet Surveys", line = 1)

mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)
mtext('Prairie Ridge', outer = TRUE, side = 3, line = 1, cex.lab = 2)

# 2nd pages of pdf

#----Auchenorrhyncha Visual Surveys----

# Get mean density and mean biomass data from function
compam.prauch <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prauch$julianday, compam.prauch$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.35))
par(new = T)
plot(compam.prauch$julianday, compam.prauch$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 2.5))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("AUCH Visual Surveys", line = 1)


#----Auchenorrhyncha Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prauch <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prauch$julianday, compbs.prauch$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.28))
par(new = T)
plot(compbs.prauch$julianday, compbs.prauch$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.8))
axis(side=4)
title("AUCH Beat Sheet Surveys", line = 1)


#----Araneae Visual Surveys----

# Get mean density and mean biomass data from function
compam.praran <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.praran$julianday, compam.praran$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.09))
par(new = T)
plot(compam.praran$julianday, compam.praran$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.6))
axis(side=4)
title("ARAN Visual Surveys", line = 1)


#----Araneae Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.praran <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.praran$julianday, compbs.praran$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.06))
par(new = T)
plot(compbs.praran$julianday, compbs.praran$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.55))
axis(side=4)
title("ARAN Beat Sheet Surveys", line = 1)


#----Coleoptera Visual Surveys----

# Get mean density and mean biomass data from function
compam.prcole <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prcole$julianday, compam.prcole$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.22))
par(new = T)
plot(compam.prcole$julianday, compam.prcole$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.9))
axis(side=4)
title("COLE Visual Surveys", line = 1)


#----Coleoptera Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prcole <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prcole$julianday, compbs.prcole$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.25))
par(new = T)
plot(compbs.prcole$julianday, compbs.prcole$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.6))
axis(side=4)
title("COLE Beat Sheet Surveys", line = 1)


mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)
mtext('Prairie Ridge', outer = TRUE, side = 3, line = 1, cex.lab = 2)