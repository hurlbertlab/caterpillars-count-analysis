# Final plotting for 395
# Only using Prairie Ridge data
# Tracie Hayes

#----Visual Surveys----

# Get mean density and mean biomass data from function
compam.pr <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(compam.pr$julianday, compam.pr$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "Julian day", ylab = "Mean density", xlim = c(134, 204), ylim = c(0.15, 0.64))
par(new = T)
plot(compam.pr$julianday, compam.pr$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.3))
axis(side=4)
mtext("Mean biomass", side=4, las = 0, line = 3)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("Visual Surveys", line = 1)


#----Beat Sheet Surveys----

# Get mean density and mean biomass data from function
compbs.pr <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(compbs.pr$julianday, compbs.pr$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "Julian day", ylab = "Mean density", xlim = c(134, 204), ylim = c(0.15, 0.64))
par(new = T)
plot(compbs.pr$julianday, compbs.pr$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.3))
axis(side=4)
mtext("Mean biomass", side=4, las = 0, line = 3)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("Beat Sheet Surveys", line = 1)


#----Caterpillars Visual Surveys----

# Get mean density and mean biomass data from function
compam.prlepl <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'LEPL', 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(compam.prlepl$julianday, compam.prlepl$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "Julian day", ylab = "Mean density", xlim = c(134, 204), ylim = c(0, 0.13))
par(new = T)
plot(compam.prlepl$julianday, compam.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1))
axis(side=4)
mtext("Mean biomass", side=4, las = 0, line = 3)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("Caterpillar Visual Surveys", line = 1)


#----Caterpillars Beat Sheet Surveys---- #have not fixed axes of this one yet

# Get mean density and mean biomass data from function
compbs.prlepl <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'LEPL', 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(compbs.prlepl$julianday, compbs.prlepl$meanDensity, type = 'l', lwd = 2, col = 'red',
     xlab = "Julian day", ylab = "Mean density", xlim = c(134, 204), ylim = c(0.15, 0.64))
par(new = T)
plot(compbs.prlepl$julianday, compbs.prlepl$meanBiomass, type = 'l', lwd = 2, col = 'blue', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.3))
axis(side=4)
mtext("Mean biomass", side=4, las = 0, line = 3)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('red', 'blue'))
title("Caterpillar Beat Sheet Surveys", line = 1)