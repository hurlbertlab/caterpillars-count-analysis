# Final plotting for 395
# Only using Prairie Ridge data
# Tracie Hayes


#pdf('pr_density_biomass.pdf', width = 7, height = 9)
par(mfrow = c(3, 2), mar = c(3, 2.5, 3, 2.5), oma = c(4, 4, 3, 4))

#----Visual Surveys----

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


#----Beat Sheet Surveys----

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


#----Caterpillars Visual Surveys----

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
title("LEPL Visual Surveys", line = 1)


#----Caterpillar Beat Sheet Surveys---- 

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


#----Orthoptera Visual Surveys----

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


#----Orthoptera Beat Sheet Surveys---- 

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
mtext('Prairie Ridge', outer = TRUE, side = 3, line = 1, cex.lab = 2)

# 2nd pages of pdf

#----Auchenorrhyncha Visual Surveys----

# Get mean density and mean biomass data from function
compam.prauch <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prauch$julianday, compam.prauch$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.35))
par(new = T)
plot(compam.prauch$julianday, compam.prauch$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 2.5))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('darkorange2', 'darkslategray3'))
title("AUCH Visual Surveys", line = 1)


#----Auchenorrhyncha Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prauch <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prauch$julianday, compbs.prauch$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.28))
par(new = T)
plot(compbs.prauch$julianday, compbs.prauch$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.8))
axis(side=4)
title("AUCH Beat Sheet Surveys", line = 1)


#----Araneae Visual Surveys----

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


#----Araneae Beat Sheet Surveys---- 

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


#----Coleoptera Visual Surveys----

# Get mean density and mean biomass data from function
compam.prcole <- meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.prcole$julianday, compam.prcole$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.22))
par(new = T)
plot(compam.prcole$julianday, compam.prcole$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.9))
axis(side=4)
title("COLE Visual Surveys", line = 1)


#----Coleoptera Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.prcole <- meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.prcole$julianday, compbs.prcole$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.25))
par(new = T)
plot(compbs.prcole$julianday, compbs.prcole$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.6))
axis(side=4)
title("COLE Beat Sheet Surveys", line = 1)


mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)
mtext('Prairie Ridge', outer = TRUE, side = 3, line = 1, cex.lab = 2)

dev.off()



#---- BOT GARDEN ----
#---- PROBLEMS ---- 

#pdf('pr_density_biomass.pdf', width = 7, height = 9)
par(mfrow = c(3, 2), mar = c(3, 2.5, 3, 2.5), oma = c(4, 4, 3, 4))

#----Visual Surveys----

# Get mean density and mean biomass data from function
compam.bg <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bg$julianday, compam.bg$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0.15, 0.7))
par(new = T)
plot(compam.bg$julianday, compam.bg$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(1.0, 4.6))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('darkorange2', 'darkslategray3'))
title("Visual Surveys", line = 1)


#----Beat Sheet Surveys----

# Get mean density and mean biomass data from function
compbs.bg <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = regorders, 
                              minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bg$julianday, compbs.bg$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.64))
par(new = T)
plot(compbs.bg$julianday, compbs.bg$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 4.3))
axis(side=4)
title("Beat Sheet Surveys", line = 1)


#----Caterpillars Visual Surveys----

# Get mean density and mean biomass data from function
compam.bglepl <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = 'LEPL', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bglepl$julianday, compam.bglepl$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.125))
par(new = T)
plot(compam.bglepl$julianday, compam.bglepl$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.4))
axis(side=4)
title("LEPL Visual Surveys", line = 1)


#----Caterpillar Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.bglepl <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = 'LEPL', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bglepl$julianday, compbs.bglepl$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.bglepl$julianday, compbs.bglepl$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("LEPL Beat Sheet Surveys", line = 1)


#----Orthoptera Visual Surveys----

# Get mean density and mean biomass data from function
compam.bgorth <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bgorth$julianday, compam.bgorth$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.105))
par(new = T)
plot(compam.bgorth$julianday, compam.bgorth$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.7))
axis(side=4)
title("ORTH Visual Surveys", line = 1)


#----Orthoptera Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.bgorth <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = 'ORTH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bgorth$julianday, compbs.bgorth$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.16))
par(new = T)
plot(compbs.bgorth$julianday, compbs.bgorth$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
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
compam.bgauch <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bgauch$julianday, compam.bgauch$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.35))
par(new = T)
plot(compam.bgauch$julianday, compam.bgauch$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 2.5))
axis(side=4)
legend("topleft", c('Mean density', 'Mean biomass'),
       lwd = c(2,2), lty = c(1,1), col = c('darkorange2', 'darkslategray3'))
title("AUCH Visual Surveys", line = 1)


#----Auchenorrhyncha Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.bgauch <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = 'AUCH', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bgauch$julianday, compbs.bgauch$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.28))
par(new = T)
plot(compbs.bgauch$julianday, compbs.bgauch$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.8))
axis(side=4)
title("AUCH Beat Sheet Surveys", line = 1)


#----Araneae Visual Surveys----

# Get mean density and mean biomass data from function
compam.bgaran <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bgaran$julianday, compam.bgaran$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.09))
par(new = T)
plot(compam.bgaran$julianday, compam.bgaran$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.6))
axis(side=4)
title("ARAN Visual Surveys", line = 1)


#----Araneae Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.bgaran <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = 'ARAN', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bgaran$julianday, compbs.bgaran$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.06))
par(new = T)
plot(compbs.bgaran$julianday, compbs.bgaran$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.45))
axis(side=4)
title("ARAN Beat Sheet Surveys", line = 1)


#----Coleoptera Visual Surveys----

# Get mean density and mean biomass data from function
compam.bgcole <- meanDensityByDay(surveyData = amsurvey.bg, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compam.bgcole$julianday, compam.bgcole$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.22))
par(new = T)
plot(compam.bgcole$julianday, compam.bgcole$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.9))
axis(side=4)
title("COLE Visual Surveys", line = 1)


#----Coleoptera Beat Sheet Surveys---- 

# Get mean density and mean biomass data from function
compbs.bgcole <- meanDensityByDay(surveyData = beatsheet.bg, ordersToInclude = 'COLE', 
                                  minLength = 5, inputSite = 117, inputYear = 2015, plot = F)

# Plot mean density and mean biomass on same plot
plot(compbs.bgcole$julianday, compbs.bgcole$meanDensity, type = 'l', lwd = 2, col = 'darkorange2',
     xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 0.25))
par(new = T)
plot(compbs.bgcole$julianday, compbs.bgcole$meanBiomass, type = 'l', lwd = 2, col = 'darkslategray3', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(134, 204), ylim = c(0, 1.6))
axis(side=4)
title("COLE Beat Sheet Surveys", line = 1)


mtext('Julian day', side = 1, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean density', side = 2, outer = TRUE, line = 1, cex.lab = 2)
mtext('Mean biomass', side = 4, outer = TRUE, line = 1, cex.lab = 2)
mtext('Prairie Ridge', outer = TRUE, side = 3, line = 1, cex.lab = 2)

dev.off()