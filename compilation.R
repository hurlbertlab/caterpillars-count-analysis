# Comparing GDD, greenup, arthropod phenology, and avian reproductive timing

source('C:/git/caterpillars-count-analysis/prism_10year.R')

source('C:/git/caterpillars-count-analysis/modis_10year.R')

source('C:/git/caterpillars-count-analysis/extensive_plotting.R')

setwd('c:/git/caterpillars-count-analysis')

# Is greenup affected by GDD?
# Working with two datasets (one from prism_10year.R and one from modis_10year.R)

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)


#---- GDD and greenup (deviation from mean) ----
par(mar = c(4,4,2,2), mfrow = c(3,2))
plot(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, col = 'white',
     xlab = 'GDD JD deviation from mean', ylab = 'Greenup JD deviation from mean',
     xlim = c(-11,11), ylim = c(-10,10), main = 'Greenup JD vs. GDD JD')
#abline(1,0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, greenupgdd$year, col = 'red')
text(greenupgdd$bg.gdd.dev, greenupgdd$bggreenup.dev, greenupgdd$year, col = 'blue')

# Blank plot for pdf
plot.new()
legend('bottomleft', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15), cex = 1.2)
legend('topleft', c('Prairie Ridge', 'Botanical Garden'), pch = 16, col = c('red', 'blue'), cex = 1.2)

#---- Arths and greenup - visual surveys----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth JD',xlab='Greenup JD', ylim = c(160,185), xlim = c(85, 95),
     main = 'Visual Surveys: Arth JD vs. Greenup JD')
maxden1 <- max(PR.LEPL15vis[PR.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15vis[PR.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16vis[PR.LEPL16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16vis[PR.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
         col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15vis[BG.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15vis[BG.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16vis[BG.LEPL16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16vis[BG.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
         col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
         col = 'blue')
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))

#---- Arths and GDD - visual surveys----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth JD',xlab='GDD JD', ylim = c(160,185), xlim = c(155, 165), 
main = 'Visual Surveys: Arth JD vs. GDD JD')
maxden1 <- max(PR.LEPL15vis[PR.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15vis[PR.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16vis[PR.LEPL16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16vis[PR.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
         col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15vis[BG.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15vis[BG.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16[BG.LEPL16$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16vis[BG.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
         col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
         col = 'blue')
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))

#---- Arths and greenup - beat sheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth JD',xlab='Greenup JD', ylim = c(125,185), xlim = c(85, 95), 
     main = 'Beat sheets: Arth JD vs. Greenup JD')
maxden1 <- max(PR.LEPL15bts[PR.LEPL15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15bts$julianday[PR.LEPL15bts$meanDensity == maxden1 & PR.LEPL15bts$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15bts[PR.BIRD15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15bts$julianday[PR.BIRD15bts$meanDensity == maxden2 & PR.BIRD15bts$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16bts[PR.LEPL16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16bts$julianday[PR.LEPL16bts$meanDensity == maxden3 & PR.LEPL16bts$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16bts[PR.BIRD16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16bts$julianday[PR.BIRD16bts$meanDensity == maxden4 & PR.BIRD16bts$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15bts$julianday[PR.LEPL15bts$meanDensity == maxden1 & PR.LEPL15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16bts$julianday[PR.LEPL16bts$meanDensity == maxden3 & PR.LEPL16bts$julianday %in% c(80:185)], 
         col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15bts$julianday[PR.BIRD15bts$meanDensity == maxden2 & PR.BIRD15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16bts$julianday[PR.BIRD16bts$meanDensity == maxden4 & PR.BIRD16bts$julianday %in% c(80:185)], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15bts[BG.LEPL15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15bts$julianday[BG.LEPL15bts$meanDensity == maxden5 & BG.LEPL15bts$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15bts[BG.BIRD15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15bts$julianday[BG.BIRD15bts$meanDensity == maxden6 & BG.BIRD15bts$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16bts[BG.LEPL16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16bts$julianday[BG.LEPL16bts$meanDensity == maxden7 & BG.LEPL16bts$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16bts[BG.BIRD16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16bts$julianday[BG.BIRD16bts$meanDensity == maxden8 & BG.BIRD16bts$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15bts$julianday[BG.LEPL15bts$meanDensity == maxden5 & BG.LEPL15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16bts$julianday[BG.LEPL16bts$meanDensity == maxden7 & BG.LEPL16bts$julianday %in% c(80:185)], 
         col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15bts$julianday[BG.BIRD15bts$meanDensity == maxden6 & BG.BIRD15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16bts$julianday[BG.BIRD16bts$meanDensity == maxden8 & BG.BIRD16bts$julianday %in% c(80:185)], 
         col = 'blue')
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))

#---- Arths and GDD - beatsheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth JD',xlab='GDD JD', ylim = c(125,185), xlim = c(155, 165), 
     main = 'Beat sheets: Arth JD vs. GDD JD')
maxden1 <- max(PR.LEPL15bts[PR.LEPL15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15bts$julianday[PR.LEPL15bts$meanDensity == maxden1 & PR.LEPL15bts$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15bts[PR.BIRD15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15bts$julianday[PR.BIRD15bts$meanDensity == maxden2 & PR.BIRD15bts$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16bts[PR.LEPL16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16bts$julianday[PR.LEPL16bts$meanDensity == maxden3 & PR.LEPL16bts$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16bts[PR.BIRD16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16bts$julianday[PR.BIRD16bts$meanDensity == maxden4 & PR.BIRD16bts$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15bts$julianday[PR.LEPL15bts$meanDensity == maxden1 & PR.LEPL15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16bts$julianday[PR.LEPL16bts$meanDensity == maxden3 & PR.LEPL16bts$julianday %in% c(80:185)], 
         col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15bts$julianday[PR.BIRD15bts$meanDensity == maxden2 & PR.BIRD15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16bts$julianday[PR.BIRD16bts$meanDensity == maxden4 & PR.BIRD16bts$julianday %in% c(80:185)], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15bts[BG.LEPL15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15bts$julianday[BG.LEPL15bts$meanDensity == maxden5 & BG.LEPL15bts$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15bts[BG.BIRD15bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15bts$julianday[BG.BIRD15bts$meanDensity == maxden6 & BG.BIRD15bts$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16[BG.LEPL16$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16bts$julianday[BG.LEPL16bts$meanDensity == maxden7 & BG.LEPL16bts$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16bts[BG.BIRD16bts$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16bts$julianday[BG.BIRD16bts$meanDensity == maxden8 & BG.BIRD16bts$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15bts$julianday[BG.LEPL15bts$meanDensity == maxden5 & BG.LEPL15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16bts$julianday[BG.LEPL16bts$meanDensity == maxden7 & BG.LEPL16bts$julianday %in% c(80:185)], 
         col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15bts$julianday[BG.BIRD15bts$meanDensity == maxden6 & BG.BIRD15bts$julianday %in% c(80:185)], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16bts$julianday[BG.BIRD16bts$meanDensity == maxden8 & BG.BIRD16bts$julianday %in% c(80:185)], 
         col = 'blue')
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))


# Birds and greenup

# Birds and GDD


# ANOVAs

prmod1 <- lm(prgreenup.log ~ pr.gdd, data = greenupgdd)
summary(prmod1) # significant
prmod2 <- lm(prgreenup.half ~ pr.gdd, data = greenupgdd)
summary(prmod2)

bgmod1 <- lm(bggreenup.log ~ bg.gdd, data = greenupgdd)
summary(bgmod1)
greenupgdd$temp.resids<-residuals(bgmod1)
new<-lm(temp.resids ~ bg.gdd, data = greenupgdd)
summary(new)

bgmod2 <- lm(bggreenup.half ~ bg.gdd, data = greenupgdd)
summary(bgmod2)

hbmod1 <- lm(hbgreenup.log ~ hb.gdd, data = greenupgdd)
summary(hbmod1)
hbmod2 <- lm(hbgreenup.half ~ hb.gdd, data = greenupgdd)
summary(hbmod2)

# None of the others significant
# [GDD not a good predictor of greenup??]

# Interesting plots (from meeting on 10/26/2016)
plot(greenupgdd$pr.gdd, greenupgdd$prgreenup.log)
text(greenupgdd$pr.gdd, greenupgdd$prgreenup.log, greenupgdd$year)
plot(greenupgdd$bg.gdd, greenupgdd$bggreenup.log)
text(greenupgdd$bg.gdd, greenupgdd$bggreenup.log, greenupgdd$year)
plot(greenupgdd$bg.gdd, greenupgdd$pr.gdd)
plot(greenupgdd$prgreenup.log, greenupgdd$bggreenup.log)

# Calculate max cat for 2015 and 2016 (visual, beat sheet)
# Still unsure about what to use as peak to find JD
# Just largest number before a certain julian day like 180?
#greenupgdd$prlepl15 <- c(rep(0,8), PR.LEPL15[max(PR.LEPL15[PR.LEPL15$julianday %in% c(80:180),]$meanDensity),]$julianday)
