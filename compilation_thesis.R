# Comparing GDD, greenup, arthropod phenology, and avian reproductive timing

source('C:/git/caterpillars-count-analysis/prism_10year.R') # GDDs

source('C:/git/caterpillars-count-analysis/modis_10year.R') # Spring green-up

source('C:/git/caterpillars-count-analysis/spline_thesis.R') # Arths peaks

source('C:/git/caterpillars-count-analysis/eBird_logistics.R') # Bird arrival

setwd('c:/git/caterpillars-count-analysis')



# Is greenup affected by GDD?
# Working with two datasets (one from prism_10year.R and one from modis_10year.R)

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)

#---- FIGURE 3 RESULTS----

#---- GDD and greenup (deviation from mean) ----
par(mar = c(4,4,2,2), mfrow = c(2,2), oma = c(2,2,1,1))
plot(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, col = 'white',
     xlab = 'GDD JD deviation from mean', ylab = 'Greenup JD deviation from mean',
     xlim = c(-12,12), ylim = c(-12,12))
legend("topleft", "A", bty="n")
#abline(1,1, lty = 2) # what's going on with this?
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, greenupgdd$year, col = 'red')
text(greenupgdd$bg.gdd.dev, greenupgdd$bggreenup.dev, greenupgdd$year, col = 'blue')

# Blank plot for pdf
plot.new()
legend('topleft', c('Prairie Ridge (PR)', 'Botanical Garden (BG)'), pch = 16, col = c('red', 'blue'), cex = 1)

plot(greenupgdd$pr.gdd, greenupgdd$bg.gdd, xlab = 'PR GDD', ylab = 'BG GDD')
legend("topleft", "B", bty="n")
gddlm = lm(greenupgdd$bg.gdd ~ greenupgdd$pr.gdd)
abline(gddlm)
summary(gddlm)
abline(1,1, lty = 2)
plot(greenupgdd$prgreenup.log, greenupgdd$bggreenup.log, xlab = 'PR Greenup', ylab = 'BG Greenup')
legend("topleft", "C", bty="n")
greenuplm = lm(greenupgdd$bggreenup.log ~ greenupgdd$prgreenup.log)
abline(greenuplm)
summary(greenuplm)
abline(1,1, lty = 2)


#---- FIGURE 4 RESULTS ----
par(mar = c(4,4,2,2), mfrow = c(3,2))

# Blank plot for pdf
plot.new()
legend('bottomleft', c('caterpillars','orthopterans', 'bird food'), pch = 16, cex = 1.2, col = c('seagreen3', 'plum', 'orange3'))
legend('topleft', c('Prairie Ridge 2015 21', 'Prairie Ridge 2016 16','Botanical Garden 2015 22', 'Botanical Garden 2016 15'), pch = c(21,16,22,15), cex = 1.2)
plot.new()

#---- Arths and greenup - visual surveys----
par(mar = c(4,4,2,2), mfrow = c(2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak JD',xlab='Greenup JD', xlim = c(86,94), ylim = c(150, 210),
     main = 'Visual')
legend("topleft", "A", bty="n")
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15.vis.max, 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15.vis.max, 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.ORTH15.vis.max, 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16.vis.max, 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16.vis.max, 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.ORTH16.vis.max, 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15.vis.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15.vis.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.ORTH15.vis.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16.vis.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16.vis.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.ORTH16.vis.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.vis.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))

#---- Arths and GDD - visual surveys----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak JD',xlab='GDD JD', xlim = c(156,164), ylim = c(150, 210),
main = 'Visual')
legend("topleft", "B", bty="n")
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15.vis.max, 
       pch = 21, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15.vis.max, 
       pch = 21, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.ORTH15.vis.max, 
       pch = 21, type = 'p', cex = 2, col = 'plum')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16.vis.max, 
       pch = 16, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16.vis.max, 
       pch = 16, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.ORTH16.vis.max, 
       pch = 16, type = 'p', cex = 2, col = 'plum')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.vis.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.vis.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.vis.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15.vis.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15.vis.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.ORTH15.vis.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'plum')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16.vis.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16.vis.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.ORTH16.vis.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'plum')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.vis.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.vis.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.vis.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))

#---- Arths and greenup - beat sheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak JD',xlab='Greenup JD', xlim = c(86,94), ylim = c(150, 210),
     main = 'Beat sheets')
legend("topleft", "C", bty="n")
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.ORTH15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.ORTH16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.ORTH15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.ORTH16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))

#---- Arths and GDD - beat sheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak JD',xlab='GDD JD', xlim = c(156,164), ylim = c(150, 210),
     main = 'Beat sheets')
legend("topleft", "D", bty="n")
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.ORTH15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'plum')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.ORTH16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'plum')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.ORTH15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'plum')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.ORTH16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'plum')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))


#---- FIGURE 5 RESULTS ----

par(mar = c(4,4,2,2), mfrow = c(3,2))

# Blank plot for pdf
plot.new()
legend('bottomleft', c('indigo bunting', 'red-eyed vireo', 'common yellowthroat', 'blue-gray gnatcatcher'), pch = 16, col = c('blue', 'red', 'orange', 'gray'), cex = 1.2)
plot.new()

# Prairie Ridge GDD

inf_pr_inbu = inflection_pr[inflection_pr$scientific_name == 'Passerina cyanea',]
inbu_pr = merge(inf_pr_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_pr$pr.gdd, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'dodgerblue4', 
     ylim = c(82, 117), main = 'Prairie Ridge', xlab = 'GDD JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "A", bty="n")
inbu_lm_prgdd = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$pr.gdd)
abline(inbu_lm_prgdd, col = 'blue')
summary(inbu_lm_prgdd)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$pr.gdd, as.numeric(as.character(revi_pr$inflection_pt)), pch = 16, col = 'red')
revi_lm_prgdd = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$pr.gdd)
abline(revi_lm_prgdd, col = 'red')
summary(revi_lm_prgdd)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$pr.gdd, as.numeric(as.character(coye_pr$inflection_pt)), pch = 16, col = 'orange')
coye_lm_prgdd = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$pr.gdd)
abline(coye_lm_prgdd, col = 'orange')
summary(coye_lm_prgdd)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$pr.gdd, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 16, col = 'gray')
bggn_lm = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$pr.gdd)
abline(bggn_lm_prgdd, col = 'gray')
summary(bggn_lm_prgdd)


# Prairie Ridge Greenup

inf_pr_inbu = inflection_pr[inflection_pr$scientific_name == 'Passerina cyanea',]
inbu_pr = merge(inf_pr_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_pr$prgreenup.log, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'dodgerblue4', 
     ylim = c(82, 117), main = 'Prairie Ridge', xlab = 'Greenup JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "B", bty="n")
inbu_lm_prgreen = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$prgreenup.log)
abline(inbu_lm_prgreen, col = 'blue')
summary(inbu_lm_prgreen)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$prgreenup.log, as.numeric(as.character(revi_pr$inflection_pt)), pch = 16, col = 'red')
revi_lm_prgreen = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$prgreenup.log)
abline(revi_lm_prgreen, col = 'red')
summary(revi_lm_prgreen)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$prgreenup.log, as.numeric(as.character(coye_pr$inflection_pt)), pch = 16, col = 'orange')
coye_lm_prgreen = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$prgreenup.log)
abline(coye_lm_prgreen, col = 'orange')
summary(coye_lm_prgreen)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$prgreenup.log, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 16, col = 'gray')
bggn_lm_prgreen = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$prgreenup.log)
abline(bggn_lm_prgreen, col = 'gray')
summary(bggn_lm_prgreen)

# Botanical Garden GDD

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bg.gdd, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue4', 
     ylim = c(82, 117), main = 'Botanical Garden', xlab = 'GDD JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "C", bty="n")
inbu_lm_bggdd = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bg.gdd)
abline(inbu_lm_bggdd, col = 'blue')
summary(inbu_lm_bggdd)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bg.gdd, as.numeric(as.character(revi_bg$inflection_pt)), pch = 16, col = 'red')
revi_lm_bggdd = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bg.gdd)
abline(revi_lm_bggdd, col = 'red')
summary(revi_lm_bggdd)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bg.gdd, as.numeric(as.character(coye_bg$inflection_pt)), pch = 16, col = 'orange')
coye_lm_bggdd = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bg.gdd)
abline(coye_lm_bggdd, col = 'orange')
summary(coye_lm_bggdd)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
bggn_bg = bggn_bg[bggn_bg$year != 2008,]
points(bggn_bg$bg.gdd, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 16, col = 'gray')
bggn_lm_bggdd = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bg.gdd)
abline(bggn_lm_bggdd, col = 'gray')
summary(bggn_lm_bggdd)


# Botanical Garden Greenup

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bggreenup.log, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue4', 
     ylim = c(82, 117), main = 'Botanical Garden', xlab = 'Greenup JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "D", bty="n")
inbu_lm_bggreen = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bggreenup.log)
abline(inbu_lm_bggreen, col = 'blue')
summary(inbu_lm_bggreen)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bggreenup.log, as.numeric(as.character(revi_bg$inflection_pt)), pch = 16, col = 'red')
revi_lm_bggreen = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bggreenup.log)
abline(revi_lm_bggreen, col = 'red')
summary(revi_lm_bggreen)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bggreenup.log, as.numeric(as.character(coye_bg$inflection_pt)), pch = 16, col = 'orange')
coye_lm_bggreen = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bggreenup.log)
abline(coye_lm_bggreen, col = 'orange')
summary(coye_lm_bggreen)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
bggn_bg = bggn_bg[bggn_bg$year != 2008,]
points(bggn_bg$bggreenup.log, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 16, col = 'gray')
bggn_lm_bggreen = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bggreenup.log)
abline(bggn_lm_bggreen, col = 'gray')
summary(bggn_lm_bggreen)

# Stats of this


#---- FIGURE 6 RESULTS ----

# Final results plot, will be comparing arth peaks to bird peaks (pick one bird species??)

inflection_pr$year = as.numeric(as.character(inflection_pr$year))
inflection_pr$inflection_pt = as.numeric(as.character(inflection_pr$inflection_pt))
inflection_pr$scientific_name = as.character(inflection_pr$scientific_name)

#---- Arths and inbu - visual surveys----

# INDIGO BUNTING <3

# Visual
par(mar = c(4,4,2,2), mfrow = c(2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Bird arrival JD',xlab='Arth peak JD', xlim = c(150,210), ylim = c(114.7, 114.9),
     main = 'Visual')
legend("topleft", "A", bty="n")
points(PR.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)


# RED-EYED VIREO

par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Bird arrival JD',xlab='Arth peak JD', xlim = c(150,210), ylim = c(99, 103),
     main = 'Visual')
legend("topleft", "A", bty="n")
points(PR.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)

# Botanical Garden
points(BG.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)







# Other miscellanea

plot(greenupgdd$year, greenupgdd$pr.gdd, type = 'l')
points(inf_pr_inbu$year, inf_pr_inbu$inflection_pt, type = 'l', col = 'red')
#points(greenupgdd$year, greenupgdd$prgreenup.log, type = 'l', col = 'red')



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
