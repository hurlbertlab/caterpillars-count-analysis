# Comparing GDD, greenup, arthropod phenology, and avian reproductive timing

source('analysis_scripts/prism_10year.R') # GDDs

source('analysis_scripts/modis_10year.R') # Spring green-up

source('archived_scripts/Tracie_thesis/spline_thesis.R') # Arths peaks

source('analysis_scripts/eBird_logistics.R') # Bird arrival

# Is greenup affected by GDD?
# Working with two datasets (one from prism_10year.R and one from modis_10year.R)

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)

#---- FIGURE 3 RESULTS----

# Create pdf
#pdf(file = 'output/plots/thesis_plots_tracie/resultsfigure3.pdf', width = 6.5, height = 6)

#---- GDD and greenup (deviation from mean) ----
par(mar = c(4,4,2,2), mfrow = c(2,2), oma = c(2,2,1,1))
plot(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, col = 'white',
     xlab = 'GDD deviation (day)', ylab = 'Greenup deviation (day)',
     xlim = c(-12,12), ylim = c(-12,12))
abline(0,1, lty = 2)
legend("topleft", "A", bty="n")
abline(h = 0, lty = 3, col = 'gray60')
abline(v = 0, lty = 3, col = 'gray60')
text(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, greenupgdd$year, col = 'deeppink3', cex = 0.7)
text(greenupgdd$bg.gdd.dev, greenupgdd$bggreenup.dev, greenupgdd$year, col = 'steelblue3', cex = 0.7)

allgdd <- c(greenupgdd$pr.gdd.dev, greenupgdd$bg.gdd.dev)
allgreenup <- c(greenupgdd$prgreenup.dev, greenupgdd$bggreenup.dev)
all_lm <- lm(allgreenup ~ allgdd)

legend('bottomright', 
       paste("r =", round(cor(allgdd, allgreenup), 2), ", p < 0.01"), bty = 'n')


# Blank plot for pdf
plot.new()
l = legend('topleft', c('Prairie Ridge (PR)', 'Botanical Garden (BG)'), pch = 16, col = c('deeppink3', 'steelblue3'), cex = 1)
legend('bottomleft', c('Linear regression', '1:1 line'), lty = c(1,2), cex = 1)


plot(greenupgdd$pr.gdd, greenupgdd$bg.gdd, xlab = 'PR GDD (Julian day)', ylab = 'BG GDD (Julian day)', 
     ylim = c(142,172), xlim = c(142,172))
abline(0,1, lty = 2)
l = legend("topleft", "B", bty="n")
gddlm = lm(greenupgdd$bg.gdd ~ greenupgdd$pr.gdd)
abline(gddlm)
summary(gddlm)
abline(0,1, lty = 2)
legend('bottomright', 
     paste("r =", round(cor(greenupgdd$pr.gdd, greenupgdd$bg.gdd), 2), ", p < 0.01"), bty = 'n')
summary(gddlm)$r.squared

plot(greenupgdd$prgreenup.log, greenupgdd$bggreenup.log, xlab = 'PR Greenup (Julian day)', ylab = 'BG Greenup (Julian day)'
     , ylim = c(68,110), xlim = c(68,110))
l = legend("topleft", "C", bty="n")
greenuplm = lm(greenupgdd$bggreenup.log ~ greenupgdd$prgreenup.log)
abline(greenuplm)
summary(greenuplm)
abline(0,1, lty = 2)
legend('bottomright', 
     paste("r =", round(cor(greenupgdd$prgreenup.log, greenupgdd$bggreenup.log), 2), ", p = ", round(summary(greenuplm)$coefficients[2,4], 2)), bty = 'n')

#dev.off()

diffvec = greenupgdd$bg.gdd - greenupgdd$pr.gdd
mean(diffvec)

#---- FIGURE 4 RESULTS ----
#pdf(file = 'output/plots/thesis_plots_tracie/results_legendfig4&6.pdf', width = 6, height = 3.5)

par(mar = c(4,4,2,2), mfrow = c(1,1))

# Blank plot for pdf
plot.new()
legend('bottomleft', c('caterpillars','orthopterans', 'bird food'), pch = 16, cex = 1.2, col = c('seagreen3', 'plum', 'orange3'))
legend('topleft', c('Prairie Ridge 2015', 'Prairie Ridge 2016','Botanical Garden 2015', 'Botanical Garden 2016'), pch = c(21,16,22,15), cex = 1.2)
#dev.off()



#pdf(file = 'output/plots/thesis_plots_tracie/resultsfigure4.pdf', width = 6.5, height = 6)

#---- Arths and GDD - visual surveys----
par(mar = c(4,2,2,2), mfrow = c(2,2), oma = c(1,4,1,1))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='GDD (Julian day)', xlim = c(156,164), ylim = c(150, 215),
     main = 'Visual')
legend("topleft", "A", bty="n")
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

#---- Arths and GDD - beat sheets----
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='GDD (Julian day)', xlim = c(156,164), ylim = c(150, 215),
     main = 'Beat sheet')
legend("topleft", "B", bty="n")
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


#---- Arths and greenup - visual surveys----

# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='Greenup (Julian day)', xlim = c(86,94), ylim = c(150, 215),
     main = '')
legend("topleft", "C", bty="n")
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


#---- Arths and greenup - beat sheets----

# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='Greenup (Julian day)', xlim = c(86,94), ylim = c(150, 215),
     main = '')
legend("topleft", "D", bty="n")
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

mtext("Arthropod peak (Julian day)", side = 2, outer = TRUE, line = 1.5)

#dev.off()

#---- FIGURE 5 RESULTS ----
#pdf(file = 'output/plots/thesis_plots_tracie/results_legendfig5.pdf', width = 6.5, height = 3)

par(mar = c(4,4,2,2), mfrow = c(1,1), oma = c(1,3,1,1))

# Blank plot for pdf
plot.new()
legend('bottomleft', c('Indigo bunting', 'Red-eyed vireo', 'Common yellowthroat', 'Blue-gray gnatcatcher'), pch = c(16, 24, 15, 9), col = c('dodgerblue3', 'red', 'orange', 'gray67'), cex = 1.2)

#dev.off()

#pdf(file = 'output/plots/thesis_plots_tracie/resultsfigure5.pdf', width = 6.5, height = 7)

par(mar = c(4,2,2,2), mfrow = c(2,2), oma = c(1,3,1,1))

# Prairie Ridge GDD

inf_pr_inbu = inflection_pr[inflection_pr$scientific_name == 'Passerina cyanea',]
inbu_pr = merge(inf_pr_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_pr$pr.gdd, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = 'Prairie Ridge', xlab = 'GDD (Julian day)', ylab = '')
legend("topleft", "A", bty="n")
inbu_lm_prgdd = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$pr.gdd, weights = 1/inbu_pr$confint)
abline(inbu_lm_prgdd, col = 'dodgerblue3')
summary(inbu_lm_prgdd)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$pr.gdd, as.numeric(as.character(revi_pr$inflection_pt)), pch = 24, col = 'red')
revi_lm_prgdd = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$pr.gdd, weights = 1/revi_pr$confint)
abline(revi_lm_prgdd, col = 'red')
summary(revi_lm_prgdd)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$pr.gdd, as.numeric(as.character(coye_pr$inflection_pt)), pch = 15, col = 'orange')
coye_lm_prgdd = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$pr.gdd, weights = 1/coye_pr$confint)
abline(coye_lm_prgdd, col = 'orange')
summary(coye_lm_prgdd)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$pr.gdd, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_prgdd = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$pr.gdd, weights = 1/bggn_pr$confint)
abline(bggn_lm_prgdd, col = 'gray67')
summary(bggn_lm_prgdd)

# Botanical Garden GDD

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bg.gdd, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = 'Botanical Garden', xlab = 'GDD (Julian day)', ylab = '')
legend("topleft", "B", bty="n")
inbu_lm_bggdd = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bg.gdd, weights = 1/inbu_bg$confint)
abline(inbu_lm_bggdd, col = 'dodgerblue3')
summary(inbu_lm_bggdd)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bg.gdd, as.numeric(as.character(revi_bg$inflection_pt)), pch = 24, col = 'red')
revi_lm_bggdd = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bg.gdd, weights = 1/revi_bg$confint)
abline(revi_lm_bggdd, col = 'red')
summary(revi_lm_bggdd)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bg.gdd, as.numeric(as.character(coye_bg$inflection_pt)), pch = 15, col = 'orange')
coye_lm_bggdd = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bg.gdd, weights = 1/coye_bg$confint)
abline(coye_lm_bggdd, col = 'orange')
summary(coye_lm_bggdd)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_bg$bg.gdd, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_bggdd = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bg.gdd, weights = 1/bggn_bg$confint)
abline(bggn_lm_bggdd, col = 'gray67')
summary(bggn_lm_bggdd)




# Prairie Ridge Greenup

inf_pr_inbu = inflection_pr[inflection_pr$scientific_name == 'Passerina cyanea',]
inbu_pr = merge(inf_pr_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_pr$prgreenup.log, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = '', xlab = 'Greenup (Julian day)', ylab = '')
legend("topleft", "C", bty="n")
inbu_lm_prgreen = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$prgreenup.log, weights = 1/inbu_pr$confint)
abline(inbu_lm_prgreen, col = 'dodgerblue3')
summary(inbu_lm_prgreen)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$prgreenup.log, as.numeric(as.character(revi_pr$inflection_pt)), pch = 24, col = 'red')
revi_lm_prgreen = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$prgreenup.log, weights = 1/revi_pr$confint)
abline(revi_lm_prgreen, col = 'red')
summary(revi_lm_prgreen)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$prgreenup.log, as.numeric(as.character(coye_pr$inflection_pt)), pch = 15, col = 'orange')
coye_lm_prgreen = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$prgreenup.log, weights = 1/coye_pr$confint)
abline(coye_lm_prgreen, col = 'orange')
summary(coye_lm_prgreen)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$prgreenup.log, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_prgreen = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$prgreenup.log, weights = 1/bggn_pr$confint)
abline(bggn_lm_prgreen, col = 'gray67')
summary(bggn_lm_prgreen)


# Botanical Garden Greenup

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bggreenup.log, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = '', xlab = 'Greenup (Julian day)', ylab = '')
legend("topleft", "D", bty="n")
inbu_lm_bggreen = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bggreenup.log, weights = 1/inbu_bg$confint)
abline(inbu_lm_bggreen, col = 'dodgerblue3')
summary(inbu_lm_bggreen)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bggreenup.log, as.numeric(as.character(revi_bg$inflection_pt)), pch = 24, col = 'red')
revi_lm_bggreen = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bggreenup.log, weights = 1/revi_bg$confint)
abline(revi_lm_bggreen, col = 'red')
summary(revi_lm_bggreen)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bggreenup.log, as.numeric(as.character(coye_bg$inflection_pt)), pch = 15, col = 'orange')
coye_lm_bggreen = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bggreenup.log, weights = 1/coye_bg$confint)
abline(coye_lm_bggreen, col = 'orange')
summary(coye_lm_bggreen)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_bg$bggreenup.log, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_bggreen = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bggreenup.log, weights = 1/bggn_bg$confint)
abline(bggn_lm_bggreen, col = 'gray67')
summary(bggn_lm_bggreen)

mtext("Bird arrival (Julian day)", side = 2, outer = TRUE, line = 1.5)

#dev.off()

# Stats of this
# ONE OF THE COMMON YELLOWTHROATS IS BAD OH NO

datalist = c(inbu_lm_prgdd, revi_lm_prgdd, coye_lm_prgdd, bggn_lm_prgdd)

birdstats = data.frame(Species = rep(c("Indigo bunting", "Red-eyed vireo", "Common yellowthroat", "Blue-gray gnatcatcher"), 4),
                      Site = c(rep('PR', 4), rep('BG', 4), rep('PR', 4), rep('BG', 4)),
                      Metric = c(rep('GDD', 8), rep('Greenup', 8)),
                      R2 = c(round(summary(inbu_lm_prgdd)$r.squared, 2), round(summary(revi_lm_prgdd)$r.squared, 2),
                            round(summary(coye_lm_prgdd)$r.squared, 2), round(summary(bggn_lm_prgdd)$r.squared, 2),
                            round(summary(inbu_lm_bggdd)$r.squared, 2), round(summary(revi_lm_bggdd)$r.squared, 2),
                            round(summary(coye_lm_bggdd)$r.squared, 2), round(summary(bggn_lm_bggdd)$r.squared, 2),
                            round(summary(inbu_lm_prgreen)$r.squared, 2), round(summary(revi_lm_prgreen)$r.squared, 2),
                            round(summary(coye_lm_prgreen)$r.squared, 2), round(summary(bggn_lm_prgreen)$r.squared, 2),
                            round(summary(inbu_lm_bggreen)$r.squared, 2), round(summary(revi_lm_bggreen)$r.squared, 2),
                            round(summary(coye_lm_bggreen)$r.squared, 2), round(summary(bggn_lm_bggreen)$r.squared, 2)),
                      p = c(round(summary(inbu_lm_prgdd)$coefficients[2,4], 2), round(summary(revi_lm_prgdd)$coefficients[2,4], 2),
                            round(summary(coye_lm_prgdd)$coefficients[2,4], 2), round(summary(bggn_lm_prgdd)$coefficients[2,4], 2),
                            round(summary(inbu_lm_bggdd)$coefficients[2,4], 2), round(summary(revi_lm_bggdd)$coefficients[2,4], 2),
                            round(summary(coye_lm_bggdd)$coefficients[2,4], 2), round(summary(bggn_lm_bggdd)$coefficients[2,4], 2),
                            round(summary(inbu_lm_prgreen)$coefficients[2,4], 2), round(summary(revi_lm_prgreen)$coefficients[2,4], 2),
                            round(summary(coye_lm_prgreen)$coefficients[2,4], 2), round(summary(bggn_lm_prgreen)$coefficients[2,4], 2),
                            round(summary(inbu_lm_bggreen)$coefficients[2,4], 2), round(summary(revi_lm_bggreen)$coefficients[2,4], 2),
                            round(summary(coye_lm_bggreen)$coefficients[2,4], 2), round(summary(bggn_lm_bggreen)$coefficients[2,4], 2)))

#write.csv(birdstats, file = 'output/tabular/birdstats.csv')

#---- FIGURE 6 RESULTS: Arths and birds ----

# Final results plot, comparing arth peaks to bird peaks


#pdf(file = 'output/plots/thesis_plots_tracie/resultsfigure6.pdf', width = 6.5, height = 8)

par(mfrow = c(4,2), mar = c(0,4,2,2), oma = c(5,5,3,3))

# INDIGO BUNTING <3

# Visual
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Indigo bunting',xlab='', xlim = c(150,210), ylim = c(111, 115),
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
points(BG.LEPL15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='', xlim = c(150,210), ylim = c(111, 115),
     main = 'Beat sheet')
legend("topleft", "B", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# RED-EYED VIREO

# Visual
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Red-eyed vireo',xlab='', xlim = c(150,210), ylim = c(99, 106),
     main = '')
legend("topleft", "C", bty="n")
points(PR.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='', xlim = c(150,210), ylim = c(99, 106),
     main = '')
legend("topleft", "D", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# COMMON YELLOWTHROAT

# Visual
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Common yellowthroat',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "E", bty="n")
points(PR.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "F", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# BLUE-GRAY GNATCATCHER

# Visual
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Blue-gray gnatcatcher',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "G", bty="n")
points(PR.LEPL15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.vis.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.vis.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.vis.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.vis.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.vis.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.vis.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.vis.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.vis.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.vis.max, 
         col = 'plum', lwd = 2, lty = 1)

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "H", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

mtext("Bird arrival (Julian day)", side = 2, outer = TRUE, line = 1.5)
mtext("Arthropod peak (Julian day)", side = 1, outer = TRUE, line = 2)

#dev.off()


#---- METHODS (FIGURE 2) to show how everything was calculated ----

#pdf(file = 'outputs/plots/thesis_plots_tracie/methodsfigure2.pdf', width = 7, height = 6.5)

par(mfrow = c(2,2), mar = c(2,4,4,2), oma = c(3,1,1,1))

# GDD
# 1000 GGDs 
subdata <- prismtemp[, c('PR', "date", "jday", "year")]
pregddfull <- subdata[subdata$year == 2015,]
pregdd <- pregddfull[,'PR']
pregdd[pregdd < 7] = 7
pregdd[pregdd > 30] = 30 # need to do more research about thresholds
pregdd1 <- pregdd - 7
gdd <- cumsum(pregdd1)
GDD <- data.frame(pregddfull$jday, gdd)
names(GDD) <- c('jday', 'GDD')
plot(GDD$jday, GDD$GDD, xlab = "", ylab = "GDD", type = 'l', lwd = 1, main = 'Temperature')
legend("topleft", "A", bty="n")

# Calculate reference julian day for comparison (at 1000 GDDs)
refnum <- which.min(abs(GDD$GDD - 1000))
segments(y0 = 1000,
         x0 = 0, 
         y1 = 1000, 
         x1 = 161, 
         col = 'darkorchid1', lwd = 2, lty = 1)
segments(y0 = 0,
         x0 = 161, 
         y1 = 1000, 
         x1 = 161, 
         col = 'plum', lwd = 2, lty = 2)
points(x = refnum, y = 1000, pch = 8, cex = 2, lwd = 3, col = 'chocolate2')

# Greenup
# Logistic inflection of EVI
setwd(file.path('C:','git','caterpillars-count-analysis','modis-and-temp',2015))
prmodis <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
prmodis$julianday <- as.numeric(substring(prmodis$V8, 6,8))
tempprevi = prmodis[grep("EVI", prmodis$V6),]
previ <- tempprevi[11:91]
prmean1 <- apply(previ, 1, mean)
prmean2 <- prmean1 / 10000
prmean <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
#points(prmean$julianday, prmean$EVImean, col = 'red', type = 'l')
subprmean = prmean[prmean$julianday %in% 1:200,]
subprmean$EVIdis = subprmean$EVImean - min(subprmean$EVImean)+.01
prlog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subprmean) # CHECK ON THIS EVIDIS STUFF
#par(mar=c(5, 4, 4, 4) + 0.1)
plot(subprmean$julianday, subprmean$EVIdis, xlab = "", ylab = "Mean EVI - min EVI",
     col = 'black', type = 'l', lwd = 1, main = 'Greenup')
curve(predict(prlog,data.frame(julianday=x),type="resp"),add=TRUE, col = 'darkorchid1', lwd = 2)
subprmean$prEVIlog = predict(prlog)+min(subprmean$EVImean)-.01
#points(prmean$julianday, prmean$prEVIlog, col = 'red', lwd = 3, 
#       lty = 'dashed', type = 'l')
prgreenup.log <- summary(prlog)$coefficients["xmid","Estimate"]
abline(v=prgreenup.log, col='plum', lwd = 2, lty = 2)
points(x = prgreenup.log, y = .124, pch = 8, cex = 2, lwd = 3, col = 'chocolate2') # estimation of y
legend("topleft", "B", bty="n")

# Arthropods
# Spline curve and line through peak
PR.LEPL15.vis.max = splinearth(PR.LEPL15.vis, 5)
title('Arthropods')
legend("topleft", "C", bty="n")

# Birds
# Logistic inflection of proportion checklists
inflection.pt.output=c()
singlesp = c()
singlesp1yr = c()

for(sp in 1) { 
  for (yr in 2015) {                   # 
    #for(i in 1:(nrow(long)-1)) {         # no lat long needed because datasets already subsetted
    #for(j in 1:(nrow(lat)-1)) {          # so only need year and sp
    
    #sampling effort
    
    t.samp=subset(sampling_pr, sampling_pr$Year.Collected==yr)
    obs_pr$Scientific.Name=as.character(obs_pr$Scientific.Name)
    t.obs.NA1s = subset(obs_pr, obs_pr$Year==yr & obs_pr$Scientific.Name==as.character(splist[sp]))
    t.obs.NA1s$Observation.Count = as.numeric(as.character(t.obs.NA1s$Observation.Count))
    
    #((nrow(inf_jds1[inf_jds1$prop>0,]))>=30)==T
    if(nrow(t.samp)>0 & nrow(t.obs.NA1s)>0 & (length(unique(t.obs.NA1s$JulianDay))>=30)==T){    #       nrow(t.samp)>49 &
      effort.by.day = aggregate(t.samp$Lat.Long, list(t.samp$Year.Collected, t.samp$JulianDay), function(x) length(unique(x)))
      names(effort.by.day)=c('Year','JulianDay','Num.Unique.locs')            #number of unique locs in sampling per day
      window.days = 60:180     #the days without records are discarded, and here we're adding the 0's back in to replace the NA's
      temp.data1 = merge(effort.by.day,as.data.frame(window.days),by.x='JulianDay',by.y='window.days',all=T)
      temp.data1[is.na(temp.data1$Num.Unique.locs),'Num.Unique.locs'] = 0
      
      #observations
      
      t.obs.NA1s[is.na(t.obs.NA1s$Observation.Count)==T,'Observation.Count'] = 1                                    #give all the ones without obs counts a value of 1
      combos = do.call('paste', t.obs.NA1s[c('Scientific.Name','Latitude','Longitude','Year','JulianDay')])         #paste these columns together
      max.by.siteday = aggregate(t.obs.NA1s$Observation.Count, by=list(combos), function(x) max(x))                 #take the max observation count 
      max.by.siteday.split = as.data.frame(matrix(unlist(strsplit(max.by.siteday$Group.1, " ")), ncol=6, byrow=T))  #unsplit them
      MAX.by.siteday.split = data.frame(do.call('paste', max.by.siteday.split[c('V1','V2')]), do.call('paste',max.by.siteday.split[c('V3','V4')]), max.by.siteday.split$V3, max.by.siteday.split$V4, max.by.siteday.split$V5, max.by.siteday.split$V6)
      pre.max.by.siteday = cbind(MAX.by.siteday.split, max.by.siteday$x)         
      names(pre.max.by.siteday) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count')     #<-Added in Lat and Long separately too
      pre.max.by.siteday[,'Year'] = as.numeric(as.character(pre.max.by.siteday[,'Year']))
      pre.max.by.siteday[,'JulianDay'] = as.numeric(as.character(pre.max.by.siteday[,'JulianDay']))
      sorted.max.by.siteday = pre.max.by.siteday[order(pre.max.by.siteday$Scientific.Name, pre.max.by.siteday$Year, pre.max.by.siteday$JulianDay),]         #<-Now with Lat.Long added in.
      
      hmm = cbind(sorted.max.by.siteday, 1)
      names(hmm) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count','uniq.count')
      combos2 = do.call('paste', hmm[c('Latitude','Longitude','Year','JulianDay')])
      gee = aggregate(hmm$uniq.count, by=list(combos2), sum)
      goe = as.data.frame(matrix(unlist(strsplit(gee$Group.1, " ")), ncol=4, byrow=T))
      gob = data.frame(goe$V1, goe$V2, goe$V3, goe$V4)
      goc = cbind(gob, gee$x)
      names(goc) = c('Latitude','Longitude','Year','JulianDay','Num.of.uniq.locs')
      combos3 = do.call('paste', goc[c('Year','JulianDay')])
      loc = aggregate(goc$Num.of.uniq.locs, by=list(combos3), sum)
      Loc = as.data.frame(matrix(unlist(strsplit(loc$Group.1, " ")), ncol=2, byrow=T))
      locc = cbind(Loc, loc$x)
      names(locc) = c('Year','JulianDay','Num.uniq.locs')
      locc[,'Year'] = as.numeric(as.character(locc[,'Year']))
      locc[,'JulianDay'] = as.numeric(as.character(locc[,'JulianDay']))
      locc[,'Num.uniq.locs'] = as.numeric(as.character(locc[,'Num.uniq.locs']))
      
      window.days2 = as.data.frame(as.matrix(window.days, ncol=1, byrow=T))
      temp.data2 = merge(locc[locc$Year==yr,],window.days2,by.x='JulianDay',by.y='V1',all=T)
      temp.data2[is.na(temp.data2$Num.uniq.locs),'Num.uniq.locs'] = 0
      
      temp.data2 = temp.data2[temp.data2$JulianDay<=180 & temp.data2$JulianDay>=60, ]         ##<<Change both here if want to change window of days seen<<##
      temp.data1 = temp.data1[temp.data1$JulianDay<=180 & temp.data1$JulianDay>=60, ]
      
      temp.data2$prop = temp.data2$Num.uniq.locs/temp.data1$Num.Unique.locs
      
      #plotting
      plot(temp.data2$JulianDay, temp.data2$prop,ylim=c(0,1),xlab='',ylab='Proportion of checklists',main='Birds', xlim=c(60,180), pch=16, cex = .7, type='o')
      #text(min(temp.data2$JulianDay)+5, 0.9, yr, cex=1.5)
      #text(160, 0.9, as.character(splist[sp]))
      
      # FUNCTION FOR DRAWING A LOGISTIC CURVE GIVEN LOGISTIC FIT COEFFICIENTS
      exp.mod<-function(coes,jd){
        Asym<-plogis(coes[1])
        xmid<-coes[2]
        scal<-coes[3]
        Asym/(1 + exp((xmid - jd)/scal))
      }
      
      #mle fitting : BEGIN FITTING AND MAXIMUM LIKELIHOOD ESTIMATION OF LOGISTIC CURVE
      ll.exp.con<-function(Asym,xmid,scal){
        if(xmid>max(temp.data2$JulianDay)){
          nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
            1000 *(abs(max(temp.data2$JulianDay)-xmid))^2        #<-make it huge if it veers outside of constraints of jd
        }
        else{
          if(xmid<min(temp.data2$JulianDay)){
            nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
              1000 *(abs(min(temp.data2$JulianDay)-xmid))^2
          }
          else{
            nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
          }}
        nll
      }
      nll<-numeric()
      xmids<-seq(60,180,20)
      coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))
      
      for(xm in 1:length(xmids)){
        guess <- list(Asym=.6,xmid=xmids[xm],scal=1)
        fit.exp.con<- mle2(ll.exp.con, start = guess, method = "Nelder-Mead",skip.hessian=T)
        coef.mat[xm,]<-coef(fit.exp.con)
        Asym<-coef(fit.exp.con)[1]
        xmid<-coef(fit.exp.con)[2]
        scal<-coef(fit.exp.con)[3]
        nll[xm]<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
      }
      best.coef<-coef.mat[order(nll)[1],] ##only takes coef from the model with the smallest neg.log.likelihood
      #ADD BEST FIT LOGISTIC CURVE TO PLOT
      lines(temp.data2$JulianDay,exp.mod(best.coef,temp.data2$JulianDay),col='darkorchid1', lwd = 2) ##model result
      abline(v=best.coef[2], col='plum', lwd = 2, lty = 2)
      points(x = best.coef[2], y = .43, pch = 8, cex = 2, lwd = 3, col = 'chocolate2') # estimation of y
      
      
      temp.data2$prop[is.nan(temp.data2$prop)==T] = 0                                                     
      temp.jd = temp.data2$JulianDay#[is.nan(temp.data2$prop)==F]
      temp.prop = temp.data2$prop     #[is.nan(temp.data2$prop)==F]
      temp.yr = rep(temp.data2$Year, length(temp.prop))
      
      x=lm(exp.mod(best.coef, temp.data2$JulianDay)~temp.prop)
      
      singlesp1yr = c(as.character(splist[sp]), yr, best.coef[2])
      singlesp = rbind(singlesp, singlesp1yr)
      
    }}
  inflection.pt.output = rbind(inflection.pt.output, singlesp)
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, exp.mod(best.coef, temp.data2$JulianDay), summary(x)$r.squared, long[i,], lat[j,], long[i+1,], lat[j+1,]))
  
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, long[i,], lat[j,]))
}
legend("topleft", "D", bty="n")

mtext("Julian day", side = 1, outer = TRUE, line = 1.5)

#dev.off()





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







#---- Added plots----
par(mfrow = c(2,2), mar = c(4,2,3,2), oma = c(1,3,1,1))
hist(as.numeric(as.character(inflection_pr$scale_param)), xlab = 'R2', main = 'Prairie Ridge', ylab = '')
legend("topright", "A", bty="n")
hist(as.numeric(as.character(inflection_bg$scale_param)), xlab = 'R2', main = 'Botanical Garden', ylab = '')
legend("topright", "B", bty="n")

par(mfrow = c(1,2), mar = c(4,2,3,2), oma = c(1,3,1,1))
hist(as.numeric(as.character(inflection_pr$scale_param)), xlab = 'Scale parameter', main = '', ylab = '')
#legend("topright", "C", bty="n")
hist(as.numeric(as.character(inflection_bg$scale_param)), xlab = 'Scale parameter', main = '', ylab = '')
#legend("topright", "D", bty="n")
mtext("Frequency", side = 2, outer = TRUE, line = 1.5)


# Results figure 2 (a) (after figure 4 of paper)

pdf(file = 'c:/git/caterpillars-count-analysis/plots/thesis_plots_tracie/resultsfigure2a.pdf', width = 6.5, height = 6.5)

par(mfrow = c(1,1), mar = c(4,4,2,2), oma = c(1,1,1,1))
plot(PR.LEPL15.vis.max, PR.LEPL15.bs.max, pch = 21, col = 'seagreen3', 
     xlab = 'Arthropod peak (Julian day) visual', ylab = 'Arthropod peak (Julian day) beat sheet',
     xlim = c(150, 210), ylim = c(150, 210), cex = 2)
abline(0,1, lty = 2)
points(PR.LEPL16.vis.max, PR.LEPL16.bs.max, pch = 16, col = 'seagreen3', cex = 2)
points(PR.ORTH15.vis.max, PR.ORTH15.bs.max, pch = 21, col = 'plum', cex = 2)
points(PR.ORTH16.vis.max, PR.ORTH16.bs.max, pch = 16, col = 'plum', cex = 2)
points(PR.BIRD15.vis.max, PR.BIRD15.bs.max, pch = 21, col = 'orange3', cex = 2)
points(PR.BIRD16.vis.max, PR.BIRD16.bs.max, pch = 16, col = 'orange3', cex = 2)
points(BG.LEPL15.vis.max, BG.LEPL15.bs.max, pch = 22, col = 'seagreen3', cex = 2)
points(BG.LEPL16.vis.max, BG.LEPL16.bs.max, pch = 15, col = 'seagreen3', cex = 2)
points(BG.ORTH15.vis.max, BG.ORTH15.bs.max, pch = 22, col = 'plum', cex = 2)
points(BG.ORTH16.vis.max, BG.ORTH16.bs.max, pch = 15, col = 'plum', cex = 2)
points(BG.BIRD15.vis.max, BG.BIRD15.bs.max, pch = 22, col = 'orange3', cex = 2)
points(BG.BIRD16.vis.max, BG.BIRD16.bs.max, pch = 15, col = 'orange3', cex = 2)

arthpeakvis <- c(PR.LEPL15.vis.max, PR.LEPL16.vis.max,  
                 PR.ORTH15.vis.max, PR.ORTH16.vis.max, 
                 PR.BIRD15.vis.max, PR.BIRD16.vis.max,
                 BG.LEPL15.vis.max, BG.LEPL16.vis.max,  
                 BG.ORTH15.vis.max, BG.ORTH16.vis.max,
                 BG.BIRD15.vis.max, BG.BIRD16.vis.max)
arthpeakbs <- c(PR.LEPL15.bs.max, PR.LEPL16.bs.max, 
                PR.ORTH15.bs.max, PR.ORTH16.bs.max,
                PR.BIRD15.bs.max, PR.BIRD16.bs.max,
                BG.LEPL15.bs.max, BG.LEPL16.bs.max, 
                BG.ORTH15.bs.max, BG.ORTH16.bs.max,
                BG.BIRD15.bs.max, BG.BIRD16.bs.max)
peaklm <- lm(arthpeakbs ~ arthpeakvis)
legend('bottomright', 
       paste("r =", round(cor(arthpeakvis, arthpeakbs), 2), ", p = ", round(summary(peaklm)$coefficients[2,4], 2)), bty = 'n')
legend('topleft', 
       '1:1 line', lty = 2, bty = 'n')


dev.off()


