# Comparing GDD, greenup, arthropod phenology, and avian reproductive timing

source('C:/git/caterpillars-count-analysis/prism_10year.R') # GDDs

source('C:/git/caterpillars-count-analysis/modis_10year.R') # Spring green-up

source('C:/git/caterpillars-count-analysis/gams.R') # eventually arths peaks, once I get this to work

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
legend('bottomleft', c('2015 caterpillars','2015 bird food', '2016 caterpillars', '2016 bird food'), pch = c(21, 22, 16, 15), cex = 1.2)
legend('topleft', c('Prairie Ridge (PR)', 'Botanical Garden (BG)'), pch = 16, col = c('red', 'blue'), cex = 1.2)
plot.new()

#---- Arths and greenup - visual surveys----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak week',xlab='Greenup JD', xlim = c(86,94), ylim = c(22, 30),
     main = 'Visual')
legend("topleft", "A", bty="n")
maxden1 <- max(PR.LEPL15.vis$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15.vis$week[PR.LEPL15.vis$fracSurveys == maxden1], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15.vis$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15.vis$week[PR.BIRD15.vis$fracSurveys == maxden2], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16.vis$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16.vis$week[PR.LEPL16.vis$fracSurveys == maxden3], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16.vis$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16.vis$week[PR.BIRD16.vis$fracSurveys == maxden4], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.vis$week[PR.LEPL15.vis$fracSurveys == maxden1], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.vis$week[PR.LEPL16.vis$fracSurveys == maxden3], 
         col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.vis$week[PR.BIRD15.vis$fracSurveys == maxden2], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.vis$week[PR.BIRD16.vis$fracSurveys == maxden4], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15.vis$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15.vis$week[BG.LEPL15.vis$fracSurveys == maxden5], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15.vis$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15.vis$week[BG.BIRD15.vis$fracSurveys == maxden6], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16.vis$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16.vis$week[BG.LEPL16.vis$fracSurveys == maxden7], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16.vis$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16.vis$week[BG.BIRD16.vis$fracSurveys == maxden8], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.vis$week[BG.LEPL15.vis$fracSurveys == maxden5], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.vis$week[BG.LEPL16.vis$fracSurveys == maxden7], 
         col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.vis$week[BG.BIRD15.vis$fracSurveys == maxden6], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.vis$week[BG.BIRD16.vis$fracSurveys == maxden8], 
         col = 'blue')
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))

#---- Arths and GDD - visual surveys----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak week',xlab='GDD JD', xlim = c(156,164), ylim = c(22, 30),
main = 'Visual')
legend("topleft", "B", bty="n")
maxden1 <- max(PR.LEPL15.vis$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15.vis$week[PR.LEPL15.vis$fracSurveys == maxden1], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15.vis$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15.vis$week[PR.BIRD15.vis$fracSurveys == maxden2], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16.vis$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16.vis$week[PR.LEPL16.vis$fracSurveys == maxden3], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16.vis$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16.vis$week[PR.BIRD16.vis$fracSurveys == maxden4], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.vis$week[PR.LEPL15.vis$fracSurveys == maxden1], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.vis$week[PR.LEPL16.vis$fracSurveys == maxden3], 
         col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.vis$week[PR.BIRD15.vis$fracSurveys == maxden2], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.vis$week[PR.BIRD16.vis$fracSurveys == maxden4], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15.vis$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15.vis$week[BG.LEPL15.vis$fracSurveys == maxden5], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15.vis$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15.vis$week[BG.BIRD15.vis$fracSurveys == maxden6], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16.vis$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16.vis$week[BG.LEPL16.vis$fracSurveys == maxden7], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16.vis$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16.vis$week[BG.BIRD16.vis$fracSurveys == maxden8], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.vis$week[BG.LEPL15.vis$fracSurveys == maxden5], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.vis$week[BG.LEPL16.vis$fracSurveys == maxden7], 
         col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.vis$week[BG.BIRD15.vis$fracSurveys == maxden6], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.vis$week[BG.BIRD16.vis$fracSurveys == maxden8], 
         col = 'blue')
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))

#---- Arths and greenup - beat sheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak week',xlab='Greenup JD', ylim = c(22, 30), xlim = c(85, 95), 
     main = 'Beat sheets')
legend("topleft", "C", bty="n")
maxden1 <- max(PR.LEPL15.bs$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15.bs$week[PR.LEPL15.bs$fracSurveys == maxden1], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15.bs$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15.bs$week[PR.BIRD15.bs$fracSurveys == maxden2], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16.bs$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16.bs$week[PR.LEPL16.bs$fracSurveys == maxden3], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16.bs$fracSurveys)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16.bs$week[PR.BIRD16.bs$fracSurveys == maxden4], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs$week[PR.LEPL15.bs$fracSurveys == maxden1], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs$week[PR.LEPL16.bs$fracSurveys == maxden3], 
         col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs$week[PR.BIRD15.bs$fracSurveys == maxden2], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs$week[PR.BIRD16.bs$fracSurveys == maxden4], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15.bs$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15.bs$week[BG.LEPL15.bs$fracSurveys == maxden5], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15.bs$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15.bs$week[BG.BIRD15.bs$fracSurveys == maxden6], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16.bs$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16.bs$week[BG.LEPL16.bs$fracSurveys == maxden7], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16.bs$fracSurveys)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16.bs$week[BG.BIRD16.bs$fracSurveys == maxden8], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs$week[BG.LEPL15.bs$fracSurveys == maxden5], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs$week[BG.LEPL16.bs$fracSurveys == maxden7], 
         col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs$week[BG.BIRD15.bs$fracSurveys == maxden6], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs$week[BG.BIRD16.bs$fracSurveys == maxden8], 
         col = 'blue')
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))

#---- Arths and GDD - beatsheets----
par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth peak week',xlab='GDD JD', ylim = c(22, 30), xlim = c(155, 165), 
     main = 'Beat sheets')
legend("topleft", "D", bty="n")
maxden1 <- max(PR.LEPL15.bs$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15.bs$week[PR.LEPL15.bs$fracSurveys == maxden1], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15.bs$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15.bs$week[PR.BIRD15.bs$fracSurveys == maxden2], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16.bs$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16.bs$week[PR.LEPL16.bs$fracSurveys == maxden3], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16.bs$fracSurveys)
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16.bs$week[PR.BIRD16.bs$fracSurveys == maxden4], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs$week[PR.LEPL15.bs$fracSurveys == maxden1], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs$week[PR.LEPL16.bs$fracSurveys == maxden3], 
         col = 'red')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs$week[PR.BIRD15.bs$fracSurveys == maxden2], 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs$week[PR.BIRD16.bs$fracSurveys == maxden4], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15.bs$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15.bs$week[BG.LEPL15.bs$fracSurveys == maxden5], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15.bs$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15.bs$week[BG.BIRD15.bs$fracSurveys == maxden6], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16.bs$week[BG.LEPL16.bs$fracSurveys == maxden7], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16.bs$fracSurveys)
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16.bs$week[BG.BIRD16.bs$fracSurveys == maxden8], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs$week[BG.LEPL15.bs$fracSurveys == maxden5], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs$week[BG.LEPL16.bs$fracSurveys == maxden7], 
         col = 'blue')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs$week[BG.BIRD15.bs$fracSurveys == maxden6], 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs$week[BG.BIRD16.bs$fracSurveys == maxden8], 
         col = 'blue')
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
plot(inbu_pr$pr.gdd, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'blue', 
     ylim = c(82, 117), main = 'Prairie Ridge', xlab = 'GDD JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "A", bty="n")
inbu_lm = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$pr.gdd)
abline(inbu_lm, col = 'blue')
summary(inbu_lm)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$pr.gdd, as.numeric(as.character(revi_pr$inflection_pt)), pch = 16, col = 'red')
revi_lm = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$pr.gdd)
abline(revi_lm, col = 'red')
summary(revi_lm)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$pr.gdd, as.numeric(as.character(coye_pr$inflection_pt)), pch = 16, col = 'orange')
coye_lm = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$pr.gdd)
abline(coye_lm, col = 'orange')
summary(coye_lm)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$pr.gdd, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 16, col = 'gray')
bggn_lm = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$pr.gdd)
abline(bggn_lm, col = 'gray')
summary(bggn_lm)


# Prairie Ridge Greenup

inf_pr_inbu = inflection_pr[inflection_pr$scientific_name == 'Passerina cyanea',]
inbu_pr = merge(inf_pr_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_pr$prgreenup.log, as.numeric(as.character(inbu_pr$inflection_pt)), pch = 16, col = 'blue', 
     ylim = c(82, 117), main = 'Prairie Ridge', xlab = 'Greenup JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "B", bty="n")
inbu_lm = lm(as.numeric(as.character(inbu_pr$inflection_pt)) ~ inbu_pr$prgreenup.log)
abline(inbu_lm, col = 'blue')
summary(inbu_lm)

inf_pr_revi = inflection_pr[inflection_pr$scientific_name == 'Vireo olivaceus',]
revi_pr = merge(inf_pr_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_pr$prgreenup.log, as.numeric(as.character(revi_pr$inflection_pt)), pch = 16, col = 'red')
revi_lm = lm(as.numeric(as.character(revi_pr$inflection_pt)) ~ revi_pr$prgreenup.log)
abline(revi_lm, col = 'red')
summary(revi_lm)

inf_pr_coye = inflection_pr[inflection_pr$scientific_name == 'Geothlypis trichas',]
coye_pr = merge(inf_pr_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_pr$prgreenup.log, as.numeric(as.character(coye_pr$inflection_pt)), pch = 16, col = 'orange')
coye_lm = lm(as.numeric(as.character(coye_pr$inflection_pt)) ~ coye_pr$prgreenup.log)
abline(coye_lm, col = 'orange')
summary(coye_lm)

inf_pr_bggn = inflection_pr[inflection_pr$scientific_name == 'Polioptila caerulea',]
bggn_pr = merge(inf_pr_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_pr$prgreenup.log, as.numeric(as.character(bggn_pr$inflection_pt)), pch = 16, col = 'gray')
bggn_lm = lm(as.numeric(as.character(bggn_pr$inflection_pt)) ~ bggn_pr$prgreenup.log)
abline(bggn_lm, col = 'gray')
summary(bggn_lm)

# Botanical Garden GDD

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bg.gdd, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'blue', 
     ylim = c(82, 117), main = 'Botanical Garden', xlab = 'GDD JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "C", bty="n")
inbu_lm = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bg.gdd)
abline(inbu_lm, col = 'blue')
summary(inbu_lm)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bg.gdd, as.numeric(as.character(revi_bg$inflection_pt)), pch = 16, col = 'red')
revi_lm = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bg.gdd)
abline(revi_lm, col = 'red')
summary(revi_lm)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bg.gdd, as.numeric(as.character(coye_bg$inflection_pt)), pch = 16, col = 'orange')
coye_lm = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bg.gdd)
abline(coye_lm, col = 'orange')
summary(coye_lm)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
bggn_bg = bggn_bg[bggn_bg$year != 2008,]
points(bggn_bg$bg.gdd, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 16, col = 'gray')
bggn_lm = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bg.gdd)
abline(bggn_lm, col = 'gray')
summary(bggn_lm)


# Botanical Garden Greenup

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bggreenup.log, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'blue', 
     ylim = c(82, 117), main = 'Botanical Garden', xlab = 'Greenup JD', ylab = 'Bird arrival inflection point JD')
legend("topleft", "D", bty="n")
inbu_lm = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bggreenup.log)
abline(inbu_lm, col = 'blue')
summary(inbu_lm)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bggreenup.log, as.numeric(as.character(revi_bg$inflection_pt)), pch = 16, col = 'red')
revi_lm = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bggreenup.log)
abline(revi_lm, col = 'red')
summary(revi_lm)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bggreenup.log, as.numeric(as.character(coye_bg$inflection_pt)), pch = 16, col = 'orange')
coye_lm = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bggreenup.log)
abline(coye_lm, col = 'orange')
summary(coye_lm)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
bggn_bg = bggn_bg[bggn_bg$year != 2008,]
points(bggn_bg$bggreenup.log, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 16, col = 'gray')
bggn_lm = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bggreenup.log)
abline(bggn_lm, col = 'gray')
summary(bggn_lm)

#---- FIGURE 6 RESULTS ----

# Final results plot, will be comparing arth peaks to bird peaks




plot(greenupgdd$year, greenupgdd$pr.gdd, type = 'l')
points(inf_pr_inbu$year, inf_pr_inbu$inflection_pt, type = 'l', col = 'red')
#points(greenupgdd$year, greenupgdd$prgreenup.log, type = 'l', col = 'red')



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
