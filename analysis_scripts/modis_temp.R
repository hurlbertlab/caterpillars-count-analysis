# Script for NCBG and PR MODIS greenup data and temperature data
# Tracie Hayes
# September 8, 2015

# See http://onlinelibrary.wiley.com/doi/10.1002/ece3.1273/full for information about MODIS data
# (Table 2 under Method)

# Set working directory to where you want data downloaded to:
# Example: setwd('C:/git/caterpillars-count-analysis/data/modis-and-temp')

# Load required libraries
library(MODISTools)
library(raster)

library(maps) #don't know if I need all of these libraries
library(sp)
library(rgdal)
library(maptools)
library(rgeos)

# Format MODIS data
## CAN ADJUST FOR YEAR AND LOCATION
## Currently set for 2015 and 2016 PR and BG data

# 2015
modis15 = data.frame(lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis15$start.date = rep(2015, nrow(modis15)) #not sure if these dates are formatted correctly
modis15$end.date = rep(2015, nrow(modis15))
modis15$ID = c(1,2)
# 2016
modis16 = data.frame(lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis16$start.date = rep(2016, nrow(modis16)) #not sure if these dates are formatted correctly
modis16$end.date = rep(2016, nrow(modis16))
modis16$ID = c(1,2)

# Download MODIS data
## CAN ADJUST WHAT TYPE OF DATA IS DOWNLOADED
# 2015
MODISSubsets(LoadDat = modis15, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(1,1))
bgmodis15 <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
prmodis15 <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
bgmodis15$julianday <- as.numeric(substring(bgmodis15$V8, 6,8))
prmodis15$julianday <- as.numeric(substring(prmodis15$V8, 6,8))
# 2016
MODISSubsets(LoadDat = modis16, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(1,1))
bgmodis16 <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
prmodis16 <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
bgmodis16$julianday <- as.numeric(substring(bgmodis16$V8, 6,8))
prmodis16$julianday <- as.numeric(substring(prmodis16$V8, 6,8))

# Calculating average EVI across area (not taking into account pixel reliability)
# Botanical Garden 2015:
tempbgevi = bgmodis15[grep("EVI", bgmodis15$V6),]
bgevi <- tempbgevi[11:91] # will need to adjust if you change Size (in MODISSubsets func)
bgmean1 <- apply(bgevi, 1, mean)
bgmean2 <- bgmean1 / 10000
bgmean15 <- data.frame(julianday = tempbgevi$julianday, EVImean = bgmean2)
bgmean15 <- bgmean15[bgmean15$julianday < 210,]
plot(bgmean15$julianday, bgmean15$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l')
# Prairie Ridge 2015:
tempprevi = prmodis15[grep("EVI", prmodis15$V6),]
previ <- tempprevi[11:91] # will need to adjust if you change Size (in MODISSubsets func)
prmean1 <- apply(previ, 1, mean)
prmean2 <- prmean1 / 10000
prmean15 <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
prmean15 <- prmean15[prmean15$julianday < 210,]
plot(prmean15$julianday, prmean15$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l')
# Botanical Garden 2016:
tempbgevi = bgmodis16[grep("EVI", bgmodis16$V6),]
bgevi <- tempbgevi[11:91] # will need to adjust if you change Size (in MODISSubsets func)
bgmean1 <- apply(bgevi, 1, mean)
bgmean2 <- bgmean1 / 10000
bgmean16 <- data.frame(julianday = tempbgevi$julianday, EVImean = bgmean2)
bgmean16 <- bgmean16[bgmean16$julianday < 210,]
plot(bgmean16$julianday, bgmean16$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l')
# Prairie Ridge 2016:
tempprevi = prmodis16[grep("EVI", prmodis16$V6),]
previ <- tempprevi[11:91] # will need to adjust if you change Size (in MODISSubsets func)
prmean1 <- apply(previ, 1, mean)
prmean2 <- prmean1 / 10000
prmean16 <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
prmean16 <- prmean16[prmean16$julianday < 210,]
plot(prmean16$julianday, prmean16$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l')

# Taking into account pixel reliability?
# Botanical Garden:
tempbgpix = bgmodis15[grep("reliability", bgmodis15$V6),]
# All data is either 0 (Use with confidence) or 1 (Useful, but look at other QA info)
# 1s present on rows 19, 25-30 

# Plotting MODIS data


library(stats)
# Fitting a logistic curve to EVI data and using this to estimate greenup date:

# Prairie Ridge 2015               
prmean15$EVIdis = prmean15$EVImean - min(prmean15$EVImean)+.01
prlog15 = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = prmean15)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(prmean15$julianday, prmean15$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l', lwd = 3)
prmean15$prEVIlog = predict(prlog15)+min(prmean15$EVImean)-.01
points(prmean15$julianday, prmean15$prEVIlog, col = 'red', lwd = 3, 
       lty = 'dashed', type = 'l')

# Botanical Garden 2015
bgmean15$EVIdis = bgmean15$EVImean - min(bgmean15$EVImean)+.01
bglog15 = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = bgmean15)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(bgmean15$julianday, bgmean15$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l', lwd = 3)
bgmean15$bgEVIlog = predict(bglog15)+min(bgmean15$EVImean)-.01
points(bgmean15$julianday, bgmean15$bgEVIlog, col = 'blue', lwd = 3, 
       lty = 'dashed', type = 'l')

summary(prlog15)
prgreenup15 <- summary(prlog15)$coefficients["xmid","Estimate"]

summary(bglog15)
bggreenup15 <- summary(bglog15)$coefficients["xmid","Estimate"]

# Prairie Ridge 2016               
prmean16$EVIdis = prmean16$EVImean - min(prmean16$EVImean)+.01
prlog16 = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = prmean16)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(prmean16$julianday, prmean16$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l', lwd = 3)
prmean16$prEVIlog = predict(prlog16)+min(prmean16$EVImean)-.01
points(prmean16$julianday, prmean16$prEVIlog, col = 'red', lwd = 3, 
       lty = 'dashed', type = 'l')

# Botanical Garden 2016
bgmean16$EVIdis = bgmean16$EVImean - min(bgmean16$EVImean)+.01
bglog16 = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = bgmean16)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(bgmean16$julianday, bgmean16$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l', lwd = 3)
bgmean16$bgEVIlog = predict(bglog16)+min(bgmean16$EVImean)-.01
points(bgmean16$julianday, bgmean16$bgEVIlog, col = 'blue', lwd = 3, 
       lty = 'dashed', type = 'l')

summary(prlog16)
prgreenup16 <- summary(prlog16)$coefficients["xmid","Estimate"]

summary(bglog16)
bggreenup16 <- summary(bglog16)$coefficients["xmid","Estimate"]

# Plot all greenup curves
plot(prmean15$julianday, predict(prlog15)+min(prmean15$EVImean)-.01, col = 'red', lwd = 3, 
       lty = 'dashed', type = 'l', ylim = c(0.15, 0.53), xlab = "Julian Day", ylab = "Mean EVI")
points(bgmean15$julianday, predict(bglog15)+min(bgmean15$EVImean)-.01, col = 'blue', lwd = 3, 
       lty = 'dashed', type = 'l')
points(prmean16$julianday, predict(prlog16)+min(prmean16$EVImean)-.01, col = 'red', lwd = 3, 
       lty = 3, type = 'l')
points(bgmean16$julianday, predict(bglog16)+min(bgmean16$EVImean)-.01, col = 'blue', lwd = 3, 
       lty = 3, type = 'l')
legend("topleft", c('BG 2015', 'PR 2015', 'BG 2016', 'PR 2016'), lwd = c(3,3,3,3), lty = c(2,2,3,3), 
       col = c('blue', 'red', 'blue', 'red'))

# View greenup dates
prgreenup15
bggreenup15
prgreenup16
bggreenup16

# Plot greenups to visualize
greenups <- data.frame(cbind(c(2015,2016), c(prgreenup15, prgreenup16), c(bggreenup15, bggreenup16)))
colnames(greenups) <- c('year', 'PR','BG')
plot(greenups$year, greenups$PR, type = 'l', col = 'red', xlab = "Year", ylab = "Julian day",
     ylim = c(83,97), xaxt = 'n', xlim = c(2014.8,2016.2))
axis(side = 1, at = c(2015,2016))
points(greenups$year, greenups$BG, type = 'l', col = 'blue')
legend("topleft", c('PR', 'BG'), lwd = c(1,1), lty = c(1,1), 
       col = c('red', 'blue'))

# TEMPERATURE
# This temperature data was never used beyond 395, PRISM was used for thesis -Tracie
# Keeping it here in case useful

# Read in and clean temperature data Prairie Ridge (Reedy Creek Weather Station)
prtemp1 = read.csv('data/modis-and-temp/pr_temp.csv') # Data retrieved from the past 180 days on Sept. 1, 2015, does not include Sept. 1 
prtemp1$julianday = c(64:243) # Add Julian Day for date conversion, do days match up?
prtemp1$date = as.Date(prtemp1$julianday, origin = '2014-12-31') # JD 1 is Jan. 1, 2015
prtemp1$avgmaxmin = (prtemp$maxtemp + prtemp$mintemp)/2
prtemp = prtemp1[,c(10,9,3,6,2,11)]
names(prtemp) = c('date', 'julianday', 'maxtemp', 'mintemp', 'avgtemp', 'avgmaxmin')

# Download and clean temperature data NC Botanical Garden
bgtemp1 = read.table('data/modis-and-temp/ncbg_temp.txt')
bgtemp2 = bgtemp1[,c(1, 4, 5)]
names(bgtemp2) = c('date', 'hitemp', 'lowtemp') # Each date has 23 temp values, one each hour except midnight
# For loops for determining highest of max values each day and lowest of min values each day:
datelist = unique(bgtemp2$date)
maxvec = vector(length = length(datelist))
for (i in 1:length(datelist)) {maxvec[i] = max(as.numeric(bgtemp2$hitemp[bgtemp2$date == datelist[i]]))}
minvec = vector(length = length(datelist))
for (i in 1:length(datelist)) {minvec[i] = min(as.numeric(bgtemp2$lowtemp[bgtemp2$date == datelist[i]]))}
#################
bgtemp = data.frame(date = datelist, julianday = c(1:244), maxtemp = ((maxvec - 32)*5/9), 
                    mintemp = ((minvec - 32)*5/9))
bgtemp$avgmaxmin = (bgtemp$maxtemp + bgtemp$mintemp)/2
plot(bgtemp$julianday, bgtemp$maxtemp)
plot(bgtemp$julianday, bgtemp$mintemp)

# Calculating Growing Degree Days (GDD), only using BG data because starts Jan. 1
# (and is very similar to PR data anyway)
pregdd <- bgtemp$avgmaxmin
pregdd[pregdd < 10] = 10
pregdd[pregdd > 30] = 30 # need to do more research about thresholds
pregdd1 <- pregdd - 10
gdd <- cumsum(pregdd1)
GDD <- data.frame(bgtemp$julianday, gdd)
names(GDD) <- c('julianday', 'GDD')
plot(GDD$julianday, GDD$GDD)

# Plotting everything together
par(mar=c(5, 4, 4, 4) + 0.1)
plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l', lwd = 3)
points(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l', lwd = 3)

# Adding temperature data
par(new = T)
plot(bgtemp$julianday, bgtemp$maxtemp, col = 'blue', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(-5, 40))
par(new = T)
plot(prtemp$julianday, prtemp$maxtemp, col = 'red', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(-5, 40))
axis(side=4)
mtext("Maximum Temperature (C)", side=4, las = 0, line = 3)
legend("topleft", c('BG mean EVI', 'PR mean EVI', 'BG max temp', 'PR max temp'),
       lwd = c(3,3,1,1), lty = c(1,1,1,1), col = c('blue', 'red', 'blue', 'red'))

# Adding arth data to graph, see summary_functions.r and arth_analyses.r to run functions.
# BG = Bot garden julian day and mean density
# PRam = Prairie Ridge julian day and mean density
par(new = T)
plot(BG$julianday, BG$meanDensity, col = 'blue', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(0.4, 1.4))
par(new = T)
plot(PRam$julianday, PRam$meanDensity, col = 'red', axes = FALSE, bty = "n", 
     xlab = "", ylab = "", type = 'l', xlim = c(0, 244), ylim = c(0.4, 1.4))
axis(side=4)
mtext("Selected Arthropod Mean Density", side=4, las = 0, line = 3)
legend("topleft", c('BG mean EVI', 'PR mean EVI', 'BG arth density', 'PR arth density'),
       lwd = c(3,3,1,1), lty = c(1,1,1,1), col = c('blue', 'red', 'blue', 'red'))

# Merging
bgtempmerge = bgtemp[c(1,2,5)]
names(bgtempmerge) = c('date', 'julianday', 'bgavgtemp')
bgtempmerge$bgavgtemp = round(bgtempmerge$bgavgtemp, digits = 2)
prtempmerge = prtemp[c(2,6)]
names(prtempmerge) = c('julianday', 'pravgtemp')
bgEVImerge = bgmean[c(1,2,4)]
names(bgEVImerge) = c('julianday', 'bgEVI', 'bgEVIlog')
prEVImerge = prmean[c(1,2,4)]
names(prEVImerge) = c('julianday', 'prEVI', 'prEVIlog')
mergetemp = merge(bgtempmerge, prtempmerge, by = 'julianday', all = T)
mergeEVI = merge(bgEVImerge, prEVImerge, by = 'julianday', all = T)
premerge = merge(mergetemp, mergeEVI, by = 'julianday', all = T)
temp_EVI_GDD = merge(premerge, GDD, by = 'julianday', all = T)



