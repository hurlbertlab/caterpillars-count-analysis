# Calculating greenup date for the last 10 years using average EVI at Prairie Ridge and NCBG
# 1/14/2016

# setwd('C:/git/caterpillars-count-analysis/modis-and-temp')

# Load required libraries
library(MODISTools)
library(raster)

library(maps) #don't know if I need all of these libraries
library(sp)
library(rgdal)
library(maptools)
library(rgeos)

# For loop for formatting data for each year: 
# THIS ONLY NEED TO RUN ONCE
multyears <- 2006:2015
for (year in 1:length(multyears)) {

# Format MODIS data
modis = data.frame(lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis$start.date = rep(multyears[[year]], nrow(modis)) #not sure if these dates are formatted correctly
modis$end.date = rep(multyears[[year]], nrow(modis))
modis$ID = c(1,2)

# Set working directory to year file the data will be downloaded in
setwd(file.path('C:','git','caterpillars-count-analysis','modis-and-temp',multyears[[year]]))

# Download MODIS data
MODISSubsets(LoadDat = modis, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(1,1))
}

# For loop for plotting EVI and calculating greenup for each year
multyears <- 2006:2015

samp.dataframe = data.frame(prgreenup = numeric(), prgreenup = numeric())

# Set plot grid for EVI plots
par(mfrow = c(5, 2), mar = c(1, 1, 1, 1))

for (year in 1:length(multyears)) {
  
# Set working directory to year file you want to read from
setwd(file.path('C:','git','caterpillars-count-analysis','modis-and-temp',multyears[[year]]))

# Read in MODIS data
bgmodis <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
prmodis <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
bgmodis$julianday <- as.numeric(substring(bgmodis$V8, 6,8))
#bgmodis$date <- strptime(bgmodis$julianday, "%m/%d/%Y")
prmodis$julianday <- as.numeric(substring(prmodis$V8, 6,8))

# Calculating average EVI across area (not taking into account pixel reliability)
# Botanical Garden:
tempbgevi = bgmodis[grep("EVI", bgmodis$V6),]
bgevi <- tempbgevi[11:91]
bgmean1 <- apply(bgevi, 1, mean)
bgmean2 <- bgmean1 / 10000
bgmean <- data.frame(julianday = tempbgevi$julianday, EVImean = bgmean2)
plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'blue', type = 'l')
# Prairie Ridge:
tempprevi = prmodis[grep("EVI", prmodis$V6),]
previ <- tempprevi[11:91]
prmean1 <- apply(previ, 1, mean)
prmean2 <- prmean1 / 10000
prmean <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
points(prmean$julianday, prmean$EVImean, col = 'red', type = 'l')

legend("topleft", c('BG mean EVI', 'PR mean EVI'), col = c('blue', 'red'))

# Fitting a logistic curve to EVI data and using this to estimate greenup date:

# Prairie Ridge               
prmean$EVIdis = prmean$EVImean - min(prmean$EVImean)+.01
prlog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = prmean)
#par(mar=c(5, 4, 4, 4) + 0.1)
#plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
#     col = 'red', type = 'l', lwd = 3)
prmean$prEVIlog = predict(prlog)+min(prmean$EVImean)-.01
#points(prmean$julianday, prmean$prEVIlog, col = 'red', lwd = 3, 
#       lty = 'dashed', type = 'l')

# Botanical Garden
bgmean$EVIdis = bgmean$EVImean - min(bgmean$EVImean)+.01
bglog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = bgmean)
#par(mar=c(5, 4, 4, 4) + 0.1)
#plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
#     col = 'blue', type = 'l', lwd = 3)
bgmean$bgEVIlog = predict(bglog)+min(bgmean$EVImean)-.01
#points(bgmean$julianday, bgmean$bgEVIlog, col = 'blue', lwd = 3, 
#       lty = 'dashed', type = 'l')


#plot(prmean$julianday, predict(prlog)+min(prmean$EVImean)-.01, col = 'red', lwd = 3, 
#     lty = 'dashed', type = 'l', ylim = c(0.15, 0.53), xlab = "Julian Day", ylab = "Mean EVI")
#points(bgmean$julianday, predict(bglog)+min(bgmean$EVImean)-.01, col = 'blue', lwd = 3, 
#       lty = 'dashed', type = 'l')
#legend("topleft", c('BG mean EVI', 'PR mean EVI'), lwd = c(3,3), lty = c(2,2), col = c('blue', 'red'))

#summary(prlog)
prgreenup <- summary(prlog)$coefficients["xmid","Estimate"]

#summary(bglog)
bggreenup <- summary(bglog)$coefficients["xmid","Estimate"]

temp.dataframe = data.frame(prgreenup, bggreenup)

samp.dataframe = rbind(samp.dataframe, temp.dataframe)

}

return(samp.dataframe)