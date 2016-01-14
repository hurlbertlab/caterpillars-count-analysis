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

# Format MODIS data
modis = data.frame(lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis$start.date = rep(2014, nrow(modis)) #not sure if these dates are formatted correctly
modis$end.date = rep(2014, nrow(modis))
modis$ID = c(1,2)

# Download MODIS data
MODISSubsets(LoadDat = modis, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(1,1))
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
plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
     col = 'red', type = 'l')