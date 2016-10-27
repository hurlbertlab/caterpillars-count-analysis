# Calculating greenup date for the last 10 years using average EVI at Prairie Ridge and NCBG
# 1/14/2016

setwd('c:/git/caterpillars-count-analysis')

source('C:/git/caterpillars-count-analysis/summary_functions.r')

source('C:/git/caterpillars-count-analysis/data_cleaning.R')

setwd('C:/git/caterpillars-count-analysis/modis-and-temp')



# Load required libraries
library(MODISTools)
library(raster)

library(maps) #don't know if I need all of these libraries
library(sp)
library(rgdal)
library(maptools)
library(rgeos)

# For loop for downloading/formatting data for each year: 
# THIS ONLY NEED TO RUN ONCE
multyears <- 2000:2016

if (0) {

for (year in 1:length(multyears)) {

  # Format MODIS data
  modis = data.frame(lat = c(35.898645, 35.809674, 43.942407), long = c(-79.031469, -78.716546, -71.710066))
  modis$start.date = rep(multyears[[year]], nrow(modis)) #not sure if these dates are formatted correctly
  modis$end.date = rep(multyears[[year]], nrow(modis))
  modis$ID = c(1,2,3)

  # Set working directory to year file the data will be downloaded in
  setwd(file.path('C:','git','caterpillars-count-analysis','modis-and-temp',multyears[[year]], sep = '/'))

  # Download MODIS data
  MODISSubsets(LoadDat = modis, Products = 'MOD13Q1', 
               Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
               Size = c(1,1))
} # end of for loop for downloading/formatting data

} # end if (0)


# For loop for plotting EVI and calculating greenup for each year
multyears <- 2000:2016

samp.dataframe = data.frame(prgreenup.half = numeric(), bggreenup.half = numeric(),
                            hbgreenup.half = numeric(), prgreenup.log = numeric(), 
                            bggreenup.log = numeric(), hbgreenup.log = numeric())

# Set plot grid for EVI plots
par(mfrow = c(5, 2), mar = c(2, 2, 1, 1))

for (year in 1:length(multyears)) {
  
  # Set working directory to year file you want to read from
  setwd(file.path('C:','git','caterpillars-count-analysis','modis-and-temp',multyears[[year]]))

  # Read in MODIS data
  bgmodis <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
  prmodis <- read.csv(list.files(pattern = ".asc")[2], header = FALSE, as.is = TRUE)
  hbmodis <- read.csv(list.files(pattern = ".asc")[3], header = FALSE, as.is = TRUE)
  bgmodis$julianday <- as.numeric(substring(bgmodis$V8, 6,8))
  prmodis$julianday <- as.numeric(substring(prmodis$V8, 6,8))
  hbmodis$julianday <- as.numeric(substring(hbmodis$V8, 6,8))

  # Calculating average EVI across area (not taking into account pixel reliability)
  # Hubbard Brook:
  temphbevi = hbmodis[grep("EVI", hbmodis$V6),]
  hbevi <- temphbevi[11:91]
  hbmean1 <- apply(hbevi, 1, mean)
  hbmean2 <- hbmean1 / 10000
  hbmean <- data.frame(julianday = temphbevi$julianday, EVImean = hbmean2)
  plot(hbmean$julianday, hbmean$EVImean, col = 'green3', type = 'l', 
       xlab = "Julian Day", ylab = "Mean EVI")  
  # Botanical Garden:
  tempbgevi = bgmodis[grep("EVI", bgmodis$V6),]
  bgevi <- tempbgevi[11:91]
  bgmean1 <- apply(bgevi, 1, mean)
  bgmean2 <- bgmean1 / 10000
  bgmean <- data.frame(julianday = tempbgevi$julianday, EVImean = bgmean2)
  points(bgmean$julianday, bgmean$EVImean, col = 'blue', type = 'l')
  # Prairie Ridge:
  tempprevi = prmodis[grep("EVI", prmodis$V6),]
  previ <- tempprevi[11:91]
  prmean1 <- apply(previ, 1, mean)
  prmean2 <- prmean1 / 10000
  prmean <- data.frame(julianday = tempprevi$julianday, EVImean = prmean2)
  points(prmean$julianday, prmean$EVImean, col = 'red', type = 'l')


  

  # Fitting a logistic curve to EVI data (first 200 days of each year) and 
  # using this to estimate greenup date:
  
  # Prairie Ridge  
  subprmean = prmean[prmean$julianday %in% 1:200,]
  subprmean$EVIdis = subprmean$EVImean - min(subprmean$EVImean)+.01
  prlog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subprmean)
  #par(mar=c(5, 4, 4, 4) + 0.1)
  #plot(prmean$julianday, prmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
  #     col = 'red', type = 'l', lwd = 3)
  subprmean$prEVIlog = predict(prlog)+min(subprmean$EVImean)-.01
  #points(prmean$julianday, prmean$prEVIlog, col = 'red', lwd = 3, 
  #       lty = 'dashed', type = 'l')

  # Botanical Garden
  subbgmean = bgmean[bgmean$julianday %in% 1:200,]
  subbgmean$EVIdis = subbgmean$EVImean - min(subbgmean$EVImean)+.01
  bglog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subbgmean)
  #par(mar=c(5, 4, 4, 4) + 0.1)
  #plot(bgmean$julianday, bgmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
  #     col = 'blue', type = 'l', lwd = 3)
  subbgmean$bgEVIlog = predict(bglog)+min(subbgmean$EVImean)-.01
  #points(bgmean$julianday, bgmean$bgEVIlog, col = 'blue', lwd = 3, 
  #       lty = 'dashed', type = 'l')

  # Hubbard Brook
  subhbmean = hbmean[hbmean$julianday %in% 1:200,]
  subhbmean$EVIdis = subhbmean$EVImean - min(subhbmean$EVImean)+.01
  hblog = nls(EVIdis ~ SSlogis(julianday, Asym, xmid, scal), data = subhbmean)
  #par(mar=c(5, 4, 4, 4) + 0.1)
  #plot(hbmean$julianday, hbmean$EVImean, xlab = "Julian Day", ylab = "Mean EVI",
  #     col = 'blue', type = 'l', lwd = 3)
  subhbmean$hbEVIlog = predict(hblog)+min(subhbmean$EVImean)-.01
  #points(hbmean$julianday, hbmean$hbEVIlog, col = 'blue', lwd = 3, 
  #       lty = 'dashed', type = 'l') 


  # Extract greenup from logistic fit
  
  #summary(prlog)
  prgreenup.log <- summary(prlog)$coefficients["xmid","Estimate"]

  #summary(bglog)
  bggreenup.log <- summary(bglog)$coefficients["xmid","Estimate"]
  
  #summary(hblog)
  hbgreenup.log <- summary(hblog)$coefficients["xmid","Estimate"]  
  
  # Extract greenup by finding the day the EVI is half its maximum:
  
  # Bot garden
  subbgmean$inflectdiff = subbgmean$EVImean - (0.5*(max(subbgmean$EVImean)-min(subbgmean$EVImean)) + min(subbgmean$EVImean))
  yhat = 0.5*(max(subbgmean$EVImean)-min(subbgmean$EVImean)) + min(subbgmean$EVImean)
  y1 = subbgmean$EVImean[subbgmean$inflectdiff == tail(subbgmean$inflectdiff[subbgmean$inflectdiff <= 0], 1)]
  y2 = subbgmean$EVImean[subbgmean$inflectdiff == head(subbgmean$inflectdiff[subbgmean$inflectdiff >= 0], 1)]
  jd1 = subbgmean$julianday[subbgmean$inflectdiff == tail(subbgmean$inflectdiff[subbgmean$inflectdiff <= 0], 1)]
  jd2 = subbgmean$julianday[subbgmean$inflectdiff == head(subbgmean$inflectdiff[subbgmean$inflectdiff >= 0], 1)]
  bggreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
  #figure out how to do this with regular bgmean? (got a really messed up number)
  
  # Prairie Ridge
  subprmean$inflectdiff = subprmean$EVImean - (0.5*(max(subprmean$EVImean)-min(subprmean$EVImean)) + min(subprmean$EVImean))
  yhat = 0.5*(max(subprmean$EVImean)-min(subprmean$EVImean)) + min(subprmean$EVImean)
  y1 = subprmean$EVImean[subprmean$inflectdiff == tail(subprmean$inflectdiff[subprmean$inflectdiff <= 0], 1)]
  y2 = subprmean$EVImean[subprmean$inflectdiff == head(subprmean$inflectdiff[subprmean$inflectdiff >= 0], 1)]
  jd1 = subprmean$julianday[subprmean$inflectdiff == tail(subprmean$inflectdiff[subprmean$inflectdiff <= 0], 1)]
  jd2 = subprmean$julianday[subprmean$inflectdiff == head(subprmean$inflectdiff[subprmean$inflectdiff >= 0], 1)]
  prgreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
  #figure out how to do this with regular prmean?
  
  # Hubbard Brook
  subhbmean$inflectdiff = subhbmean$EVImean - (0.5*(max(subhbmean$EVImean)-min(subhbmean$EVImean)) + min(subhbmean$EVImean))
  yhat = 0.5*(max(subhbmean$EVImean)-min(subhbmean$EVImean)) + min(subhbmean$EVImean)
  y1 = subhbmean$EVImean[subhbmean$inflectdiff == tail(subhbmean$inflectdiff[subhbmean$inflectdiff <= 0], 1)]
  y2 = subhbmean$EVImean[subhbmean$inflectdiff == head(subhbmean$inflectdiff[subhbmean$inflectdiff >= 0], 1)]
  jd1 = subhbmean$julianday[subhbmean$inflectdiff == tail(subhbmean$inflectdiff[subhbmean$inflectdiff <= 0], 1)]
  jd2 = subhbmean$julianday[subhbmean$inflectdiff == head(subhbmean$inflectdiff[subhbmean$inflectdiff >= 0], 1)]
  hbgreenup.half = jd1 + ((yhat-y1)/(y2-y1))*(jd2-jd1)
  #figure out how to do this with regular hbmean?

  temp.dataframe = data.frame(prgreenup.half, bggreenup.half, hbgreenup.half, 
                              prgreenup.log, bggreenup.log, hbgreenup.log)

  samp.dataframe = rbind(samp.dataframe, temp.dataframe)
} # end for loop

greenup <- samp.dataframe
greenup$year <- c(2000:2016)

# Plotting
par(mar = c(2,2,3,2), mfrow = c(1,1), oma = c(2,2,2,2))
plot(greenup$year, greenup$prgreenup.log, col = 'red', type = 'l', ylim = c(70,180),
     xlab = 'Year', ylab = "Julian day of greenup", lwd = 2)
points(greenup$year, greenup$bggreenup.log, col = 'blue', type = 'l', lwd = 2)
points(greenup$year, greenup$hbgreenup.log, col = 'green3', type = 'l', lwd = 2)
points(greenup$year, greenup$prgreenup.half, col = 'red', type = 'l', lwd = 2, lty = 2)
points(greenup$year, greenup$bggreenup.half, col = 'blue', type = 'l', lwd = 2, lty = 2)
points(greenup$year, greenup$hbgreenup.half, col = 'green3', type = 'l', lwd = 2, lty = 2)
legend("topleft", c('PR logistic', 'BG logistic', 'HB logistic', 'PR half max', 'BG half max', 'HB half max'), lwd = 2, 
       lty = c(1,1,1,2,2,2), col = c('red', 'blue', 'green3', 'red', 'blue', 'green3')) 
title('Greenup 2000-2016', line = 1)



