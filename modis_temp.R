# Script for NCBG and PR MODIS greenup data and temperature data - from Summer 2015
# Tracie Hayes
# September 1, 2015

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
modis = data.frame(location = c('NC Botanical Garden', 'Prairie Ridge Ecostation'), 
                   lat = c(35.898645, 35.809674), long = c(-79.031469, -78.716546))
modis$start.date = rep(2015, nrow(modis)) #not sure if these dates are formatted correctly
modis$end.date = rep(2015, nrow(modis))

# Download MODIS data
GetProducts()
GetBands(Product = 'MOD13Q1')
GetDates(Product = 'MOD13Q1', Lat = modis$lat[1], Long = modis$long[1])
MODISSubsets(LoadDat = modis, Products = 'MOD13Q1', 
             Bands = c('250m_16_days_EVI', '250m_16_days_pixel_reliability'), 
             Size = c(3,3))
subset.string <- read.csv(list.files(pattern = ".asc")[1], header = FALSE, as.is = TRUE)
example = subset.string[1,]
# still working on this

# Download and clean temperature data Prairie Ridge (Reedy Creek Weather Station)
prtemp1 = read.csv('pr_temp.csv') # Data retrieved from the past 180 days on Sept. 1, 2015, includes Sept. 1 (?)
prtemp1$julianday = c(65:244) # Add Julian Day for date conversion, do days match up?
prtemp1$date = as.Date(prtemp1$julianday, origin = '2015-01-01')
prtemp = prtemp1[,c(10,9,3,6,2)]
names(prtemp) = c('date', 'julianday', 'maxtemp', 'mintemp', 'avgtemp')
plot(prtemp$julianday, prtemp$maxtemp)

# Download and clean temperature data NC Botanical Garden
bgtemp1 = read.table('ncbg_temp.txt')
bgtemp2 = bgtemp1[,c(1, 4, 5)]
names(bgtemp2) = c('date', 'hitemp', 'lowtemp') # Each date has 23 temp values, one each hour except midnight
datelist = unique(bgtemp2$date)

maxvec = vector(length = length(datelist))

for (i in 1:length(datelist)) {maxvec[i] = max(as.numeric(bgtemp2$hitemp[bgtemp2$date == datelist[i]]))}



# Change date to date class
bands$bandingDate <- as.Date(bands$bandingDate, '%m/%d/%Y')

CRS("+proj=laea +lat_0=40 +lon_0=-100") # lambert azimuthal equal area
