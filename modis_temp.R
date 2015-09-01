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

# Download temperature data
prtemp = read.csv('pr_temp.csv')
bgtemp1 = read.table('ncbg_temp.txt')
bgtemp = bgtemp1[,c(1, 4, 5, 6)]
names(bgtemp) = c('date', 'hitemp', 'lowtemp', 'outhum') # Each date has 23 temp values, one each hour except midnight

# Change date to date class
bands$bandingDate <- as.Date(bands$bandingDate, '%m/%d/%Y')

CRS("+proj=laea +lat_0=40 +lon_0=-100") # lambert azimuthal equal area
