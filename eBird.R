# eBird data for Caterpillars Count! Project

# Downloaded from eBird Sept. 22, 2016
# Files include all data from 2006 to 2016 in NC (Prairie Ridge, Botanical Garden) 
# and NH (Hubbard Brook for comparison)

library(lubridate)

setwd('c:/users/hayeste/my documents/495/eBird/NC')

nc_ebird <- read.table(file = 'nc_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '')
nc_ebird_sub <- nc_ebird[,c(3,4,5,8,10,14,20,23,24,25,26)]
names(nc_ebird_sub) <- c('category', 'common_name', 'scientific_name', 'count', 'age_sex', 
                         'state', 'locality', 'lat', 'long', 'date', 'time')
nc_ebird_sub$scientific_name <- as.character(nc_ebird_sub$scientific_name)
nc_ebird_sub$year = as.numeric(as.character(substr(nc_ebird_sub$date, 1, 4)))

setwd('c:/users/hayeste/my documents/495/eBird/NH')

nh_ebird <- read.table(file = 'nc_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '')

# Migratory status

migratoryguild <- read.table('c:/users/hayeste/my documents/465_project/data/migratoryguild.txt', header = T, sep = ',', quote = '"')
migratoryguild$scientificName = paste(migratoryguild$Genus, migratoryguild$Species, sep = ' ')
migratoryguild$MigClass[migratoryguild$MigClass == 'unk'] = NA
migratoryguild$migclass1[migratoryguild$migclass1 == 'unk'] = NA
migraguild <- migratoryguild[,5:7]
names(migraguild) <- c('migclass', 'migclass_gen', 'scientific_name')
nc_ebird_migra <- merge(nc_ebird_sub, migraguild, by = 'scientific_name', all.x = T, all.y = F)

# Making a dataframe of the longitude and latitude quadrants for each site with a given
# starting point in longlat (function)

longlatquad = function(longpoint,   # starting longitude (center of location)
                       latpoint,    # starting latitude (center of location)
                       res_km = 1)  # resolution in km (length of the side of the quadrant)
  
  {
  NWlat = latpoint + (.5*res_km)/110.574
  NWlong = longpoint - (.5*res_km)/(111.320*cos(latpoint*pi/180))
  NElat = latpoint + (.5*res_km)/110.574
  NElong = longpoint + (.5*res_km)/(111.320*cos(latpoint*pi/180))
  SWlat = latpoint - (.5*res_km)/110.574
  SWlong = longpoint - (.5*res_km)/(111.320*cos(latpoint*pi/180))
  SElat = latpoint - (.5*res_km)/110.574
  SElong = longpoint + (.5*res_km)/(111.320*cos(latpoint*pi/180))
  
  longlatdataframe = data.frame(quadpoint = c('NW', 'NE', 'SW', 'SE'),
                                lat = c(NWlat, NElat, SWlat, SElat), 
                                long = c(NWlong, NElong, SWlong, SElong))
  
  return(longlatdataframe)
  } # end function
  
PRquad = longlatquad(latpoint = 35.809674, longpoint = -78.716546, res_km = 4)
BGquad = longlatquad(latpoint = 35.898645, longpoint = -79.031469, res_km = 4)
HBquad = longlatquad(latpoint = 43.942407, longpoint = -71.710066, res_km = 4)

nc_ebird_migra <- nc_ebird_migra[!is.na(nc_ebird_migra$long),] # if no longitude, no latitude either

# edit this eventually to make it work
nc_ebird_migra$date = as.Date(as.character(nc_ebird_migra$date), format = '%Y/%m/%d')
nc_ebird_migra$jday = yday(nc_ebird_migra$date)

# Pull out site-specific subsets
pr_ebird = subset(nc_ebird_migra, lat >= PRquad[4,2] & lat <= PRquad[2,2] & 
                    long >= PRquad[1,3] & long <= PRquad[2,3])
bg_ebird = subset(nc_ebird_migra, lat >= BGquad[4,2] & lat <= BGquad[2,2] & 
                    long >= BGquad[1,3] & long <= BGquad[2,3])


# Plotting
plot(pr_ebird[pr_ebird$year == 2016,]$long, pr_ebird[pr_ebird$year == 2016,]$lat)

plot(nc_ebird_sub[nc_ebird_sub$year == 2015,]$long, nc_ebird_sub[nc_ebird_sub$year == 2015,]$lat, 
     axes = FALSE, xlab = "", ylab = "")







