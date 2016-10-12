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

nh_ebird <- read.table(file = 'nh_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '')
nh_ebird_sub <- nh_ebird[,c(3,4,5,8,10,14,20,23,24,25,26)]
names(nh_ebird_sub) <- c('category', 'common_name', 'scientific_name', 'count', 'age_sex', 
                         'state', 'locality', 'lat', 'long', 'date', 'time')
nh_ebird_sub$scientific_name <- as.character(nh_ebird_sub$scientific_name)
nh_ebird_sub$year = as.numeric(as.character(substr(nh_ebird_sub$date, 1, 4)))

# Migratory status

migratoryguild <- read.table('c:/users/hayeste/my documents/465_project/data/migratoryguild.txt', header = T, sep = ',', quote = '"')
migratoryguild$scientificName = paste(migratoryguild$Genus, migratoryguild$Species, sep = ' ')
migratoryguild$MigClass[migratoryguild$MigClass == 'unk'] = NA
migratoryguild$migclass1[migratoryguild$migclass1 == 'unk'] = NA
migraguild <- migratoryguild[,5:7]
names(migraguild) <- c('migclass', 'migclass_gen', 'scientific_name')
nc_ebird_migra <- merge(nc_ebird_sub, migraguild, by = 'scientific_name', all.x = T, all.y = F)
nh_ebird_migra <- merge(nh_ebird_sub, migraguild, by = 'scientific_name', all.x = T, all.y = F)

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
nh_ebird_migra <- nh_ebird_migra[!is.na(nh_ebird_migra$long),]

# Pull out site-specific subsets
pr_ebird = subset(nc_ebird_migra, lat >= PRquad[4,2] & lat <= PRquad[2,2] & 
                    long >= PRquad[1,3] & long <= PRquad[2,3])
bg_ebird = subset(nc_ebird_migra, lat >= BGquad[4,2] & lat <= BGquad[2,2] & 
                    long >= BGquad[1,3] & long <= BGquad[2,3])
hb_ebird = subset(nh_ebird_migra, lat >= HBquad[4,2] & lat <= HBquad[2,2] & 
                    long >= HBquad[1,3] & long <= HBquad[2,3])

# Change date class and create a yday column

pr_ebird$date = as.Date(as.character(pr_ebird$date), format = '%Y-%m-%d')
pr_ebird$jday = yday(pr_ebird$date)

bg_ebird$date = as.Date(as.character(bg_ebird$date), format = '%Y-%m-%d')
bg_ebird$jday = yday(bg_ebird$date)

hb_ebird$date = as.Date(as.character(hb_ebird$date), format = '%Y-%m-%d')
hb_ebird$jday = yday(hb_ebird$date)

# Plot number of entries each year for each site, to compare
par(mar = c(4,4,2,2), oma = c(1,1,1,1))

pr_entries <- data.frame(table(pr_ebird$year))
names(pr_entries) <- c('year', 'num_entries')
pr_entries$year <- as.numeric(as.character(pr_entries$year))
plot(pr_entries$year, pr_entries$num_entries, col = 'red', type = 'l', lwd = 2,
     xlab = 'Year', ylab = 'Number of entries', ylim = c(0, 19000))

bg_entries <- data.frame(table(bg_ebird$year))
names(bg_entries) <- c('year', 'num_entries')
bg_entries$year <- as.numeric(as.character(bg_entries$year))
points(bg_entries$year, bg_entries$num_entries, col = 'blue', type = 'l', lwd = 2)

hb_entries <- data.frame(table(hb_ebird$year))
names(hb_entries) <- c('year', 'num_entries')
hb_entries$year <- as.numeric(as.character(hb_entries$year))
points(hb_entries$year, hb_entries$num_entries, col = 'green3', type = 'l', lwd = 2)

legend('topleft', c('PR', 'BG', 'HB'), lwd = 2, col = c('red', 'blue', 'green3'))

# Function for plotting abundances of some of the possible focal species per year for each site

focalbird = function(species, # species common name to plot for
                     adjusty = c(0,150), # adjust ylim
                     prdata = pr_ebird, bgdata = bg_ebird, hbdata = hb_ebird)
  
{ # start function
  years = data.frame(year=c(2007:2016))
  
  pr_entries <- data.frame(table(prdata$year[prdata$common_name == species]))
  names(pr_entries) <- c('year', 'num_entries')
  pr_entries$num_entries <- as.numeric(as.character(pr_entries$num_entries))
  pr_entries$year <- as.numeric(as.character(pr_entries$year))
  pr_entries <- merge(pr_entries, years, by = 'year', all = TRUE)
  pr_entries$num_entries[is.na(pr_entries$num_entries)] <- 0
  plot(pr_entries$year, pr_entries$num_entries, col = 'red', type = 'l', lwd = 2,
       xlab = 'Year', ylab = 'Number of entries', ylim = adjusty, main = species)
  
  bg_entries <- data.frame(table(bgdata$year[bgdata$common_name == species]))
  names(bg_entries) <- c('year', 'num_entries')
  bg_entries$num_entries <- as.numeric(as.character(bg_entries$num_entries))
  bg_entries$year <- as.numeric(as.character(bg_entries$year))
  bg_entries <- merge(bg_entries, years, by = 'year', all = TRUE)
  bg_entries$num_entries[is.na(bg_entries$num_entries)] <- 0
  points(bg_entries$year, bg_entries$num_entries, col = 'blue', type = 'l', lwd = 2)
  
  hb_entries <- data.frame(table(hbdata$year[hbdata$common_name == species]))
  names(hb_entries) <- c('year', 'num_entries')
  hb_entries$num_entries <- as.numeric(as.character(hb_entries$num_entries))
  hb_entries$year <- as.numeric(as.character(hb_entries$year))
  hb_entries <- merge(hb_entries, years, by = 'year', all = TRUE)
  hb_entries$num_entries[is.na(hb_entries$num_entries)] <- 0
  points(hb_entries$year, hb_entries$num_entries, col = 'green3', type = 'l', lwd = 2)
  
  legend('topleft', c('PR', 'BG', 'HB'), lwd = 2, col = c('red', 'blue', 'green3'))
}

# Plotting
par(mfrow = c(3,2), mar = c(4,4,2,2), oma = c(1,1,1,1))
focalbird('Red-eyed Vireo', c(0,275))
focalbird('House Wren', c(0,275))
focalbird('Common Yellowthroat', c(0,275))
focalbird('Indigo Bunting', c(0,275))
focalbird('Blue-gray Gnatcatcher', c(0,275))
focalbird('Summer Tanager', c(0,275))








plot(pr_ebird[pr_ebird$year == 2016,]$long, pr_ebird[pr_ebird$year == 2016,]$lat)

plot(nc_ebird_sub[nc_ebird_sub$year == 2015,]$long, nc_ebird_sub[nc_ebird_sub$year == 2015,]$lat, 
     axes = FALSE, xlab = "", ylab = "")







