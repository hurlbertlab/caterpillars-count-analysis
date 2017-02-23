# eBird data for Caterpillars Count! Project
# Reading in, subsetting, cleaning and organizing data, creating CSVs for eBird_logistics.R

# Downloaded from eBird Sept. 22, 2016
# Files include all data from 2006 to 2016 in NC (Prairie Ridge, Botanical Garden) 
# and NH (Hubbard Brook for comparison)

library(lubridate)

# ---- Read in files and clean ----

setwd('c:/users/hayeste/my documents/495/eBird/NC')

nc_ebird <- read.table(file = 'nc_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '')

# ---- Exclude casual observations, long travel distances, and short durations



nc_ebird_sub <- nc_ebird[,c(1,3,4,5,8,10,14,20,23,24,25,26)]
names(nc_ebird_sub) <- c('identifier', 'category', 'common_name', 'scientific_name', 'count', 'age_sex', 
                         'state', 'locality', 'lat', 'long', 'date', 'time')
nc_ebird_sub$scientific_name <- as.character(nc_ebird_sub$scientific_name)
nc_ebird_sub$year = as.numeric(as.character(substr(nc_ebird_sub$date, 1, 4)))


if (0) { # not using Hubbard Brook data currently
  
setwd('c:/users/hayeste/my documents/495/eBird/NH')

nh_ebird <- read.table(file = 'nh_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '')
nh_ebird_sub <- nh_ebird[,c(3,4,5,8,10,14,20,23,24,25,26)]
names(nh_ebird_sub) <- c('identifier', 'category', 'common_name', 'scientific_name', 'count', 'age_sex', 
                         'state', 'locality', 'lat', 'long', 'date', 'time')
nh_ebird_sub$scientific_name <- as.character(nh_ebird_sub$scientific_name)
nh_ebird_sub$year = as.numeric(as.character(substr(nh_ebird_sub$date, 1, 4)))

} # end if (0)

# Migratory status

migratoryguild <- read.table('c:/users/hayeste/my documents/465_project/data/migratoryguild.txt', header = T, sep = ',', quote = '"')
migratoryguild$scientificName = paste(migratoryguild$Genus, migratoryguild$Species, sep = ' ')
migratoryguild$MigClass[migratoryguild$MigClass == 'unk'] = NA
migratoryguild$migclass1[migratoryguild$migclass1 == 'unk'] = NA
migraguild <- migratoryguild[,5:7]
names(migraguild) <- c('migclass', 'migclass_gen', 'scientific_name')
nc_ebird_migra <- merge(nc_ebird_sub, migraguild, by = 'scientific_name', all.x = T, all.y = F)
#nh_ebird_migra <- merge(nh_ebird_sub, migraguild, by = 'scientific_name', all.x = T, all.y = F)

# ---- longlatquad function ----
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
  
#PRquad = longlatquad(latpoint = 35.809674, longpoint = -78.716546, res_km = 8)
#BGquad = longlatquad(latpoint = 35.898645, longpoint = -79.031469, res_km = 8)
#HBquad = longlatquad(latpoint = 43.942407, longpoint = -71.710066, res_km = 8)

PRquad = longlatquad(latpoint = 35.809674, longpoint = -78.716546, res_km = 4)
BGquad = longlatquad(latpoint = 35.898645, longpoint = -79.031469, res_km = 4)
#HBquad = longlatquad(latpoint = 43.942407, longpoint = -71.710066, res_km = 4)

nc_ebird_migra <- nc_ebird_migra[!is.na(nc_ebird_migra$long),] # if no longitude, no latitude either
#nh_ebird_migra <- nh_ebird_migra[!is.na(nh_ebird_migra$long),]



# ---- Site-specific subsets ----

# Pull out site-specific subsets
pr_ebird = subset(nc_ebird_migra, lat >= PRquad[4,2] & lat <= PRquad[2,2] & 
                    long >= PRquad[1,3] & long <= PRquad[2,3])
bg_ebird = subset(nc_ebird_migra, lat >= BGquad[4,2] & lat <= BGquad[2,2] & 
                    long >= BGquad[1,3] & long <= BGquad[2,3])
#hb_ebird = subset(nh_ebird_migra, lat >= HBquad[4,2] & lat <= HBquad[2,2] & 
#                    long >= HBquad[1,3] & long <= HBquad[2,3])

# Change date class and create a yday column

pr_ebird$date = as.Date(as.character(pr_ebird$date), format = '%Y-%m-%d')
pr_ebird$jday = yday(pr_ebird$date)

bg_ebird$date = as.Date(as.character(bg_ebird$date), format = '%Y-%m-%d')
bg_ebird$jday = yday(bg_ebird$date)

#hb_ebird$date = as.Date(as.character(hb_ebird$date), format = '%Y-%m-%d')
#hb_ebird$jday = yday(hb_ebird$date)

#---- Organize data into format for logistics script ----

sampling_pr = pr_ebird[,c('lat', 'long', 'year', 'jday')]
sampling_pr$Lat.Long = paste(sampling_pr$lat, sampling_pr$long, sep = "")
names(sampling_pr) = c('Latitude', 'Longitude', 'Year.Collected', 'JulianDay', 'Lat.Long')
write.csv(sampling_pr, 'c:/git/caterpillars-count-analysis/data/ebird_sampling_pr.csv')

obs_pr = pr_ebird[,c('scientific_name', 'lat', 'long', 'count', 'year', 'jday')]
names(obs_pr) = c('Scientific.Name', 'Latitude', 'Longitude', 'Observation.Count', 'Year', 'JulianDay')
write.csv(obs_pr, 'c:/git/caterpillars-count-analysis/data/ebird_obs_pr.csv')

sampling_bg = bg_ebird[,c('lat', 'long', 'year', 'jday')]
sampling_bg$Lat.Long = paste(sampling_bg$lat, sampling_bg$long, sep = "")
names(sampling_bg) = c('Latitude', 'Longitude', 'Year.Collected', 'JulianDay', 'Lat.Long')
write.csv(sampling_bg, 'c:/git/caterpillars-count-analysis/data/ebird_sampling_bg.csv')

obs_bg = bg_ebird[,c('scientific_name', 'lat', 'long', 'count', 'year', 'jday')]
names(obs_bg) = c('Scientific.Name', 'Latitude', 'Longitude', 'Observation.Count', 'Year', 'JulianDay')
write.csv(obs_bg, 'c:/git/caterpillars-count-analysis/data/ebird_obs_bg.csv')

#----Everthing below is just experimenting with the data----
# Can use datasets cleaned above for logistics script

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

# ---- focalbird function ----
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
} # end function

# Plotting
par(mfrow = c(3,2), mar = c(4,4,2,2), oma = c(1,1,1,1))
focalbird('Red-eyed Vireo', c(0,275))
focalbird('House Wren', c(0,275))
focalbird('Common Yellowthroat', c(0,275))
focalbird('Indigo Bunting', c(0,275))
focalbird('Blue-gray Gnatcatcher', c(0,275))
focalbird('Summer Tanager', c(0,275)) # error because no HB data for summer tanager

# ---- focalabun function ----
# Plotting one species' changing abundance throughout spring/summer season of one year (by week) at each site

focalabun = function(species, # species common name to plot for
                     chooseyear, # choose year
                     adjusty = c(0,20), # adjust ylim
                     adjustx = c(10,40), # adjust xlim
                     prdata = pr_ebird, bgdata = bg_ebird, hbdata = hb_ebird)
  
{ # start function
  pr_entries <- subset(prdata, year == chooseyear & common_name == species)
  pr_entries$week = floor(pr_entries$jday/7) + 1
  pr_weekly <- data.frame(table(pr_entries$week))
  names(pr_weekly) <- c('week', 'abunpr')
  pr_weekly$week <- as.numeric(as.character(pr_weekly$week))
  pr_weekly$abunpr <- as.numeric(as.character(pr_weekly$abunpr))
  plot(pr_weekly$week, pr_weekly$abunpr, col = 'red', type = 'l', lwd = 2,
       xlab = 'Week', ylab = 'Number of entries', ylim = adjusty, xlim = adjustx, main = species)
  
  bg_entries <- subset(bgdata, year == chooseyear & common_name == species)
  bg_entries$week = floor(bg_entries$jday/7) + 1
  bg_weekly <- data.frame(table(bg_entries$week))
  names(bg_weekly) <- c('week', 'abunbg')
  bg_weekly$week <- as.numeric(as.character(bg_weekly$week))
  bg_weekly$abunbg <- as.numeric(as.character(bg_weekly$abunbg))
  points(bg_weekly$week, bg_weekly$abunbg, col = 'blue', type = 'l', lwd = 2)
  
  hb_entries <- subset(hbdata, year == chooseyear & common_name == species)
  hb_entries$week = floor(hb_entries$jday/7) + 1
  hb_weekly <- data.frame(table(hb_entries$week))
  names(hb_weekly) <- c('week', 'abunhb')
  hb_weekly$week <- as.numeric(as.character(hb_weekly$week))
  hb_weekly$abunhb <- as.numeric(as.character(hb_weekly$abunhb))
  points(hb_weekly$week, hb_weekly$abunhb, col = 'green3', type = 'l', lwd = 2)
  
  legend('topleft', c('PR', 'BG', 'HB'), lwd = 2, col = c('red', 'blue', 'green3'))  
  
  prbg <- merge(pr_weekly, bg_weekly, by = 'week', all = TRUE)
  all <- merge(prbg, hb_weekly, by = 'week', all = TRUE)
  
  return(all)
} # end function
  
# Plotting
par(mfrow = c(3,2), mar = c(4,4,2,2), oma = c(1,1,1,1))
revi11 <- focalabun('Red-eyed Vireo', 2011)
revi12 <- focalabun('Red-eyed Vireo', 2012)
revi13 <- focalabun('Red-eyed Vireo', 2013)
revi14 <- focalabun('Red-eyed Vireo', 2014)
revi15 <- focalabun('Red-eyed Vireo', 2015)
revi16 <- focalabun('Red-eyed Vireo', 2016)

inbu11 <- focalabun('Indigo Bunting', 2011)
inbu12 <- focalabun('Indigo Bunting', 2012)
inbu13 <- focalabun('Indigo Bunting', 2013)
inbu14 <- focalabun('Indigo Bunting', 2014)
inbu15 <- focalabun('Indigo Bunting', 2015)
inbu16 <- focalabun('Indigo Bunting', 2016)

revi16 <- focalabun('Red-eyed Vireo', 2016)
howr16 <- focalabun('House Wren', 2016)
coye16 <- focalabun('Common Yellowthroat', 2016)
inbu16 <- focalabun('Indigo Bunting', 2016)
bggn16 <- focalabun('Blue-gray Gnatcatcher', 2016)
suta16 <- focalabun('Summer Tanager', 2016)


# Still need to edit for loop below
revimaxweek <- data.frame(year = c(), prmaxweek = c(), bgmaxweek = c())

for (year in c(2007:2016)) {
  data <- focalabun('Red-eyed Vireo', year)
  prmax <- max(data$abunpr[data$week %in% c(10:25)], na.rm = TRUE)
  prmaxweek <- data$week[data$abunpr == prmax][1]
  bgmax <- max(data$abunbg[data$week %in% c(10:25)], na.rm = TRUE)
  bgmaxweek <- data$week[data$abunbg == bgmax][1]
  line <- c(year, prmaxweek, bgmaxweek)
  revimaxweek <- rbind(revimaxweek, line)
}





plot(pr_ebird[pr_ebird$year == 2016,]$long, pr_ebird[pr_ebird$year == 2016,]$lat)

plot(nc_ebird_sub[nc_ebird_sub$year == 2015,]$long, nc_ebird_sub[nc_ebird_sub$year == 2015,]$lat, 
     axes = FALSE, xlab = "", ylab = "")







