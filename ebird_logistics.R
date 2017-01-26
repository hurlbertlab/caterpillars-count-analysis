library(lubridate)

setwd('c:/users/hayeste/my documents/495/eBird/NC')

nc_ebird <- read.table(file = 'nc_ebird.txt', header = TRUE, sep = '\t', fill = TRUE, quote = '') 
# once I decide on a resolution I won't have to read in this whole file
nc_ebird_sub <- nc_ebird[,c(1,3,4,5,8,10,14,20,23,24,25,26)]
names(nc_ebird_sub) <- c('identifier', 'category', 'common_name', 'scientific_name', 'count', 'age_sex', 
                         'state', 'locality', 'lat', 'long', 'date', 'time')
nc_ebird_sub$scientific_name <- as.character(nc_ebird_sub$scientific_name)
nc_ebird_sub$year = as.numeric(as.character(substr(nc_ebird_sub$date, 1, 4)))
nc_ebird_sub$date = as.Date(as.character(nc_ebird_sub$date))
nc_ebird_sub$julianday = yday(nc_ebird_sub$date)
nc_ebird_sub$lat_long = paste(nc_ebird_sub$lat, nc_ebird_sub$long, sep = ' ')
  
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

PRquad = longlatquad(latpoint = 35.809674, longpoint = -78.716546, res_km = 8)
BGquad = longlatquad(latpoint = 35.898645, longpoint = -79.031469, res_km = 8)
HBquad = longlatquad(latpoint = 43.942407, longpoint = -71.710066, res_km = 8)  
  
nc_ebird_sub <- nc_ebird_sub[!is.na(nc_ebird_sub$long),] #taking out non-locations (?)

# Pull out site-specific subsets
pr_ebird = subset(nc_ebird_sub, lat >= PRquad[4,2] & lat <= PRquad[2,2] & 
                    long >= PRquad[1,3] & long <= PRquad[2,3])
bg_ebird = subset(nc_ebird_sub, lat >= BGquad[4,2] & lat <= BGquad[2,2] & 
                    long >= BGquad[1,3] & long <= BGquad[2,3])


  
# Based on Hurlbert-Liang logistic script:

effort.by.day = aggregate(pr_ebird$lat_long, list(pr_ebird$year, pr_ebird$julianday), function(x) length(unique(x)))
names(effort.by.day)=c('year','julianday','num_unique_locs')            #number of unique locs in sampling per day
window.days = 80:180     #the days without records are discarded, and here we're adding the 0's back in to replace the NA's
temp.data1 = merge(effort.by.day,as.data.frame(window.days),by.x='julianday',by.y='window.days',all=F)
temp.data1[is.na(temp.data1$num_unique_locs),'num_unique_locs'] = 0

#observations
pr_ebird[is.na(t.obs.NA1s$Observation.Count)==T,'Observation.Count'] = 1                                    #give all the ones without obs counts a value of 1
combos = do.call('paste', t.obs.NA1s[c('Scientific.Name','Latitude','Longitude','Year','JulianDay')])         #paste these columns together
max.by.siteday = aggregate(t.obs.NA1s$Observation.Count, by=list(combos), function(x) max(x))                 #take the max observation count 
max.by.siteday.split = as.data.frame(matrix(unlist(strsplit(max.by.siteday$Group.1, " ")), ncol=6, byrow=T))  #unsplit them
MAX.by.siteday.split = data.frame(do.call('paste', max.by.siteday.split[c('V1','V2')]), do.call('paste',max.by.siteday.split[c('V3','V4')]), max.by.siteday.split$V3, max.by.siteday.split$V4, max.by.siteday.split$V5, max.by.siteday.split$V6)
pre.max.by.siteday = cbind(MAX.by.siteday.split, max.by.siteday$x)         
names(pre.max.by.siteday) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count')     #<-Added in Lat and Long separately too
pre.max.by.siteday[,'Year'] = as.numeric(as.character(pre.max.by.siteday[,'Year']))
pre.max.by.siteday[,'JulianDay'] = as.numeric(as.character(pre.max.by.siteday[,'JulianDay']))
sorted.max.by.siteday = pre.max.by.siteday[order(pre.max.by.siteday$Scientific.Name, pre.max.by.siteday$Year, pre.max.by.siteday$JulianDay),]         #<-Now with Lat.Long added in.


