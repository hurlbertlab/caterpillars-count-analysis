library(lubridate)

setwd('c:/users/hayeste/my documents/495/eBird/NC')

library(stringr)

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


  
# Based on Hurlbert-Liang filtered_logistics script:

# First subset pr_ebird for YEAR for EFFORT BY DAY
pr_ebird_2015 = pr_ebird[pr_ebird$year == 2015,]
effort.by.day = aggregate(pr_ebird_2015$lat_long, list(pr_ebird_2015$year, pr_ebird_2015$julianday), function(x) length(unique(x)))
names(effort.by.day)=c('year','julianday','num_unique_locs')            #number of unique locs in sampling per day
window.days = 80:180     #the days without records are discarded, and here we're adding the 0's back in to replace the NA's
temp.data1 = merge(effort.by.day,as.data.frame(window.days),by.x='julianday',by.y='window.days',all=F)
temp.data1[is.na(temp.data1$num_unique_locs),'num_unique_locs'] = 0

#observations
# Subset pr_ebird for SPECIES
pr_ebird_INBU = pr_ebird_2015[pr_ebird_2015$scientific_name == 'Passerina cyanea',]
pr_ebird_INBU[pr_ebird_INBU$count == 'X',]$count = 1 # if count = x, still present, changing to 1
pr_ebird_INBU$count = as.numeric(pr_ebird_INBU$count) # change from being a factor
pr_ebird_INBU[is.na(pr_ebird_INBU$count)==T,'count'] = 1 #give all the ones without obs counts a value of 1 (don't think this is in my data)

combos = do.call('paste', pr_ebird_INBU[c('scientific_name','lat','long','year','julianday')]) #paste these columns together
max.by.siteday = aggregate(pr_ebird_INBU$count, by=list(combos), FUN = max) #take the max observation count (shortens list since by day)

max.by.siteday.split = data.frame(str_split_fixed(max.by.siteday$Group.1, " ", 6)) # split rows back
max.by.siteday.split$X7 = paste(max.by.siteday.split$X1, max.by.siteday.split$X2, sep = ' ') # create sci name column
max.by.siteday.split$X8 = paste(max.by.siteday.split$X3, max.by.siteday.split$X4, sep = ' ') # create long lat column
max.by.siteday.split = max.by.siteday.split[, c('X7', 'X8', 'X3', 'X4', 'X5', 'X6')]

max.by.siteday.final = cbind(max.by.siteday.split, max.by.siteday$x)         
names(max.by.siteday.final) = c('scientific_name','lat_long','lat','long','year','julianday','count')     #<-Added in Lat and Long separately too
max.by.siteday.final[,'year'] = as.numeric(as.character(max.by.siteday.final[,'year']))
max.by.siteday.final[,'julianday'] = as.numeric(as.character(max.by.siteday.final[,'julianday']))
# NAs introduced by coercion??
sorted.max.by.siteday = max.by.siteday.final[order(max.by.siteday.final$scientific_name, max.by.siteday.final$year, max.by.siteday.final$julianday),]         #<-Now with Lat.Long added in.



hmm = cbind(sorted.max.by.siteday, 1)
names(hmm) = c('scientific_name','lat_long','lat','long','year','julianday','count','uniq.count')
combos2 = do.call('paste', hmm[c('lat','long','year','julianday')])
gee = aggregate(hmm$uniq.count, by=list(combos2), sum)
goe = as.data.frame(matrix(unlist(strsplit(gee$Group.1, " ")), ncol=4, byrow=T))
gob = data.frame(goe$V1, goe$V2, goe$V3, goe$V4)
goc = cbind(gob, gee$x)
names(goc) = c('lat','long','year','julianday','Num.of.uniq.locs')
combos3 = do.call('paste', goc[c('year','julianday')])
loc = aggregate(goc$Num.of.uniq.locs, by=list(combos3), sum)
Loc = as.data.frame(matrix(unlist(strsplit(loc$Group.1, " ")), ncol=2, byrow=T))
locc = cbind(Loc, loc$x)
names(locc) = c('year','julianday','Num.uniq.locs')
locc[,'year'] = as.numeric(as.character(locc[,'year']))
locc[,'julianday'] = as.numeric(as.character(locc[,'julianday']))
locc[,'Num.uniq.locs'] = as.numeric(as.character(locc[,'Num.uniq.locs']))

window.days2 = as.data.frame(as.matrix(window.days, ncol=1, byrow=T))
temp.data2 = merge(locc[locc$year==yr,],window.days2,by.x='julianday',by.y='V1',all=T)
temp.data2[is.na(temp.data2$Num.uniq.locs),'Num.uniq.locs'] = 0

temp.data2 = temp.data2[temp.data2$julianday<=180 & temp.data2$julianday>=80, ]         ##<<Change both here if want to change window of days seen<<##
temp.data1 = temp.data1[temp.data1$julianday<=180 & temp.data1$julianday>=80, ]

temp.data2$prop = temp.data2$Num.uniq.locs/temp.data1$Num.Unique.locs
