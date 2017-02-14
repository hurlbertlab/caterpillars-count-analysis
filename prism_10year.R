# Downloading temperature data from prism and calculating GDD

# Install prism package
library(prism)
library(raster)

setwd('c:/users/hayeste/my documents/495/prism') 
# Currently only have this data on my personal computer (not in git)
# A csv is written and placed in the repo, but if any years are added will need to rerun skip section

options(prism.path = "c:/users/hayeste/my documents/495/prism")

### SKIP SECTION BELOW IF ALREADY HAVE DATA DOWNLOADED ###


if (0) {

# Set path and download data
# This takes many hours
get_prism_dailys(type = 'tmean', minDate = "2000-1-1", maxDate = "2006-12-31",
                 keepZip = FALSE)

} # end if (0)


# Longitude and latitude of 3 locations
longlat = data.frame(location = c("Botanical Garden", "Prairie Ridge", "Hubbard Brook"), 
                     lat = c(35.898645, 35.809674, 43.942407), 
                     long = c(-79.031469, -78.716546, -71.710066))

### SKIP SECTION BELOW IF ALREADY HAVE TEMP DATA ORGANIZED ###

if (0) {

# Might need to change Hubbard Brook longlat

list <- ls_prism_data()[,1]
files<-paste('c:/users/hayeste/my documents/495/prism/',list,'/',list,'.bil',sep='')
tmeans<-stack(files)
ls_prism_data()[1:10,]

foo = extract(tmeans, longlat[,3:2])
temp <- t(foo)
temp <- data.frame(temp)
names(temp) = c('BG', 'PR', "HB")
filenames <- gsub('_provisional', "", ls_prism_data()[,1])
filenames <- gsub('_stable', "", filenames)
temp$date <- as.numeric(word(filenames, 4, 4, sep = fixed("_")))
temp$date = as.Date(as.character(temp$date), format = '%Y%m%d')
temp$jday = yday(temp$date)
temp$year = as.numeric(substr(as.character(temp$date), 1, 4))
temp.order <- temp[with(temp, order(year, jday)), ]
write.csv(temp.order, file = "c:/git/caterpillars-count-analysis/data/prism_temp.csv")

} # end if (0)

### END OF SKIP SECTION ###

prismtemp <- read.csv(file = "c:/git/caterpillars-count-analysis/data/prism_temp.csv", header = TRUE, sep = ",",
            quote = "\"")
prismtemp <- prismtemp[,2:7]

# Calculating growing degree days (GDD)
# Using 7 degrees C as threshold based on paper: Fu et al 2014

gddcalc = function(data,      # dataset with a year, date, and julian day column (labeled jday, date, year)
                   site,      # either "PR", "BG", or "HB"
                   year)      # enter year between 2007 and 2016
{  
  
subdata <- data[, c(site, "date", "jday", "year")]
pregddfull <- subdata[subdata$year == year,]
pregdd <- pregddfull[,site]
pregdd[pregdd < 7] = 7
pregdd[pregdd > 30] = 30 # need to do more research about thresholds
pregdd1 <- pregdd - 7
gdd <- cumsum(pregdd1)
GDD <- data.frame(pregddfull$jday, gdd)
names(GDD) <- c('jday', 'GDD')
plot(GDD$jday, GDD$GDD, xlab = "Julian day", ylab = "GDD", pch = 20)


# Calculate reference julian day for comparison (at 1000 GDDs)
refnum <- which.min(abs(GDD$GDD - 1000))
points(x = refnum, y = 1000, col = 'red', pch = 8, cex = 1.5)

return(refnum)

} # end function


### For loop for creating a dataset of all years and all sites' reference jday
par(mfrow = c(3,10), mar = c(2,2,1,1), oma = c(1,1,1,1))
gddyear <- c()

for (year in 2000:2016) {
  PR <- gddcalc(prismtemp, "PR", year)
  BG <- gddcalc(prismtemp, "BG", year)
  HB <- gddcalc(prismtemp, "HB", year)
  singleyear <- c(PR, BG, HB)
  gddyear <- rbind(gddyear, singleyear)
}

par(mfrow = c(1,1))
gddyear <- data.frame(gddyear) # Error is ok, I wanted that to happen
names(gddyear) <- c('pr.gdd','bg.gdd','hb.gdd')
gddyear$year = c(2000:2016)
plot(gddyear$year, gddyear$pr.gdd, col = 'red', type = 'l', lwd = 2, ylim = c(145,250))
points(gddyear$year, gddyear$bg.gdd, col = 'blue', type = 'l', lwd = 2)
points(gddyear$year, gddyear$hb.gdd, col = 'green3', type = 'l', lwd = 2)

# See a point location
# Assumes no other prism files in ls, need lat first and then long
# Provides a ggplot [not really that useful]
#bg <- prism_slice(c(longlat[1,3],longlat[1,2]),ls_prism_data()[1:80,1])
#pr <- prism_slice(c(longlat[2,2],longlat[2,3]),ls_prism_data()[,1])
#hb <- prism_slice(c(longlat[3,2],longlat[3,3]),ls_prism_data()[,1])




