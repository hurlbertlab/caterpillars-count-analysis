# Install prism package
install.packages("prism")
library(prism)
library(raster)

setwd('c:/users/hayeste/my documents/495/prism')

# Set path and download data
# This took ~1.5 hours on my computer
options(prism.path = "c:/users/hayeste/my documents/495/prism")
get_prism_dailys(type = 'tmean', minDate = "2013-09-19", maxDate = "2016-08-31",
                 keepZip = FALSE)

# Longitude and latitude of 3 locations
longlat = data.frame(location = c("Botanical Garden", "Prairie Ridge", "Hubbard Brook"), 
                     lat = c(35.898645, 35.809674, 43.942407), 
                     long = c(-79.031469, -78.716546, -71.710066))
# Might need to change HUbbard Brook longlat
numbers <- as.numeric(word(ls_prism_data()[,1], 5, 5, sep = fixed("_")))
files<-paste('c:/users/hayeste/my documents/495/prism/PRISM_tmean_stable_4kmD1_',numbers,'_bil/PRISM_tmean_stable_4kmD1_',numbers,'_bil.bil',sep='')
tmeans<-stack(files)
ls_prism_data()[1:10,]

foo = extract(tmeans, longlat[,3:2])

# See a point location
# Assumes no other prism files in ls, need lat first and then long
# Provides a ggplot
bg <- prism_slice(c(longlat[1,3],longlat[1,2]),ls_prism_data()[1:80,1])
pr <- prism_slice(c(longlat[2,2],longlat[2,3]),ls_prism_data()[,1])
hb <- prism_slice(c(longlat[3,2],longlat[3,3]),ls_prism_data()[,1])






