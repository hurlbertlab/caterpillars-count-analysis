## Temperature and precipitation normals data downloaded from PRISM
# extracted to lat-longs for Hubbard brook, Singer et al. 2012, NCBG, PR, and all Appalachian survey sites

# load relevant packages 
library(prism)
library(raster)

options(prism.path = "~tmckinno/prismtmp")
# Set path and download data
get_prism_normals(type = 'tmean', resolution = "800m", m = c(5,6,7), keepZip = FALSE)
get_prism_normals(type = 'ppt', resolution = "800m", m = c(1:12), keepZip = FALSE)

# Longitude and latitude of 5 locations
BBS_coordinates <- read.table("data/BBS_stop_latlongs.txt", sep= '\t', quote="\"", header=TRUE)
names(BBS_coordinates) = c("location", "lat", "long")

oth_coordinates = data.frame(location = c("Botanical Garden", "Prairie Ridge", "Hubbard Brook", "Moosilauke", "Russell", "Stinson", "C", "H", "M"), 
                             lat = c(35.898645, 35.809674, 43.93903, 43.990567,44.004677, 43.83500, 41.460058, 41.521779, 41.476715), 
                             long = c(-79.031469, -78.716546, -71.75115, -71.773936, -71.645864, -71.773936, -72.520527, -72.541126, -72.631527))
all_coordinates = rbind(BBS_coordinates, oth_coordinates)

### SKIP SECTION BELOW IF ALREADY HAVE DATA ORGANIZED ###

#how tracie's code said to read in the files
list_temp = as.vector(ls_prism_data()[13:15,1])
files_temp = paste("/Users/tmckinno/prismtmp/",list_temp,"/",list_temp,".bil", sep="")
normals_temp = raster::stack(files_temp) 
#ls_prism_data()[1:10,]

#how tracie's code said to read in the files
list_precip = as.vector(ls_prism_data()[1:12,1])
files_precip = paste("/Users/tmckinno/prismtmp/",list_precip,"/",list_precip,".bil", sep="")
normals_precip = raster::stack(files_precip) 

#30year temp normals data for all 5 regions
temp = extract(normals_temp, all_coordinates[,3:2]) 
temp1 = data.frame(t(temp))
names(temp1) = all_coordinates$location #up til here is definitely useful
temp2 = data.frame(t(temp1))
names(temp2) = c("temp_normals_may", "temp_normals_june", "temp_normals_july")
write.csv(temp2, file = "~/Desktop/caterpillars-count-analysis/data/prism_temp_northerncomp.csv")

#30 year temp normals data for all 5 regions
precip = extract(normals_precip, all_coordinates[,3:2]) 
precip1 <- data.frame(t(precip))
names(precip1) = all_coordinates$location
precip2 = data.frame(t(precip1))
names(precip2) = c("ppt_normals_1", "ppt_normals_2", "ppt_normals_3", "ppt_normals_4",
                   "ppt_normals_5", "ppt_normals_6", "ppt_normals_7", "ppt_normals_8",
                   "ppt_normals_9", "ppt_normals_10", "ppt_normals_11", "ppt_normals_12")
write.csv(precip2, file = "~/Desktop/caterpillars-count-analysis/data/prism_precip_northerncomp.csv")

# Read in csvs if finished obtaining data
temperature = read.csv("data/environmental/prism_temp_northerncomp.csv")
precipitation = read.csv("data/environmental/prism_precip_northerncomp.csv")

