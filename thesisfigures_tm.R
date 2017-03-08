#Figures going into Thesis 
#Top-down experiment, Bottom-up experiment, and regional results

#load libraries
library(RgoogleMaps)
library(maps)

#source documents where data files are contained
source("data_analysis.R") # Southern Appalachian exclosures (top-down)
source("Exclosure Analysis/ExclosureAnalysis_Summer2016.R") #Triangle exclosures (top-down)
source("tree_species_model.R") #Tree species model (#Bottom up)
source("northern_comparisons.R") # Regional comparisons

#something about the histogram function makes it so I can't plot them in panels. need to change function to hist for all histograms
#Figure 1 - Differences of Differences histogram comparing effects of exclosures on 
#Bird Food Arthropods and caterpillars in the Southern Appalachians and in the triangle #still need to figure out how to do this in a panel/do I really want to show this?
par(mfrow = c(2, 2), mar=c(4,4,2,2), oma = c(3,3,2,2))
hist(food_time4_12$VFX_VF_dif, breaks=30, col = "deepskyblue", ylab="S. Appalachinas", xlab = "", main = "Bird Food",
         plot = TRUE) #bird food app
hist(caterpillar_time4_12$VFX_VF_dif, breaks=10, col = "deepsky blue", ylab="", xlab = "", main = "Caterpillars",
         plot = TRUE) #caterpillars app
hist(food_time4_t$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="NC Piedmont", xlab = "",
          main="", plot = TRUE) #bird food tri
hist(caterpillar_time4_t$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="", xlab = "",
          main="", plot = TRUE) #caterpillars tri
mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.5)
mtext("Difference in Change in Arthropod Density", side = 1, outer = TRUE, line = 2)

#ylab = percent of total
#xlab  = "Difference in Change in Arth Density between VFX and VF
#Figure 2- Raw arth densities for bird food, caterpillars for triangle and southern appalachians
par(mfrow = c(4, 2), mar=c(4,4,2,2))
hist(food_time4_12$Visit3VF,  ylim=c(0,80), main ="Control", xlab = "Bird Food Density", ylab = "",
     col="deepskyblue", plot = TRUE)
hist(food_time4_12$Visit3VFX,  ylim=c(0,80), main ="Exclosure", xlab = "Bird Food Density", ylab = "",
     col="deepskyblue", plot = TRUE)
hist(caterpillar_time4_12$Visit3VF,  ylim=c(0,80),main ="", xlab = "Caterpillar Density", ylab = "",
     col="deepskyblue", plot = TRUE)
hist(caterpillar_time4_12$Visit3VFX,  ylim=c(0,80), main ="", xlab = "Caterpillar Density", ylab = "",
     col="deepskyblue", plot = TRUE)
hist(food_final$Visit3VF,  ylim=c(0,25), main ="", xlab = "Bird Food Density", ylab = "",
       col="mediumspringgreen", plot = TRUE)
hist(food_final$Visit3VFX,  ylim=c(0,25), main ="", xlab = "Bird Food Density", ylab = "",
       col="mediumspringgreen", plot = TRUE)
hist(caterpillar_final$Visit3VF,  ylim=c(0,25), main ="", xlab = "Caterpillar Density", ylab = "",
     col="mediumspringgreen", plot = TRUE)
hist(caterpillar_final$Visit3VFX,  ylim=c(0,25), main ="", xlab = "Caterpillar Density", ylab = "",
     col="mediumspringgreen", plot = TRUE)

mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.5)



#Figure 3 
#Plot HSD results
par(mfrow = c(2, 2), mar=c(7,4,3,3))
barplot(plotting.log_app_food$means, names.arg=plotting.log_app_food$tree_sp, las=2,  
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = (ifelse (plotting.log_tri_caterpillar$M == "a", "blue", 
                             ifelse(plotting.log_tri_caterpillar$M == "ab", "red", NA)))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_food$M)

#caterpillar appalachians
barplot(plotting.log_app_caterpillar$means, names.arg=plotting.log_app_caterpillar$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = (ifelse(plotting.log_tri_caterpillar$M == "a", "black", 
                             ifelse(plotting.log_tri_caterpillar$M == "b","blue", 
                                    ifelse(plotting.log_tri_caterpillar$M == "bc", "skyblue", 
                                           ifelse(plotting.log_tri_caterpillar$M == "c", "grey",
                                                  ifelse(plotting.log_tri_caterpillar$M == "d", "white", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_caterpillar$M)

#bird food triangle
barplot(plotting.log_tri_food$means, names.arg=plotting.log_tri_food$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = (ifelse(plotting.log_tri_food$M == "a", "seagreen4", 
                    ifelse(plotting.log_tri_food$M == "ab", "seagreen3", 
                      ifelse(plotting.log_tri_food$M == "b","mediumspringgreen", 
                             ifelse(plotting.log_tri_food$M == "bc", "lightgreen", 
                                    ifelse(plotting.log_tri_food$M == "c", "mintcream", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_food$M)

#caterpillar triangle
barplot(plotting.log_tri_caterpillar$means, names.arg=plotting.log_tri_caterpillar$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = (ifelse(plotting.log_tri_caterpillar$M == "a", "seagreen4", 
                            ifelse(plotting.log_tri_caterpillar$M == "ab", "seagreen3", 
                                   ifelse(plotting.log_tri_caterpillar$M == "b","mediumspringgreen", 
                                          ifelse(plotting.log_tri_caterpillar$M == "bc", "lightgreen", 
                                                 ifelse(plotting.log_tri_caterpillar$M == "c", "mintcream", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_caterpillar$M)
#if then statement for the colors?

#Map locations of sites and Regions 
library(RgoogleMaps)

##Read in BBS Lat-Long Data
BBS_coordinates <- read.table("data/BBS_stop_latlongs.txt", sep= '\t', quote="\"", header=TRUE)
names(BBS_coordinates) = c("location", "lat", "long")

oth_coordinates = data.frame(location = c("Botanical Garden", "Prairie Ridge", "Hubbard Brook", "Moosilauke", "Russell", "Stinson", "C", "H", "M"), 
                             lat = c(35.898645, 35.809674, 43.93903, 43.990567,44.004677, 43.83500, 41.460058, 41.521779, 41.476715), 
                             long = c(-79.031469, -78.716546, -71.75115, -71.773936, -71.645864, -71.773936, -72.520527, -72.541126, -72.631527))
all_coordinates = rbind(BBS_coordinates, oth_coordinates)

##Map lat-long data
par(mfrow = c(1, 1), mar=c(7,4,3,3))
hub = filter(all_coordinates, location %in% c("Hubbard Brook", "Moosilauke", "Russell", "Stinson"))
sing = filter(all_coordinates, location %in% c("C", "H", "M"))
va = filter(all_coordinates, location %in% uniq_va)
sa = filter(all_coordinates, location %in% uniq_sa)
tri = filter(all_coordinates, location %in% c("Botanical Garden", "Prairie Ridge"))

map('state', xlim = c(-85, -70), ylim = c(33, 45))
points(hub$lon, hub$lat, pch = 1, col ='pink', 
       cex = 1, bg="black", main="Survey Site Locations")
points(sing$lon, sing$lat, pch = 2, col ='purple', 
       cex = 1, bg="black", main="Survey Site Locations")
points(va$lon, va$lat, pch = 3, col ='lightblue', 
       cex = 1, bg="black", main="Survey Site Locations")
points(sa$lon, sa$lat, pch = 4, col ='blue', 
       cex = 1, bg="black", main="Survey Site Locations")
points(tri$lon, tri$lat, pch = 4, col ='green', 
       cex = 1, bg="black", main="Survey Site Locations")
