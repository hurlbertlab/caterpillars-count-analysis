#Figures going into Thesis 
#Top-down experiment, Bottom-up experiment, and regional results

#load libraries
library(RgoogleMaps)
library(maps)

#source documents where data files are contained
source("archived_scripts/Tara_thesis/ExclosureAnalysis_Summer2016.R") #Triangle exclosures (top-down)
source("archived_scripts/Tara_thesis/tree_species_model.R") #Tree species model (#Bottom up)
source("archived_scripts/Tara_thesis/northern_comparisons.R") # Regional comparisons
source("archived_scripts/Tara_thesis/data_analysis.R") # Southern Appalachian exclosures (top-down)

#Figure 1 - Differences of Differences histogram comparing effects of exclosures on 
#Bird Food Arthropods and caterpillars in the Southern Appalachians and in the triangle #still need to figure out how to do this in a panel/do I really want to show this?
caterpillar_time4_t1 = dplyr::filter(caterpillar_time4_t, VFX_VF_dif != 26)
par(mfrow = c(2, 2), mar=c(4,4.5,2,2), oma = c(3,3,2,2), cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
hist(food_time4_12$VFX_VF_dif, breaks=30, col = "deepskyblue", ylab="Appalachians", xlab = "", main = "Arthropods", ylim = c(0,70),
         plot = TRUE) #bird food app
legend("topright", "A", bty="n", cex = 1.5)
hist(caterpillar_time4_12$VFX_VF_dif, breaks=10, col = "deepsky blue", ylab="", xlab = "", main = "Caterpillars", xlim = c(-2,2), ylim = c(0,70),
         plot = TRUE) #caterpillars app
legend("topright", "B", bty="n", cex = 1.5)
hist(food_time4_t$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="Piedmont", xlab = "", ylim = c(0,20),
          main="", plot = TRUE) #bird food tri
legend("topright", "C", bty="n", cex = 1.5)
hist(caterpillar_time4_t1$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="", xlab = "", xlim = c(-2,2), ylim = c(0,20),
          main="", plot = TRUE) #caterpillars tri
legend("topright", "D", bty="n", cex = 1.5)
mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.5, cex  = 1.5)
mtext("Difference in Change in Arthropod Density", side = 1, outer = TRUE, line = .5, cex = 1.5)
#there is an outlier in NC piedmont caterpillars and bird food (26 caterpillars)
#removed for plotting ease, but should note

#ylab = percent of total
#xlab  = "Difference in Change in Arth Density between VFX and VF
#Figure 2- Raw arth counts for bird food, caterpillars for triangle and southern appalachians
caterpillar_final1 = filter(caterpillar_final, Visit3VFX != 26)
par(mfrow = c(4, 2), mar=c(4,4,2,2), cex.lab = 1.5, cex.axis = 1.6, cex.main = 1.8)
hist(food_time4_12$Visit3VF,  ylim=c(0,80), main ="Control", xlab = "Arth. density", ylab = "",
     col="deepskyblue",  plot = TRUE)
legend("topright", "A", bty="n", cex = 1.5)
hist(food_time4_12$Visit3VFX,  ylim=c(0,80), main ="Exclosure", xlab = "Arth. density", ylab = "",
     col="deepskyblue", plot = TRUE)
legend("topright", "B", bty="n", cex = 1.5)
hist(caterpillar_time4_12$Visit3VF,  ylim=c(0,80),main ="", xlab = "Cat. density", ylab = "",
     col="deepskyblue", plot = TRUE)
legend("topright", "C", bty="n", cex = 1.5)
hist(caterpillar_time4_12$Visit3VFX,  ylim=c(0,80), main ="", xlab = "Cat. density", ylab = "",
     col="deepskyblue", plot = TRUE)
legend("topright", "D", bty="n", cex = 1.5)
hist(food_final$Visit3VF,  ylim=c(0,25), main ="", xlab = "Arth. density", ylab = "",
       col="mediumspringgreen", plot = TRUE)
legend("topright", "E", bty="n", cex = 1.5)
hist(food_final$Visit3VFX,  ylim=c(0,25), main ="", xlab = "Arth. density", ylab = "",
       col="mediumspringgreen", plot = TRUE)
legend("topright", "F", bty="n", cex = 1.5)
hist(caterpillar_final$Visit3VF,  ylim=c(0,25), main ="", xlab = "Cat. density", ylab = "",
     col="mediumspringgreen", plot = TRUE)
legend("topright", "G", bty="n", cex = 1.5)
hist(caterpillar_final1$Visit3VFX,  ylim=c(0,25), main ="", xlab = "Cat. density", ylab = "",
     col="mediumspringgreen", plot = TRUE)
legend("topright", "H", bty="n", cex = 1.5)

mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.0, cex = 1.4)
#mtext("NC Piedmont                   S. Appalachians", outer = TRUE, side = 2, line = 0)

#Figure 3 
#Plot HSD results
par(mfrow = c(2, 2), mar=c(6,2.8,2,2), cex.lab = 1.2, cex.axis = 1, cex.main = 1.5)
barplot(plotting.log_app_food$means, las=2, main = "Arthropods", ylab = "Appalachians", ylim = c(-.3,.8), cex.names=.65,  
        col = (ifelse(plotting.log_app_food$M == "a", "blue", 
                      ifelse(plotting.log_app_food$M == "b","royalblue1", 
                             ifelse(plotting.log_app_food$M == "bc", "deepskyblue", 
                                    ifelse(plotting.log_app_food$M == "c", "lightskyblue1",
                                           ifelse(plotting.log_app_food$M == "d", "white", NA)))))))

text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.6, plotting.log_app_food$M)
text(x=seq(from=.9, to= 12, by = 1.29), y = -.15, labels = plotting.log_app_food$tree_sp, lheight = 1.5,
     srt = 55, adj = 1, xpd = TRUE, cex = .9)

#caterpillar appalachians
barplot(plotting.log_app_caterpillar$means, las=2, main = "Caterpillars", ylim = c(-.3,.8),  
        col = (ifelse(plotting.log_app_caterpillar$M == "a", "blue", 
                             ifelse(plotting.log_app_caterpillar$M == "b","royalblue1", 
                                    ifelse(plotting.log_app_caterpillar$M == "bc", "deepskyblue", 
                                           ifelse(plotting.log_app_caterpillar$M == "c", "lightskyblue1",
                                                  ifelse(plotting.log_app_caterpillar$M == "d", "white", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.6, plotting.log_app_caterpillar$M)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = -.15, labels = plotting.log_app_caterpillar$tree_sp, lheight = 1.5,
     srt = 55, adj = 1, xpd = TRUE, cex = .9)

#bird food triangle
barplot(plotting.log_tri_food$means, las=2, ylab = "Piedmont", ylim = c(0,.55), 
        col = (ifelse(plotting.log_tri_food$M == "a", "seagreen4", 
                    ifelse(plotting.log_tri_food$M == "ab", "seagreen3", 
                      ifelse(plotting.log_tri_food$M == "b","mediumspringgreen", 
                             ifelse(plotting.log_tri_food$M == "bc", "lightgreen", 
                                    ifelse(plotting.log_tri_food$M == "c", "mintcream", NA)))))))

text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.5, plotting.log_tri_food$M)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = 0, labels = plotting.log_tri_food$tree_sp, lheight = 1.5,
     srt = 55, adj = 1, xpd = TRUE, cex = .9)

#caterpillar triangle
barplot(plotting.log_tri_caterpillar$means, las=2, mar =c(1,1,1,1), ylim = c(0,.55), 
        col = (ifelse(plotting.log_tri_caterpillar$M == "a", "seagreen4", 
                            ifelse(plotting.log_tri_caterpillar$M == "ab", "seagreen3", 
                                   ifelse(plotting.log_tri_caterpillar$M == "b","mediumspringgreen", 
                                          ifelse(plotting.log_tri_caterpillar$M == "bc", "lightgreen", 
                                                 ifelse(plotting.log_tri_caterpillar$M == "c", "mintcream", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.5, plotting.log_tri_caterpillar$M)
mtext("Log-transformed Arth Density", side = 2, outer = TRUE, line = 1.5, cex = 1.3)
mtext("Tree Species", side = 1, outer = TRUE, line = .5, cex = 1.3)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = 0, labels = plotting.log_tri_caterpillar$tree_sp, lheight = 1.5,
     srt = 55, adj = 1, xpd = TRUE, cex = .9)

#Map locations of sites and Regions 
library(RgoogleMaps)

##Read in BBS Lat-Long Data
BBS_coordinates <- read.table("data/environmental/BBS_stop_latlongs.txt", sep= '\t', quote="\"", header=TRUE)
names(BBS_coordinates) = c("location", "lat", "long")

oth_coordinates = data.frame(location = c("Botanical Garden", "Prairie Ridge", "Hubbard Brook", "Moosilauke", "Russell", "Stinson", "C", "H", "M"), 
                             lat = c(35.898645, 35.809674, 43.93903, 43.990567,44.004677, 43.83500, 41.460058, 41.521779, 41.476715), 
                             long = c(-79.031469, -78.716546, -71.75115, -71.773936, -71.645864, -71.773936, -72.520527, -72.541126, -72.631527))
all_coordinates = rbind(BBS_coordinates, oth_coordinates)
all_coordinates1=filter(all_coordinates, location %in% unique(region_complete$site))
write.csv(all_coordinates1, "data/northern_comparisons_coordinates.csv")

##Map lat-long data
par(mfrow = c(1, 1), mar=c(7,4,3,3))
hub = filter(all_coordinates, location %in% c("Hubbard Brook", "Moosilauke", "Russell", "Stinson"))
sing = filter(all_coordinates, location %in% c("C", "H", "M"))
va = filter(all_coordinates, location %in% uniq_va)
sa = filter(all_coordinates, location %in% uniq_sa)
tri = filter(all_coordinates, location %in% c("Botanical Garden", "Prairie Ridge"))

write.csv(hub, "data/hub_latlong.csv")
write.csv(sing, "data/sing_latlong.csv")
write.csv(va, "data/va_latlong.csv")
write.csv(sa, "data/sa_latlong.csv")
write.csv(tri, "data/tri_latlong.csv")



