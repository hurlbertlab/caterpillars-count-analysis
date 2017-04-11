# Modified version of thesisfigures_tm for thesis presentation talk

source("thesisfigures_tm.R")
#Figure 1 - Differences of Differences histogram comparing effects of exclosures on 
#Bird Food Arthropods and caterpillars in the Southern Appalachians and in the triangle #still need to figure out how to do this in a panel/do I really want to show this?
caterpillar_time4_t1 = dplyr::filter(caterpillar_time4_t, VFX_VF_dif != 26)
par(mfrow = c(1, 2), mar=c(3,4.5,2,2), oma = c(3,3,2,2), cex.lab = 2, cex.axis = 2, cex.main = 2)
hist(food_time4_12$VFX_VF_dif, breaks=30, col = "deepskyblue", ylab="Number of Surveys", xlab = "", main = "Appalachians", ylim = c(0,40),
     plot = TRUE) #bird food app
hist(food_time4_t$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="", xlab = "", ylim = c(0,12),
     main="Piedmont", plot = TRUE) #bird food tri
mtext("Difference in Change in Arthropod Density", side = 1, outer = TRUE, line = .5, cex = 2)


hist(caterpillar_time4_12$VFX_VF_dif, breaks=10, col = "deepsky blue", ylab="", xlab = "", main = "Caterpillars", xlim = c(-2,2), ylim = c(0,70),
     plot = TRUE) #caterpillars app



hist(caterpillar_time4_t1$VFX_VF_dif, breaks=10, col = "mediumspringgreen", ylab="", xlab = "", xlim = c(-2,2), ylim = c(0,20),
     main="", plot = TRUE) #caterpillars tri

mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.5, cex  = 1.5)
mtext("Difference in Change in Arthropod Density", side = 1, outer = TRUE, line = .5, cex = 1.5)
#there is an outlier in NC piedmont caterpillars and bird food (26 caterpillars)
#removed for plotting ease, but should note

#ylab = percent of total
#xlab  = "Difference in Change in Arth Density between VFX and VF
#Figure 2- Raw arth counts for bird food, caterpillars for triangle and southern appalachians
caterpillar_final1 = filter(caterpillar_final, Visit3VFX != 26)
par(mfrow = c(1, 2), mar=c(3,2.5,2,2), cex.lab = 1.5, cex.axis = 2, cex.main = 2)
hist(food_final$Visit3VF,  ylim=c(0,25), main ="Control", xlab = "", ylab = "",
     col="mediumspringgreen", plot = TRUE)
hist(food_final$Visit3VFX,  ylim=c(0,25), main ="Exclosure", xlab = "", ylab = "",
     col="mediumspringgreen", plot = TRUE)
mtext("Arthropod Density", side = 1, outer = TRUE, line = .5, cex = 2)
mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.0, cex = 2)

hist(caterpillar_final$Visit3VF,  ylim=c(0,25), main ="Control", xlab = "", ylab = "",
     col="mediumspringgreen", plot = TRUE)


hist(caterpillar_final1$Visit3VFX,  ylim=c(0,25), main ="Exclosure", xlab = "", ylab = "",
     col="mediumspringgreen", plot = TRUE)

mtext("Caterpillar Density", side = 1, outer = TRUE, line = .5, cex = 2)
mtext("Number of Surveys", side = 2, outer = TRUE, line = 1.0, cex = 2)


#mtext("NC Piedmont                   S. Appalachians", outer = TRUE, side = 2, line = 0)

#Figure 3 
#Plot HSD results
plotting.log_app_food$means_plus = plotting.log_app_food$means + .3
par(mfrow = c(1, 2), mar=c(10,2.8,2,2), cex.lab = 1.2, cex.axis = 2, cex.main = 2)
barplot(plotting.log_app_food$means, las=2, main = "Arthropods", ylab = "Appalachians", ylim = c(0,1.1), cex.names=.65,  
        col = (ifelse(plotting.log_app_food$M == "a", "blue", 
                      ifelse(plotting.log_app_food$M == "b","royalblue1", 
                             ifelse(plotting.log_app_food$M == "bc", "deepskyblue", 
                                    ifelse(plotting.log_app_food$M == "c", "lightskyblue1",
                                           ifelse(plotting.log_app_food$M == "d", "white", NA)))))))

text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.6, cex = 1.5, plotting.log_app_food$M)
text(x=seq(from=.9, to= 12, by = 1.29), y = -.01, labels = plotting.log_app_food$tree_sp, lheight = 1.5,
     srt = 65, adj = 1, xpd = TRUE, cex = 1.3)

#caterpillar appalachians
plotting.log_app_caterpillar$means_plus = plotting.log_app_food$means + .3
catlabs = as.vector(c(-.3, -.1, .1, .3, .5, .7)) #create alternate x axis
barplot(plotting.log_app_caterpillar$means_plus, las=2, main = "Caterpillars", ylim = c(0,1.1), labels = F,
        col = (ifelse(plotting.log_app_caterpillar$M == "a", "blue", 
                      ifelse(plotting.log_app_caterpillar$M == "b","royalblue1", 
                             ifelse(plotting.log_app_caterpillar$M == "bc", "deepskyblue", 
                                    ifelse(plotting.log_app_caterpillar$M == "c", "lightskyblue1",
                                           ifelse(plotting.log_app_caterpillar$M == "d", "white", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.6, cex = 1.5, plotting.log_app_caterpillar$M)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = -.01, labels = plotting.log_app_caterpillar$tree_sp, lheight = 1.5,
     srt = 65, adj = 1, xpd = TRUE, cex = 1.3)
mtext("Log-transformed Arth Density", side = 2, outer = TRUE, line = 1.5, cex = 2)
mtext("Tree Species", side = 1, outer = TRUE, line = .5, cex = 2)
text(y=seq(0,1, by = .2), x=-.3, labels = catlabs, pos = 2)
text(x = -.2, y = 0, labels = "-0.3", pos = 2)
text(x = 0, y = .2, labels = "-0.1", pos = 2)
text(x = 0, y = .4, labels = "0.1", pos = 2)
text(x = 0, y = .6, labels = "0.3", pos = 2)
text(x = 0, y = .8, labels = "0.5", pos = 2)
text(x = 0, y = 1, labels = "0.7", pos = 2)

par(mfrow = c(1, 2), mar=c(12,2.8,2,2), cex.lab = 1.2, cex.axis = 2, cex.main = 2)
#bird food triangle
barplot(plotting.log_tri_food$means, las=2, ylab = "Piedmont", ylim = c(0,.55), main = "Arthropods",
        col = (ifelse(plotting.log_tri_food$M == "a", "seagreen4", 
                      ifelse(plotting.log_tri_food$M == "ab", "seagreen3", 
                             ifelse(plotting.log_tri_food$M == "b","mediumspringgreen", 
                                    ifelse(plotting.log_tri_food$M == "bc", "lightgreen", 
                                           ifelse(plotting.log_tri_food$M == "c", "mintcream", NA)))))))

text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.5, plotting.log_tri_food$M, cex = 1.5)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = 0, labels = plotting.log_tri_food$tree_sp, lheight = 1.5,
     srt = 70, adj = 1, xpd = TRUE, cex = 1.5)

#caterpillar triangle
barplot(plotting.log_tri_caterpillar$means, las=2, mar =c(1,1,1,1), ylim = c(0,.55), main = "Caterpillars",
        col = (ifelse(plotting.log_tri_caterpillar$M == "a", "seagreen4", 
                      ifelse(plotting.log_tri_caterpillar$M == "ab", "seagreen3", 
                             ifelse(plotting.log_tri_caterpillar$M == "b","mediumspringgreen", 
                                    ifelse(plotting.log_tri_caterpillar$M == "bc", "lightgreen", 
                                           ifelse(plotting.log_tri_caterpillar$M == "c", "mintcream", NA)))))))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.5, plotting.log_tri_caterpillar$M, cex = 1.5)
mtext("Log-transformed Arth Density", side = 2, outer = TRUE, line = 1.5, cex = 2)
mtext("Tree Species", side = 1, outer = TRUE, line = .5, cex = 2)
#plotting.log_tri_caterpillar$tree_sp
text(x=seq(from=.9, to= 12, by = 1.29), y = 0, labels = plotting.log_tri_caterpillar$tree_sp, lheight = 1.5,
     srt = 70, adj = 1, xpd = TRUE, cex = 1.5)

