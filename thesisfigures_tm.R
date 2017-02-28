#Figures going into Thesis 
#Top-down experiment, Bottom-up experiment, and regional results

#source documents where data files are contained
source("data_analysis.R") # Southern Appalachian exclosures (top-down)
source("ExclosureAnalysis_Summer2016.R") #Triangle exclosures (top-down)
source("tree_species_model.R") #Tree species model (#Bottom up)
source("northern_comparisons.R") # Regional comparisons

#something about the histogram function makes it so I can't plot them in panels. need to change function to hist for all histograms
#Figure 1 - Differences of Differences histogram comparing effects of exclosures on 
#Bird Food Arthropods and caterpillars in the Southern Appalachians and in the triangle #still need to figure out how to do this in a panel/do I really want to show this?
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
histogram(food_time4_12$VFX_VF_dif, breaks=30, 
          xlab="Difference in Change in Arth Density between VFX and VF", 
          main="Bird Food Arthropods")
histogram(caterpillar_time4_12$VFX_VF_dif, breaks=10, ylab="Percent of Total",
          xlab="Difference in Change in Arth Density between VFX and VF",
          main="Caterpillars")
histogram(food_time4_t$VFX_VF_dif, breaks=10, ylab="Percent of Total",
          xlab="Difference in Change in Arth Density between VFX and VF",
          main="Bird Food Arthropods")
histogram(caterpillar_time4_t$VFX_VF_dif, breaks=10, ylab="Percent of Total",
          xlab="Difference in Change in Arth Density between VFX and VF",
          main="Caterpillars")

#Figure 2- Raw arth densities for bird food, caterpillars and not bird food for Triangle and Southern Appalachians
par(mfrow = c(2, 2), mar=c(6,4,4,4))
histogram(food_final$Visit3VF, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(food_final$Visit3VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(caterpillar_final$Visit3VF, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(caterpillar_final$Visit3VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
#app
histogram(food_time4_12$Visit3VF, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(food_time4_12$Visit3VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(caterpillar_time4_12$Visit3VF, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(caterpillar_time4_12$Visit3VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
#Figure 3 
#Plot HSD results
par(mfrow = c(2, 2), mar=c(7,4,3,3))
barplot(plotting.log_app_food$means, names.arg=plotting.log_app_food$tree_sp, las=2,  
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = blues9)
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_food$M)

#caterpillar appalachians
barplot(plotting.log_app_caterpillar$means, names.arg=plotting.log_app_caterpillar$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = blues9)
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_caterpillar$M)

#bird food triangle
barplot(plotting.log_tri_food$means, names.arg=plotting.log_tri_food$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = blues9)
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_food$M)

#caterpillar triangle
barplot(plotting.log_tri_caterpillar$means, names.arg=plotting.log_tri_caterpillar$tree_sp, las=2, 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = blues9)
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_caterpillar$M)
#if then statement for the colors?
