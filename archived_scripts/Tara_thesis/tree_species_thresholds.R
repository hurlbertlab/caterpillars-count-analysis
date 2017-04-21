#-----------------------------------------------------------------------test for how often you see an arthropod density at least this extreme-----------------------
source("tree_species_model.R")

#create columns with Y/N data for certain arth density thresholds
count_merged3 = count_merged2
count_merged3$percent40 = ifelse(count_merged3$sum_count >= 40, "Yes", "No")
count_merged3$percent10 = ifelse(count_merged3$sum_count >= 10, "Yes", "No")
count_merged3$percent5 = ifelse(count_merged3$sum_count >= 5, "Yes", "No")
count_merged3$percent1 = ifelse(count_merged3$sum_count >= 1, "Yes", "No")

#get percentage of each tree species that meet the density criteria for each threshold 
percent40 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc40 = sum(percent40=="Yes")/length(percent40))
percent40 = data.frame(percent40)
percent40$not40 = 1-percent40$perc40

percent10 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc10=sum(percent10=="Yes")/length(percent10))
percent10 = data.frame(percent10)
percent10$not10 = 1-percent10$perc10

percent5 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc5 = sum(percent5=="Yes")/length(percent5))
percent5 = data.frame(percent5)
percent5$not5 = 1-percent5$perc5

percent1 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc1 = sum(percent1=="Yes")/length(percent1))
percent1 = data.frame(percent1)
percent1$not1 = 1-percent1$perc1

#create df with all threshold percentages for all species
percent40.10 = merge(percent40, percent10, by="plantSpecies")
percent40.10.5 = merge(percent40.10, percent5, by = "plantSpecies")
extremes_bysp = merge(percent40.10.5, percent1, by = "plantSpecies")

#merge threshold percentages with avg leaf areas per species
extremes_bysp1 = merge(extremes_bysp, leaves_sp1, by.x = "plantSpecies", by.y= "ComName") #decrease in trees here- why???? 29 out of 52 possible -> ones we don't have leaf areas for?
extremes_24 = dplyr::filter(extremes_bysp1, plantSpecies %in% common_24$trees) #missing two of these-> why?

#merge plant codes back in for plotting purposes
extremes_241=merge(extremes_24, plant_codes, by.x = "plantSpecies", by.y = "ComName", all.x = TRUE)
extremes_241 = dplyr::select(extremes_241, -TreeSciName, -Notes)

#plot relationship between likelihood of seeing an extreme arth dens and leaf area 
#(tree species above and below the curve tell us which species are good quality, whcih species are poorer quality)

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
lm.40 =lm(extremes_241$perc40 ~ extremes_241$avg_leaf_area_cm2)
p40=plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc40, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=40 Arth", cex.axis = .6)
abline(lm.40)

plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc10, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=10 Arth")
lm.10 =lm(extremes_241$perc10 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.10)

par(mfrow = c(1, 1), mar = c(5, 5, 1, 1))
plot5=plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc5, ylim=c(0,.6), xlab = "Mean Leaf Area (cm2)", ylab = "Proportion of Surveys >=5 Arth", col = "aquamarine", pch=16)
lm.5 =lm(extremes_241$perc5 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.5)
text(extremes_241$avg_leaf_area_cm2, extremes_241$perc5, labels = extremes_241$TreeCode, cex = .7)


plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc1, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=1 Arth")
lm.1 =lm(extremes_241$perc1 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.1)

#chi squared data reshaping
#table40 = table(count_merged3$percent40, count_merged3$plantSpecies)
#table10 = table(count_merged3$percent10, count_merged3$plantSpecies)
#table5 = table(count_merged3$percent5, count_merged3$plantSpecies)
#table1 = table(count_merged3$percent1, count_merged3$plantSpecies)
#chisq.test(table40)
#chisq.test(table10)
#chisq.test(table5)
#chisq.test(table1)

#so the above results tell me that the number of arthropods seen at each threshold is dependent on 
#tree species (is this true for all tree species)?