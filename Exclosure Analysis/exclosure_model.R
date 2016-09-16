#Mixed Effects Model for Arth Density
library(ggplot2)
library(lme4)

source("Exclosure Analysis/ExclosureAnalysis_Summer2016.R")
source("~/Desktop/insect-exclosure/data_analysis.R")

#Figure out which probability distribution fits data -None of the following are very good (all for normal distributions)
#food_time$Visit3.1 <- food_time$Visit3 + .1
#qqp(food_time$Visit3.1, "norm")
#qqp(food_time$Visit3.1, "lnorm")

#data shaping
food = food_time1
food$identifier = paste0(food$siteID, food$circle, food$survey)
food = merge(food, all_surveyTrees, by.x="identifier", by.y= "identifier")
food = select(food, -siteID.y, -circle.y, -survey.y)
names(food)= c("identifier", "TrapType", "siteID", "survey", "circle", "food_sum", "surveyTrees")

#data shaping
caterpillar = caterpillar_time1
caterpillar <-dplyr::filter(caterpillar, Visit3 < 6)
caterpillar$identifier = paste0(caterpillar$siteID, caterpillar$circle, caterpillar$survey)
caterpillar = merge(caterpillar, all_surveyTrees, by.x="identifier", by.y= "identifier")
caterpillar = select(caterpillar, -siteID.y, -circle.y, -survey.y)
names(caterpillar)= c("identifier", "TrapType", "siteID", "survey", "circle", "caterpillar_sum", "surveyTrees")

#mixed models for 2016 
mix_mod_food = lmer(food_sum ~ TrapType + (1 | surveyTrees), food)
mix_mod_caterpillar = lmer(caterpillar_sum ~ TrapType + (1 | surveyTrees), caterpillar)

#data shaping for 2012
food_new = food_time
food_new$identifier = paste0(food_new$StateRouteStop, food_new$Station, food_new$TrapType) 

#merge tree species data with density data
uniqTrees = unique(topdown9[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', "TreeSpecies")])
uniqTreesV3 = uniqTrees[uniqTrees$VisitNumber =="3",]
uniqTreesV3$identifier = paste0(uniqTreesV3$StateRouteStop, uniqTreesV3$Station, uniqTreesV3$TrapType) 
food_2012 = merge(food_new, uniqTreesV3, by.x="identifier", by.y="identifier", all.x=TRUE)
food_2012$TreeSpecies=as.factor(food_2012$TreeSpecies)
food_2012 = food_2012[,c("StateRouteStop.x", "Station.x", "TrapType.x", "Visit3", "TreeSpecies")]
names(food_2012) = c("StateRouteStop", "Station", "TrapType", "Visit3", "TreeSpecies")

#mixed models for 2012
mix_2012 = lmer(Visit3 ~ TrapType + (1 | TreeSpecies) + (1 | StateRouteStop), food_2012)
mix_2012_2 =




