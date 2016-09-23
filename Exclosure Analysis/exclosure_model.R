#Mixed Effects Model for Arth Density
library(ggplot2)
library(lme4)

source("Exclosure Analysis/ExclosureAnalysis_Summer2016.R")
source("~/Desktop/insect-exclosure/data_analysis.R")

#data shaping food 2016 data
food = food_time1
food$identifier = paste0(food$siteID, food$circle, food$survey)
food = merge(food, all_surveyTrees, by.x="identifier", by.y= "identifier")
food = select(food, -siteID.y, -circle.y, -survey.y)
names(food)= c("identifier", "TrapType", "siteID", "survey", "circle", "food_sum", "surveyTrees")

#data shaping caterpillar 2016 data
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
food_new = food_time_12
food_new$identifier = paste0(food_new$StateRouteStop, food_new$Station, food_new$TrapType) 

#merge tree species data with density data
uniqTrees = unique(topdown9[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', "TreeSpecies")])
uniqTreesV3 = uniqTrees[uniqTrees$VisitNumber =="3",]
uniqTreesV3$identifier = paste0(uniqTreesV3$StateRouteStop, uniqTreesV3$Station, uniqTreesV3$TrapType) 
food_2012 = merge(food_new, uniqTreesV3, by.x="identifier", by.y="identifier", all.x=TRUE)
food_2012$TreeSpecies=as.factor(food_2012$TreeSpecies)
food_2012 = food_2012[,c("StateRouteStop.x", "Station.x", "TrapType.x", "Visit3", "TreeSpecies", "identifier")]
names(food_2012) = c("StateRouteStop", "Station", "TrapType", "Visit3", "TreeSpecies", "identifier")

#merge unique observer data with density data
uniqObserver = unique(topdown9[, c('StateRouteStop', 'Station', 'TrapType', 'VisitNumber', "Observer")])
uniqObserverV3 = uniqObserver[uniqObserver$VisitNumber =="3",]
uniqObserverV3$identifier = paste0(uniqObserverV3$StateRouteStop, uniqObserverV3$Station, uniqObserverV3$TrapType)
food_2012_1 = merge(food_2012, uniqObserverV3, by.x= "identifier", by.y="identifier", all.x = T)
food_2012_2 = food_2012_1[,c("StateRouteStop.x", "Station.x", "TrapType.x", "Visit3", "TreeSpecies", "Observer")]
names(food_2012_2) = c("StateRouteStop", "Station", "TrapType", "Visit3", "TreeSpecies", "Observer")

#mixed models for 2012 #none of these are very good, just doesn't seem to be an effect
spec_2012 = lmer(Visit3 ~ TrapType + (1 | TreeSpecies), food_2012_2)
obvs_2012 = lmer(Visit3 ~ TrapType + (1 | Observer), food_2012_2)
srs_2012 = lmer(Visit3 ~ TrapType + (1 | StateRouteStop), food_2012_2)
all_2012 = lmer(Visit3 ~ TrapType + (1 | TreeSpecies) + (1 | Observer), food_2012_2)

#counts = food_2012_2 %>% group_by(StateRouteStop, Station) %>% tally() %>% filter(n != 2)




