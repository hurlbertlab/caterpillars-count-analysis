#Mixed Effects Model for Arth Density
library(ggplot2)
library(lme4)
source('Exclosure Analysis/ExclosureAnalysis_Summer2016.R')

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
food_2012 = food_time
surveyTrees_A = select(topdown3, StateRouteStop, Station, TrapType, TreeSpecies)


#mixed models for 2012




