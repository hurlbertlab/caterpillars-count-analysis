#Mixed Effects Model for Arth Density
library(car)
library(MASS)
library(ggplot2)
library(lme4)
source(ExclosureAnalysis_Summer2016.R)

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

#mixed model #why am I getting 0 values for the surveyTrees?
mix_mod = lmer(food_sum ~ TrapType + (1 | surveyTrees) + (1 | siteID), food)
mix_mod1 = lmer(food_sum ~ TrapType + (1 | surveyTrees), food)

#is there an effect of site
anova(mix_mod, mix_mod1)


#mix_gmod = glmer(food_sum ~ TrapType + (1 | surveyTrees), food)

