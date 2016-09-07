#Mixed Effects Model for Arth Density
library(car)
library(MASS)
library(ggplot2)
source(ExclosureAnalysis_Summer2016.R)
#Figure out which probability distribution fits data -None of the following are very good (all for normal distributions)
food_time$Visit3.1 <- food_time$Visit3 + .1
qqp(food_time$Visit3.1, "norm")
qqp(food_time$Visit3.1, "lnorm")
nbinom <- fitdistr(food_time$Visit3.1, "Negative Binomial")
qqp(food_time$Visit3.1, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(food_time$Visit3.1, "Poisson")
qqp(food_time$Visit3.1, "pois", poisson$estimate)
gamma <- fitdistr(food_time$Visit3.1, "gamma")
qqp(food_time$Visit3.1, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#data shaping
food <- food_time1
food$identifier <- paste0(food$siteID, food$circle, food$survey)
food = merge(food, all_surveyTrees, by.x="identifier", by.y= "identifier")
food = select(food, -siteID.y, -circle.y, -survey.y)
names(food)= c("identifier", "TrapType", "siteID", "survey", "circle", "food_sum", "surveyTrees")

lmer(arth_sum ~ TrapType, + ())

