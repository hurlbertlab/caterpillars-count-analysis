# Comparing GDD, greenup, arthropod phenology, and avian reproductive timing

source('C:/git/caterpillars-count-analysis/prism_10year.R')

source('C:/git/caterpillars-count-analysis/modis_10year.R')

source('C:/git/caterpillars-count-analysis/extensive_plotting.R')

setwd('c:/git/caterpillars-count-analysis')

# Is greenup affected by GDD?
# Working with two datasets (one from prism_10year.R and one from modis_10year.R)

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)

# Calculate max cat for 2015 and 2016 (visual, beat sheet)
# Still unsure about what to use as peak to find JD
# Just largest number before a certain julian day like 180?
#greenupgdd$prlepl15 <- c(rep(0,8), PR.LEPL15[max(PR.LEPL15[PR.LEPL15$julianday %in% c(80:180),]$meanDensity),]$julianday)

# Plotting deviations
par(mar = c(4,4,2,2))
# GDD and greenup
plot(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, col = 'white',
     xlab = 'GDD JD deviation from mean', ylab = 'Greenup JD deviation from mean')
#abline(1,0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, greenupgdd$year, col = 'red')
text(greenupgdd$bg.gdd.dev, greenupgdd$bggreenup.dev, greenupgdd$year, col = 'blue')
# Caterpillars and greenup

# Caterpillars and GDD

# Birds and greenup

# Birds and GDD


# ANOVAs

prmod1 <- lm(prgreenup.log ~ pr.gdd, data = greenupgdd)
summary(prmod1) # significant
prmod2 <- lm(prgreenup.half ~ pr.gdd, data = greenupgdd)
summary(prmod2)

bgmod1 <- lm(bggreenup.log ~ bg.gdd, data = greenupgdd)
summary(bgmod1)
greenupgdd$temp.resids<-residuals(bgmod1)
new<-lm(temp.resids ~ bg.gdd, data = greenupgdd)
summary(new)

bgmod2 <- lm(bggreenup.half ~ bg.gdd, data = greenupgdd)
summary(bgmod2)

hbmod1 <- lm(hbgreenup.log ~ hb.gdd, data = greenupgdd)
summary(hbmod1)
hbmod2 <- lm(hbgreenup.half ~ hb.gdd, data = greenupgdd)
summary(hbmod2)

# None of the others significant
# [GDD not a good predictor of greenup??]

# Interesting plots (from meeting on 10/26/2016)
plot(greenupgdd$pr.gdd, greenupgdd$prgreenup.log)
text(greenupgdd$pr.gdd, greenupgdd$prgreenup.log, greenupgdd$year)
plot(greenupgdd$bg.gdd, greenupgdd$bggreenup.log)
text(greenupgdd$bg.gdd, greenupgdd$bggreenup.log, greenupgdd$year)
plot(greenupgdd$bg.gdd, greenupgdd$pr.gdd)
plot(greenupgdd$prgreenup.log, greenupgdd$bggreenup.log)



