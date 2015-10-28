# Script for estimating peaks based on varying surveying methods and varying
# number of surveys a month.
# Both quadratic and cubic fits.

# Run summary_functions.r, data_cleaning.R, general_plotting.R
source('summary_functions.r')
source('data_cleaning.R')





maxjdStat = function(alldata, arthropods, site, year) {
  
  samp.dataframe = data.frame(circle = integer(),
                              i = integer(),
                              freq= integer(), 
                              startval = integer(), 
                              max.jd = integer(), 
                              max.jd.quad = numeric(), 
                              R2.quad = numeric(),
                              p.quad = numeric(),
                              p.quad2 = numeric())

for (circleSample in 4:(length(unique(alldata$circle)))) {
  for (i in 1:10) {
  circlesToUse = sample(1:12, circleSample, replace = F)
  sampledata = subset(alldata, circle %in% circlesToUse)
    for (freq in 1:6) {
      for (startval in 1:freq) {
        uniqJDs = unique(sampledata$julianday)
        jdsToUse = uniqJDs[seq(startval, length(uniqJDs), freq)]
        subsampledata = subset(sampledata, julianday %in% jdsToUse)
        statdata = meanDensityByDay(subsampledata, ordersToInclude = arthropods, plotVar = 'meanDensity', inputYear = year, inputSite = site)
        
        # Max day without fit
        max.jd = statdata$julianday[statdata$meanDensity == max(statdata$meanDensity)]
        
        # For smooth fits:
        jd = c(min(statdata$julianday):max(statdata$julianday))
        
        # Run quadratic model
        if (length(statdata$meanDensity[statdata$meanDensity > 0]) >= 4) {
          quad = lm(meanDensity ~ julianday + I(julianday^2), data = statdata)
          R2.quad = summary(quad)$r.squared
          p.quad = summary(quad)$coefficients[2,4]
          p.quad2 = summary(quad)$coefficients[3,4]
          quad.predict = quad$coefficients[1] + quad$coefficients[2]*jd + quad$coefficients[3]*jd^2
          max.jd.quad = jd[quad.predict == max(quad.predict)]
        } else {
          R2.quad = NA
          p.quad = NA
          p.quad2 = NA
          max.jd.quad = NA
        }

        temp.dataframe = data.frame(circle = circleSample,
                                    i,
                                    freq, 
                                    startval, 
                                    max.jd, 
                                    max.jd.quad, 
                                    R2.quad,
                                    p.quad,
                                    p.quad2)
        
        samp.dataframe = rbind(samp.dataframe, temp.dataframe)
      }
    } # end freq loop
  } # end i loop
} # end circle loop
    
return(samp.dataframe)
} # end function

circleSample = 12
alldata = amsurvey.pr
freq = 1
startval = 1
arthropods = multorders
site = 117
minLength = 5
inputSite = 117
inputYear = 2015
year = 2015



# About the dataframe created:
# circle: number of survey circles used for calculations
# i: 
# freq: inverse frequency (a freq of 6 is 1 survey for every 6 done maximally)
# startval: integer denoting which julian day to start freq pattern
# max.jd: maximum julian data based on raw data
# max.jd.quad: 


multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')
PRam.max = maxjdStat(amsurvey.pr, multorders, 117, 2015)
write.table(PRam.max, file = "PR_am_survey_max_jd")

colfunc <- colorRampPalette(c('blue', 'green'))

plot(PRam.max[PRam.max$circle == 4,]$freq, PRam.max[PRam.max$circle == 4,]$max.jd.quad, col = colfunc(1), pch = 16)
points(PRam.max[PRam.max$circle == 5,]$freq, PRam.max[PRam.max$circle == 5,]$max.jd.quad, col = colfunc(2), pch = 16)
points(PRam.max[PRam.max$circle == 6,]$freq, PRam.max[PRam.max$circle == 6,]$max.jd.quad, col = colfunc(3),, pch = 16)
points(PRam.max[PRam.max$circle == 7,]$freq, PRam.max[PRam.max$circle == 7,]$max.jd.quad, col = colfunc(4), pch = 16)
points(PRam.max[PRam.max$circle == 8,]$freq, PRam.max[PRam.max$circle == 8,]$max.jd.quad, col = colfunc(5), pch = 16)
points(PRam.max[PRam.max$circle == 9,]$freq, PRam.max[PRam.max$circle == 9,]$max.jd.quad, col = colfunc(6), pch = 16)
points(PRam.max[PRam.max$circle == 10,]$freq, PRam.max[PRam.max$circle == 10,]$max.jd.quad, col = colfunc(7), pch = 16)
points(PRam.max[PRam.max$circle == 11,]$freq, PRam.max[PRam.max$circle == 11,]$max.jd.quad, col = colfunc(8), pch = 16)
points(PRam.max[PRam.max$circle == 12,]$freq, PRam.max[PRam.max$circle == 12,]$max.jd.quad, col = colfunc(9), pch = 16)



multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')
PRbs.max = maxjdStat(beatsheet.pr, multorders, 117, 2015)
colfunc <- colorRampPalette(c('blue', 'green'))

plot(PRbs.max[PRbs.max$circle == 4,]$freq, PRbs.max[PRbs.max$circle == 4,]$max.jd.quad, col = colfunc(1), pch = 16)
points(PRbs.max[PRbs.max$circle == 5,]$freq, PRbs.max[PRbs.max$circle == 5,]$max.jd.quad, col = colfunc(2), pch = 16)
points(PRbs.max[PRbs.max$circle == 6,]$freq, PRbs.max[PRbs.max$circle == 6,]$max.jd.quad, col = colfunc(3),, pch = 16)
points(PRbs.max[PRbs.max$circle == 7,]$freq, PRbs.max[PRbs.max$circle == 7,]$max.jd.quad, col = colfunc(4), pch = 16)
points(PRbs.max[PRbs.max$circle == 8,]$freq, PRbs.max[PRbs.max$circle == 8,]$max.jd.quad, col = colfunc(5), pch = 16)
points(PRbs.max[PRbs.max$circle == 9,]$freq, PRbs.max[PRbs.max$circle == 9,]$max.jd.quad, col = colfunc(6), pch = 16)
points(PRbs.max[PRbs.max$circle == 10,]$freq, PRbs.max[PRbs.max$circle == 10,]$max.jd.quad, col = colfunc(7), pch = 16)
points(PRbs.max[PRbs.max$circle == 11,]$freq, PRbs.max[PRbs.max$circle == 11,]$max.jd.quad, col = colfunc(8), pch = 16)
points(PRbs.max[PRbs.max$circle == 12,]$freq, PRbs.max[PRbs.max$circle == 12,]$max.jd.quad, col = colfunc(9), pch = 16)


par(mfrow = c(1, 1))

peakorders <- c('LEPL', 'ORTH', 'AUCH')
PRbs.max = maxjdStat(beatsheet.pr, peakorders, 117, 2015)
colfunc <- colorRampPalette(c('blue', 'green'))

plot(PRbs.max[PRbs.max$circle == 4,]$freq, PRbs.max[PRbs.max$circle == 4,]$max.jd.quad, col = colfunc(1), pch = 16)
points(PRbs.max[PRbs.max$circle == 5,]$freq, PRbs.max[PRbs.max$circle == 5,]$max.jd.quad, col = colfunc(2), pch = 16)
points(PRbs.max[PRbs.max$circle == 6,]$freq, PRbs.max[PRbs.max$circle == 6,]$max.jd.quad, col = colfunc(3),, pch = 16)
points(PRbs.max[PRbs.max$circle == 7,]$freq, PRbs.max[PRbs.max$circle == 7,]$max.jd.quad, col = colfunc(4), pch = 16)
points(PRbs.max[PRbs.max$circle == 8,]$freq, PRbs.max[PRbs.max$circle == 8,]$max.jd.quad, col = colfunc(5), pch = 16)
points(PRbs.max[PRbs.max$circle == 9,]$freq, PRbs.max[PRbs.max$circle == 9,]$max.jd.quad, col = colfunc(6), pch = 16)
points(PRbs.max[PRbs.max$circle == 10,]$freq, PRbs.max[PRbs.max$circle == 10,]$max.jd.quad, col = colfunc(7), pch = 16)
points(PRbs.max[PRbs.max$circle == 11,]$freq, PRbs.max[PRbs.max$circle == 11,]$max.jd.quad, col = colfunc(8), pch = 16)
points(PRbs.max[PRbs.max$circle == 12,]$freq, PRbs.max[PRbs.max$circle == 12,]$max.jd.quad, col = colfunc(9), pch = 16)




# 'numCircles', 'aveMax', 'stanDev', 'aveMaxQuad', 'stanDevQuad'

aveMaxPerCircleNum = data.frame(numCircles = integer(), aveMax = numeric(), ) #this function doesn't work

for (i in 4:(max(PRam.max$circle))) {

  aveMax = mean(PRam.max[PRam.max$circle == i,]$max.jd, na.rm = T)
  stanDev = sd(PRam.max[PRam.max$circle == i,]$max.jd, na.rm = T)
  aveMaxQuad = mean(PRam.max[PRam.max$circle == i,]$max.jd.quad, na.rm = T)
  stanDevQuad = sd(PRam.max[PRam.max$circle == i,]$max.jd.quad, na.rm = T)

  temp = c(numCircles = i, aveMax = aveMax, stanDev = stanDev, aveMaxQuad = aveMaxQuad, stanDevQuad = stanDevQuad)

  aveMaxPerCircleNum = rbind(aveMaxPerCircleNum, temp)
}

aveMaxPerCircleNum = data.frame(aveMaxPerCircleNum$num)


#at meeting
mean.maxjd.quad = aggregate(PRam.max$max.jd.quad, by = list(PRam.max$circle, PRam.max$freq), function(x) mean(x, na.rm = T))
mean.maxjds = aggregate(PRam.max$max.jd, by = list(PRam.max$circle, PRam.max$freq), function(x) mean(x, na.rm = T))
plot(mean.maxjd.quad$Group.2, mean.maxjd.quad$x, col = mean.maxjd.quad$Group.1, pch = 16)

statdata = meanDensityByDay(subsampledata, ordersToInclude = arthropods, plotVar = 'meanDensity', inputYear = year, inputSite = site)
plot(statdata$julianday, statdata$meanDensity)
statdata = meanDensityByDay(subsampledata, ordersToInclude = 'LEPL', plotVar = 'meanDensity', inputYear = year, inputSite = site)
plot(statdata$julianday, statdata$meanDensity)
spline(statdata$julianday, statdata$meanDensity)
points(spline(statdata$julianday, statdata$meanDensity, n = 3), type = 'l', col = 'red')
points(spline(statdata$julianday, statdata$meanDensity, n = 5), type = 'l', col = 'red')
points(spline(statdata$julianday, statdata$meanDensity, n = 8), type = 'l', col = 'red')
points(spline(statdata$julianday, statdata$meanDensity, n = 12), type = 'l', col = 'red')
points(spline(statdata$julianday, statdata$meanDensity, n = 34), type = 'l', col = 'blue')

#------------------------------------------------------------------------------------------------------------
### Quadratic Fit

## Caterpillars only

## Prairie Ridge- run lepl PR plot in general_plotting.R to adds points

#Prairie Ridge, 8 surveys a month
PRlepl.am.quad = lm(density_am ~ julianday + I(julianday^2), data = PRall.lepl)
PRlepl.am.quad.predict = PRlepl.am.quad$coefficients[1] + PRlepl.am.quad$coefficients[2]*jd + PRlepl.am.quad$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.predict, type = 'l', col = 'blue', lwd = 5)
PRlepl.am.quad.mday = jd[PRlepl.am.quad.predict == max(PRlepl.am.quad.predict)]

PRlepl.bs.quad = lm(density_bs ~ julianday + I(julianday^2), data = PRall.lepl)
PRlepl.bs.quad.predict = PRlepl.bs.quad$coefficients[1] + PRlepl.bs.quad$coefficients[2]*jd + PRlepl.bs.quad$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.predict, type = 'l', col = 'orange', lwd = 5)
PRlepl.bs.quad.mday = jd[PRlepl.bs.quad.predict == max(PRlepl.bs.quad.predict)]

PRlepl.pm.quad = lm(density_pm ~ julianday + I(julianday^2), data = PRall.lepl)
PRlepl.pm.quad.predict = PRlepl.pm.quad$coefficients[1] + PRlepl.pm.quad$coefficients[2]*jd + PRlepl.pm.quad$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.predict, type = 'l', col = 'red', lwd = 5)
PRlepl.pm.quad.mday = jd[PRlepl.pm.quad.predict == max(PRlepl.pm.quad.predict)]

PRlepl.vol.quad = lm(density_vol ~ julianday + I(julianday^2), data = PRall.lepl)
PRlepl.vol.quad.predict = PRlepl.vol.quad$coefficients[1] + PRlepl.vol.quad$coefficients[2]*jd + PRlepl.vol.quad$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.predict, type = 'l', col = 'green', lwd = 5)
PRlepl.vol.quad.mday = jd[PRlepl.vol.quad.predict == max(PRlepl.vol.quad.predict)]

#Prairie Ridge, 4 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 2)]
PRall.lepl.4 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.quad.4 = lm(density_am ~ julianday + I(julianday^2), data = PRall.lepl.4)
PRlepl.am.quad.4.predict = PRlepl.am.quad.4$coefficients[1] + PRlepl.am.quad.4$coefficients[2]*jd + PRlepl.am.quad.4$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.4.predict, type = 'l', col = 'blue', lwd = 3)
PRlepl.am.quad.4.mday = jd[PRlepl.am.quad.4.predict == max(PRlepl.am.quad.4.predict)]

PRlepl.bs.quad.4 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.lepl.4)
PRlepl.bs.quad.4.predict = PRlepl.bs.quad.4$coefficients[1] + PRlepl.bs.quad.4$coefficients[2]*jd + PRlepl.bs.quad.4$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.4.predict, type = 'l', col = 'orange', lwd = 3)
PRlepl.bs.quad.4.mday = jd[PRlepl.bs.quad.4.predict == max(PRlepl.bs.quad.4.predict)]

PRlepl.pm.quad.4 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.lepl.4)
PRlepl.pm.quad.4.predict = PRlepl.pm.quad.4$coefficients[1] + PRlepl.pm.quad.4$coefficients[2]*jd + PRlepl.pm.quad.4$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.4.predict, type = 'l', col = 'red', lwd = 3)
PRlepl.pm.quad.4.mday = jd[PRlepl.pm.quad.4.predict == max(PRlepl.pm.quad.4.predict)]

PRlepl.vol.quad.4 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.lepl.4)
PRlepl.vol.quad.4.predict = PRlepl.vol.quad.4$coefficients[1] + PRlepl.vol.quad.4$coefficients[2]*jd + PRlepl.vol.quad.4$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.4.predict, type = 'l', col = 'green', lwd = 3)
PRlepl.vol.quad.4.mday = jd[PRlepl.vol.quad.4.predict == max(PRlepl.vol.quad.4.predict)]

#Prairie Ridge, 2 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 4)]
PRall.lepl.2 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.quad.2 = lm(density_am ~ julianday + I(julianday^2), data = PRall.lepl.2)
PRlepl.am.quad.2.predict = PRlepl.am.quad.2$coefficients[1] + PRlepl.am.quad.2$coefficients[2]*jd + PRlepl.am.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.2.predict, type = 'l', col = 'blue', lwd = 1)
PRlepl.am.quad.2.mday = jd[PRlepl.am.quad.2.predict == max(PRlepl.am.quad.2.predict)]

PRlepl.bs.quad.2 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.lepl.2)
PRlepl.bs.quad.2.predict = PRlepl.bs.quad.2$coefficients[1] + PRlepl.bs.quad.2$coefficients[2]*jd + PRlepl.bs.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.2.predict, type = 'l', col = 'orange', lwd = 1)
PRlepl.bs.quad.2.mday = jd[PRlepl.bs.quad.2.predict == max(PRlepl.bs.quad.2.predict)]

PRlepl.pm.quad.2 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.lepl.2)
PRlepl.pm.quad.2.predict = PRlepl.pm.quad.2$coefficients[1] + PRlepl.pm.quad.2$coefficients[2]*jd + PRlepl.pm.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.2.predict, type = 'l', col = 'red', lwd = 1)
PRlepl.pm.quad.2.mday = jd[PRlepl.pm.quad.2.predict == max(PRlepl.pm.quad.2.predict)]

PRlepl.vol.quad.2 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.lepl.2)
PRlepl.vol.quad.2.predict = PRlepl.vol.quad.2$coefficients[1] + PRlepl.vol.quad.2$coefficients[2]*jd + PRlepl.vol.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.2.predict, type = 'l', col = 'green', lwd = 1)
PRlepl.vol.quad.2.mday = jd[PRlepl.vol.quad.2.predict == max(PRlepl.vol.quad.2.predict)]

#Prairie Ridge, 1 survey a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 8)]
PRall.lepl.1 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.quad.1 = lm(density_am ~ julianday + I(julianday^2), data = PRall.lepl.1)
PRlepl.am.quad.1.predict = PRlepl.am.quad.1$coefficients[1] + PRlepl.am.quad.1$coefficients[2]*jd + PRlepl.am.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.1.predict, type = 'l', col = 'blue', lty = "dotted")
PRlepl.am.quad.1.mday = jd[PRlepl.am.quad.1.predict == max(PRlepl.am.quad.1.predict)]

PRlepl.bs.quad.1 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.lepl.1)
PRlepl.bs.quad.1.predict = PRlepl.bs.quad.1$coefficients[1] + PRlepl.bs.quad.1$coefficients[2]*jd + PRlepl.bs.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.1.predict, type = 'l', col = 'orange', lty = "dotted")
PRlepl.bs.quad.1.mday = jd[PRlepl.bs.quad.1.predict == max(PRlepl.bs.quad.1.predict)]

PRlepl.pm.quad.1 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.lepl.1)
PRlepl.pm.quad.1.predict = PRlepl.pm.quad.1$coefficients[1] + PRlepl.pm.quad.1$coefficients[2]*jd + PRlepl.pm.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.1.predict, type = 'l', col = 'red', lty = "dotted")
PRlepl.pm.quad.1.mday = jd[PRlepl.pm.quad.1.predict == max(PRlepl.pm.quad.1.predict)]

PRlepl.vol.quad.1 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.lepl.1)
PRlepl.vol.quad.1.predict = PRlepl.vol.quad.1$coefficients[1] + PRlepl.vol.quad.1$coefficients[2]*jd + PRlepl.vol.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.1.predict, type = 'l', col = 'green', lty = "dotted")
PRlepl.vol.quad.1.mday = jd[PRlepl.vol.quad.1.predict == max(PRlepl.vol.quad.1.predict)]

title('PR lepl quad fits')

## Botanical Garden- run lepl BG plot in general_plotting.R to adds points

#Botanical Garden, 8 surveys a month
BGlepl.am.quad = lm(density_am ~ julianday + I(julianday^2), data = BGall.lepl)
BGlepl.am.quad.predict = BGlepl.am.quad$coefficients[1] + BGlepl.am.quad$coefficients[2]*jd + BGlepl.am.quad$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.predict, type = 'l', col = 'blue')
BGlepl.am.quad.mday = jd[BGlepl.am.quad.predict == max(BGlepl.am.quad.predict)]

BGlepl.bs.quad = lm(density_bs ~ julianday + I(julianday^2), data = BGall.lepl)
BGlepl.bs.quad.predict = BGlepl.bs.quad$coefficients[1] + BGlepl.bs.quad$coefficients[2]*jd + BGlepl.bs.quad$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.mday = jd[BGlepl.bs.quad.predict == max(BGlepl.bs.quad.predict)]

#Botanical Garden, 4 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 2)]
BGall.lepl.4 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.quad.4 = lm(density_am ~ julianday + I(julianday^2), data = BGall.lepl.4)
BGlepl.am.quad.4.predict = BGlepl.am.quad.4$coefficients[1] + BGlepl.am.quad.4$coefficients[2]*jd + BGlepl.am.quad.4$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.4.predict, type = 'l', col = 'blue')
BGlepl.am.quad.4.mday = jd[BGlepl.am.quad.4.predict == max(BGlepl.am.quad.4.predict)]

BGlepl.bs.quad.4 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.lepl.4)
BGlepl.bs.quad.4.predict = BGlepl.bs.quad.4$coefficients[1] + BGlepl.bs.quad.4$coefficients[2]*jd + BGlepl.bs.quad.4$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.4.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.4.mday = jd[BGlepl.bs.quad.4.predict == max(BGlepl.bs.quad.4.predict)]

#Botanical Garden, 2 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 4)]
BGall.lepl.2 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.quad.2 = lm(density_am ~ julianday + I(julianday^2), data = BGall.lepl.2)
BGlepl.am.quad.2.predict = BGlepl.am.quad.2$coefficients[1] + BGlepl.am.quad.2$coefficients[2]*jd + BGlepl.am.quad.2$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.2.predict, type = 'l', col = 'blue')
BGlepl.am.quad.2.mday = jd[BGlepl.am.quad.2.predict == max(BGlepl.am.quad.2.predict)]

BGlepl.bs.quad.2 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.lepl.2)
BGlepl.bs.quad.2.predict = BGlepl.bs.quad.2$coefficients[1] + BGlepl.bs.quad.2$coefficients[2]*jd + BGlepl.bs.quad.2$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.2.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.2.mday = jd[BGlepl.bs.quad.2.predict == max(BGlepl.bs.quad.2.predict)]

#Botanical Garden, 1 survey a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 8)]
BGall.lepl.1 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.quad.1 = lm(density_am ~ julianday + I(julianday^2), data = BGall.lepl.1)
BGlepl.am.quad.1.predict = BGlepl.am.quad.1$coefficients[1] + BGlepl.am.quad.1$coefficients[2]*jd + BGlepl.am.quad.1$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.1.predict, type = 'l', col = 'blue')
BGlepl.am.quad.1.mday = jd[BGlepl.am.quad.1.predict == max(BGlepl.am.quad.1.predict)]

BGlepl.bs.quad.1 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.lepl.1)
BGlepl.bs.quad.1.predict = BGlepl.bs.quad.1$coefficients[1] + BGlepl.bs.quad.1$coefficients[2]*jd + BGlepl.bs.quad.1$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.1.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.1.mday = jd[BGlepl.bs.quad.1.predict == max(BGlepl.bs.quad.1.predict)]

title('BG lepl quad fits')

## Plotting max days in relationship to frequency of surveys

# PR, caterpillars only
PRmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.am.quad.mday, PRlepl.am.quad.4.mday, PRlepl.am.quad.2.mday, PRlepl.am.quad.1.mday))
PRmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.bs.quad.mday, PRlepl.bs.quad.4.mday, PRlepl.bs.quad.2.mday, PRlepl.bs.quad.1.mday))
PRmaxdaypm <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.pm.quad.mday, PRlepl.pm.quad.4.mday, PRlepl.pm.quad.2.mday, PRlepl.pm.quad.1.mday))
PRmaxdayvol <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.vol.quad.mday, PRlepl.vol.quad.4.mday, PRlepl.vol.quad.2.mday, PRlepl.vol.quad.1.mday))
plot(PRmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(PRmaxdaybs, col = 'orange', pch = 19)
points(PRmaxdaypm, col = 'red', pch = 19)
points(PRmaxdayvol, col = 'green', pch = 19)
title('PR lepl quad fit max day')

# BG, caterpillars only
BGmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(BGlepl.am.quad.mday, BGlepl.am.quad.4.mday, BGlepl.am.quad.2.mday, BGlepl.am.quad.1.mday))
BGmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(BGlepl.bs.quad.mday, BGlepl.bs.quad.4.mday, BGlepl.bs.quad.2.mday, BGlepl.bs.quad.1.mday))
plot(BGmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(BGmaxdaybs, col = 'orange', pch = 19)
title('BG lepl quad fit max day')



## Selected arthropods

## Prairie Ridge- run mult PR plot in general_plotting.R to adds points

#Prairie Ridge, 8 surveys a month
PRmult.am.quad = lm(density_am ~ julianday + I(julianday^2), data = PRall.mult)
PRmult.am.quad.predict = PRmult.am.quad$coefficients[1] + PRmult.am.quad$coefficients[2]*jd + PRmult.am.quad$coefficients[3]*jd^2
points(jd, PRmult.am.quad.predict, type = 'l', col = 'blue')
PRmult.am.quad.mday = jd[PRmult.am.quad.predict == max(PRmult.am.quad.predict)]

PRmult.bs.quad = lm(density_bs ~ julianday + I(julianday^2), data = PRall.mult)
PRmult.bs.quad.predict = PRmult.bs.quad$coefficients[1] + PRmult.bs.quad$coefficients[2]*jd + PRmult.bs.quad$coefficients[3]*jd^2
points(jd, PRmult.bs.quad.predict, type = 'l', col = 'orange')
PRmult.bs.quad.mday = jd[PRmult.bs.quad.predict == max(PRmult.bs.quad.predict)]

PRmult.pm.quad = lm(density_pm ~ julianday + I(julianday^2), data = PRall.mult)
PRmult.pm.quad.predict = PRmult.pm.quad$coefficients[1] + PRmult.pm.quad$coefficients[2]*jd + PRmult.pm.quad$coefficients[3]*jd^2
points(jd, PRmult.pm.quad.predict, type = 'l', col = 'red')
PRmult.pm.quad.mday = jd[PRmult.pm.quad.predict == max(PRmult.pm.quad.predict)]

PRmult.vol.quad = lm(density_vol ~ julianday + I(julianday^2), data = PRall.mult)
PRmult.vol.quad.predict = PRmult.vol.quad$coefficients[1] + PRmult.vol.quad$coefficients[2]*jd + PRmult.vol.quad$coefficients[3]*jd^2
points(jd, PRmult.vol.quad.predict, type = 'l', col = 'green')
PRmult.vol.quad.mday = jd[PRmult.vol.quad.predict == max(PRmult.vol.quad.predict)]

#Prairie Ridge, 4 surveys a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 2)]
PRall.mult.4 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.quad.4 = lm(density_am ~ julianday + I(julianday^2), data = PRall.mult.4)
PRmult.am.quad.4.predict = PRmult.am.quad.4$coefficients[1] + PRmult.am.quad.4$coefficients[2]*jd + PRmult.am.quad.4$coefficients[3]*jd^2
points(jd, PRmult.am.quad.4.predict, type = 'l', col = 'blue')
PRmult.am.quad.4.mday = jd[PRmult.am.quad.4.predict == max(PRmult.am.quad.4.predict)]

PRmult.bs.quad.4 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.mult.4)
PRmult.bs.quad.4.predict = PRmult.bs.quad.4$coefficients[1] + PRmult.bs.quad.4$coefficients[2]*jd + PRmult.bs.quad.4$coefficients[3]*jd^2
points(jd, PRmult.bs.quad.4.predict, type = 'l', col = 'orange')
PRmult.bs.quad.4.mday = jd[PRmult.bs.quad.4.predict == max(PRmult.bs.quad.4.predict)]

PRmult.pm.quad.4 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.mult.4)
PRmult.pm.quad.4.predict = PRmult.pm.quad.4$coefficients[1] + PRmult.pm.quad.4$coefficients[2]*jd + PRmult.pm.quad.4$coefficients[3]*jd^2
points(jd, PRmult.pm.quad.4.predict, type = 'l', col = 'red')
PRmult.pm.quad.4.mday = jd[PRmult.pm.quad.4.predict == max(PRmult.pm.quad.4.predict)]

PRmult.vol.quad.4 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.mult.4)
PRmult.vol.quad.4.predict = PRmult.vol.quad.4$coefficients[1] + PRmult.vol.quad.4$coefficients[2]*jd + PRmult.vol.quad.4$coefficients[3]*jd^2
points(jd, PRmult.vol.quad.4.predict, type = 'l', col = 'green')
PRmult.vol.quad.4.mday = jd[PRmult.vol.quad.4.predict == max(PRmult.vol.quad.4.predict)]

#Prairie Ridge, 2 surveys a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 4)]
PRall.mult.2 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.quad.2 = lm(density_am ~ julianday + I(julianday^2), data = PRall.mult.2)
PRmult.am.quad.2.predict = PRmult.am.quad.2$coefficients[1] + PRmult.am.quad.2$coefficients[2]*jd + PRmult.am.quad.2$coefficients[3]*jd^2
points(jd, PRmult.am.quad.2.predict, type = 'l', col = 'blue')
PRmult.am.quad.2.mday = jd[PRmult.am.quad.2.predict == max(PRmult.am.quad.2.predict)]

PRmult.bs.quad.2 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.mult.2)
PRmult.bs.quad.2.predict = PRmult.bs.quad.2$coefficients[1] + PRmult.bs.quad.2$coefficients[2]*jd + PRmult.bs.quad.2$coefficients[3]*jd^2
points(jd, PRmult.bs.quad.2.predict, type = 'l', col = 'orange')
PRmult.bs.quad.2.mday = jd[PRmult.bs.quad.2.predict == max(PRmult.bs.quad.2.predict)]

PRmult.pm.quad.2 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.mult.2)
PRmult.pm.quad.2.predict = PRmult.pm.quad.2$coefficients[1] + PRmult.pm.quad.2$coefficients[2]*jd + PRmult.pm.quad.2$coefficients[3]*jd^2
points(jd, PRmult.pm.quad.2.predict, type = 'l', col = 'red')
PRmult.pm.quad.2.mday = jd[PRmult.pm.quad.2.predict == max(PRmult.pm.quad.2.predict)]

PRmult.vol.quad.2 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.mult.2)
PRmult.vol.quad.2.predict = PRmult.vol.quad.2$coefficients[1] + PRmult.vol.quad.2$coefficients[2]*jd + PRmult.vol.quad.2$coefficients[3]*jd^2
points(jd, PRmult.vol.quad.2.predict, type = 'l', col = 'green')
PRmult.vol.quad.2.mday = jd[PRmult.vol.quad.2.predict == max(PRmult.vol.quad.2.predict)]

#Prairie Ridge, 1 survey a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 8)]
PRall.mult.1 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.quad.1 = lm(density_am ~ julianday + I(julianday^2), data = PRall.mult.1)
PRmult.am.quad.1.predict = PRmult.am.quad.1$coefficients[1] + PRmult.am.quad.1$coefficients[2]*jd + PRmult.am.quad.1$coefficients[3]*jd^2
points(jd, PRmult.am.quad.1.predict, type = 'l', col = 'blue')
PRmult.am.quad.1.mday = jd[PRmult.am.quad.1.predict == max(PRmult.am.quad.1.predict)]

PRmult.bs.quad.1 = lm(density_bs ~ julianday + I(julianday^2), data = PRall.mult.1)
PRmult.bs.quad.1.predict = PRmult.bs.quad.1$coefficients[1] + PRmult.bs.quad.1$coefficients[2]*jd + PRmult.bs.quad.1$coefficients[3]*jd^2
points(jd, PRmult.bs.quad.1.predict, type = 'l', col = 'orange')
PRmult.bs.quad.1.mday = jd[PRmult.bs.quad.1.predict == max(PRmult.bs.quad.1.predict)]

PRmult.pm.quad.1 = lm(density_pm ~ julianday + I(julianday^2), data = PRall.mult.1)
PRmult.pm.quad.1.predict = PRmult.pm.quad.1$coefficients[1] + PRmult.pm.quad.1$coefficients[2]*jd + PRmult.pm.quad.1$coefficients[3]*jd^2
points(jd, PRmult.pm.quad.1.predict, type = 'l', col = 'red')
PRmult.pm.quad.1.mday = jd[PRmult.pm.quad.1.predict == max(PRmult.pm.quad.1.predict)]

PRmult.vol.quad.1 = lm(density_vol ~ julianday + I(julianday^2), data = PRall.mult.1)
PRmult.vol.quad.1.predict = PRmult.vol.quad.1$coefficients[1] + PRmult.vol.quad.1$coefficients[2]*jd + PRmult.vol.quad.1$coefficients[3]*jd^2
points(jd, PRmult.vol.quad.1.predict, type = 'l', col = 'green')
PRmult.vol.quad.1.mday = jd[PRmult.vol.quad.1.predict == max(PRmult.vol.quad.1.predict)]

title('PR mult quad fits')


## Botanical Garden- run mult BG plot in general_plotting.R to adds points

#Botanical Garden, 8 surveys a month
BGmult.am.quad = lm(density_am ~ julianday + I(julianday^2), data = BGall.mult)
BGmult.am.quad.predict = BGmult.am.quad$coefficients[1] + BGmult.am.quad$coefficients[2]*jd + BGmult.am.quad$coefficients[3]*jd^2
points(jd, BGmult.am.quad.predict, type = 'l', col = 'blue')
BGmult.am.quad.mday = jd[BGmult.am.quad.predict == max(BGmult.am.quad.predict)]

BGmult.bs.quad = lm(density_bs ~ julianday + I(julianday^2), data = BGall.mult)
BGmult.bs.quad.predict = BGmult.bs.quad$coefficients[1] + BGmult.bs.quad$coefficients[2]*jd + BGmult.bs.quad$coefficients[3]*jd^2
points(jd, BGmult.bs.quad.predict, type = 'l', col = 'orange')
BGmult.bs.quad.mday = jd[BGmult.bs.quad.predict == max(BGmult.bs.quad.predict)]

#Botanical Garden, 4 surveys a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 2)]
BGall.mult.4 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.quad.4 = lm(density_am ~ julianday + I(julianday^2), data = BGall.mult.4)
BGmult.am.quad.4.predict = BGmult.am.quad.4$coefficients[1] + BGmult.am.quad.4$coefficients[2]*jd + BGmult.am.quad.4$coefficients[3]*jd^2
points(jd, BGmult.am.quad.4.predict, type = 'l', col = 'blue')
BGmult.am.quad.4.mday = jd[BGmult.am.quad.4.predict == max(BGmult.am.quad.4.predict)]

BGmult.bs.quad.4 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.mult.4)
BGmult.bs.quad.4.predict = BGmult.bs.quad.4$coefficients[1] + BGmult.bs.quad.4$coefficients[2]*jd + BGmult.bs.quad.4$coefficients[3]*jd^2
points(jd, BGmult.bs.quad.4.predict, type = 'l', col = 'orange')
BGmult.bs.quad.4.mday = jd[BGmult.bs.quad.4.predict == max(BGmult.bs.quad.4.predict)]

#Botanical Garden, 2 surveys a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 4)]
BGall.mult.2 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.quad.2 = lm(density_am ~ julianday + I(julianday^2), data = BGall.mult.2)
BGmult.am.quad.2.predict = BGmult.am.quad.2$coefficients[1] + BGmult.am.quad.2$coefficients[2]*jd + BGmult.am.quad.2$coefficients[3]*jd^2
points(jd, BGmult.am.quad.2.predict, type = 'l', col = 'blue')
BGmult.am.quad.2.mday = jd[BGmult.am.quad.2.predict == max(BGmult.am.quad.2.predict)]

BGmult.bs.quad.2 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.mult.2)
BGmult.bs.quad.2.predict = BGmult.bs.quad.2$coefficients[1] + BGmult.bs.quad.2$coefficients[2]*jd + BGmult.bs.quad.2$coefficients[3]*jd^2
points(jd, BGmult.bs.quad.2.predict, type = 'l', col = 'orange')
BGmult.bs.quad.2.mday = jd[BGmult.bs.quad.2.predict == max(BGmult.bs.quad.2.predict)]

#Botanical Garden, 1 survey a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 8)]
BGall.mult.1 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.quad.1 = lm(density_am ~ julianday + I(julianday^2), data = BGall.mult.1)
BGmult.am.quad.1.predict = BGmult.am.quad.1$coefficients[1] + BGmult.am.quad.1$coefficients[2]*jd + BGmult.am.quad.1$coefficients[3]*jd^2
points(jd, BGmult.am.quad.1.predict, type = 'l', col = 'blue')
BGmult.am.quad.1.mday = jd[BGmult.am.quad.1.predict == max(BGmult.am.quad.1.predict)]

BGmult.bs.quad.1 = lm(density_bs ~ julianday + I(julianday^2), data = BGall.mult.1)
BGmult.bs.quad.1.predict = BGmult.bs.quad.1$coefficients[1] + BGmult.bs.quad.1$coefficients[2]*jd + BGmult.bs.quad.1$coefficients[3]*jd^2
points(jd, BGmult.bs.quad.1.predict, type = 'l', col = 'orange')
BGmult.bs.quad.1.mday = jd[BGmult.bs.quad.1.predict == max(BGmult.bs.quad.1.predict)]

title('BG mult quad fits')

## Plotting max days in relationship to frequency of surveys

# PR, selected arthropods
PRmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.am.quad.mday, PRmult.am.quad.4.mday, PRmult.am.quad.2.mday, PRmult.am.quad.1.mday))
PRmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.bs.quad.mday, PRmult.bs.quad.4.mday, PRmult.bs.quad.2.mday, PRmult.bs.quad.1.mday))
PRmaxdaypm <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.pm.quad.mday, PRmult.pm.quad.4.mday, PRmult.pm.quad.2.mday, PRmult.pm.quad.1.mday))
PRmaxdayvol <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.vol.quad.mday, PRmult.vol.quad.4.mday, PRmult.vol.quad.2.mday, PRmult.vol.quad.1.mday))
plot(PRmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(PRmaxdaybs, col = 'orange', pch = 19)
points(PRmaxdaypm, col = 'red', pch = 19)
points(PRmaxdayvol, col = 'green', pch = 19)
title('PR mult quad fit max day')

# BG, selected arthropods
BGmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(BGmult.am.quad.mday, BGmult.am.quad.4.mday, BGmult.am.quad.2.mday, BGmult.am.quad.1.mday))
BGmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(BGmult.bs.quad.mday, BGmult.bs.quad.4.mday, BGmult.bs.quad.2.mday, BGmult.bs.quad.1.mday))
plot(BGmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(BGmaxdaybs, col = 'orange', pch = 19)
title('PR mult quad fit max day')





#------------------------------------------------------------------------------------------------------------
### Cubic Fit

jd = c(130:210)

## Caterpillars only

## Prairie Ridge- run lepl PR plot in general_plotting.R to adds points

#Prairie Ridge, 8 surveys a month
PRlepl.am.cub = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl)
PRlepl.am.cub.predict = PRlepl.am.cub$coefficients[1] + PRlepl.am.cub$coefficients[2]*jd + PRlepl.am.cub$coefficients[3]*jd^2 + PRlepl.am.cub$coefficients[4]*jd^3
points(jd, PRlepl.am.cub.predict, type = 'l', col = 'blue')
PRlepl.am.cub.mday = jd[PRlepl.am.cub.predict == max(PRlepl.am.cub.predict)]

PRlepl.bs.cub = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl)
PRlepl.bs.cub.predict = PRlepl.bs.cub$coefficients[1] + PRlepl.bs.cub$coefficients[2]*jd + PRlepl.bs.cub$coefficients[3]*jd^2 + PRlepl.bs.cub$coefficients[4]*jd^3
points(jd, PRlepl.bs.cub.predict, type = 'l', col = 'orange')
PRlepl.bs.cub.mday = jd[PRlepl.bs.cub.predict == max(PRlepl.bs.cub.predict)]

PRlepl.pm.cub = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl)
PRlepl.pm.cub.predict = PRlepl.pm.cub$coefficients[1] + PRlepl.pm.cub$coefficients[2]*jd + PRlepl.pm.cub$coefficients[3]*jd^2 + PRlepl.pm.cub$coefficients[4]*jd^3
points(jd, PRlepl.pm.cub.predict, type = 'l', col = 'red')
PRlepl.pm.cub.mday = jd[PRlepl.pm.cub.predict == max(PRlepl.pm.cub.predict)]

PRlepl.vol.cub = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl)
PRlepl.vol.cub.predict = PRlepl.vol.cub$coefficients[1] + PRlepl.vol.cub$coefficients[2]*jd + PRlepl.vol.cub$coefficients[3]*jd^2  + PRlepl.vol.cub$coefficients[4]*jd^3
points(jd, PRlepl.vol.cub.predict, type = 'l', col = 'green')
PRlepl.vol.cub.mday = jd[PRlepl.vol.cub.predict == max(PRlepl.vol.cub.predict)]

#Prairie Ridge, 4 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 2)]
PRall.lepl.4 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.cub.4 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.4)
PRlepl.am.cub.4.predict = PRlepl.am.cub.4$coefficients[1] + PRlepl.am.cub.4$coefficients[2]*jd + PRlepl.am.cub.4$coefficients[3]*jd^2 + PRlepl.am.cub.4$coefficients[4]*jd^3
points(jd, PRlepl.am.cub.4.predict, type = 'l', col = 'blue')
PRlepl.am.cub.4.mday = jd[PRlepl.am.cub.4.predict == max(PRlepl.am.cub.4.predict)]

PRlepl.bs.cub.4 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.4)
PRlepl.bs.cub.4.predict = PRlepl.bs.cub.4$coefficients[1] + PRlepl.bs.cub.4$coefficients[2]*jd + PRlepl.bs.cub.4$coefficients[3]*jd^2 + PRlepl.bs.cub.4$coefficients[4]*jd^3
points(jd, PRlepl.bs.cub.4.predict, type = 'l', col = 'orange')
PRlepl.bs.cub.4.mday = jd[PRlepl.bs.cub.4.predict == max(PRlepl.bs.cub.4.predict)]

PRlepl.pm.cub.4 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.4)
PRlepl.pm.cub.4.predict = PRlepl.pm.cub.4$coefficients[1] + PRlepl.pm.cub.4$coefficients[2]*jd + PRlepl.pm.cub.4$coefficients[3]*jd^2 + PRlepl.pm.cub.4$coefficients[4]*jd^3
points(jd, PRlepl.pm.cub.4.predict, type = 'l', col = 'red')
PRlepl.pm.cub.4.mday = jd[PRlepl.pm.cub.4.predict == max(PRlepl.pm.cub.4.predict)]

PRlepl.vol.cub.4 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.4)
PRlepl.vol.cub.4.predict = PRlepl.vol.cub.4$coefficients[1] + PRlepl.vol.cub.4$coefficients[2]*jd + PRlepl.vol.cub.4$coefficients[3]*jd^2 + PRlepl.vol.cub.4$coefficients[4]*jd^3
points(jd, PRlepl.vol.cub.4.predict, type = 'l', col = 'green')
PRlepl.vol.cub.4.mday = jd[PRlepl.vol.cub.4.predict == max(PRlepl.vol.cub.4.predict)]

#Prairie Ridge, 2 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 4)]
PRall.lepl.2 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.cub.2 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.2)
PRlepl.am.cub.2.predict = PRlepl.am.cub.2$coefficients[1] + PRlepl.am.cub.2$coefficients[2]*jd + PRlepl.am.cub.2$coefficients[3]*jd^2 + PRlepl.am.cub.2$coefficients[4]*jd^3
points(jd, PRlepl.am.cub.2.predict, type = 'l', col = 'blue')
PRlepl.am.cub.2.mday = jd[PRlepl.am.cub.2.predict == max(PRlepl.am.cub.2.predict)]

PRlepl.bs.cub.2 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.2)
PRlepl.bs.cub.2.predict = PRlepl.bs.cub.2$coefficients[1] + PRlepl.bs.cub.2$coefficients[2]*jd + PRlepl.bs.cub.2$coefficients[3]*jd^2 + PRlepl.bs.cub.2$coefficients[4]*jd^3
points(jd, PRlepl.bs.cub.2.predict, type = 'l', col = 'orange')
PRlepl.bs.cub.2.mday = jd[PRlepl.bs.cub.2.predict == max(PRlepl.bs.cub.2.predict)]

PRlepl.pm.cub.2 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.2)
PRlepl.pm.cub.2.predict = PRlepl.pm.cub.2$coefficients[1] + PRlepl.pm.cub.2$coefficients[2]*jd + PRlepl.pm.cub.2$coefficients[3]*jd^2 + PRlepl.pm.cub.2$coefficients[4]*jd^3
points(jd, PRlepl.pm.cub.2.predict, type = 'l', col = 'red')
PRlepl.pm.cub.2.mday = jd[PRlepl.pm.cub.2.predict == max(PRlepl.pm.cub.2.predict)]

PRlepl.vol.cub.2 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.2)
PRlepl.vol.cub.2.predict = PRlepl.vol.cub.2$coefficients[1] + PRlepl.vol.cub.2$coefficients[2]*jd + PRlepl.vol.cub.2$coefficients[3]*jd^2 + PRlepl.vol.cub.2$coefficients[4]*jd^3
points(jd, PRlepl.vol.cub.2.predict, type = 'l', col = 'green')
PRlepl.vol.cub.2.mday = jd[PRlepl.vol.cub.2.predict == max(PRlepl.vol.cub.2.predict)]

#Prairie Ridge, 1 survey a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 8)]
PRall.lepl.1 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

PRlepl.am.cub.1 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.1)
PRlepl.am.cub.1.predict = PRlepl.am.cub.1$coefficients[1] + PRlepl.am.cub.1$coefficients[2]*jd + PRlepl.am.cub.1$coefficients[3]*jd^2 + PRlepl.am.cub.1$coefficients[4]*jd^3
points(jd, PRlepl.am.cub.1.predict, type = 'l', col = 'blue')
PRlepl.am.cub.1.mday = jd[PRlepl.am.cub.1.predict == max(PRlepl.am.cub.1.predict)]

PRlepl.bs.cub.1 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.1)
PRlepl.bs.cub.1.predict = PRlepl.bs.cub.1$coefficients[1] + PRlepl.bs.cub.1$coefficients[2]*jd + PRlepl.bs.cub.1$coefficients[3]*jd^2 + PRlepl.bs.cub.1$coefficients[4]*jd^3
points(jd, PRlepl.bs.cub.1.predict, type = 'l', col = 'orange')
PRlepl.bs.cub.1.mday = jd[PRlepl.bs.cub.1.predict == max(PRlepl.bs.cub.1.predict)]

PRlepl.pm.cub.1 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.1)
PRlepl.pm.cub.1.predict = PRlepl.pm.cub.1$coefficients[1] + PRlepl.pm.cub.1$coefficients[2]*jd + PRlepl.pm.cub.1$coefficients[3]*jd^2 + PRlepl.pm.cub.1$coefficients[4]*jd^3
points(jd, PRlepl.pm.cub.1.predict, type = 'l', col = 'red')
PRlepl.pm.cub.1.mday = jd[PRlepl.pm.cub.1.predict == max(PRlepl.pm.cub.1.predict)]

PRlepl.vol.cub.1 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.lepl.1)
PRlepl.vol.cub.1.predict = PRlepl.vol.cub.1$coefficients[1] + PRlepl.vol.cub.1$coefficients[2]*jd + PRlepl.vol.cub.1$coefficients[3]*jd^2 + PRlepl.vol.cub.1$coefficients[4]*jd^3
points(jd, PRlepl.vol.cub.1.predict, type = 'l', col = 'green')
PRlepl.vol.cub.1.mday = jd[PRlepl.vol.cub.1.predict == max(PRlepl.vol.cub.1.predict)]

title('PR lepl cub fits')

## Botanical Garden- run lepl BG plot in general_plotting.R to adds points

#Botanical Garden, 8 surveys a month
BGlepl.am.cub = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl)
BGlepl.am.cub.predict = BGlepl.am.cub$coefficients[1] + BGlepl.am.cub$coefficients[2]*jd + BGlepl.am.cub$coefficients[3]*jd^2 + BGlepl.am.cub$coefficients[4]*jd^3
points(jd, BGlepl.am.cub.predict, type = 'l', col = 'blue')
BGlepl.am.cub.mday = jd[BGlepl.am.cub.predict == max(BGlepl.am.cub.predict)]

BGlepl.bs.cub = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl)
BGlepl.bs.cub.predict = BGlepl.bs.cub$coefficients[1] + BGlepl.bs.cub$coefficients[2]*jd + BGlepl.bs.cub$coefficients[3]*jd^2 + BGlepl.bs.cub$coefficients[4]*jd^3
points(jd, BGlepl.bs.cub.predict, type = 'l', col = 'orange')
BGlepl.bs.cub.mday = jd[BGlepl.bs.cub.predict == max(BGlepl.bs.cub.predict)]

#Botanical Garden, 4 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 2)]
BGall.lepl.4 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.cub.4 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.4)
BGlepl.am.cub.4.predict = BGlepl.am.cub.4$coefficients[1] + BGlepl.am.cub.4$coefficients[2]*jd + BGlepl.am.cub.4$coefficients[3]*jd^2 + BGlepl.am.cub.4$coefficients[4]*jd^3
points(jd, BGlepl.am.cub.4.predict, type = 'l', col = 'blue')
BGlepl.am.cub.4.mday = jd[BGlepl.am.cub.4.predict == max(BGlepl.am.cub.4.predict)]

BGlepl.bs.cub.4 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.4)
BGlepl.bs.cub.4.predict = BGlepl.bs.cub.4$coefficients[1] + BGlepl.bs.cub.4$coefficients[2]*jd + BGlepl.bs.cub.4$coefficients[3]*jd^2 + BGlepl.bs.cub.4$coefficients[4]*jd^3
points(jd, BGlepl.bs.cub.4.predict, type = 'l', col = 'orange')
BGlepl.bs.cub.4.mday = jd[BGlepl.bs.cub.4.predict == max(BGlepl.bs.cub.4.predict)]

#Botanical Garden, 2 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 4)]
BGall.lepl.2 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.cub.2 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.2)
BGlepl.am.cub.2.predict = BGlepl.am.cub.2$coefficients[1] + BGlepl.am.cub.2$coefficients[2]*jd + BGlepl.am.cub.2$coefficients[3]*jd^2 + BGlepl.am.cub.2$coefficients[4]*jd^3
points(jd, BGlepl.am.cub.2.predict, type = 'l', col = 'blue')
BGlepl.am.cub.2.mday = jd[BGlepl.am.cub.2.predict == max(BGlepl.am.cub.2.predict)]

BGlepl.bs.cub.2 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.2)
BGlepl.bs.cub.2.predict = BGlepl.bs.cub.2$coefficients[1] + BGlepl.bs.cub.2$coefficients[2]*jd + BGlepl.bs.cub.2$coefficients[3]*jd^2 + BGlepl.bs.cub.2$coefficients[4]*jd^3
points(jd, BGlepl.bs.cub.2.predict, type = 'l', col = 'orange')
BGlepl.bs.cub.2.mday = jd[BGlepl.bs.cub.2.predict == max(BGlepl.bs.cub.2.predict)]

#Botanical Garden, 1 survey a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 8)]
BGall.lepl.1 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

BGlepl.am.cub.1 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.1)
BGlepl.am.cub.1.predict = BGlepl.am.cub.1$coefficients[1] + BGlepl.am.cub.1$coefficients[2]*jd + BGlepl.am.cub.1$coefficients[3]*jd^2 + BGlepl.am.cub.1$coefficients[4]*jd^3
points(jd, BGlepl.am.cub.1.predict, type = 'l', col = 'blue')
BGlepl.am.cub.1.mday = jd[BGlepl.am.cub.1.predict == max(BGlepl.am.cub.1.predict)]

BGlepl.bs.cub.1 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.lepl.1)
BGlepl.bs.cub.1.predict = BGlepl.bs.cub.1$coefficients[1] + BGlepl.bs.cub.1$coefficients[2]*jd + BGlepl.bs.cub.1$coefficients[3]*jd^2 + BGlepl.bs.cub.1$coefficients[4]*jd^3
points(jd, BGlepl.bs.cub.1.predict, type = 'l', col = 'orange')
BGlepl.bs.cub.1.mday = jd[BGlepl.bs.cub.1.predict == max(BGlepl.bs.cub.1.predict)]

title('BG lepl cub fits')

## Plotting max days in relationship to frequency of surveys

# PR, caterpillars only
PRmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.am.cub.mday, PRlepl.am.cub.4.mday, PRlepl.am.cub.2.mday, PRlepl.am.cub.1.mday))
PRmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.bs.cub.mday, PRlepl.bs.cub.4.mday, PRlepl.bs.cub.2.mday, PRlepl.bs.cub.1.mday))
PRmaxdaypm <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.pm.cub.mday, PRlepl.pm.cub.4.mday, PRlepl.pm.cub.2.mday, PRlepl.pm.cub.1.mday))
PRmaxdayvol <- data.frame(frequency = c(8,4,2,1), maxday = c(PRlepl.vol.cub.mday, PRlepl.vol.cub.4.mday, PRlepl.vol.cub.2.mday, PRlepl.vol.cub.1.mday))
plot(PRmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(PRmaxdaybs, col = 'orange', pch = 19)
points(PRmaxdaypm, col = 'red', pch = 19)
points(PRmaxdayvol, col = 'green', pch = 19)
title('PR lepl cub fit max day')

# BG, caterpillars only
BGmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(BGlepl.am.cub.mday, BGlepl.am.cub.4.mday, BGlepl.am.cub.2.mday, BGlepl.am.cub.1.mday))
BGmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(BGlepl.bs.cub.mday, BGlepl.bs.cub.4.mday, BGlepl.bs.cub.2.mday, BGlepl.bs.cub.1.mday))
plot(BGmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(BGmaxdaybs, col = 'orange', pch = 19)
title('BG lepl cub fit max day')



## Selected arthropods

## Prairie Ridge- run mult PR plot in general_plotting.R to adds points

#Prairie Ridge, 8 surveys a month
PRmult.am.cub = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult)
PRmult.am.cub.predict = PRmult.am.cub$coefficients[1] + PRmult.am.cub$coefficients[2]*jd + PRmult.am.cub$coefficients[3]*jd^2 + PRmult.am.cub$coefficients[4]*jd^3
points(jd, PRmult.am.cub.predict, type = 'l', col = 'blue')
PRmult.am.cub.mday = jd[PRmult.am.cub.predict == max(PRmult.am.cub.predict)]

PRmult.bs.cub = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult)
PRmult.bs.cub.predict = PRmult.bs.cub$coefficients[1] + PRmult.bs.cub$coefficients[2]*jd + PRmult.bs.cub$coefficients[3]*jd^2 + PRmult.bs.cub$coefficients[4]*jd^3
points(jd, PRmult.bs.cub.predict, type = 'l', col = 'orange')
PRmult.bs.cub.mday = jd[PRmult.bs.cub.predict == max(PRmult.bs.cub.predict)]

PRmult.pm.cub = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult)
PRmult.pm.cub.predict = PRmult.pm.cub$coefficients[1] + PRmult.pm.cub$coefficients[2]*jd + PRmult.pm.cub$coefficients[3]*jd^2 + PRmult.pm.cub$coefficients[4]*jd^3
points(jd, PRmult.pm.cub.predict, type = 'l', col = 'red')
PRmult.pm.cub.mday = jd[PRmult.pm.cub.predict == max(PRmult.pm.cub.predict)]

PRmult.vol.cub = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult)
PRmult.vol.cub.predict = PRmult.vol.cub$coefficients[1] + PRmult.vol.cub$coefficients[2]*jd + PRmult.vol.cub$coefficients[3]*jd^2  + PRmult.vol.cub$coefficients[4]*jd^3
points(jd, PRmult.vol.cub.predict, type = 'l', col = 'green')
PRmult.vol.cub.mday = jd[PRmult.vol.cub.predict == max(PRmult.vol.cub.predict)]

#Prairie Ridge, 4 surveys a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 2)]
PRall.mult.4 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.cub.4 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.4)
PRmult.am.cub.4.predict = PRmult.am.cub.4$coefficients[1] + PRmult.am.cub.4$coefficients[2]*jd + PRmult.am.cub.4$coefficients[3]*jd^2 + PRmult.am.cub.4$coefficients[4]*jd^3
points(jd, PRmult.am.cub.4.predict, type = 'l', col = 'blue')
PRmult.am.cub.4.mday = jd[PRmult.am.cub.4.predict == max(PRmult.am.cub.4.predict)]

PRmult.bs.cub.4 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.4)
PRmult.bs.cub.4.predict = PRmult.bs.cub.4$coefficients[1] + PRmult.bs.cub.4$coefficients[2]*jd + PRmult.bs.cub.4$coefficients[3]*jd^2 + PRmult.bs.cub.4$coefficients[4]*jd^3
points(jd, PRmult.bs.cub.4.predict, type = 'l', col = 'orange')
PRmult.bs.cub.4.mday = jd[PRmult.bs.cub.4.predict == max(PRmult.bs.cub.4.predict)]

PRmult.pm.cub.4 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.4)
PRmult.pm.cub.4.predict = PRmult.pm.cub.4$coefficients[1] + PRmult.pm.cub.4$coefficients[2]*jd + PRmult.pm.cub.4$coefficients[3]*jd^2 + PRmult.pm.cub.4$coefficients[4]*jd^3
points(jd, PRmult.pm.cub.4.predict, type = 'l', col = 'red')
PRmult.pm.cub.4.mday = jd[PRmult.pm.cub.4.predict == max(PRmult.pm.cub.4.predict)]

PRmult.vol.cub.4 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.4)
PRmult.vol.cub.4.predict = PRmult.vol.cub.4$coefficients[1] + PRmult.vol.cub.4$coefficients[2]*jd + PRmult.vol.cub.4$coefficients[3]*jd^2 + PRmult.vol.cub.4$coefficients[4]*jd^3
points(jd, PRmult.vol.cub.4.predict, type = 'l', col = 'green')
PRmult.vol.cub.4.mday = jd[PRmult.vol.cub.4.predict == max(PRmult.vol.cub.4.predict)]

#Prairie Ridge, 2 surveys a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 4)]
PRall.mult.2 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.cub.2 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.2)
PRmult.am.cub.2.predict = PRmult.am.cub.2$coefficients[1] + PRmult.am.cub.2$coefficients[2]*jd + PRmult.am.cub.2$coefficients[3]*jd^2 + PRmult.am.cub.2$coefficients[4]*jd^3
points(jd, PRmult.am.cub.2.predict, type = 'l', col = 'blue')
PRmult.am.cub.2.mday = jd[PRmult.am.cub.2.predict == max(PRmult.am.cub.2.predict)]

PRmult.bs.cub.2 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.2)
PRmult.bs.cub.2.predict = PRmult.bs.cub.2$coefficients[1] + PRmult.bs.cub.2$coefficients[2]*jd + PRmult.bs.cub.2$coefficients[3]*jd^2 + PRmult.bs.cub.2$coefficients[4]*jd^3
points(jd, PRmult.bs.cub.2.predict, type = 'l', col = 'orange')
PRmult.bs.cub.2.mday = jd[PRmult.bs.cub.2.predict == max(PRmult.bs.cub.2.predict)]

PRmult.pm.cub.2 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.2)
PRmult.pm.cub.2.predict = PRmult.pm.cub.2$coefficients[1] + PRmult.pm.cub.2$coefficients[2]*jd + PRmult.pm.cub.2$coefficients[3]*jd^2 + PRmult.pm.cub.2$coefficients[4]*jd^3
points(jd, PRmult.pm.cub.2.predict, type = 'l', col = 'red')
PRmult.pm.cub.2.mday = jd[PRmult.pm.cub.2.predict == max(PRmult.pm.cub.2.predict)]

PRmult.vol.cub.2 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.2)
PRmult.vol.cub.2.predict = PRmult.vol.cub.2$coefficients[1] + PRmult.vol.cub.2$coefficients[2]*jd + PRmult.vol.cub.2$coefficients[3]*jd^2 + PRmult.vol.cub.2$coefficients[4]*jd^3
points(jd, PRmult.vol.cub.2.predict, type = 'l', col = 'green')
PRmult.vol.cub.2.mday = jd[PRmult.vol.cub.2.predict == max(PRmult.vol.cub.2.predict)]

#Prairie Ridge, 1 survey a month
tempjd <- PRall.mult$julianday[seq(1, length(PRall.mult$julianday), 8)]
PRall.mult.1 <- PRall.mult[PRall.mult$julianday %in% tempjd,]

PRmult.am.cub.1 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.1)
PRmult.am.cub.1.predict = PRmult.am.cub.1$coefficients[1] + PRmult.am.cub.1$coefficients[2]*jd + PRmult.am.cub.1$coefficients[3]*jd^2 + PRmult.am.cub.1$coefficients[4]*jd^3
points(jd, PRmult.am.cub.1.predict, type = 'l', col = 'blue')
PRmult.am.cub.1.mday = jd[PRmult.am.cub.1.predict == max(PRmult.am.cub.1.predict)]

PRmult.bs.cub.1 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.1)
PRmult.bs.cub.1.predict = PRmult.bs.cub.1$coefficients[1] + PRmult.bs.cub.1$coefficients[2]*jd + PRmult.bs.cub.1$coefficients[3]*jd^2 + PRmult.bs.cub.1$coefficients[4]*jd^3
points(jd, PRmult.bs.cub.1.predict, type = 'l', col = 'orange')
PRmult.bs.cub.1.mday = jd[PRmult.bs.cub.1.predict == max(PRmult.bs.cub.1.predict)]

PRmult.pm.cub.1 = lm(density_pm ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.1)
PRmult.pm.cub.1.predict = PRmult.pm.cub.1$coefficients[1] + PRmult.pm.cub.1$coefficients[2]*jd + PRmult.pm.cub.1$coefficients[3]*jd^2 + PRmult.pm.cub.1$coefficients[4]*jd^3
points(jd, PRmult.pm.cub.1.predict, type = 'l', col = 'red')
PRmult.pm.cub.1.mday = jd[PRmult.pm.cub.1.predict == max(PRmult.pm.cub.1.predict)]

PRmult.vol.cub.1 = lm(density_vol ~ julianday + I(julianday^2) + I(julianday^3), data = PRall.mult.1)
PRmult.vol.cub.1.predict = PRmult.vol.cub.1$coefficients[1] + PRmult.vol.cub.1$coefficients[2]*jd + PRmult.vol.cub.1$coefficients[3]*jd^2 + PRmult.vol.cub.1$coefficients[4]*jd^3
points(jd, PRmult.vol.cub.1.predict, type = 'l', col = 'green')
PRmult.vol.cub.1.mday = jd[PRmult.vol.cub.1.predict == max(PRmult.vol.cub.1.predict)]

title('PR mult cub fits')

## Botanical Garden- run mult BG plot in general_plotting.R to adds points

#Botanical Garden, 8 surveys a month
BGmult.am.cub = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult)
BGmult.am.cub.predict = BGmult.am.cub$coefficients[1] + BGmult.am.cub$coefficients[2]*jd + BGmult.am.cub$coefficients[3]*jd^2 + BGmult.am.cub$coefficients[4]*jd^3
points(jd, BGmult.am.cub.predict, type = 'l', col = 'blue')
BGmult.am.cub.mday = jd[BGmult.am.cub.predict == max(BGmult.am.cub.predict)]

BGmult.bs.cub = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult)
BGmult.bs.cub.predict = BGmult.bs.cub$coefficients[1] + BGmult.bs.cub$coefficients[2]*jd + BGmult.bs.cub$coefficients[3]*jd^2 + BGmult.bs.cub$coefficients[4]*jd^3
points(jd, BGmult.bs.cub.predict, type = 'l', col = 'orange')
BGmult.bs.cub.mday = jd[BGmult.bs.cub.predict == max(BGmult.bs.cub.predict)]

#Botanical Garden, 4 surveys a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 2)]
BGall.mult.4 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.cub.4 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.4)
BGmult.am.cub.4.predict = BGmult.am.cub.4$coefficients[1] + BGmult.am.cub.4$coefficients[2]*jd + BGmult.am.cub.4$coefficients[3]*jd^2 + BGmult.am.cub.4$coefficients[4]*jd^3
points(jd, BGmult.am.cub.4.predict, type = 'l', col = 'blue')
BGmult.am.cub.4.mday = jd[BGmult.am.cub.4.predict == max(BGmult.am.cub.4.predict)]

BGmult.bs.cub.4 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.4)
BGmult.bs.cub.4.predict = BGmult.bs.cub.4$coefficients[1] + BGmult.bs.cub.4$coefficients[2]*jd + BGmult.bs.cub.4$coefficients[3]*jd^2 + BGmult.bs.cub.4$coefficients[4]*jd^3
points(jd, BGmult.bs.cub.4.predict, type = 'l', col = 'orange')
BGmult.bs.cub.4.mday = jd[BGmult.bs.cub.4.predict == max(BGmult.bs.cub.4.predict)]

#Botanical Garden, 2 surveys a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 4)]
BGall.mult.2 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.cub.2 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.2)
BGmult.am.cub.2.predict = BGmult.am.cub.2$coefficients[1] + BGmult.am.cub.2$coefficients[2]*jd + BGmult.am.cub.2$coefficients[3]*jd^2 + BGmult.am.cub.2$coefficients[4]*jd^3
points(jd, BGmult.am.cub.2.predict, type = 'l', col = 'blue')
BGmult.am.cub.2.mday = jd[BGmult.am.cub.2.predict == max(BGmult.am.cub.2.predict)]

BGmult.bs.cub.2 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.2)
BGmult.bs.cub.2.predict = BGmult.bs.cub.2$coefficients[1] + BGmult.bs.cub.2$coefficients[2]*jd + BGmult.bs.cub.2$coefficients[3]*jd^2 + BGmult.bs.cub.2$coefficients[4]*jd^3
points(jd, BGmult.bs.cub.2.predict, type = 'l', col = 'orange')
BGmult.bs.cub.2.mday = jd[BGmult.bs.cub.2.predict == max(BGmult.bs.cub.2.predict)]

#Botanical Garden, 1 survey a month
tempjd <- BGall.mult$julianday[seq(1, length(BGall.mult$julianday), 8)]
BGall.mult.1 <- BGall.mult[BGall.mult$julianday %in% tempjd,]

BGmult.am.cub.1 = lm(density_am ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.1)
BGmult.am.cub.1.predict = BGmult.am.cub.1$coefficients[1] + BGmult.am.cub.1$coefficients[2]*jd + BGmult.am.cub.1$coefficients[3]*jd^2 + BGmult.am.cub.1$coefficients[4]*jd^3
points(jd, BGmult.am.cub.1.predict, type = 'l', col = 'blue')
BGmult.am.cub.1.mday = jd[BGmult.am.cub.1.predict == max(BGmult.am.cub.1.predict)]

BGmult.bs.cub.1 = lm(density_bs ~ julianday + I(julianday^2) + I(julianday^3), data = BGall.mult.1)
BGmult.bs.cub.1.predict = BGmult.bs.cub.1$coefficients[1] + BGmult.bs.cub.1$coefficients[2]*jd + BGmult.bs.cub.1$coefficients[3]*jd^2 + BGmult.bs.cub.1$coefficients[4]*jd^3
points(jd, BGmult.bs.cub.1.predict, type = 'l', col = 'orange')
BGmult.bs.cub.1.mday = jd[BGmult.bs.cub.1.predict == max(BGmult.bs.cub.1.predict)]

title('BG mult cub fits')

## Plotting max days in relationship to frequency of surveys

# PR, selected arthropods
PRmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.am.cub.mday, PRmult.am.cub.4.mday, PRmult.am.cub.2.mday, PRmult.am.cub.1.mday))
PRmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.bs.cub.mday, PRmult.bs.cub.4.mday, PRmult.bs.cub.2.mday, PRmult.bs.cub.1.mday))
PRmaxdaypm <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.pm.cub.mday, PRmult.pm.cub.4.mday, PRmult.pm.cub.2.mday, PRmult.pm.cub.1.mday))
PRmaxdayvol <- data.frame(frequency = c(8,4,2,1), maxday = c(PRmult.vol.cub.mday, PRmult.vol.cub.4.mday, PRmult.vol.cub.2.mday, PRmult.vol.cub.1.mday))
plot(PRmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(PRmaxdaybs, col = 'orange', pch = 19)
points(PRmaxdaypm, col = 'red', pch = 19)
points(PRmaxdayvol, col = 'green', pch = 19)
title('PR mult cub fit max day')

# BG, selected arthropods
BGmaxdayam <- data.frame(frequency = c(8,4,2,1), maxday = c(BGmult.am.cub.mday, BGmult.am.cub.4.mday, BGmult.am.cub.2.mday, BGmult.am.cub.1.mday))
BGmaxdaybs <- data.frame(frequency = c(8,4,2,1), maxday = c(BGmult.bs.cub.mday, BGmult.bs.cub.4.mday, BGmult.bs.cub.2.mday, BGmult.bs.cub.1.mday))
plot(BGmaxdayam, col = 'blue', pch = 19, ylim = c(140,210))
points(BGmaxdaybs, col = 'orange', pch = 19)
title('BG mult cub fit max day')
