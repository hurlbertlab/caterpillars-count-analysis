# Phenology plot for paper

#setwd('c:/git/caterpillars-count-analysis')
source("cleaning_scripts/data_cleaning.r")

par(mfrow = c(3,2), mar = c(3,4,3,2), oma = c(3,3,2,2))
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

#----PHENOLOGY PLOT MEAN DENSITY----

# First panel
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.36), ylab = "Caterpillars", main = '2015 Visual')
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Second panel
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 Beat Sheet')
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Third panel
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Orthopterans", main = '')
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Fourth panel
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", main = '')
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Fifth panel
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.5), ylab = "Bird food", main = '')
PR.BIRD15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))


# Sixth panel
PR.BIRDL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                  xlim = c(130,207), ylim = c(0,1.2), ylab = "", main = '')
PR.BIRD16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = F, minLength = 5, lwd = 2, lty = 2)
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))

mtext("Julian day", side=1, line=1, outer=TRUE)
mtext("Mean Density", side=2, line=1, outer=TRUE)

#----GAUSSIAN FITS FOR TABLE 1 PHENOLOGY----

fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
}

par(mfrow = c(1,1))

# First panel 1
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.36), ylab = "Mean density of caterpillars", main = '2015 Visual')

# Fit a normal curve using least squares
gfit1 = fitG(PR.LEPL15.sci$julianday, PR.LEPL15.sci$meanDensity, weighted.mean(PR.LEPL15.sci$julianday, PR.LEPL15.sci$meanDensity),
            2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit1$par
r2 = cor(PR.LEPL15.sci$julianday, p[3]*dnorm(PR.LEPL15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# First panel 2
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)
legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = c(1, 2))

# Fit a normal curve using least squares
gfit2 = fitG(PR.LEPL15.cs$julianday, PR.LEPL15.cs$meanDensity, weighted.mean(PR.LEPL15.cs$julianday, PR.LEPL15.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit2$par
r2 = cor(PR.LEPL15.cs$julianday, p[3]*dnorm(PR.LEPL15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Second panel 1
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 Beat Sheet')

# Fit a normal curve using least squares
gfit3 = fitG(PR.LEPL16.sci$julianday, PR.LEPL16.sci$meanDensity, weighted.mean(PR.LEPL16.sci$julianday, PR.LEPL16.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit3$par
r2 = cor(PR.LEPL16.sci$julianday, p[3]*dnorm(PR.LEPL16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Second panel 2
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit4 = fitG(PR.LEPL16.cs$julianday, PR.LEPL16.cs$meanDensity, weighted.mean(PR.LEPL16.cs$julianday, PR.LEPL16.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit4$par
r2 = cor(PR.LEPL16.cs$julianday, p[3]*dnorm(PR.LEPL16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Third panel 1
PR.ORTH15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Mean density of orthopterans", main = '')

# Fit a normal curve using least squares
gfit5 = fitG(PR.ORTH15.sci$julianday, PR.ORTH15.sci$meanDensity, weighted.mean(PR.ORTH15.sci$julianday, PR.ORTH15.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit5$par
r2 = cor(PR.ORTH15.sci$julianday, p[3]*dnorm(PR.ORTH15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Third panel 2
PR.ORTH15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit6 = fitG(PR.ORTH15.cs$julianday, PR.ORTH15.cs$meanDensity, weighted.mean(PR.ORTH15.cs$julianday, PR.ORTH15.cs$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit6$par
r2 = cor(PR.ORTH15.cs$julianday, p[3]*dnorm(PR.ORTH15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fourth panel 1
PR.ORTH16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", main = '')

# Fit a normal curve using least squares
gfit7 = fitG(PR.ORTH16.sci$julianday, PR.ORTH16.sci$meanDensity, weighted.mean(PR.ORTH16.sci$julianday, PR.ORTH16.sci$meanDensity),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit7$par
r2 = cor(PR.ORTH16.sci$julianday, p[3]*dnorm(PR.ORTH16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fourth panel 2
PR.ORTH16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit8 = fitG(PR.ORTH16.cs$julianday, PR.ORTH16.cs$meanDensity, weighted.mean(PR.ORTH16.cs$julianday, PR.ORTH16.cs$meanDensity),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit8$par
r2 = cor(PR.ORTH16.cs$julianday, p[3]*dnorm(PR.ORTH16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fifth panel 1
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.5), ylab = "Mean density of bird food", main = '')

# Fit a normal curve using least squares
gfit9 = fitG(PR.BIRD15.sci$julianday, PR.BIRD15.sci$meanDensity, weighted.mean(PR.BIRD15.sci$julianday, PR.BIRD15.sci$meanDensity),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit9$par
r2 = cor(PR.BIRD15.sci$julianday, p[3]*dnorm(PR.BIRD15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fifth panel 2
PR.BIRD15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit10 = fitG(PR.BIRD15.cs$julianday, PR.BIRD15.cs$meanDensity, weighted.mean(PR.BIRD15.cs$julianday, PR.BIRD15.cs$meanDensity),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit10$par
r2 = cor(PR.BIRD15.cs$julianday, p[3]*dnorm(PR.BIRD15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 1
PR.BIRD16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                  xlim = c(130,207), ylim = c(0,1.2), ylab = "", main = '')

# Fit a normal curve using least squares
gfit11 = fitG(PR.BIRD16.sci$julianday, PR.BIRD16.sci$meanDensity, weighted.mean(PR.BIRD16.sci$julianday, PR.BIRD16.sci$meanDensity),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit11$par
r2 = cor(PR.BIRD16.sci$julianday, p[3]*dnorm(PR.BIRD16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.sci$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 2
PR.BIRD16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit12 = fitG(PR.BIRD16.cs$julianday, PR.BIRD16.cs$meanDensity, weighted.mean(PR.BIRD16.cs$julianday, PR.BIRD16.cs$meanDensity),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit12$par
r2 = cor(PR.BIRD16.cs$julianday, p[3]*dnorm(PR.BIRD16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.cs$meanDensity)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

#----PHENOLOGY SHIFT: LEPL and bird food, 2015 and 2016, mean density, both sites----
# See compilation.R for more

par(mar = c(4,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Arth JD',xlab='Greenup JD', ylim = c(160,185), xlim = c(85, 95),
     main = 'Visual Surveys: Arth JD vs. Greenup JD')
maxden1 <- max(PR.LEPL15vis[PR.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'red')
maxden2 <- max(PR.BIRD15vis[PR.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'red')
maxden3 <- max(PR.LEPL16vis[PR.LEPL16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'red')
maxden4 <- max(PR.BIRD16vis[PR.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15vis$julianday[PR.LEPL15vis$meanDensity == maxden1 & PR.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16vis$julianday[PR.LEPL16vis$meanDensity == maxden3 & PR.LEPL16vis$julianday %in% c(80:185)], 
         col = 'red')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15vis$julianday[PR.BIRD15vis$meanDensity == maxden2 & PR.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16vis$julianday[PR.BIRD16vis$meanDensity == maxden4 & PR.BIRD16vis$julianday %in% c(80:185)], 
         col = 'red')
# Botanical Garden
maxden5 <- max(BG.LEPL15vis[BG.LEPL15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
       pch = 21, type = 'p', col = 'blue')
maxden6 <- max(BG.BIRD15vis[BG.BIRD15vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
       pch = 22, type = 'p', col = 'blue')
maxden7 <- max(BG.LEPL16vis[BG.LEPL16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
       pch = 16, type = 'p', col = 'blue')
maxden8 <- max(BG.BIRD16vis[BG.BIRD16vis$julianday %in% c(80:185),]$meanDensity)
points(greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
       pch = 15, type = 'p', col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15vis$julianday[BG.LEPL15vis$meanDensity == maxden5 & BG.LEPL15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16vis$julianday[BG.LEPL16vis$meanDensity == maxden7 & BG.LEPL16vis$julianday %in% c(80:185)], 
         col = 'blue')
segments(x0 = greenupgdd$bggreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15vis$julianday[BG.BIRD15vis$meanDensity == maxden6 & BG.BIRD15vis$julianday %in% c(80:185)], 
         x1 = greenupgdd$bggreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16vis$julianday[BG.BIRD16vis$meanDensity == maxden8 & BG.BIRD16vis$julianday %in% c(80:185)], 
         col = 'blue')
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))


# bird food orders
# caterpillar colonies