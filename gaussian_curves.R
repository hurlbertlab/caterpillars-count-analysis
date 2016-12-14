#####################################################################
# Fitting Gaussians

fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
}

# mu = mean
# sig = standard deviation
###################################################################

multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')


#----Test 1: LEPL 2015 by week----

# Make plot/data of what you will fit normal curve to
prlepl15.weekly = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                    ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                    plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                    xlim = c(18,32), ylim = c(0,.2), ylab = "Mean density", main = 'PR LEPL by week')

# Fit a normal curve using least squares
gfit = fitG(prlepl15.weekly$week, prlepl15.weekly$meanDensity, weighted.mean(prlepl15.weekly$week, prlepl15.weekly$meanDensity),
            2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit$par
r2 = cor(prlepl15.weekly$week, p[3]*dnorm(prlepl15.weekly$week, p[1], p[2]))^2
totalDensity = sum(prlepl15.weekly$meanDensity)
lines(18:32, p[3]*dnorm(18:32, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


#----Test 2: LEPL 2015 by day----

# Make plot/data of what you will fit normal curve to
prlepl15.daily = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                  plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                  xlim = c(130,210), ylim = c(0,.2), ylab = "Mean density", main = 'PR LEPL by day')

# Fit a normal curve using least squares
gfit2 = fitG(prlepl15.daily$julianday, prlepl15.daily$meanDensity, weighted.mean(prlepl15.daily$julianday, prlepl15.daily$meanDensity),
             20, 4, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p2 = gfit2$par
r2.2 = cor(prlepl15.daily$julianday, p2[3]*dnorm(prlepl15.daily$julianday, p2[1], p2[2]))^2
totalDensity = sum(prlepl15.daily$meanDensity)
lines(130:210, p2[3]*dnorm(130:210, p2[1], p2[2]), col = 'blue') # make sure it appears on the right plot

#----Test 3: Bird food by week----

# Make plot/data of what you will fit normal curve to
prmult15.weekly = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                                    ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                    plotVar = 'meanDensity', new = T, minLength = 5, lwd = 2,
                                    xlim = c(18,32), ylim = c(.1,.7), ylab = "Mean density", main = 'PR mult by week')

# Fit a normal curve using least squares
gfit3 = fitG(prmult15.weekly$week, prmult15.weekly$meanDensity, weighted.mean(prmult15.weekly$week, prmult15.weekly$meanDensity),
             4, 10, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p3 = gfit3$par
r2.3 = cor(prmult15.weekly$week, p3[3]*dnorm(prmult15.weekly$week, p3[1], p3[2]))^2
totalDensity = sum(prmult15.weekly$meanDensity)
lines(18:32, p3[3]*dnorm(18:32, p3[1], p3[2]), col = 'blue') # make sure it appears on the right plot

