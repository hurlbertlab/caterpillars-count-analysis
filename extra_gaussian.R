# extra gaussian fits 
# scientist beat sheets in 2015 and scientist visual in 2016

fitG = function(x, y, mu, sig, scale, ...){
  f = function(p){
    d = p[3] * dnorm(x, mean = p[1], sd = p[2])
    sum((d - y) ^ 2)
  }
  optim(c(mu, sig, scale), f)
}

multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')
par(mfrow = c(1,1))

# LEPL 2015 BEAT SHEET
PR.LEPL15.sci.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(138:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.36), ylab = "", main = '2015 BS')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfita = fitG(PR.LEPL15.sci.bs$julianday, PR.LEPL15.sci.bs$fracSurveys, weighted.mean(PR.LEPL15.sci.bs$julianday, PR.LEPL15.sci.bs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfita$par
r2 = cor(PR.LEPL15.sci.bs$julianday, p[3]*dnorm(PR.LEPL15.sci.bs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.sci.bs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# ORTH 2015 BEAT SHEET
PR.ORTH15.sci.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(138:204),], 
                                    ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                    plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                    xlim = c(130,207), ylim = c(0,.36), ylab = "", main = '2015 BS')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfitb = fitG(PR.ORTH15.sci.bs$julianday, PR.ORTH15.sci.bs$fracSurveys, weighted.mean(PR.ORTH15.sci.bs$julianday, PR.ORTH15.sci.bs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfitb$par
r2 = cor(PR.ORTH15.sci.bs$julianday, p[3]*dnorm(PR.ORTH15.sci.bs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.sci.bs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# BIRD 2015 BEAT SHEET
PR.BIRD15.sci.bs = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(138:204),], 
                                    ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                    plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                    xlim = c(130,207), ylim = c(0.2,.55), ylab = "", main = '2015 BS')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfitc = fitG(PR.BIRD15.sci.bs$julianday, PR.BIRD15.sci.bs$fracSurveys, weighted.mean(PR.BIRD15.sci.bs$julianday, PR.BIRD15.sci.bs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfitc$par
r2 = cor(PR.BIRD15.sci.bs$julianday, p[3]*dnorm(PR.BIRD15.sci.bs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.sci.bs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# LEPL 2016 VISUAL
PR.LEPL16.sci.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                    ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                    plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                    xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 vis')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfitd = fitG(PR.LEPL16.sci.vis$julianday, PR.LEPL16.sci.vis$fracSurveys, weighted.mean(PR.LEPL16.sci.vis$julianday, PR.LEPL16.sci.vis$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfitd$par
r2 = cor(PR.LEPL16.sci.vis$julianday, p[3]*dnorm(PR.LEPL16.sci.vis$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.sci.vis$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# ORTH 2016 VISUAL
PR.ORTH16.sci.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                     ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                     plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                     xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 vis')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfite = fitG(PR.ORTH16.sci.vis$julianday, PR.ORTH16.sci.vis$fracSurveys, weighted.mean(PR.ORTH16.sci.vis$julianday, PR.ORTH16.sci.vis$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfite$par
r2 = cor(PR.ORTH16.sci.vis$julianday, p[3]*dnorm(PR.ORTH16.sci.vis$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.sci.vis$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot



# BIRD 2016 VISUAL
PR.BIRD16.sci.vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                     ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                     plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                     xlim = c(130,207), ylim = c(.1,.5), ylab = "", main = '2016 vis')
# adjusted jday limit because only 5 beatsheet surveys on jday 135

# Fit a normal curve using least squares
gfitf = fitG(PR.BIRD16.sci.vis$julianday, PR.BIRD16.sci.vis$fracSurveys, weighted.mean(PR.BIRD16.sci.vis$julianday, PR.BIRD16.sci.vis$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfitf$par
r2 = cor(PR.BIRD16.sci.vis$julianday, p[3]*dnorm(PR.BIRD16.sci.vis$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.sci.vis$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


