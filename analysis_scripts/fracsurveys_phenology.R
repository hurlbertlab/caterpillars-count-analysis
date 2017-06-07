# Phenology plots - comparing cit sci to trained sci [for overall paper], only Prairie Ridge
# Included in 495 paper (Tracie Fall 2016)
# This script is a mess- ORGANIZE
# DO BY WEEK

source("cleaning_scripts/data_cleaning.r")

#pdf(file = 'c:/git/caterpillars-count-analysis/output/plots/paper_plots/phenology.pdf', width = 6, height = 7)

par(mfrow = c(3,2), mar = c(2,3,2,2), oma = c(5,5,3,3))
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

jdmax = 193

#----PHENOLOGY PLOT for paper, by week----

# First panel
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,jdmax), ylim = c(0,.15), ylab = "Caterpillars", main = '2015 Visual', col = 'blueviolet')
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
legend(20, .2, c('trained', 'citizen'), lwd = 2, lty = 1, col = c('blueviolet', 'darkgoldenrod3'))
#mtext("A", 1, adj=2, line=1, las = 2)
legend("topleft", "B", bty="n")
LEPL15 = merge(PR.LEPL15.sci[,c('fracSurveys', 'week')], PR.LEPL15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL15$fracSurveys.x, LEPL15$fracSurveys.y), 2))), bty="n")


# Second panel
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,jdmax), ylim = c(0,.15), ylab = "", main = '2016 Beat Sheet', col = 'blueviolet')
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "A", bty="n")
LEPL16 = merge(PR.LEPL16.sci[,c('fracSurveys', 'week')], PR.LEPL16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(LEPL16$fracSurveys.x, LEPL16$fracSurveys.y), 2))), bty="n")


# Third panel
PR.ORTH15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,jdmax), ylim = c(0,.3), ylab = "Orthopterans", main = '', col = 'blueviolet')
PR.ORTH15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "C", bty="n")
ORTH15 = merge(PR.ORTH15.sci[,c('fracSurveys', 'week')], PR.ORTH15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH15$fracSurveys.x, ORTH15$fracSurveys.y), 2))), bty="n")

# Fourth panel
PR.ORTH16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,jdmax), ylim = c(0,.3), ylab = "", main = '', col = 'blueviolet')
PR.ORTH16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "D", bty="n")
ORTH16 = merge(PR.ORTH16.sci[,c('fracSurveys', 'week')], PR.ORTH16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(ORTH16$fracSurveys.x, ORTH16$fracSurveys.y), 2))), bty="n")

# Fifth panel
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,jdmax), ylim = c(0,.5), ylab = "Bird food", main = '', col = 'blueviolet')
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "E", bty="n")
BIRD15 = merge(PR.BIRD15.sci[,c('fracSurveys', 'week')], PR.BIRD15.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD15$fracSurveys.x, BIRD15$fracSurveys.y), 2))), bty="n")

# Sixth panel
PR.BIRD16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(135,jdmax), ylim = c(0,.5), ylab = "", main = '', col = 'blueviolet')
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'darkgoldenrod3')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "F", bty="n")
BIRD16 = merge(PR.BIRD16.sci[,c('fracSurveys', 'week')], PR.BIRD16.cs[,c('fracSurveys', 'week')], by = 'week', all = F)
legend('topright', legend = c(paste("r =", round(cor(BIRD16$fracSurveys.x, BIRD16$fracSurveys.y), 2))), bty="n")

mtext("Occurrence (fraction of surveys)", side = 2, outer = TRUE, line = 2)
mtext("Julian day", side = 1, outer = TRUE, line = 2)

#dev.off()

#----FIGURE 3 PHENOLOGY PLOT *used in 495 paper*, fraction of surveys----

# First panel
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Caterpillars", main = '2015 Visual')
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
legend("topright", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1, col = c('black', 'red'))
legend("topleft", "A", bty="n")


# Second panel
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "", main = '2016 Beat Sheet')
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "B", bty="n")


# Third panel
PR.LEPL15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.4), ylab = "Orthopterans", main = '')
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "C", bty="n")


# Fourth panel
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.4), ylab = "", main = '')
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "D", bty="n")


# Fifth panel
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "Bird food", main = '')
PR.BIRD15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "E", bty="n")


# Sixth panel
PR.BIRDL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                  xlim = c(130,207), ylim = c(0,.7), ylab = "", main = '')
PR.BIRD16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'red')
#legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)
legend("topleft", "F", bty="n")

mtext("Fraction of surveys", side = 2, outer = TRUE, line = 2)
mtext("Julian day", side = 1, outer = TRUE, line = 2)

#----GAUSSIAN FITS FOR TABLE 1 PHENOLOGY (in Tracie's 495 paper)----
# Gaussian fit plots for above 6-panel used in 495 paper

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
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.36), ylab = "Mean density of caterpillars", main = '2015 Visual')

# Fit a normal curve using least squares
gfit1 = fitG(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys, weighted.mean(PR.LEPL15.sci$julianday, PR.LEPL15.sci$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit1$par
r2 = cor(PR.LEPL15.sci$julianday, p[3]*dnorm(PR.LEPL15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# First panel 2
PR.LEPL15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)
legend("topleft", c('trained scientists', 'citizen scientists'), lwd = 2, lty = 1)

# Fit a normal curve using least squares
gfit2 = fitG(PR.LEPL15.cs$julianday, PR.LEPL15.cs$fracSurveys, weighted.mean(PR.LEPL15.cs$julianday, PR.LEPL15.cs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit2$par
r2 = cor(PR.LEPL15.cs$julianday, p[3]*dnorm(PR.LEPL15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL15.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Second panel 1
PR.LEPL16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.2), ylab = "", main = '2016 Beat Sheet')

# Fit a normal curve using least squares
gfit3 = fitG(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys, weighted.mean(PR.LEPL16.sci$julianday, PR.LEPL16.sci$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit3$par
r2 = cor(PR.LEPL16.sci$julianday, p[3]*dnorm(PR.LEPL16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Second panel 2
PR.LEPL16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit4 = fitG(PR.LEPL16.cs$julianday, PR.LEPL16.cs$fracSurveys, weighted.mean(PR.LEPL16.cs$julianday, PR.LEPL16.cs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit4$par
r2 = cor(PR.LEPL16.cs$julianday, p[3]*dnorm(PR.LEPL16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.LEPL16.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Third panel 1
PR.ORTH15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.25), ylab = "Mean density of orthopterans", main = '')

# Fit a normal curve using least squares
gfit5 = fitG(PR.ORTH15.sci$julianday, PR.ORTH15.sci$fracSurveys, weighted.mean(PR.ORTH15.sci$julianday, PR.ORTH15.sci$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit5$par
r2 = cor(PR.ORTH15.sci$julianday, p[3]*dnorm(PR.ORTH15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Third panel 2
PR.ORTH15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit6 = fitG(PR.ORTH15.cs$julianday, PR.ORTH15.cs$fracSurveys, weighted.mean(PR.ORTH15.cs$julianday, PR.ORTH15.cs$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit6$par
r2 = cor(PR.ORTH15.cs$julianday, p[3]*dnorm(PR.ORTH15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH15.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fourth panel 1
PR.ORTH16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,.7), ylab = "", main = '')

# Fit a normal curve using least squares
gfit7 = fitG(PR.ORTH16.sci$julianday, PR.ORTH16.sci$fracSurveys, weighted.mean(PR.ORTH16.sci$julianday, PR.ORTH16.sci$fracSurveys),
             2, 3.5, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit7$par
r2 = cor(PR.ORTH16.sci$julianday, p[3]*dnorm(PR.ORTH16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fourth panel 2
PR.ORTH16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit8 = fitG(PR.ORTH16.cs$julianday, PR.ORTH16.cs$fracSurveys, weighted.mean(PR.ORTH16.cs$julianday, PR.ORTH16.cs$fracSurveys),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit8$par
r2 = cor(PR.ORTH16.cs$julianday, p[3]*dnorm(PR.ORTH16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.ORTH16.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot


# Fifth panel 1
PR.BIRD15.sci = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.5), ylab = "Mean density of bird food", main = '')

# Fit a normal curve using least squares
gfit9 = fitG(PR.BIRD15.sci$julianday, PR.BIRD15.sci$fracSurveys, weighted.mean(PR.BIRD15.sci$julianday, PR.BIRD15.sci$fracSurveys),
             2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit9$par
r2 = cor(PR.BIRD15.sci$julianday, p[3]*dnorm(PR.BIRD15.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Fifth panel 2
PR.BIRD15.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit10 = fitG(PR.BIRD15.cs$julianday, PR.BIRD15.cs$fracSurveys, weighted.mean(PR.BIRD15.cs$julianday, PR.BIRD15.cs$fracSurveys),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit10$par
r2 = cor(PR.BIRD15.cs$julianday, p[3]*dnorm(PR.BIRD15.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD15.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 1
PR.BIRD16.sci = meanDensityByDay(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(130,207), ylim = c(0,1.2), ylab = "", main = '')

# Fit a normal curve using least squares
gfit11 = fitG(PR.BIRD16.sci$julianday, PR.BIRD16.sci$fracSurveys, weighted.mean(PR.BIRD16.sci$julianday, PR.BIRD16.sci$fracSurveys),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit11$par
r2 = cor(PR.BIRD16.sci$julianday, p[3]*dnorm(PR.BIRD16.sci$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.sci$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot

# Sixth panel 2
PR.BIRD16.cs = meanDensityByDay(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 2)

# Fit a normal curve using least squares
gfit12 = fitG(PR.BIRD16.cs$julianday, PR.BIRD16.cs$fracSurveys, weighted.mean(PR.BIRD16.cs$julianday, PR.BIRD16.cs$fracSurveys),
              2, 2, control = list(maxit = 10000), method="L-BFGS-B", lower=c(0,0,0,0,0,0))

# Curve parameters
p = gfit12$par
r2 = cor(PR.BIRD16.cs$julianday, p[3]*dnorm(PR.BIRD16.cs$julianday, p[1], p[2]))^2
totalDensity = sum(PR.BIRD16.cs$fracSurveys)
lines(130:207, p[3]*dnorm(130:207, p[1], p[2]), col = 'blue') # make sure it appears on the right plot



#----FIGURE 4 PHENOLOGY PLOT----
# Is this plot useful? Can't run now with missing script

fracval = read.csv("c:/git/caterpillars-count-analysis/output/tabular/gaussian_fracsurveys.csv")

par(mar = c(4,4,2,2))
source('C:/git/caterpillars-count-analysis/analysis_scripts/prism_10year.R')

source('C:/git/caterpillars-count-analysis/analysis_scripts/modis_10year.R')

source('C:/git/caterpillars-count-analysis/extensive_plotting.R') # deleted this script earlier

setwd('c:/git/caterpillars-count-analysis')

# Is greenup affected by GDD?
# Working with two datasets (one from prism_10year.R and one from modis_10year.R)

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)


#---- GDD and greenup (deviation from mean) ----
par(mar = c(4,4,2,2), mfrow = c(3,2))
plot(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, col = 'white',
     xlab = 'GDD JD deviation from mean', ylab = 'Greenup JD deviation from mean',
     xlim = c(-11,11), ylim = c(-10,10), main = '')
#abline(1,0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
text(greenupgdd$pr.gdd.dev, greenupgdd$prgreenup.dev, greenupgdd$year)
#text(greenupgdd$bg.gdd.dev, greenupgdd$bggreenup.dev, greenupgdd$year, col = 'blue')
legend("topleft", "A", bty="n")

# Blank plot for pdf
plot.new()
legend('bottomleft', c('2015','2016'), pch = c(1, 16), cex = 1.5)
legend('topleft', c('Visual Survey', 'Beat Sheet'), pch = c(16, 15), col = c('darkorchid2', 'gold3'), cex = 1.5)

#---- Arths and greenup - visual surveys----
par(mar = c(4,4,2,2))


## Greenup with arth
plot(0,bty='n',pch='',ylab='Caterpillar peak JD',xlab='Greenup JD', ylim = c(170,205), xlim = c(85, 95),
     main = '')
# Visual LEPL
maxden1 <- fracval[1,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       maxden1, type = 'p', col = 'darkorchid2', cex = 2)
maxden2 <- fracval[2,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       maxden2, type = 'p', col = 'darkorchid2', pch = 16, cex = 2)
# Beat Sheet LEPL
maxden3 <- fracval[4,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       maxden3, type = 'p', col = 'gold3', cex = 2, pch = 22)
maxden4 <- fracval[5,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       maxden4, type = 'p', col = 'gold3', pch = 15, cex = 2)
legend("topleft", "B", bty="n")
# Segments
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = maxden1, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = maxden2, 
         col = 'darkorchid2', lwd = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = maxden3, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = maxden4, 
         col = 'gold3', lwd = 2)

plot(0,bty='n',pch='',ylab='Orthoptera peak JD',xlab='Greenup JD', ylim = c(170,205), xlim = c(85, 95),
     main = '')
# Visual ORTH
maxden5 <- fracval[7,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       maxden5, type = 'p', col = 'darkorchid2', cex = 2)
maxden6 <- fracval[8,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       maxden6, type = 'p', col = 'darkorchid2', cex = 2, pch = 16)
# Beat Sheet ORTH
maxden7 <- fracval[10,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       maxden7, type = 'p', col = 'gold3', cex = 2, pch = 22)
maxden8 <- fracval[11,5]
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       maxden8, type = 'p', col = 'gold3', cex = 2, pch = 15)
# Segments
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = maxden5, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = maxden6, 
         col = 'darkorchid2', lwd = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = maxden7, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = maxden8, 
         col = 'gold3', lwd = 2)
legend("topleft", "C", bty="n")


## Greenup with GDD
plot(0,bty='n',pch='',ylab='Caterpillar peak JD',xlab='GDD JD', ylim = c(170,205), xlim = c(155, 165),
     main = '')
# Visual LEPL
maxden1 <- fracval[1,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       maxden1, type = 'p', col = 'darkorchid2', cex = 2)
maxden2 <- fracval[2,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       maxden2, type = 'p', col = 'darkorchid2', cex = 2, pch = 16)
# Beat Sheet LEPL
maxden3 <- fracval[4,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       maxden3, type = 'p', col = 'gold3', cex = 2, pch = 22)
maxden4 <- fracval[5,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       maxden4, type = 'p', col = 'gold3', cex = 2, pch = 15)
# Segments
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = maxden1, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = maxden2, 
         col = 'darkorchid2', lwd = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = maxden3, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = maxden4, 
         col = 'gold3', lwd = 2)
legend("topleft", "D", bty="n")

plot(0,bty='n',pch='',ylab='Orthoptera peak JD',xlab='GDD JD', ylim = c(170,205), xlim = c(155, 165),
     main = '')
# Visual ORTH
maxden5 <- fracval[7,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       maxden5, type = 'p', col = 'darkorchid2', cex = 2)
maxden6 <- fracval[8,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       maxden6, type = 'p', col = 'darkorchid2', cex = 2, pch = 16)
# Beat Sheet ORTH
maxden7 <- fracval[10,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       maxden7, type = 'p', col = 'gold3', cex = 2, pch = 22)
maxden8 <- fracval[11,5]
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       maxden8, type = 'p', col = 'gold3', cex = 2, pch = 15)
# Segments
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = maxden5, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = maxden6, 
         col = 'darkorchid2', lwd = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = maxden7, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = maxden8, 
         col = 'gold3', lwd = 2)
legend("topleft", "E", bty="n")

