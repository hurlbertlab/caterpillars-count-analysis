# Script for estimating peaks based on varying surveying methods and varying
# number of surveys a month.
# Both quadratic and cubic fits.

# Run summary_functions.r, data_cleaning.R, general_plotting.R
source('summary_functions.r')
source('data_cleaning.R')
source('general_plotting.R')

#------------------------------------------------------------------------------------------------------------
### Quadratic Fit

jd = c(130:210)

## Caterpillars only

#Prairie Ridge, 8 surveys a month
jd1 = PRall.lepl$julianday

PRlepl.am.quad = lm(PRall.lepl$density_am ~ jd1 + I(jd1^2))
PRlepl.am.quad.predict = PRlepl.am.quad$coefficients[1] + PRlepl.am.quad$coefficients[2]*jd1 + PRlepl.am.quad$coefficients[3]*jd1^2
points(jd1, PRlepl.am.quad.predict, type = 'l', col = 'blue')
PRlepl.am.quad.mday = jd1[PRlepl.am.quad.predict == max(PRlepl.am.quad.predict)]

PRlepl.bs.quad = lm(PRall.lepl$density_bs ~ jd1 + I(jd1^2))
PRlepl.bs.quad.predict = PRlepl.bs.quad$coefficients[1] + PRlepl.bs.quad$coefficients[2]*jd1 + PRlepl.bs.quad$coefficients[3]*jd1^2
points(jd1, PRlepl.bs.quad.predict, type = 'l', col = 'orange')
PRlepl.bs.quad.mday = jd1[PRlepl.bs.quad.predict == max(PRlepl.bs.quad.predict)]

PRlepl.pm.quad = lm(PRall.lepl$density_pm ~ jd1 + I(jd1^2))
PRlepl.pm.quad.predict = PRlepl.pm.quad$coefficients[1] + PRlepl.pm.quad$coefficients[2]*jd1 + PRlepl.pm.quad$coefficients[3]*jd1^2
points(jd1, PRlepl.pm.quad.predict, type = 'l', col = 'red')
PRlepl.pm.quad.mday = jd1[PRlepl.pm.quad.predict == max(PRlepl.pm.quad.predict)]

PRlepl.vol.quad = lm(PRall.lepl$density_vol ~ jd1 + I(jd1^2))
PRlepl.vol.quad.predict = PRlepl.vol.quad$coefficients[1] + PRlepl.vol.quad$coefficients[2]*jd1 + PRlepl.vol.quad$coefficients[3]*jd1^2
points(jd1, PRlepl.vol.quad.predict, type = 'l', col = 'green')
PRlepl.vol.quad.mday = jd1[PRlepl.vol.quad.predict == max(PRlepl.vol.quad.predict)]

#Botanical Garden, 8 surveys a month
jd2 = BGall.lepl$julianday

BGlepl.am.quad = lm(BGall.lepl$density_am ~ jd2 + I(jd2^2))
BGlepl.am.quad.predict = BGlepl.am.quad$coefficients[1] + BGlepl.am.quad$coefficients[2]*jd2 + BGlepl.am.quad$coefficients[3]*jd2^2
points(jd2, BGlepl.am.quad.predict, type = 'l', col = 'blue')
BGlepl.am.quad.mday = jd2[BGlepl.am.quad.predict == max(BGlepl.am.quad.predict)]

BGlepl.bs.quad = lm(BGall.lepl$density_bs ~ jd2 + I(jd2^2))
BGlepl.bs.quad.predict = BGlepl.bs.quad$coefficients[1] + BGlepl.bs.quad$coefficients[2]*jd2 + BGlepl.bs.quad$coefficients[3]*jd2^2
points(jd2, BGlepl.bs.quad.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.mday = jd2[BGlepl.bs.quad.predict == max(BGlepl.bs.quad.predict)]

#Prairie Ridge, 4 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 2)]
PRall.lepl.4 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

jd3 = PRall.lepl.4$julianday

PRlepl.am.quad.4 = lm(PRall.lepl.4$density_am ~ jd3 + I(jd3^2))
PRlepl.am.quad.4.predict = PRlepl.am.quad.4$coefficients[1] + PRlepl.am.quad.4$coefficients[2]*jd3 + PRlepl.am.quad.4$coefficients[3]*jd3^2
points(jd3, PRlepl.am.quad.4.predict, type = 'l', col = 'blue')
PRlepl.am.quad.4.mday = jd3[PRlepl.am.quad.4.predict == max(PRlepl.am.quad.4.predict)]

PRlepl.bs.quad.4 = lm(PRall.lepl.4$density_bs ~ jd3 + I(jd3^2))
PRlepl.bs.quad.4.predict = PRlepl.bs.quad.4$coefficients[1] + PRlepl.bs.quad.4$coefficients[2]*jd3 + PRlepl.bs.quad.4$coefficients[3]*jd3^2
points(jd3, PRlepl.bs.quad.4.predict, type = 'l', col = 'orange')
PRlepl.bs.quad.4.mday = jd3[PRlepl.bs.quad.4.predict == max(PRlepl.bs.quad.4.predict)]

PRlepl.pm.quad.4 = lm(PRall.lepl.4$density_pm ~ jd3 + I(jd3^2))
PRlepl.pm.quad.4.predict = PRlepl.pm.quad.4$coefficients[1] + PRlepl.pm.quad.4$coefficients[2]*jd3 + PRlepl.pm.quad.4$coefficients[3]*jd3^2
points(jd3, PRlepl.pm.quad.4.predict, type = 'l', col = 'red')
PRlepl.pm.quad.4.mday = jd3[PRlepl.pm.quad.4.predict == max(PRlepl.pm.quad.4.predict)]

PRlepl.vol.quad.4 = lm(PRall.lepl.4$density_vol ~ jd3 + I(jd3^2))
PRlepl.vol.quad.4.predict = PRlepl.vol.quad.4$coefficients[1] + PRlepl.vol.quad.4$coefficients[2]*jd3 + PRlepl.vol.quad.4$coefficients[3]*jd3^2
points(jd3, PRlepl.vol.quad.4.predict, type = 'l', col = 'green')
PRlepl.vol.quad.4.mday = jd3[PRlepl.vol.quad.4.predict == max(PRlepl.vol.quad.4.predict)]

#Botanical Garden, 4 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 2)]
BGall.lepl.4 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

jd4 = BGall.lepl.4$julianday

BGlepl.am.quad.4 = lm(BGall.lepl.4$density_am ~ jd4 + I(jd4^2))
BGlepl.am.quad.4.predict = BGlepl.am.quad.4$coefficients[1] + BGlepl.am.quad.4$coefficients[2]*jd + BGlepl.am.quad.4$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.4.predict, type = 'l', col = 'blue')
BGlepl.am.quad.4.mday = jd[BGlepl.am.quad.4.predict == max(BGlepl.am.quad.4.predict)]

BGlepl.bs.quad.4 = lm(BGall.lepl.4$density_bs ~ jd4 + I(jd4^2))
BGlepl.bs.quad.4.predict = BGlepl.bs.quad.4$coefficients[1] + BGlepl.bs.quad.4$coefficients[2]*jd + BGlepl.bs.quad.4$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.4.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.4.mday = jd[BGlepl.bs.quad.4.predict == max(BGlepl.bs.quad.4.predict)]

#Prairie Ridge, 2 surveys a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 4)]
PRall.lepl.2 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

jd5 = PRall.lepl.2$julianday

PRlepl.am.quad.2 = lm(PRall.lepl.2$density_am ~ jd5 + I(jd5^2))
PRlepl.am.quad.2.predict = PRlepl.am.quad.2$coefficients[1] + PRlepl.am.quad.2$coefficients[2]*jd + PRlepl.am.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.2.predict, type = 'l', col = 'blue')
PRlepl.am.quad.2.mday = jd[PRlepl.am.quad.2.predict == max(PRlepl.am.quad.2.predict)]

PRlepl.bs.quad.2 = lm(PRall.lepl.2$density_bs ~ jd5 + I(jd5^2))
PRlepl.bs.quad.2.predict = PRlepl.bs.quad.2$coefficients[1] + PRlepl.bs.quad.2$coefficients[2]*jd + PRlepl.bs.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.2.predict, type = 'l', col = 'orange')
PRlepl.bs.quad.2.mday = jd[PRlepl.bs.quad.2.predict == max(PRlepl.bs.quad.2.predict)]

PRlepl.pm.quad.2 = lm(PRall.lepl.2$density_pm ~ jd5 + I(jd5^2))
PRlepl.pm.quad.2.predict = PRlepl.pm.quad.2$coefficients[1] + PRlepl.pm.quad.2$coefficients[2]*jd + PRlepl.pm.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.2.predict, type = 'l', col = 'red')
PRlepl.pm.quad.2.mday = jd[PRlepl.pm.quad.2.predict == max(PRlepl.pm.quad.2.predict)]

PRlepl.vol.quad.2 = lm(PRall.lepl.2$density_vol ~ jd5 + I(jd5^2))
PRlepl.vol.quad.2.predict = PRlepl.vol.quad.2$coefficients[1] + PRlepl.vol.quad.2$coefficients[2]*jd + PRlepl.vol.quad.2$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.2.predict, type = 'l', col = 'green')
PRlepl.vol.quad.2.mday = jd[PRlepl.vol.quad.2.predict == max(PRlepl.vol.quad.2.predict)]

#Botanical Garden, 2 surveys a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 4)]
BGall.lepl.2 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

jd6 = BGall.lepl.2$julianday

BGlepl.am.quad.2 = lm(BGall.lepl.2$density_am ~ jd6 + I(jd6^2))
BGlepl.am.quad.2.predict = BGlepl.am.quad.2$coefficients[1] + BGlepl.am.quad.2$coefficients[2]*jd + BGlepl.am.quad.2$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.2.predict, type = 'l', col = 'blue')
BGlepl.am.quad.2.mday = jd[BGlepl.am.quad.2.predict == max(BGlepl.am.quad.2.predict)]

BGlepl.bs.quad.2 = lm(BGall.lepl.2$density_bs ~ jd6 + I(jd6^2))
BGlepl.bs.quad.2.predict = BGlepl.bs.quad.2$coefficients[1] + BGlepl.bs.quad.2$coefficients[2]*jd + BGlepl.bs.quad.2$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.2.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.2.mday = jd[BGlepl.bs.quad.2.predict == max(BGlepl.bs.quad.2.predict)]

#Prairie Ridge, 1 survey a month
tempjd <- PRall.lepl$julianday[seq(1, length(PRall.lepl$julianday), 8)]
PRall.lepl.1 <- PRall.lepl[PRall.lepl$julianday %in% tempjd,]

jd7 = PRall.lepl.1$julianday

PRlepl.am.quad.1 = lm(PRall.lepl.1$density_am ~ jd7 + I(jd7^2))
PRlepl.am.quad.1.predict = PRlepl.am.quad.1$coefficients[1] + PRlepl.am.quad.1$coefficients[2]*jd + PRlepl.am.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.am.quad.1.predict, type = 'l', col = 'blue')
PRlepl.am.quad.1.mday = jd[PRlepl.am.quad.1.predict == max(PRlepl.am.quad.1.predict)]

PRlepl.bs.quad.1 = lm(PRall.lepl.1$density_bs ~ jd7 + I(jd7^2))
PRlepl.bs.quad.1.predict = PRlepl.bs.quad.1$coefficients[1] + PRlepl.bs.quad.1$coefficients[2]*jd + PRlepl.bs.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.bs.quad.1.predict, type = 'l', col = 'orange')
PRlepl.bs.quad.1.mday = jd[PRlepl.bs.quad.1.predict == max(PRlepl.bs.quad.1.predict)]

PRlepl.pm.quad.1 = lm(PRall.lepl.1$density_pm ~ jd7 + I(jd7^2))
PRlepl.pm.quad.1.predict = PRlepl.pm.quad.1$coefficients[1] + PRlepl.pm.quad.1$coefficients[2]*jd + PRlepl.pm.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.pm.quad.1.predict, type = 'l', col = 'red')
PRlepl.pm.quad.1.mday = jd[PRlepl.pm.quad.1.predict == max(PRlepl.pm.quad.1.predict)]

PRlepl.vol.quad.1 = lm(PRall.lepl.1$density_vol ~ jd7 + I(jd7^2))
PRlepl.vol.quad.1.predict = PRlepl.vol.quad.1$coefficients[1] + PRlepl.vol.quad.1$coefficients[2]*jd + PRlepl.vol.quad.1$coefficients[3]*jd^2
points(jd, PRlepl.vol.quad.1.predict, type = 'l', col = 'green')
PRlepl.vol.quad.1.mday = jd[PRlepl.vol.quad.1.predict == max(PRlepl.vol.quad.1.predict)]

#Botanical Garden, 1 survey a month
tempjd <- BGall.lepl$julianday[seq(1, length(BGall.lepl$julianday), 8)]
BGall.lepl.1 <- BGall.lepl[BGall.lepl$julianday %in% tempjd,]

jd8 = BGall.lepl.1$julianday

BGlepl.am.quad.1 = lm(BGall.lepl.1$density_am ~ jd8 + I(jd8^2))
BGlepl.am.quad.1.predict = BGlepl.am.quad.1$coefficients[1] + BGlepl.am.quad.1$coefficients[2]*jd + BGlepl.am.quad.1$coefficients[3]*jd^2
points(jd, BGlepl.am.quad.1.predict, type = 'l', col = 'blue')
BGlepl.am.quad.1.mday = jd[BGlepl.am.quad.1.predict == max(BGlepl.am.quad.1.predict)]

BGlepl.bs.quad.1 = lm(BGall.lepl.1$density_bs ~ jd8 + I(jd8^2))
BGlepl.bs.quad.1.predict = BGlepl.bs.quad.1$coefficients[1] + BGlepl.bs.quad.1$coefficients[2]*jd + BGlepl.bs.quad.1$coefficients[3]*jd^2
points(jd, BGlepl.bs.quad.1.predict, type = 'l', col = 'orange')
BGlepl.bs.quad.1.mday = jd[BGlepl.bs.quad.1.predict == max(BGlepl.bs.quad.1.predict)]




PRmaxday <- c()