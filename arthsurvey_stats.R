# Linear regression, ANCOVA, for Prairie Ridge and NC Botanical Garden Arthropod Data

# Tracie Hayes
# September 23, 2015

# Run summary_functions.r
library(lattice)

# Merge mean densities into one dataframe

# Just caterpillars:
PRall.lepl1 = merge(PRam.lepl[,c('julianday','meanDensity')], PRbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.lepl1) = c('julianday','density_am','density_bs')
PRall.lepl2 = merge(PRall.lepl1, PRpm.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl2)[4] = 'density_pm'
PRall.lepl = merge(PRall.lepl2, PRvol.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl)[5] = 'density_vol'

# Selected orders:
PRall.mult1 = merge(PRam.mult[,c('julianday','meanDensity')], PRbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.mult1) = c('julianday','density_am','density_bs')
PRall.mult2 = merge(PRall.mult1, PRpm.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult2)[4] = 'density_pm'
PRall.mult = merge(PRall.mult2, PRvol.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult)[5] = 'density_vol'


# Linear regression plots for just caterpillars

plot(PRall.lepl$density_am, PRall.lepl$density_bs, xlab = "am Visual", ylab = "am Beat sheet")
lm.PRlepl.ambs = lm(PRall.lepl$density_bs ~ PRall.lepl$density_am)
abline(lm.PRlepl.ambs)
summary(lm.PRlepl.ambs)
text(0.05, 0.24, expression(R^2 ==  0.1825))
text(0.05, 0.22, "p-value = 0.06027")

plot(PRall.lepl$density_am, PRall.lepl$density_pm, xlab = "am Visual", ylab = "pm Visual")
lm.PRlepl.ampm = lm(PRall.lepl$density_pm ~ PRall.lepl$density_am)
abline(lm.PRlepl.ampm)
summary(lm.PRlepl.ampm)
text(0.05, 0.11, expression(R^2 == 0.003973))
text(0.05, 0.10, "p-value = 0.8821")

plot(PRall.lepl$density_pm, PRall.lepl$density_vol, xlab = "pm Visual", ylab = "pm Volunteers")
lm.PRlepl.pmvol = lm(PRall.lepl$density_vol ~ PRall.lepl$density_pm)
abline(lm.PRlepl.pmvol)
summary(lm.PRlepl.pmvol)
text(0.05, 0.45, expression(R^2 == 0.0279))
text(0.05, 0.40, "p-value = 0.7518")


# Linear regression plots for selected arthropods

plot(PRall.mult$density_am, PRall.mult$density_bs, xlab = "am Visual", ylab = "am Beat sheet")
lm.PRmult.ambs = lm(PRall.mult$density_bs ~ PRall.mult$density_am)
abline(lm.PRmult.ambs)
summary(lm.PRmult.ambs)
text(0.3, 1.0, expression(R^2 ==  0.1354))
text(0.3, 0.9, "p-value = 0.1105")

plot(PRall.mult$density_am, PRall.mult$density_pm, xlab = "am Visual", ylab = "pm Visual")
lm.PRmult.ampm = lm(PRall.mult$density_pm ~ PRall.mult$density_am)
abline(lm.PRmult.ampm)
summary(lm.PRmult.ampm)
text(0.3, 0.65, expression(R^2 == 0.1221))
text(0.3, 0.6, "p-value = 0.3963")

plot(PRall.mult$density_pm, PRall.mult$density_vol, xlab = "pm Visual", ylab = "pm Volunteers")
lm.PRmult.pmvol = lm(PRall.mult$density_vol ~ PRall.mult$density_pm)
abline(lm.PRmult.pmvol)
summary(lm.PRmult.pmvol)
text(0.3, 1.2, expression(R^2 == 0.7048))
text(0.3, 1.1, "p-value = 0.03657")


