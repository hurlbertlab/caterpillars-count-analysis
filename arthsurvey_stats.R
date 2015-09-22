# Linear regression, ANCOVA, for Prairie Ridge and NC Botanical Garden Arthropod Data

# Tracie Hayes
# September 22, 2015

# Run summary_functions.r

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


# Linear regression
plot(PRall.lepl$density_am, PRall.lepl$density_bs)
lm.PRlepl.ambs = lm(PRall.lepl$density_bs ~ PRall.lepl$density_am)
abline(lm.PRlepl.ambs)
summary(lm.PRlepl.ambs)
