# Linear regression, ANCOVA, for Prairie Ridge and NC Botanical Garden Arthropod Data

# Tracie Hayes
# September 23, 2015

# Run summary_functions.r
library(lattice)

## Merge mean densities into one dataframe

# Just caterpillars:
# Prairie Ridge
PRall.lepl1 = merge(PRam.lepl[,c('julianday','meanDensity')], PRbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.lepl1) = c('julianday','density_am','density_bs')
PRall.lepl2 = merge(PRall.lepl1, PRpm.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl2)[4] = 'density_pm'
PRall.lepl = merge(PRall.lepl2, PRvol.lepl[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.lepl)[5] = 'density_vol'
# Botanical Garden
BGall.lepl = merge(BGam.lepl[,c('julianday','meanDensity')], BGbs.lepl[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.lepl) = c('julianday','density_am','density_bs')

# Selected orders:
# Prairie Ridge
PRall.mult1 = merge(PRam.mult[,c('julianday','meanDensity')], PRbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(PRall.mult1) = c('julianday','density_am','density_bs')
PRall.mult2 = merge(PRall.mult1, PRpm.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult2)[4] = 'density_pm'
PRall.mult = merge(PRall.mult2, PRvol.mult[,c('julianday','meanDensity')], by = 'julianday', all = T)
names(PRall.mult)[5] = 'density_vol'
# Botanical Garden
BGall.mult = merge(BGam.mult[,c('julianday','meanDensity')], BGbs.mult[, c('julianday','meanDensity')], by='julianday', all = T)
names(BGall.mult) = c('julianday','density_am','density_bs')

## Linear regression plots for just caterpillars

# am bs
# Prairie Ridge
plot(PRall.lepl$density_am, PRall.lepl$density_bs, xlab = "am Visual", ylab = "am Beat sheet", col = 'red')
lm.PRlepl.ambs = lm(PRall.lepl$density_bs ~ PRall.lepl$density_am)
abline(lm.PRlepl.ambs, col = 'red')
summary(lm.PRlepl.ambs)
text(0.05, 0.24, expression(R^2 ==  0.1384), col = 'red')
text(0.05, 0.22, "p-value = 0.1062", col = 'red')
title("Caterpillars")
# Botanical Garden
points(BGall.lepl$density_am, BGall.lepl$density_bs, col = 'blue', xlab = "am Visual", ylab = "am Beat sheet")
lm.BGlepl.ambs = lm(BGall.lepl$density_bs ~ BGall.lepl$density_am)
abline(lm.BGlepl.ambs, col = 'blue')
summary(lm.BGlepl.ambs)
text(0.05, 0.18, expression(R^2 ==  0.4767), col = 'blue')
text(0.05, 0.16, "p-value = 0.001516", col = 'blue')
legend("topright", c('Prairie Ridge', 'Botanical Garden'),lwd = 1, lty = 'solid', 
       col = c('red', 'blue'))

# am pm
plot(PRall.lepl$density_am, PRall.lepl$density_pm, xlab = "am Visual", ylab = "pm Visual")
lm.PRlepl.ampm = lm(PRall.lepl$density_pm ~ PRall.lepl$density_am)
abline(lm.PRlepl.ampm)
summary(lm.PRlepl.ampm)
text(0.05, 0.11, expression(R^2 == 0.06012))
text(0.05, 0.10, "p-value = 0.5584")
title("Caterpillars")

# pm vol
plot(PRall.lepl$density_pm, PRall.lepl$density_vol, xlab = "pm Visual", ylab = "pm Volunteers")
lm.PRlepl.pmvol = lm(PRall.lepl$density_vol ~ PRall.lepl$density_pm)
abline(lm.PRlepl.pmvol)
summary(lm.PRlepl.pmvol)
text(0.03, 0.4, expression(R^2 == 0.007751))
text(0.03, 0.35, "p-value = 0.8683")
title("Caterpillars")


## Linear regression plots for selected arthropods

# am bs
# Prairie Ridge
plot(PRall.mult$density_am, PRall.mult$density_bs, col = 'red', xlab = "am Visual", ylab = "am Beat sheet")
lm.PRmult.ambs = lm(PRall.mult$density_bs ~ PRall.mult$density_am)
abline(lm.PRmult.ambs, col = 'red')
summary(lm.PRmult.ambs)
text(0.3, 1.1, expression(R^2 ==  0.1354), col = 'red')
text(0.3, 1.0, "p-value = 0.1105", col ='red')
title("Selected Arthropods")
# Botanical Garden
points(BGall.mult$density_am, BGall.mult$density_bs, col = 'blue', xlab = "am Visual", ylab = "am Beat sheet")
lm.BGmult.ambs = lm(BGall.mult$density_bs ~ BGall.mult$density_am)
abline(lm.BGmult.ambs, col = 'blue')
summary(lm.BGmult.ambs)
text(0.3, 0.9, expression(R^2 ==  0.2247), col = 'blue')
text(0.3, 0.8, "p-value = 0.04691", col = 'blue')
legend("topright", c('Prairie Ridge', 'Botanical Garden'),lwd = 1, lty = 'solid', 
       col = c('red', 'blue'))

# am pm
plot(PRall.mult$density_am, PRall.mult$density_pm, xlab = "am Visual", ylab = "pm Visual")
lm.PRmult.ampm = lm(PRall.mult$density_pm ~ PRall.mult$density_am)
abline(lm.PRmult.ampm)
summary(lm.PRmult.ampm)
text(0.3, 0.65, expression(R^2 == 0.1221))
text(0.3, 0.6, "p-value = 0.3963")
title("Selected Arthropods")

# pm vol
plot(PRall.mult$density_pm, PRall.mult$density_vol, xlab = "pm Visual", ylab = "pm Volunteers")
lm.PRmult.pmvol = lm(PRall.mult$density_vol ~ PRall.mult$density_pm)
abline(lm.PRmult.pmvol)
summary(lm.PRmult.pmvol)
text(0.3, 1.2, expression(R^2 == 0.7048))
text(0.3, 1.1, "p-value = 0.03657")
title("Selected Arthropods")

