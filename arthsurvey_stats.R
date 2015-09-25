# Linear regression, ANCOVA, for Prairie Ridge and NC Botanical Garden Arthropod Data

# Tracie Hayes
# September 23, 2015

# Run summary_functions.r
source('summary_functions.r')
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

# As above but for weekly averages
# Just caterpillars:
PRall.lepl1.wk = merge(PRam.lepl.wk[,c('week','meanDensity')], PRbs.lepl.wk[, c('week','meanDensity')], by='week', all = T)
names(PRall.lepl1.wk) = c('week','density_am','density_bs')
PRall.lepl2.wk = merge(PRall.lepl1.wk, PRpm.lepl.wk[,c('week','meanDensity')], by = 'week', all = T)
names(PRall.lepl2.wk)[4] = 'density_pm'
PRall.lepl.wk = merge(PRall.lepl2.wk, PRvol.lepl.wk[,c('week','meanDensity')], by = 'week', all = T)
names(PRall.lepl.wk)[5] = 'density_vol'

# Selected orders:
PRall.mult1.wk = merge(PRam.mult.wk[,c('week','meanDensity')], PRbs.mult.wk[, c('week','meanDensity')], by='week', all = T)
names(PRall.mult1.wk) = c('week','density_am','density_bs')
PRall.mult2.wk = merge(PRall.mult1.wk, PRpm.mult.wk[,c('week','meanDensity')], by = 'week', all = T)
names(PRall.mult2.wk)[4] = 'density_pm'
PRall.mult.wk = merge(PRall.mult2.wk, PRvol.mult.wk[,c('week','meanDensity')], by = 'week', all = T)
names(PRall.mult.wk)[5] = 'density_vol'


# Matching by exact julian day

## Linear regression plots for just caterpillars

# am bs
# Prairie Ridge
par(mfcol = c(3, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 4, 0))
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

mtext("Arthropod density", 3, outer = T, at = 0.75, cex = 1.25)

#---------------------------------------------------------------------------------------------
# Matching by week (gives us more comparisons)
# Linear regression plots for just caterpillars

r2p.cex = 1.5
pdf('plots/pairwise_sampling_plots.pdf', height = 10, width = 8)
par(mfcol = c(3, 2), mar = c(5, 5, 1, 2), oma = c(0, 0, 4, 0), cex.lab = 2)
plot(PRall.lepl.wk$density_am, PRall.lepl.wk$density_bs, pch = 16, cex = 2, col = 'blue', xlab = "am Visual", ylab = "am Beat sheet")
lm.PRlepl.wk.ambs = lm(PRall.lepl.wk$density_bs ~ PRall.lepl.wk$density_am)
abline(lm.PRlepl.wk.ambs)
R2.1 = summary(lm.PRlepl.wk.ambs)$r.squared
p.1 = summary(lm.PRlepl.wk.ambs)$coefficients[2,4]
text(0.015, 0.18, bquote(R^2 == .(round(R2.1, 2))), cex = r2p.cex)
text(0.015, 0.15, bquote(p == .(round(p.1, 2))), cex = r2p.cex)

plot(PRall.lepl.wk$density_am, PRall.lepl.wk$density_pm, pch = 16, cex = 2, col = 'red', xlab = "am Visual", ylab = "pm Visual")
lm.PRlepl.wk.ampm = lm(PRall.lepl.wk$density_pm ~ PRall.lepl.wk$density_am)
abline(lm.PRlepl.wk.ampm)
summary(lm.PRlepl.wk.ampm)
R2.2 = summary(lm.PRlepl.wk.ampm)$r.squared
p.2 = summary(lm.PRlepl.wk.ampm)$coefficients[2,4]
text(0.015, 0.11, bquote(R^2 == .(round(R2.2, 2))), cex = r2p.cex)
text(0.015, 0.09, bquote(p == .(round(p.2, 2))), cex = r2p.cex)

plot(PRall.lepl.wk$density_pm, PRall.lepl.wk$density_vol, pch = 16, cex = 2, col = 'darkgreen', xlab = "pm Visual", ylab = "pm Volunteers", ylim = c(0,0.12))
lm.PRlepl.wk.pmvol = lm(PRall.lepl.wk$density_vol ~ PRall.lepl.wk$density_pm)
abline(lm.PRlepl.wk.pmvol)
summary(lm.PRlepl.wk.pmvol)
R2.3 = summary(lm.PRlepl.wk.pmvol)$r.squared
p.3 = summary(lm.PRlepl.wk.pmvol)$coefficients[2,4]
text(0.015, 0.11, bquote(R^2 == .(round(R2.3, 2))), cex = r2p.cex)
text(0.015, 0.09, bquote(p == .(round(p.3, 2))), cex = r2p.cex)

mtext("Caterpillar density", 3, outer = T, at = 0.25, cex = 1.75)


# Linear regression plots for selected arthropods

plot(PRall.mult.wk$density_am, PRall.mult.wk$density_bs, pch = 16, cex = 2, col = 'blue', xlab = "am Visual", ylab = "am Beat sheet")
lm.PRmult.wk.ambs = lm(PRall.mult.wk$density_bs ~ PRall.mult.wk$density_am)
abline(lm.PRmult.wk.ambs)
summary(lm.PRmult.wk.ambs)
R2.4 = summary(lm.PRmult.wk.ambs)$r.squared
p.4 = summary(lm.PRmult.wk.ambs)$coefficients[2,4]
text(0.55, 0.30, bquote(R^2 == .(round(R2.4, 2))), cex = r2p.cex)
text(0.55, 0.2, bquote(p == .(round(p.4, 2))), cex = r2p.cex)

plot(PRall.mult.wk$density_am, PRall.mult.wk$density_pm, pch = 16, cex = 2, col = 'red', xlab = "am Visual", ylab = "pm Visual")
lm.PRmult.wk.ampm = lm(PRall.mult.wk$density_pm ~ PRall.mult.wk$density_am)
abline(lm.PRmult.wk.ampm)
summary(lm.PRmult.wk.ampm)
R2.5 = summary(lm.PRmult.wk.ampm)$r.squared
p.5 = summary(lm.PRmult.wk.ampm)$coefficients[2,4]
text(0.55, 0.30, bquote(R^2 == .(round(R2.5, 2))), cex = r2p.cex)
text(0.55, 0.2, bquote(p == .(round(p.5, 2))), cex = r2p.cex)

plot(PRall.mult.wk$density_pm, PRall.mult.wk$density_vol, pch = 16, cex = 2, col = 'darkgreen', xlab = "pm Visual", ylab = "pm Volunteers")
lm.PRmult.wk.pmvol = lm(PRall.mult.wk$density_vol ~ PRall.mult.wk$density_pm)
abline(lm.PRmult.wk.pmvol)
summary(lm.PRmult.wk.pmvol)
R2.6 = summary(lm.PRmult.wk.pmvol)$r.squared
p.6 = summary(lm.PRmult.wk.pmvol)$coefficients[2,4]
text(0.7, 0.30, bquote(R^2 == .(round(R2.6, 2))), cex = r2p.cex)
text(0.7, 0.2, bquote(p == .(round(p.6, 2))), cex = r2p.cex)

mtext("Arthropod density", 3, outer = T, at = 0.75, cex = 1.75)
dev.off()
