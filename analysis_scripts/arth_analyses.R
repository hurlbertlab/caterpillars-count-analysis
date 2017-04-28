######################
# Arthropod Analyses #
######################


# Function for plotting the mean density over julian days for six arthropod orders 
# (LEPL, ORTH, COLE, HETE, ARAN, AUCH) as a multiplot

arthplot = function(cleandata, site, year) {

par(mfrow = c(3, 2), mar = c(2, 1, 3, 3), oma = c(4, 4, 3, 1))

lepl = meanDensityByDay(cleandata, ordersToInclude = 'LEPL', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'LEPL')
leplquad = lm(meanDensity ~ julianday + I(julianday^2), data = lepl)
R2.quad = summary(leplquad)$r.squared
p.quad = summary(leplquad)$coefficients[2,4]
p.quad2 = summary(leplquad)$coefficients[3,4]
#text('topleft', bquote(R^2 == .(round(R2.quad, 2))), cex = r2p.cex)
#text('topleft',, bquote(p == .(round(p.quad, 2))), cex = r2p.cex)
jd = c(min(lepl$julianday):max(lepl$julianday))
leplquad.predict = leplquad$coefficients[1] + leplquad$coefficients[2]*jd + leplquad$coefficients[3]*jd^2
points(jd, leplquad.predict, type = 'l')
max.jd.leplquad = jd[leplquad.predict == max(leplquad.predict)]

orth = meanDensityByDay(cleandata, ordersToInclude = 'ORTH', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'ORTH')
orthquad = lm(meanDensity ~ julianday + I(julianday^2), data = orth)
R2.quad = summary(orthquad)$r.squared
p.quad = summary(orthquad)$coefficients[2,4]
p.quad2 = summary(orthquad)$coefficients[3,4]
jd = c(min(orth$julianday):max(orth$julianday))
orthquad.predict = orthquad$coefficients[1] + orthquad$coefficients[2]*jd + orthquad$coefficients[3]*jd^2
points(jd, orthquad.predict, type = 'l')
max.jd.orthquad = jd[orthquad.predict == max(orthquad.predict)]

cole = meanDensityByDay(cleandata, ordersToInclude = 'COLE', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'COLE')
colequad = lm(meanDensity ~ julianday + I(julianday^2), data = cole)
R2.quad = summary(colequad)$r.squared
p.quad = summary(colequad)$coefficients[2,4]
p.quad2 = summary(colequad)$coefficients[3,4]
jd = c(min(cole$julianday):max(cole$julianday))
colequad.predict = colequad$coefficients[1] + colequad$coefficients[2]*jd + colequad$coefficients[3]*jd^2
points(jd, colequad.predict, type = 'l')
max.jd.colequad = jd[colequad.predict == max(colequad.predict)]

hete = meanDensityByDay(cleandata, ordersToInclude = 'HETE', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'HETE')
hetequad = lm(meanDensity ~ julianday + I(julianday^2), data = hete)
R2.quad = summary(hetequad)$r.squared
p.quad = summary(hetequad)$coefficients[2,4]
p.quad2 = summary(hetequad)$coefficients[3,4]
jd = c(min(hete$julianday):max(hete$julianday))
hetequad.predict = hetequad$coefficients[1] + hetequad$coefficients[2]*jd + hetequad$coefficients[3]*jd^2
points(jd, hetequad.predict, type = 'l')
max.jd.hetequad = jd[hetequad.predict == max(hetequad.predict)]

aran = meanDensityByDay(cleandata, ordersToInclude = 'ARAN', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'ARAN')
aranquad = lm(meanDensity ~ julianday + I(julianday^2), data = aran)
R2.quad = summary(aranquad)$r.squared
p.quad = summary(aranquad)$coefficients[2,4]
p.quad2 = summary(aranquad)$coefficients[3,4]
jd = c(min(aran$julianday):max(aran$julianday))
aranquad.predict = aranquad$coefficients[1] + aranquad$coefficients[2]*jd + aranquad$coefficients[3]*jd^2
points(jd, aranquad.predict, type = 'l')
max.jd.aranquad = jd[aranquad.predict == max(aranquad.predict)]

auch = meanDensityByDay(cleandata, ordersToInclude = 'AUCH', inputYear = year, inputSite = site, plot = T, new = T, lwd = 2, main = 'AUCH')
auchquad = lm(meanDensity ~ julianday + I(julianday^2), data = auch)
R2.quad = summary(auchquad)$r.squared
p.quad = summary(auchquad)$coefficients[2,4]
p.quad2 = summary(auchquad)$coefficients[3,4]
jd = c(min(auch$julianday):max(auch$julianday))
auchquad.predict = auchquad$coefficients[1] + auchquad$coefficients[2]*jd + auchquad$coefficients[3]*jd^2
points(jd, auchquad.predict, type = 'l')
max.jd.auchquad = jd[auchquad.predict == max(auchquad.predict)]

title(xlab = "Julian day",
      ylab = "Mean density",
      outer = TRUE, line = 2, cex.lab = 2)

}


# Create 4-page pdf of the multiplots for visual PR, beat sheet PR, visual BG, beat sheet BG in 2015
#pdf('output/plots/arth_density_multiplots_2015.pdf', width = 7, height = 9)

arthplot(amsurvey.pr, site = 117, year = 2015)
title(main = 'PR Visual 2015', outer = T, cex.main = 2)

arthplot(beatsheet.pr, site = 117, year = 2015)
title(main = 'PR Beat Sheet 2015', outer = T, cex.main = 2)

arthplot(amsurvey.bg, site = 8892356, year = 2015)
title(main = 'BG Visual 2015', outer = T, cex.main = 2)

arthplot(beatsheet.bg, site = 8892356, year = 2015)
title(main = 'BG Beat Sheet 2015', outer = T, cex.main = 2)

#dev.off()

# Create 4-page pdf of the multiplots for visual PR, beat sheet PR, visual BG, beat sheet BG in 2016
pdf('output/plots/arth_density_multiplots_2016.pdf', width = 7, height = 9)

arthplot(labdata.pr[labdata.pr$surveyType == 'Visual',], site = 117, year = 2016)
title(main = 'PR Visual 2016', outer = T, cex.main = 2)

arthplot(labdata.pr[labdata.pr$surveyType == 'Beat_Sheet',], site = 117, year = 2016)
title(main = 'PR Beat Sheet 2016', outer = T, cex.main = 2)

arthplot(labdata.bg[labdata.bg$surveyType == 'Visual' & labdata.bg$julianday %in% c(125:204),], site = 8892356, year = 2016)
title(main = 'BG Visual 2016', outer = T, cex.main = 2)

arthplot(labdata.bg[labdata.bg$surveyType == 'Beat_Sheet',], site = 8892356, year = 2016)
title(main = 'BG Beat Sheet 2016', outer = T, cex.main = 2)

#dev.off()



#-----------------------------------------------------------------------------------------


# Comparing peaks between orders
LEPLprbs = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'LEPL', inputYear = 2015, inputSite = 117)
ORTHprbs = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'ORTH', inputYear = 2015, inputSite = 117)
AUCHprbs = meanDensityByDay(beatsheet.pr, effort = effortByDay, ordersToInclude = 'AUCH', inputYear = 2015, inputSite = 117)

par(mfrow = c(1, 3))

LEPLORTH = merge(LEPLprbs, ORTHprbs, by = 'julianday', all = F)
plot(LEPLORTH$meanDensity.x, LEPLORTH$meanDensity.y, main = 'LEPL & ORTH')
LEPLORTHlm = lm(LEPLORTH$meanDensity.y ~ LEPLORTH$meanDensity.x)
abline(LEPLORTHlm)

LEPLAUCH = merge(LEPLprbs, AUCHprbs, by = 'julianday', all = F)
plot(LEPLAUCH$meanDensity.x, LEPLAUCH$meanDensity.y, main = 'LEPL & AUCH')
LEPLAUCHlm = lm(LEPLAUCH$meanDensity.y ~ LEPLAUCH$meanDensity.x)
abline(LEPLAUCHlm)

ORTHAUCH = merge(ORTHprbs, AUCHprbs, by = 'julianday', all = F)
plot(ORTHAUCH$meanDensity.x, ORTHAUCH$meanDensity.y, main = 'ORTH & AUCH')
ORTHAUCHlm = lm(ORTHAUCH$meanDensity.y ~ ORTHAUCH$meanDensity.x)
abline(ORTHAUCHlm)

