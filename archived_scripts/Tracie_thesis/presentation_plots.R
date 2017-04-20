# Plots for honors presentation

# Note that I only presented beat sheet data of arthropods -TEH

# Will need to set working directory to the caterpillars-count-analysis repository

source('analysis_scripts/prism_10year.R') # GDDs

source('analysis_scripts/modis_10year.R') # Spring green-up

source('archived_scripts/Tracie_thesis/spline_thesis.R') # Arths peaks

source('analysis_scripts/eBird_logistics.R') # Bird arrival


#---- PPT results figure 1 ----

# Create pdf

#pdf(file = 'output/plots/thesis_plots_tracie/pptresultsfigure1.pdf', width = 7, height = 4.5)


# PR by week phenology plots
par(mfrow = c(2,3), mar = c(2,2,2,2), oma = c(4,4,2,2))

# First panel
PR.LEPL15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 1, 
                                 xlim = c(135,209), ylim = c(0,.25), ylab = "", main = 'Caterpillars', col = 'deeppink3')
BG.LEPL15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, 
                                 col = 'steelblue3')
legend('topright', c('PR', 'BG'), lwd = 2, lty = 1, col = c('deeppink3', 'steelblue3'))
l = legend("topleft", "A", bty="n")

# Second panel
PR.ORTH15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 1, 
                                 xlim = c(135,209), ylim = c(0,.25), ylab = "", col = 'deeppink3', main = 'Orthopterans')
BG.ORTH15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'steelblue3')
l = legend("topleft", "B", bty="n")

# Third panel
PR.BIRD15.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2, lty = 1, 
                                 xlim = c(135,209), ylim = c(0,.7), ylab = "", col = 'deeppink3', main = 'Bird food')
BG.BIRD15.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 1, col = 'steelblue3')
l = legend("topleft", "C", bty="n")

# Fourth panel
PR.LEPL16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,209), ylab = "", main = '', col = 'deeppink3')
BG.LEPL16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                 xlim = c(135,209), ylab = "", col = 'steelblue3')
l = legend("topleft", "D", bty="n")

# Fifth panel
PR.ORTH16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,209), ylim = c(0,.25), ylab = "", col = 'deeppink3')
BG.ORTH16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2,
                                 xlim = c(135,209), ylab = "", col = 'steelblue3')
l = legend("topleft", "E", bty="n")

# Sixth panel
PR.BIRD16.bs = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                 xlim = c(135,209), ylim = c(0,.7), ylab = "", col = 'deeppink3')
BG.BIRD16.bs = meanDensityByWeek(beatsheet.bg[beatsheet.bg$surveyType == 'Beat_Sheet' & beatsheet.bg$julianday %in% c(134:204),], 
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, col = 'steelblue3',
                                 xlim = c(135,209), ylab = "")
l = legend("topleft", "F", bty="n")

mtext("Occurrence (fraction of surveys)", side = 2, outer = TRUE, line = 1.5)
mtext("Julian day", side = 1, outer = TRUE, line = 2)

#dev.off()



#---- PPT results figure 4 ----


#pdf(file = 'output/plots/thesis_plots_tracie/pptresultsfigure4.pdf', width = 6.5, height = 3.5)
par(mar = c(4,2,2,2), mfrow = c(1,2), oma = c(1,4,1,1))


# Arths and GDD
# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='GDD (Julian day)', xlim = c(156,164), ylim = c(150, 215),
     main = '')
legend("topleft", "A", bty="n")
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.LEPL15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.BIRD15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2015], 
       PR.ORTH15.bs.max, 
       pch = 21, type = 'p', cex = 2, col = 'plum')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.LEPL16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'seagreen3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.BIRD16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'orange3')
points(greenupgdd$pr.gdd[greenupgdd$year == 2016], 
       PR.ORTH16.bs.max, 
       pch = 16, type = 'p', cex = 2, col = 'plum')
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$pr.gdd[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.bs.max, 
         x1 = greenupgdd$pr.gdd[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.LEPL15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.BIRD15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2015], 
       BG.ORTH15.bs.max, 
       pch = 22, type = 'p', cex = 1.5, col = 'plum')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.LEPL16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'seagreen3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.BIRD16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'orange3')
points(greenupgdd$bg.gdd[greenupgdd$year == 2016], 
       BG.ORTH16.bs.max, 
       pch = 15, type = 'p', cex = 1.5, col = 'plum')
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$bg.gdd[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.bs.max, 
         x1 = greenupgdd$bg.gdd[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat', '2016 cat', '2015 mult', '2016 mult'), pch = c(21, 16, 22, 15))



# Arths and greenup

# Prairie Ridge
plot(0,bty='n',pch='',ylab='',xlab='Greenup (Julian day)', xlim = c(86,94), ylim = c(150, 215),
     main = '')
legend("topleft", "B", bty="n")
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.LEPL15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.BIRD15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       PR.ORTH15.bs.max, 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.LEPL16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.BIRD16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       PR.ORTH16.bs.max, 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.LEPL15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.BIRD15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = PR.ORTH15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.LEPL15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.BIRD15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
       BG.ORTH15.bs.max, 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.LEPL16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.BIRD16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
       BG.ORTH16.bs.max, 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.LEPL15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.BIRD15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(x0 = greenupgdd$prgreenup.log[greenupgdd$year == 2015], 
         y0 = BG.ORTH15.bs.max, 
         x1 = greenupgdd$prgreenup.log[greenupgdd$year == 2016], 
         y1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)
#legend('topright', c('2015 cat','2015 mult', '2016 cat', '2016 mult'), pch = c(21, 22, 16, 15))

mtext("Arthropod peak (Julian day)", side = 2, outer = TRUE, line = 1.5)

#dev.off()


#---- PPT results figure 5 ----

# Birds and GDD/Greenup

greenupgdd <- merge(gddyear, greenup, by = 'year')

# Calculate deviation from mean of greenup and gdd at each site
greenupgdd$pr.gdd.dev <- greenupgdd$pr.gdd - mean(greenupgdd$pr.gdd)
greenupgdd$bg.gdd.dev <- greenupgdd$bg.gdd - mean(greenupgdd$bg.gdd)
greenupgdd$prgreenup.dev <- greenupgdd$prgreenup.log - mean(greenupgdd$prgreenup.log)
greenupgdd$bggreenup.dev <- greenupgdd$bggreenup.log - mean(greenupgdd$bggreenup.log)

#pdf(file = 'output/plots/thesis_plots_tracie/pptresultsfigure5.pdf', width = 6.5, height = 3.5)

par(mar = c(4,2,2,2), mfrow = c(1,2), oma = c(1,3,1,1))


# Botanical Garden GDD

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bg.gdd, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = '', xlab = 'GDD (Julian day)', ylab = '')
legend("topleft", "A", bty="n")
inbu_lm_bggdd = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bg.gdd, weights = 1/inbu_bg$confint)
abline(inbu_lm_bggdd, col = 'dodgerblue3')
summary(inbu_lm_bggdd)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bg.gdd, as.numeric(as.character(revi_bg$inflection_pt)), pch = 24, col = 'red')
revi_lm_bggdd = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bg.gdd, weights = 1/revi_bg$confint)
abline(revi_lm_bggdd, col = 'red')
summary(revi_lm_bggdd)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bg.gdd, as.numeric(as.character(coye_bg$inflection_pt)), pch = 15, col = 'orange')
coye_lm_bggdd = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bg.gdd, weights = 1/coye_bg$confint)
abline(coye_lm_bggdd, col = 'orange')
summary(coye_lm_bggdd)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_bg$bg.gdd, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_bggdd = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bg.gdd, weights = 1/bggn_bg$confint)
abline(bggn_lm_bggdd, col = 'gray67')
summary(bggn_lm_bggdd)


# Botanical Garden Greenup

inf_bg_inbu = inflection_bg[inflection_bg$scientific_name == 'Passerina cyanea',]
inbu_bg = merge(inf_bg_inbu, greenupgdd, by = 'year', all = FALSE)
plot(inbu_bg$bggreenup.log, as.numeric(as.character(inbu_bg$inflection_pt)), pch = 16, col = 'dodgerblue3', 
     ylim = c(80, 119), main = '', xlab = 'Greenup (Julian day)', ylab = '')
legend("topleft", "B", bty="n")
inbu_lm_bggreen = lm(as.numeric(as.character(inbu_bg$inflection_pt)) ~ inbu_bg$bggreenup.log, weights = 1/inbu_bg$confint)
abline(inbu_lm_bggreen, col = 'dodgerblue3')
summary(inbu_lm_bggreen)

inf_bg_revi = inflection_bg[inflection_bg$scientific_name == 'Vireo olivaceus',]
revi_bg = merge(inf_bg_revi, greenupgdd, by = 'year', all = FALSE)
points(revi_bg$bggreenup.log, as.numeric(as.character(revi_bg$inflection_pt)), pch = 24, col = 'red')
revi_lm_bggreen = lm(as.numeric(as.character(revi_bg$inflection_pt)) ~ revi_bg$bggreenup.log, weights = 1/revi_bg$confint)
abline(revi_lm_bggreen, col = 'red')
summary(revi_lm_bggreen)

inf_bg_coye = inflection_bg[inflection_bg$scientific_name == 'Geothlypis trichas',]
coye_bg = merge(inf_bg_coye, greenupgdd, by = 'year', all = FALSE)
points(coye_bg$bggreenup.log, as.numeric(as.character(coye_bg$inflection_pt)), pch = 15, col = 'orange')
coye_lm_bggreen = lm(as.numeric(as.character(coye_bg$inflection_pt)) ~ coye_bg$bggreenup.log, weights = 1/coye_bg$confint)
abline(coye_lm_bggreen, col = 'orange')
summary(coye_lm_bggreen)

inf_bg_bggn = inflection_bg[inflection_bg$scientific_name == 'Polioptila caerulea',]
bggn_bg = merge(inf_bg_bggn, greenupgdd, by = 'year', all = FALSE)
points(bggn_bg$bggreenup.log, as.numeric(as.character(bggn_bg$inflection_pt)), pch = 9, col = 'gray67')
bggn_lm_bggreen = lm(as.numeric(as.character(bggn_bg$inflection_pt)) ~ bggn_bg$bggreenup.log, weights = 1/bggn_bg$confint)
abline(bggn_lm_bggreen, col = 'gray67')
summary(bggn_lm_bggreen)

mtext("Bird arrival (Julian day)", side = 2, outer = TRUE, line = 1.5)

#dev.off()

#---- PPT results figure 5 *extra for ppt* ----

# WARNING: have to run full plot in compilation_thesis.R FIRST (not as a pdf, need PR datasets)

datalist = c(inbu_lm_prgdd, revi_lm_prgdd, coye_lm_prgdd, bggn_lm_prgdd)

birdstats = data.frame(Species = rep(c("Indigo bunting", "Red-eyed vireo", "Common yellowthroat", "Blue-gray gnatcatcher"), 4),
                       Site = c(rep('PR', 4), rep('BG', 4), rep('PR', 4), rep('BG', 4)),
                       Metric = c(rep('GDD', 8), rep('Greenup', 8)),
                       R2 = c(round(summary(inbu_lm_prgdd)$r.squared, 2), round(summary(revi_lm_prgdd)$r.squared, 2),
                              round(summary(coye_lm_prgdd)$r.squared, 2), round(summary(bggn_lm_prgdd)$r.squared, 2),
                              round(summary(inbu_lm_bggdd)$r.squared, 2), round(summary(revi_lm_bggdd)$r.squared, 2),
                              round(summary(coye_lm_bggdd)$r.squared, 2), round(summary(bggn_lm_bggdd)$r.squared, 2),
                              round(summary(inbu_lm_prgreen)$r.squared, 2), round(summary(revi_lm_prgreen)$r.squared, 2),
                              round(summary(coye_lm_prgreen)$r.squared, 2), round(summary(bggn_lm_prgreen)$r.squared, 2),
                              round(summary(inbu_lm_bggreen)$r.squared, 2), round(summary(revi_lm_bggreen)$r.squared, 2),
                              round(summary(coye_lm_bggreen)$r.squared, 2), round(summary(bggn_lm_bggreen)$r.squared, 2)),
                       p = c(round(summary(inbu_lm_prgdd)$coefficients[2,4], 2), round(summary(revi_lm_prgdd)$coefficients[2,4], 2),
                             round(summary(coye_lm_prgdd)$coefficients[2,4], 2), round(summary(bggn_lm_prgdd)$coefficients[2,4], 2),
                             round(summary(inbu_lm_bggdd)$coefficients[2,4], 2), round(summary(revi_lm_bggdd)$coefficients[2,4], 2),
                             round(summary(coye_lm_bggdd)$coefficients[2,4], 2), round(summary(bggn_lm_bggdd)$coefficients[2,4], 2),
                             round(summary(inbu_lm_prgreen)$coefficients[2,4], 2), round(summary(revi_lm_prgreen)$coefficients[2,4], 2),
                             round(summary(coye_lm_prgreen)$coefficients[2,4], 2), round(summary(bggn_lm_prgreen)$coefficients[2,4], 2),
                             round(summary(inbu_lm_bggreen)$coefficients[2,4], 2), round(summary(revi_lm_bggreen)$coefficients[2,4], 2),
                             round(summary(coye_lm_bggreen)$coefficients[2,4], 2), round(summary(bggn_lm_bggreen)$coefficients[2,4], 2)))

#pdf(file = 'output/plots/thesis_plots_tracie/pptresultsfigure5extra.pdf', width = 4, height = 4)

par(mar = c(4,4,2,2), mfrow = c(1,1), oma = c(1,1,1,1))

R2plot <- data.frame(Species = birdstats$Species[1:8], GDDR2 = birdstats$R2[1:8], greenupR2 = birdstats$R2[9:16])
plot(R2plot$GDDR2[1], R2plot$greenupR2[1], pch = 16, col = 'dodgerblue3', xlim = c(0,1), ylim = c(0,1), 
     xlab = "GDD R ", ylab = 'Greenup R ', cex = 2)
abline(0,1, lty = 2)
points(R2plot$GDDR2[5], R2plot$greenupR2[5], pch = 16, col = 'dodgerblue3', cex = 2)
points(R2plot$GDDR2[2], R2plot$greenupR2[2], pch = 24, col = 'red', cex = 2)
points(R2plot$GDDR2[6], R2plot$greenupR2[6], pch = 24, col = 'red', cex = 2)
points(R2plot$GDDR2[3], R2plot$greenupR2[3], pch = 15, col = 'orange', cex = 2)
points(R2plot$GDDR2[7], R2plot$greenupR2[7], pch = 15, col = 'orange', cex = 2)
points(R2plot$GDDR2[4], R2plot$greenupR2[4], pch = 9, col = 'gray67', cex = 2)
points(R2plot$GDDR2[8], R2plot$greenupR2[8], pch = 9, col = 'gray67', cex = 2)

#dev.off()





#---- PPT results figure 6 ----


#pdf(file = 'output/plots/thesis_plots_tracie/pptresultsfigure6.pdf', width = 6.5, height = 5)

par(mfrow = c(2,2), mar = c(0,4,2,2), oma = c(5,5,3,3))

# INDIGO BUNTING <3

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Indigo bunting',xlab='', xlim = c(150,210), ylim = c(111, 115),
     main = '')
legend("topleft", "A", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Passerina cyanea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Passerina cyanea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# RED-EYED VIREO

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Red-eyed vireo',xlab='', xlim = c(150,210), ylim = c(99, 106),
     main = '')
legend("topleft", "B", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Vireo olivaceus' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Vireo olivaceus' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# COMMON YELLOWTHROAT

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Common yellowthroat',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "C", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Geothlypis trichas' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Geothlypis trichas' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

# BLUE-GRAY GNATCATCHER

# Beat sheet
par(mar = c(2,4,2,2))
# Prairie Ridge
plot(0,bty='n',pch='',ylab='Blue-gray gnatcatcher',xlab='', xlim = c(150,210), ylim = c(80, 97),
     main = '')
legend("topleft", "D", bty="n")
points(PR.LEPL15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH15.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
       pch = 21, type = 'p', cex = 2,  col = 'plum')
points(PR.LEPL16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'seagreen3')
points(PR.BIRD16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'orange3')
points(PR.ORTH16.bs.max, inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
       pch = 16, type = 'p', cex = 2,  col = 'plum')
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.LEPL15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.BIRD15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.BIRD16.bs.max, 
         col = 'orange3', lwd = 1.5, lty = 2)
segments(y0 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2015], 
         x0 = PR.ORTH15.bs.max, 
         y1 = inflection_pr$inflection_pt[inflection_pr$scientific_name == 'Polioptila caerulea' & inflection_pr$year == 2016], 
         x1 = PR.ORTH16.bs.max, 
         col = 'plum', lwd = 1.5, lty = 2)
# Botanical Garden
points(BG.LEPL15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH15.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
       pch = 22, type = 'p', cex = 1.5,  col = 'plum')
points(BG.LEPL16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'seagreen3')
points(BG.BIRD16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'orange3')
points(BG.ORTH16.bs.max, inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
       pch = 15, type = 'p', cex = 1.5,  col = 'plum')
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.LEPL15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.LEPL16.bs.max, 
         col = 'seagreen3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.BIRD15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.BIRD16.bs.max, 
         col = 'orange3', lwd = 2, lty = 1)
segments(y0 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2015], 
         x0 = BG.ORTH15.bs.max, 
         y1 = inflection_bg$inflection_pt[inflection_bg$scientific_name == 'Polioptila caerulea' & inflection_bg$year == 2016], 
         x1 = BG.ORTH16.bs.max, 
         col = 'plum', lwd = 2, lty = 1)

mtext("Bird arrival (Julian day)", side = 2, outer = TRUE, line = 1.5)
mtext("Arthropod peak (Julian day)", side = 1, outer = TRUE, line = 2)

#dev.off()


