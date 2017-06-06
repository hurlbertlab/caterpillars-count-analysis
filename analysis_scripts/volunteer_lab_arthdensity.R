# Plots for CC paper: Absolute arth densities and relative arth densities 
source("cleaning_scripts/data_cleaning.R")

# Panels A-C are based on lab data from both 2015 and 2016
# Panels D-F compare volunteers to lab members from the respective year
#    (2015 comparison based on visual surveys, 2016 based on beat sheets)

# load libraries
library(tidyr)

# ---------------------------------------------------------------------------
# Specify "bird food" categories and minimum size threshold for analysis
birdfood = c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA")
minLength = 5       #only arthropods at least this long will be included
outlierDensity = 30 #counts >= to this value will be excluded as outliers
# ---------------------------------------------------------------------------


#################################################################################
# Analysis of lab data (no julianday restrictions), Panels A-C

pre_lab_vis = filter(rbind(amsurvey.pr, amsurvey.bg), year %in% c(2015, 2016))
pre_lab_bs = filter(rbind(beatsheet.pr, beatsheet.bg), year %in% c(2015, 2016))

# Total number of surveys conducted must be derived from the 'pre' dataframes
# before surveyIDs with NONE are excluded by the length criterion
num_bs_surveys = length(unique(pre_lab_bs$surveyID[pre_lab_bs$count < outlierDensity]))
num_vis_surveys = length(unique(pre_lab_vis$surveyID[pre_lab_vis$count < outlierDensity]))

lab_vis = filter(pre_lab_vis, length >= minLength)
lab_bs = filter(pre_lab_bs, length >= minLength)

###
# calculate mean number of each type of arthropods seen by tree species for lab visual surveys in 2015 & 2016
surv_tree_lab_vis = pre_lab_vis %>% distinct(date, circle, survey, realPlantSp) %>% count(realPlantSp)

# Group less common "bird food" arthropods together for plotting; 
# decided on these because AUCH, DIPT, and COLE were in the top 5 most dense for 
# beat sheets and visual, and also in "bird food", LEPL obviously necessary; 
# change less common arthropods into one "other" group 
lab_vis_oth = lab_vis
birdfood_top6 = c( "ARAN", "AUCH", "COLE", "LEPL", "ORTH", "DIPT")
lab_vis_oth$arthCode[!lab_vis_oth$arthCode %in% birdfood_top6] = 'OTHR'

lab_vis_trees = lab_vis_oth %>% 
  group_by(realPlantSp, arthCode) %>% 
  summarize(tot_count = sum(count), presence = sum(count > 0)) %>%
  left_join(surv_tree_lab_vis, by = 'realPlantSp') %>%
  mutate(mean_count = tot_count/n, occurrence = presence/n)

# Removing large colonial insect observations (e.g. caterpillars) with counts >= 30
trees = c("American beech", "Box elder", "Common persimmon", 
          "Spicebush", "Sugar maple", "Sweet gum")

lab_vis_trees_no_outliers = lab_vis_oth %>% 
  filter(count < outlierDensity) %>%
  group_by(realPlantSp, arthCode) %>% 
  summarize(tot_count = sum(count)) %>%
  left_join(surv_tree_lab_vis, by = 'realPlantSp') %>%
  mutate(mean_count = tot_count/n) %>%
  filter(realPlantSp %in% trees)

# Getting mean & standard deviations of total arthropods by tree species

treedata = pre_lab_vis %>%
  filter(count < outlierDensity) %>%
  mutate(modcount = ifelse(length >= minLength, count, 0)) %>%
  group_by(surveyID, realPlantSp) %>%
  summarize(total = sum(modcount)) %>%
  filter(realPlantSp %in% trees) 

treemod1 = lm(total ~ realPlantSp, data = treedata)
treemod2 = glm(total ~ realPlantSp, family = poisson, data = treedata)

# note that estimates are pretty much identical for both models, 
# differences slightly stronger for the glm

# p-value = 1 - pchisq(deviance, degrees of freedom) in a glm
# from https://stats.stackexchange.com/questions/108995/interpreting-residual-and-null-deviance-in-glm-r
p = 1 - pchisq(treemod2$deviance, treemod2$df.residual)

tree_means = treedata %>%
  group_by(realPlantSp) %>%
  summarize(mean = mean(total), sd = sd(total), n = n()) %>%
  mutate(ll95 = mean - 1.96*sd/(n^.5), 
         ul95 = mean + 1.96*sd/(n^.5)) %>%
  arrange(desc(mean))



# only use most common tree sp at PR: sweet gum, common persimmon, box elder, chalk maple, pin oak
common_trees = lab_vis_trees %>% 
  filter(realPlantSp %in% trees) 
common_trees_den = spread(common_trees[, c('realPlantSp', 'arthCode', 'mean_count')], realPlantSp, mean_count) %>%
  data.frame()
names(common_trees_den) = c("arthCode", trees)

common_trees_occ = spread(common_trees[, c('realPlantSp', 'arthCode', 'occurrence')], realPlantSp, occurrence) %>%
  data.frame()
names(common_trees_occ) = names(common_trees_den)

common_trees_den_no_outliers = spread(lab_vis_trees_no_outliers[, c('realPlantSp', 'arthCode', 'mean_count')], 
                                      realPlantSp, mean_count) %>% data.frame()
names(common_trees_den_no_outliers) = names(common_trees_den)


###
# Calculate % of arthropod observations by visual survey vs beat sheet

lab_bs_rel= lab_bs %>% filter(count < outlierDensity) %>%
  group_by(arthCode) %>% summarize(proportion = (sum(count)/sum(lab_bs$count[lab_bs$count < outlierDensity])))
lab_vis_rel = lab_vis %>% filter(count < outlierDensity) %>%
  group_by(arthCode) %>% summarize(proportion = (sum(count)/sum(lab_vis$count[lab_vis$count < outlierDensity])))

lab_bs_rel$arthCode[!lab_bs_rel$arthCode %in% birdfood_top6] = 'OTHR'
lab_vis_rel$arthCode[!lab_vis_rel$arthCode %in% birdfood_top6] = 'OTHR'

lab_bs_rel_oth = lab_bs_rel %>% group_by(arthCode) %>% summarize(proportion = sum(proportion))
lab_vis_rel_oth = lab_vis_rel %>% group_by(arthCode) %>% summarize(proportion = sum(proportion))

lab_selected_rel = left_join(lab_bs_rel_oth, lab_vis_rel_oth, by = 'arthCode') %>% data.frame()



###
# Calculate absolute density of arthropod orders by beat sheet vs visual survey

lab_bs_den = lab_bs %>% filter(count < outlierDensity) %>%
  group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(density = totCount/num_bs_surveys)
                                                 
lab_vis_den = lab_vis %>% filter(count < outlierDensity) %>%
  group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(density = totCount/num_vis_surveys)

lab_bs_vis_den = left_join(lab_bs_den[, c('arthCode', 'density')], 
                           lab_vis_den[, c('arthCode', 'density')], by = 'arthCode') %>%
  rename(bs_den = density.x, vis_den = density.y) %>% filter(arthCode != 'NONE') %>%
  mutate(arthCode2 = arthCode)
  
lab_bs_vis_den$arthCode2[!lab_bs_vis_den$arthCode %in% birdfood_top6] = 'OTHR'

# For chi-square test, lump all 'OTHR' together

bs_vis_comp = lab_bs_vis_den %>%
  group_by(arthCode2) %>%
  summarize(bs_den = sum(bs_den), vis_den = sum(vis_den)) %>%
  mutate(bs_tot = bs_den*num_bs_surveys, vis_tot = vis_den*num_vis_surveys)

chisq.test(bs_vis_comp[,4:5])

#Pearson's Chi-squared test

#data:  bs_vis_comp[, 4:5]
#X-squared = 284.73, df = 6, p-value < 2.2e-16

#################################################################################
# Analysis of lab data vs volunteer data, Panels D-F
# (comparison restricted to circles 1:8)

pre_lab15_vis = filter(amsurvey.pr, year == 2015, count < outlierDensity, 
                   circle %in% 1:8, julianday <= 193 & julianday >= 147)
pre_vol15_vis = filter(volunteer.pr, year == 2015, count < outlierDensity, 
                   circle %in% 1:8, julianday <= 193 & julianday >= 147)
pre_lab16_bs = filter(beatsheet.pr, year == 2016, count < outlierDensity, 
                  circle %in% 1:8, julianday <= 193 & julianday >= 147)
pre_vol16_bs = filter(volunteer.pr, year == 2016, count < outlierDensity, 
                  circle %in% 1:8, julianday <= 193 & julianday >= 147, surveyType == 'Beat_Sheet')

num_lab15vis_surveys = length(unique(pre_lab15_vis$surveyID))
num_vol15vis_surveys = length(unique(pre_vol15_vis$surveyID))
num_lab16bs_surveys = length(unique(pre_lab16_bs$surveyID))
num_vol16bs_surveys = length(unique(pre_vol16_bs$surveyID))


lab15_vis = filter(pre_lab15_vis, length >= minLength)
vol15_vis = filter(pre_vol15_vis, length >= minLength)
lab16_bs = filter(pre_lab16_bs, length >= minLength)
vol16_bs = filter(pre_vol16_bs, length >= minLength)

# calculate relative number of arthropods in each set of surveys


vol15_vis_rel = vol15_vis %>% group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(proportion = totCount/sum(vol15_vis$count), 
         density = totCount/num_vol15vis_surveys,
         arthCode2 = arthCode)
lab15_vis_rel = lab15_vis %>% group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(proportion = totCount/sum(lab15_vis$count),
         density = totCount/num_lab15vis_surveys,
         arthCode2 = arthCode)
vol16_bs_rel = vol16_bs %>% group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(proportion = totCount/sum(vol16_bs$count),
         density = totCount/num_vol16bs_surveys,
         arthCode2 = arthCode)
lab16_bs_rel = lab16_bs %>% group_by(arthCode) %>% summarize(totCount = sum(count)) %>%
  mutate(proportion = totCount/sum(lab16_bs$count),
         density = totCount/num_lab16bs_surveys,
         arthCode2 = arthCode)

vis_comp_all = left_join(vol15_vis_rel, lab15_vis_rel, by = c('arthCode', 'arthCode2')) %>%
  rename(dens_vol = density.x, dens_lab = density.y,
         pct_vol = proportion.x, pct_lab = proportion.y) %>% 
  select(arthCode, arthCode2, dens_vol, dens_lab, pct_vol, pct_lab) %>%
  filter(arthCode != 'NONE')

bs_comp_all = left_join(vol16_bs_rel, lab16_bs_rel, by = c('arthCode', 'arthCode2')) %>%
  rename(dens_vol = density.x, dens_lab = density.y,
         pct_vol = proportion.x, pct_lab = proportion.y) %>% 
  select(arthCode, arthCode2, dens_vol, dens_lab, pct_vol, pct_lab) %>%
  filter(arthCode != 'NONE')


# replace "others"
vol15_vis_rel$arthCode2[!vol15_vis_rel$arthCode %in% birdfood_top6] = 'OTHR'
lab15_vis_rel$arthCode2[!lab15_vis_rel$arthCode %in% birdfood_top6] = 'OTHR'
vol16_bs_rel$arthCode2[!vol16_bs_rel$arthCode %in% birdfood_top6] = 'OTHR'
lab16_bs_rel$arthCode2[!lab16_bs_rel$arthCode %in% birdfood_top6] = 'OTHR'

vol15_vis_rel_oth = vol15_vis_rel %>% group_by(arthCode2) %>% 
  summarize(proportion = sum(proportion), density = sum(density))
lab15_vis_rel_oth = lab15_vis_rel %>% group_by(arthCode2) %>% 
  summarize(proportion = sum(proportion), density = sum(density))
vol16_bs_rel_oth = vol16_bs_rel %>% group_by(arthCode2) %>% 
  summarize(proportion = sum(proportion), density = sum(density))
lab16_bs_rel_oth = lab16_bs_rel %>% group_by(arthCode2) %>% 
  summarize(proportion = sum(proportion), density = sum(density))

vis_comp = left_join(vol15_vis_rel_oth, lab15_vis_rel_oth, by = 'arthCode2') %>%
  rename(vis_vol_pct = proportion.x, vis_lab_pct = proportion.y,
         vis_vol_den = density.x, vis_lab_den = density.y) %>%
  filter(arthCode2 != 'NONE') %>%
  mutate(vis_vol_tot = vis_vol_den*num_vol15vis_surveys,
         vis_lab_tot = vis_lab_den*num_lab15vis_surveys)


bs_comp = left_join(vol16_bs_rel_oth, lab16_bs_rel_oth, by = 'arthCode2') %>%
  rename(bs_vol_pct = proportion.x, bs_lab_pct = proportion.y,
         bs_vol_den = density.x, bs_lab_den = density.y) %>%
  filter(arthCode2 != 'NONE') %>%
  mutate(bs_vol_tot = bs_vol_den*num_vol16bs_surveys,
       bs_lab_tot = bs_lab_den*num_lab16bs_surveys)

# Chi-squared tests for panels D, E
chisq.test(vis_comp[, c('vis_vol_tot', 'vis_lab_tot')])

#data:  vis_comp[, c("vis_vol_tot", "vis_lab_tot")]
#X-squared = 44.943, df = 6, p-value = 4.804e-08

chisq.test(bs_comp[, c('bs_vol_tot', 'bs_lab_tot')])

#data:  bs_comp[, c("bs_vol_tot", "bs_lab_tot")]
#X-squared = 17.227, df = 6, p-value = 0.008484

#create color palette:
library(RColorBrewer)
coul = brewer.pal(7, "Set3") 

arthcols = data.frame(arthCode = c('ARAN', 'AUCH', 'COLE', 'DIPT', 'LEPL', 'ORTH', 'OTHR'),
                      col = coul[c(3, 1, 4, 2, 7, 6, 5)])
arthcols$arthCode = as.character(arthcols$arthCode)
arthcols$col = as.character(arthcols$col)
arthcols$col2 = arthcols$col
arthcols$col2[arthcols$arthCode=='DIPT'] = 'gray50'
arthcols$name = c('Spiders', 'Leafhoppers', 'Beetles', 'Flies', 'Caterpillars', 'Crickets', 'Other')
arthcols$name2 = c('Aranae', 'Auchenorrhyncha', 'Coleoptera', 'Diptera', 'Caterpillars', 'Orthoptera', 'Other')
arthcols$pch = c(rep(17, 6), 16)
arthcols$cex = c(rep(3, 6), 2)


#---------plotting for figures------------------

#plot relative and mean arth frequencies

pdf(paste('output/plots/paper_plots/Figure4_data_comparisons_minLength', minLength, '.pdf', sep = ''), 
    height = 6, width = 8)

par(xpd=FALSE)
par(mfrow = c(2, 3), mar = c(5, 5, 3, 1))


#panel A
common_trees2 = common_trees_den_no_outliers[,-1]
rownames(common_trees2) = common_trees_den_no_outliers[,1]
common_trees2 = as.matrix(common_trees2)
order = order(colSums(common_trees2, na.rm = T), decreasing = T)
common_trees2 = common_trees2[, order]
labs = c('Am. beech', 'Box elder', 'Persimmon', 'Spicebush', 'Sugar maple', 'Sweet gum')
orderedlabs = labs[order]
par(mgp = c(3.5, 1, 0))
bar = barplot(common_trees2, las = 1, cex.axis = 1.2, cex.lab = 1.5, xaxt="n", ylim = c(0, 1.3),
        xlim = c(0, 7), ylab = "Density (# / survey)", col = arthcols$col) 
segments(bar, tree_means$ll95, bar, tree_means$ul95, lwd = 2)

text(seq(.8, 6.75, length.out = 6), rep(-.02, 6), orderedlabs,
     srt = 45, xpd = TRUE, adj = 1, cex = 1.2)

# * indicates tree species where large caterpillar colonies (e.g. 100s) were observed,
# but these outlier records were removed in calculating means.
text(.7, 1.1, "*", cex = 3)
text(1.9, 0.9, "*", cex = 3)
mtext("A", 3, adj = -0.3, line = 0.5, cex = 1.75)

#panel B
par(mgp = c(3, 1, 0))
lab_selected1 = lab_selected_rel[,-1]
rownames(lab_selected1) = lab_selected_rel[,1]
lab_selected1 = as.matrix(lab_selected1) #create matrix w/o arth code?
tops = cumsum(lab_selected1[,2])
bottoms = c(0, tops[1:6])
centers = (tops + bottoms)/2

barplot(lab_selected1, las = 1, xlim = c(0, 3.2), xaxt = "n",
        ylab = "Proportion", col = arthcols$col, cex.axis = 1.2, cex.lab = 1.5)
mtext(c("Visual\nsurvey", "Beat\nsheet"), 1, at = c(.7, 2), line = 2)
mtext(arthcols$name, 4, at = centers, las = 1, line = -3.6, col = arthcols$col2, 
      cex = .7, font = 2)
mtext("B", 3, adj = -0.3, line = 0.5, cex = 1.75)


#panel C
arth_means2 = left_join(lab_bs_vis_den, arthcols, by = c('arthCode2' = 'arthCode'))
arth_means2$col[is.na(arth_means2$col)] = "#80B1D3"
plot(arth_means2$vis_den, arth_means2$bs_den, las = 1, col = arth_means2$col, pch = arth_means2$pch, 
     xlab = "Density (visual)", ylab = "Density (beat sheet)", cex = arth_means2$cex, cex.lab = 1.5, 
     xlim = c(0, 0.2), ylim = c(0, 0.35), cex.axis = 1.2, yaxt = "n")
axis(2, at = seq(0, 0.35, by = 0.05), tcl = -0.3, labels = F)
mtext(c('0.0', '0.1', '0.2', '0.3'), 2, at = seq(0, 0.3, by = 0.1), line = 1, cex = 0.8, las = 1)
points(arth_means2$vis_den[arth_means2$arthCode=='DIPT'],
       arth_means2$bs_den[arth_means2$arthCode=='DIPT'], cex = 3, pch = 2, col = 'gray80')
laboratory = lm(bs_den ~ vis_den, data = arth_means2)
abline(laboratory, col = "black", lwd = 2)
abline(a = 0, b = 1, col = 'gray50', lwd = 4, lty = 'dotted')
mtext("C", 3, adj = -0.3, line = 0.5, cex = 1.75)


#figure D - visual surveys by pct
vis_cmp = left_join(vis_comp, arthcols, by = c('arthCode2' = 'arthCode'))

barplot(as.matrix(vis_cmp[, c('vis_vol_pct', 'vis_lab_pct')]), las = 1, xaxt = "n", xlim = c(0,3.2), 
        ylab = "Proportion", col = vis_cmp$col, cex.lab = 1.5, cex.axis = 1.2)
mtext(c("Volunteers", "Trained"), 1, at = c(.7, 2), line = 1)
mtext("D", 3, adj = -0.3, line = 0.5, cex = 1.75)


#figure E
bs_cmp = left_join(bs_comp, arthcols, by = c('arthCode2' = 'arthCode'))

barplot(as.matrix(bs_cmp[, c('bs_vol_pct', 'bs_lab_pct')]), las = 1, xaxt = "n", xlim = c(0,3.2),
        ylab = "Proportion", col = arthcols$col, cex.lab = 1.5, cex.axis = 1.2)
mtext(c("Volunteers", "Trained"), 1, at = c(.7, 2), line = 1)
mtext("E", 3, adj = -0.3, line = 0.5, cex = 1.75)


#figure F
vis_cmp_all = left_join(vis_comp_all, arthcols, by = c('arthCode2' = 'arthCode'))
bs_cmp_all = left_join(bs_comp_all, arthcols, by = c('arthCode2' = 'arthCode'))

par(mgp = c(3.5, 0.75, 0))
plot(vis_cmp_all$dens_lab, vis_cmp_all$dens_vol, las = 1, xlab = "",
     ylab = "Density (volunteers)", col = vis_cmp_all$col, pch = vis_cmp_all$pch, 
     cex = vis_cmp_all$cex, cex.lab = 1.5, cex.axis = 1.2,
     xlim = c(0, 0.26), ylim = c(0, 0.31), yaxt = "n")
mtext("Density (trained)", 1, line = 3, cex.lab = 1.5)
axis(2, at = seq(0, 0.3, by = 0.05), tcl = -0.3, labels = F)
mtext(c('0.0', '0.1', '0.2', '0.3'), 2, at = seq(0, 0.3, by = 0.1), line = 1, cex = 0.8, las = 1)
vol.lab.vis =lm(dens_vol ~ dens_lab, data = vis_cmp_all)
abline(vol.lab.vis, lwd = 2)
points(bs_cmp_all$dens_lab, bs_cmp_all$dens_vol, las = 1, col = bs_cmp_all$col, pch = 1, cex =3)
points(bs_cmp_all$dens_lab, bs_cmp_all$dens_vol, las = 1, col = bs_cmp_all$col, pch = 16, cex =2)
vol.lab.bs = lm(dens_vol ~ dens_lab, data = bs_cmp_all)
abline(vol.lab.bs, lty = 'dashed', lwd = 2)
abline(a = 0, b = 1, col = 'gray50', lwd = 4, lty = 'dotted')
mtext("F", 3, adj = -0.3, line = 0.5, cex = 1.75)


legend("bottomright", c('visual', 'beat sheet'), lwd = 2, lty = c('solid', 'dashed'))

dev.off()









#----------------
#find distribution of surveys of survey type of interest by date in each year
vol15_dates = vol15 %>% group_by(julianday) %>% tally
vol16_dates = vol16 %>% group_by(julianday) %>% tally

lab15_dates = lab15_vis %>% group_by(julianday) %>% tally
lab16_bs_dates = lab16_bs %>% group_by(julianday) %>% tally

#plots for sampling windows and sampling effort
par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
plot(vol15_dates$julianday, vol15_dates$n, xlim = c(134, 204))
plot(lab15_dates$julianday, lab15_dates$n, xlim = c(134, 204))
plot(vol16_dates$julianday, vol16_dates$n, xlim = c(134, 204))
plot(lab16_bs_dates$julianday, lab16_bs_dates$n, xlim = c(134, 204))

#summarize the average density by order per tree #check to ensure there are no survey days with 2 surveys/day
uniq_vol15= vol15 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count)) 
uniq_vol16= vol16 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
uniq_lab15= lab15_vis %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
uniq_lab16_bs= lab16_bs %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))

arth_rel_15 = select(arth_rel, arthCode, mean_vol15, mean_lab15)

