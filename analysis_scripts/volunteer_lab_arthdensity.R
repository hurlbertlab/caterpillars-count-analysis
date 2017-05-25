#Plots for CC paper: Absolute arth densities and relative arth densities 
source("cleaning_scripts/data_cleaning.R")

#load libraries
library(tidyr)

# subset lab data to the the circles regularly surveyed by volunteers
# only arthropods >=5 mm included
size_thresh = 5
lab = filter(labdata.pr, circle %in% 1:8, length >= size_thresh)

#subset by year, cit/sci, and collection type 
#2015 is the comparison year for visual surveys, 2016 is the comparison year for beat sheets
# (filtering is being done in two steps because the complete set of dates is being used later on)
vol15 = filter(volunteer.pr, year == 2015 & surveyType == "Visual", length >= size_thresh) 
vol16 = filter(volunteer.pr, year == 2016 & surveyType == "Beat_Sheet", length >= size_thresh) 
lab15 = lab %>% filter(year == 2015, surveyType == "Visual")
lab16_bs = lab %>% filter(year == 2016, surveyType == "Beat_Sheet") 
lab16_vis = lab %>% filter(year == 2016, surveyType == "Visual")
    

#subset volunteer and lab data for same range of dates for each year
#2015 is visual surveys, 2016 is beat sheet surveys
vol15_sub = filter(vol15, julianday <= 193 & julianday >= 147)
vol16_sub = filter(vol16, julianday <= 193 & julianday >= 147)
lab15_sub = filter(lab15, julianday <= 193 & julianday >= 147)
lab16_bs_sub = filter(lab16_bs, julianday <= 193 & julianday >= 147)
lab16_vis_sub = filter(lab16_vis, julianday <= 193 & julianday >= 147)

#remove arthropods not in the "bird food" category for relative arthropod barplots
birdfood = c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA")
vol15_sub_bird = vol15_sub %>% filter(arthCode %in% birdfood)
vol16_sub_bird = vol16_sub %>% filter(arthCode %in% birdfood)
lab15_sub_bird = lab15_sub %>% filter(arthCode %in% birdfood)
lab16_bs_sub_bird = lab16_bs_sub %>% filter(arthCode %in% birdfood)
lab16_vis_sub_bird = lab16_vis_sub %>% filter(arthCode %in% birdfood)

#calculate relative number of arthropods in each set of surveys
vol15_rel= vol15_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol15_sub_bird$count)))
vol16_rel= vol16_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol16_sub_bird$count)))
lab15_rel= lab15_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab15_sub_bird$count)))
lab16_bs_rel= lab16_bs_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_bs_sub_bird$count)))
lab16_vis_rel = lab16_vis_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_vis_sub_bird$count)))

#decide which arthropods to include in figures based on which are on 
#the highest proportion of leaves in both visual and lab surveys
lab16_bs_rel = data.frame(lab16_bs_rel)
lab16_vis_rel = data.frame(lab16_vis_rel)
bs_arth_rank = lab16_bs_rel[order(lab16_bs_rel$proportion, decreasing = T),] 
vis_arth_rank = lab16_vis_rel[order(lab16_vis_rel$proportion, decreasing = T),] 
#choosing to display top 5 most commonly seen orders in both vis and bs, and in the "bird food" category, and LEPL

#merge lab vis and bs data for comparison of survey types within the lab, group arths not chosen for display into "other"
lab_rel = data.frame(left_join(bs_arth_rank, vis_arth_rank, by = "arthCode"))
names(lab_rel) = c("arthCode", "bs", "vis")

#merge relative arth numbers into one dataframe (for volunteer/lab comparison)
rel = left_join(vol15_rel, vol16_rel, by = "arthCode")
names(rel) = c("arthCode", "mean_vol15", "mean_vol16")
rel1 = left_join(rel, lab15_rel, by = "arthCode")
names(rel1) = c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15")
arth_rel = data.frame(left_join(rel1, lab16_bs_rel, by = "arthCode"))
names(arth_rel) = c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15", "mean_lab16_bs")

#group less common "bird food" arthropods together for plotting 
#decided on these because AUCH, DIPT, and COLE were in the top 5 most dense for beat sheets and visual, and also in "bird food", LEPL obviously necessary
birdfood_top6 = c( "ARAN", "AUCH", "COLE", "LEPL", "ORTH", "DIPT")
lab_rel$arthCode[!lab_rel$arthCode %in% birdfood_top6] = 'OTHR'

lab_selected = lab_rel %>% group_by(arthCode) %>% dplyr::summarize(vis = sum(vis), bs = sum(bs)) %>% data.frame()

arth_rel$arthCode[!arth_rel$arthCode %in% birdfood_top6] = 'OTHR'

arth_selected = arth_rel %>% group_by(arthCode) %>% dplyr::summarize(vis_vol = sum(mean_vol15), vis_lab = sum(mean_lab15), bs_vol = sum(mean_vol16), bs_lab = sum(mean_lab16_bs)) %>% data.frame()
arth_selected1 = arth_selected [,-1]
rownames(arth_selected1) = arth_selected[,1]
arth_selected_vis = select(arth_selected1, vis_vol, vis_lab)
arth_selected_bs = select(arth_selected1, bs_vol, bs_lab)



#find (# of surveys per day for each year/surveyor type)
uniq_vol15 = vol15_sub %>% count(julianday, circle, survey)
surv_vol15 = nrow(uniq_vol15)
uniq_vol16 = vol16_sub %>% count(julianday, circle, survey)
surv_vol16 = nrow(uniq_vol16)
uniq_lab15 = lab15_sub %>% count(julianday, circle, survey)
surv_lab15 = nrow(uniq_lab15)
uniq_lab16_bs = lab16_bs_sub %>% count(julianday, circle, survey)
surv_lab16_bs = nrow(uniq_lab16_bs)
uniq_lab16_vis = lab16_vis_sub %>% count(julianday, circle, survey)
surv_lab16_vis = nrow(uniq_lab16_vis)

#calculate mean number of each type of arthropods seen for each year/surveyor type
vol15_means= vol15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol15))
vol16_means= vol16_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol16))
lab15_means= lab15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab15))
lab16_bs_means= lab16_bs_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_bs))
lab16_vis_means= lab16_vis_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_vis))

#calculate mean number of each type of arthropods seen for visual surveys in the lab in 2016 by tree type (for figure 2)

############################
# I THINK THIS IS WRONG DENOMINATOR--NEED TO USE PLANT SPECIES-SPECIFIC # OF TOTAL SURVEYS
############################
lab16_vis_trees = lab16_vis_sub_bird %>% group_by(clean_plantSp, arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_vis))

#change less common arthropods into one "other" group for the tree species groups
lab16_vis_trees$arthCode[!lab16_vis_trees$arthCode %in% birdfood_top6] = 'OTHR'

lab16_vis_trees = data.frame(lab16_vis_trees)

#calculate mean number of each type of arthropods seen by tree sp 
trees_oth = lab16_vis_trees %>% group_by(clean_plantSp, arthCode) %>% dplyr::summarize(vis = sum(mean_count))

#only use 4 most common tree sp at PR (within 1-8) #sweet gum, common persimmon, box elder, chalk maple
common_trees = trees_oth %>% 
  filter(clean_plantSp %in% c("sweet gum", "common persimmon", "box elder", "chalk maple", "pin oak")) 
common_trees1 = data.frame(spread(common_trees, clean_plantSp, vis))
names(common_trees1) = c("arthCode", "Box elder", "Chalk maple", "Common persimmon", "Pin oak", "Sweet gum")

#merge all means into one dataframe
join = left_join(vol15_means, vol16_means, by = "arthCode")
names(join) = c("arthCode", "mean_vol15", "mean_vol16")
join1 = left_join(join, lab15_means, by = "arthCode")
names(join1) =c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15")
arth_means = data.frame(left_join(join1, lab16_bs_means, by = "arthCode"))
names(arth_means) = c("arthCode", "vol_vis", "vol_bs", "lab_vis", "lab_bs") #change names to reflect visual and bs (2015 is vis, 2016 is bs)

#---------plotting for figures------------------

#plot relative and mean arth frequencies

pdf('plots/paper_plots/Figures1to6_arthropods.pdf', height = 6, width = 8)

par(xpd=FALSE)
par(mfrow = c(2, 3), mar = c(5, 5, 2, 1))

#create color palette:
library(RColorBrewer)
coul = brewer.pal(7, "Set3") 

arthcols = data.frame(arthCode = lab_selected$arthCode,
                      col = coul[c(3, 1, 4, 2, 7, 6, 5)])
arthcols$col = as.character(arthcols$col)
arthcols$col2 = arthcols$col
arthcols$col2[arthcols$arthCode=='DIPT'] = 'gray50'
arthcols$name = c('Aranae', 'Auchenorrhyncha', 'Coleoptera', 'Diptera', 'Caterpillars', 'Orthoptera', 'Other')

#panel A
common_trees2 = common_trees1[,-1]
rownames(common_trees2) = common_trees1[,1]
common_trees2 = as.matrix(common_trees2)
par(mgp = c(3.5, 1, 0))
barplot(common_trees2, las = 1, cex.axis = 1.2, cex.lab = 1.5, xaxt="n", 
        xlim = c(0, 6), ylab = "Density (#/survey)", col = arthcols$col, ylim = c(0, 0.25)) 
text(seq(1, 5.5, length.out = 5), rep(-.01, 4), c('Box elder', 'Chalk maple', 'Persimmon', 'Pin oak', 'Sweet gum'),
     srt = 45, xpd = TRUE, adj = 1, cex = 1.2)

#panel B
par(mgp = c(3, 1, 0))
lab_selected1 = lab_selected[,-1]
rownames(lab_selected1) = lab_selected[,1]
lab_selected1 = as.matrix(lab_selected1) #create matrix w/o arth code?
tops = cumsum(lab_selected1[,2])
bottoms = c(0, tops[1:6])
centers = (tops + bottoms)/2

barplot(lab_selected1, las = 1, xlim = c(0, 3.2), xaxt = "n",
        ylab = "Proportion", col = arthcols$col, cex.axis = 1.2, cex.lab = 1.5)
mtext(c("Visual", "Beat sheet"), 1, at = c(.7, 2), line = 1)
mtext(arthcols$name, 4, at = centers, las = 1, line = -3.6, col = arthcols$col2, 
      cex = c(.75, .5, .75, .75, .75, .75, .75))


#panel C
arth_means2 = left_join(arth_means, arthcols)
arth_means2$col[is.na(arth_means2$col)] = "#80B1D3"
arth_means2 = arth_means2[arth_means2$arthCode != 'NONE', ]
plot(arth_means2$lab_vis, arth_means2$lab_bs, las = 1, col = arth_means2$col, pch = 17, 
     xlab = "Density (visual)", ylab = "Density (beat sheet)", cex = 3, cex.lab = 1.5, 
     xlim = c(0, 0.65), ylim = c(0, 0.65), cex.axis = 1.2)
laboratory = lm(lab_bs ~ lab_vis, data = arth_means2)
abline(laboratory, col = "black", lwd = 2)
abline(a = 0, b = 1, col = 'gray50', lwd = 1)


#figure D
arth_selected_bs1 = as.matrix(arth_selected_bs)
arth_selected_vis1 = as.matrix(arth_selected_vis)

barplot(arth_selected_vis1, las = 1, xaxt = "n", xlim = c(0,3.2), 
        ylab = "Proportion", col = arthcols$col, cex.lab = 1.5, cex.axis = 1.2)
mtext(c("Volunteers", "Trained"), 1, at = c(.7, 2), line = 1)


#figure E
barplot(arth_selected_bs1, las = 1, xaxt = "n", xlim = c(0,3.2),
        ylab = "Proportion", col = arthcols$col, cex.lab = 1.5, cex.axis = 1.2)
mtext(c("Volunteers", "Trained"), 1, at = c(.7, 2), line = 1)


#figure F
plot(arth_means2$lab_vis, arth_means2$vol_vis, las = 1, xlab = "Density (trained)",
     ylab = "Density (volunteers)", col = arth_means2$col, pch = 17, cex = 3,
     cex.lab = 1.5, cex.axis = 1.2)
vol.lab.vis =lm(vol_vis ~ lab_vis, data = arth_means2)
abline(vol.lab.vis, lwd = 2)
points(arth_means2$lab_bs, arth_means2$vol_bs, las = 1, col = arth_means2$col, pch = 1, cex =3)
points(arth_means2$lab_bs, arth_means2$vol_bs, las = 1, col = arth_means2$col, pch = 16, cex =2)
vol.lab.bs = lm(vol_bs ~ lab_vis, data = arth_means2)
abline(vol.lab.bs, lty = 'dashed', lwd = 2)
abline(a = 0, b = 1, col = 'gray50', lwd = 1)

legend("bottomright", c('visual', 'beat sheet'), lwd = 2, lty = c('solid', 'dashed'))

dev.off()

#----------------
#find distribution of surveys of survey type of interest by date in each year
vol15_dates = vol15 %>% group_by(julianday) %>% tally
vol16_dates = vol16 %>% group_by(julianday) %>% tally

lab15_dates = lab15 %>% group_by(julianday) %>% tally
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
uniq_lab15= lab15 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
uniq_lab16_bs= lab16_bs %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))

arth_rel_15 = select(arth_rel, arthCode, mean_vol15, mean_lab15)

