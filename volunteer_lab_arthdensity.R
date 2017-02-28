#Plots for CC paper: Absolute arth densities and relative arth densities 
source("data_cleaning.R")

#load libraries
library(tidyr)

#subset lab data to the the circles regularly surveyed by volunteers
lab = filter(labdata.pr, circle %in% c("1", "2", "3", "4", "5", "6", "7", "8"))

#subset by year, cit/sci, and collection type 
#2015 is the comparison year for visual surveys, 2016 is the comparison year for beat sheets
# (filtering is being done in two steps because the complete set of dates is being used later on)
vol15 = filter(volunteer.pr, year == 2015 & surveyType == "Visual") 
vol16 = filter(volunteer.pr, year == 2016 & surveyType == "Beat_Sheet") 
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
vol15_sub_bird = vol15_sub %>% filter(arthCode %in% c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA"))
vol16_sub_bird = vol16_sub %>% filter(arthCode %in% c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA"))
lab15_sub_bird = lab15_sub %>% filter(arthCode %in% c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA"))
lab16_bs_sub_bird = lab16_bs_sub %>% filter(arthCode %in% c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA"))
lab16_vis_sub_bird = lab16_vis_sub %>% filter(arthCode %in% c("DIPT", "ARAN", "AUCH", "COLE", "LEPL", "HETE", "ORTH", "LEPA"))

#calculate relative number of arthropods in each set of surveys #check that this is the right way to do this
vol15_rel= vol15_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol15_sub_bird$count)))
vol16_rel= vol16_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol16_sub_bird$count)))
lab15_rel= lab15_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab15_sub_bird$count)))
lab16_bs_rel= lab16_bs_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_bs_sub_bird$count)))
lab16_vis_rel = lab16_vis_sub_bird %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_vis_sub_bird$count)))

#decide which arthropods to include in figures based on which are on 
#the highest proportion of leaves in both visual and lab surveys
lab16_bs_rel = data.frame(lab16_bs_rel)
lab16_vis_rel = data.frame(lab16_vis_rel)
bs_arth_rank = lab16_bs_rel[order(lab16_bs_rel$proportion, decreasing = T),] #can i use a forloop in this situation? how?
vis_arth_rank = lab16_vis_rel[order(lab16_vis_rel$proportion, decreasing = T),] #can i use a forloop in this situation? how?
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
lab_rel$arthCode = ifelse(lab_rel$arthCode == "LEPL", "LEPL",
                          ifelse(lab_rel$arthCode == "COLE", "COLE",
                                 ifelse(lab_rel$arthCode == "AUCH", "AUCH",
                                        ifelse(lab_rel$arthCode == "ARAN", "ARAN",
                                               ifelse(lab_rel$arthCode == "ORTH", "ORTH",
                                                   ifelse(lab_rel$arthCode == "DIPT", "DIPT", "OTHR"))))))

lab_selected = lab_rel %>% group_by(arthCode) %>% dplyr::summarize(vis = sum(vis), bs = sum(bs)) %>% data.frame()

arth_rel$arthCode = ifelse(arth_rel$arthCode == "LEPL", "LEPL",
                           ifelse(arth_rel$arthCode == "COLE", "COLE",
                                  ifelse(arth_rel$arthCode == "AUCH", "AUCH",
                                         ifelse(arth_rel$arthCode == "ARAN", "ARAN",
                                                ifelse(arth_rel$arthCode == "ORTH", "ORTH",
                                                  ifelse(arth_rel$arthCode == "DIPT", "DIPT", "OTHR"))))))
arth_selected = arth_rel %>% group_by(arthCode) %>% dplyr::summarize(vis_vol = sum(mean_vol15), vis_lab = sum(mean_lab15), bs_vol = sum(mean_vol16), bs_lab = sum(mean_lab16_bs)) %>% data.frame()
arth_selected1 = arth_selected [,-1]
rownames(arth_selected1) = arth_selected[,1]
arth_selected_vis = select(arth_selected1, vis_vol, vis_lab)
arth_selected_bs = select(arth_selected1, bs_vol, bs_lab)



#find (# of surveys per day for each year/surveyor type)
uniq_vol15 = data.frame(table(vol15_sub$julianday, vol15_sub$circle, vol15_sub$survey)) %>% filter(Freq > 0) 
surv_vol15 = length(uniq_vol15$Freq)
uniq_vol16 = data.frame(table(vol16_sub$julianday, vol16_sub$circle, vol16_sub$survey)) %>% filter(Freq > 0) 
surv_vol16 = length(uniq_vol16$Freq)
uniq_lab15 = data.frame(table(lab15_sub$julianday, lab15_sub$circle, lab15_sub$survey)) %>% filter(Freq > 0) 
surv_lab15 = length(uniq_lab15$Freq)
uniq_lab16_bs = data.frame(table(lab16_bs_sub$julianday, lab16_bs_sub$circle, lab16_bs_sub$survey)) %>% filter(Freq > 0) 
surv_lab16_bs = length(uniq_lab16_bs$Freq)
uniq_lab16_vis = data.frame(table(lab16_vis_sub$julianday, lab16_vis_sub$circle, lab16_vis_sub$survey)) %>% filter(Freq > 0) 
surv_lab16_vis = length(uniq_lab16_vis$Freq)

#calculate mean number of each type of arthropods seen for each year/surveyor type
vol15_means= vol15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol15))
vol16_means= vol16_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol16))
lab15_means= lab15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab15))
lab16_bs_means= lab16_bs_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_bs))
lab16_vis_means= lab16_vis_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_vis))

#calculate mean number of each type of arthropods seen for visual surveys in the lab in 2016 by tree type (for figure 2)
lab16_vis_trees = lab16_vis_sub_bird %>% group_by(clean_plantSp, arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_vis))

#change less common arthropods into one "other" group for the tree species groups
lab16_vis_trees$arthCode = ifelse(lab16_vis_trees$arthCode == "LEPL", "LEPL",
                          ifelse(lab16_vis_trees$arthCode == "COLE", "COLE",
                                 ifelse(lab16_vis_trees$arthCode == "AUCH", "AUCH",
                                        ifelse(lab16_vis_trees$arthCode == "ARAN", "ARAN",
                                               ifelse(lab16_vis_trees$arthCode == "ORTH", "ORTH",
                                        ifelse(lab16_vis_trees$arthCode == "DIPT", "DIPT", "OTHR"))))))
lab16_vis_trees = data.frame(lab16_vis_trees)

#calculate mean number of each type of arthropods seen by tree sp 
trees_oth = lab16_vis_trees %>% group_by(clean_plantSp, arthCode) %>% dplyr::summarize(vis = sum(mean_count))

#only use 4 most common tree sp at PR (within 1-8) #sweet gum, common persimmon, box elder, chalk maple
common_trees = trees_oth %>% filter(clean_plantSp %in% c("Sweet gum", "Common persimmon", "Box elder", "Chalk maple")) 
common_trees1 = data.frame(spread(common_trees, clean_plantSp, vis))
names(common_trees1) = c("arthCode", "Box elder", "Chalk maple", "Common persimmon", "Sweet gum")

#merge all means into one dataframe
join = left_join(vol15_means, vol16_means, by = "arthCode")
names(join) = c("arthCode", "mean_vol15", "mean_vol16")
join1 = left_join(join, lab15_means, by = "arthCode")
names(join1) =c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15")
arth_means = data.frame(left_join(join1, lab16_bs_means, by = "arthCode"))
names(arth_means) = c("arthCode", "vol_vis", "vol_bs", "lab_vis", "lab_bs") #change names to reflect visual and bs (2015 is vis, 2016 is bs)

#---------plotting for figures------------------

#plot relative and mean arth frequencies
## Example code:
# barplot(counts, main="Car Distribution by Gears and VS",
# xlab="Number of Gears", col=c("darkblue","red"),
# legend = rownames(counts), beside=TRUE) 

pdf('plots/paper_plots/Figures1to6_arthropods.pdf', height = 8, width = 11)

par(xpd=FALSE)
par(mfrow = c(3, 2), mar = c(5, 5, 2, 1))

#figure A
lab_selected1 = lab_selected[,-1]
rownames(lab_selected1) = lab_selected[,1]
lab_selected1 = as.matrix(lab_selected1) #create matrix w/o arth code?

barplot(lab_selected1, las = 2, cex.names = .7, xlim = c(0, 3.2),
        ylab = "Proportion Arths", las = .5) #this needs to be cleaned up


#figure B
par(mar = c(6, 6, 2, 2))
common_trees2 = common_trees1[,-1]
rownames(common_trees2) = common_trees1[,1]
common_trees2 = as.matrix(common_trees2)
barplot(common_trees2, las = 2, cex.names = .6, xlim = c(0, 6), legend = arth_selected$arthCode,
        ylab = "Mean Arths") #this needs to be cleaned up

#legend

#figure D
arth_selected_bs1 = as.matrix(arth_selected_bs)
arth_selected_vis1 = as.matrix(arth_selected_vis)

barplot(arth_selected_vis1, las = 2, cex.names = .7, xlim = c(0,3.2), 
        ylab = "Proportion Arths")

#figure E
barplot(arth_selected_bs1, las = 2, cex.names = .7, xlim = c(0,3.2),
        ylab = "Proportion Arths")

#figure C (BS vs VIS) (here we're comparing different years)
plot(arth_means$vol_vis, arth_means$vol_bs, las = 2, xlab = "Mean Arths (VIS)",
     ylab = "Mean Arths (BS)", col = "purple", pch = 20)
volunteer =lm(vol_bs ~ vol_vis, data = arth_means)
abline(volunteer, col = "purple", cex = 2)
points(arth_means$lab_vis, arth_means$lab_bs, las = 2, col = "blue", pch = 15)
laboratory = lm(lab_bs ~ lab_vis, data = arth_means)
abline(laboratory, col = "blue", cex = 2)
legend(x = "bottomright",
       c("vol", "lab"),
       lty=c(1,1),
        col=c("purple", "blue"))

#figure F (volunteers vs lab)
plot(arth_means$lab_vis, arth_means$vol_vis, xlab = "Mean Arths (vol)",
     ylab = "Mean Arths (lab)", col = "pink", pch = 20) 
visual =lm(vol_vis ~ lab_vis, data = arth_means)
abline(visual, col = "pink", cex = 2)
points(arth_means$lab_bs, arth_means$vol_bs, las = 2, col = "red", pch = 15)
beat = lm(vol_bs ~ lab_bs, data = arth_means)
abline(beat, col = "red", cex = 2)
abline(0,1)

legend(x = "bottomright",
       c("vis", "BS"),
       lty=c(1,1),
       col=c("red", "pink"))

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

