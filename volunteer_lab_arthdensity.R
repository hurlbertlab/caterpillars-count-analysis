#Plots for CC paper: Absolute arth densities and relative arth densities 
source("data_cleaning.R")

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

#calculate relative number of arthropods in each set of surveys
vol15_rel= vol15_sub %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol15_sub$count)))
vol16_rel= vol16_sub %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(vol16_sub$count)))
lab15_rel= lab15_sub %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab15_sub$count)))
lab16_bs_rel= lab16_bs_sub %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_bs_sub$count)))
lab16_vis_rel = lab16_vis_sub %>% group_by(arthCode) %>% dplyr::summarize(proportion = (sum(count)/sum(lab16_vis_sub$count)))

#decide which arthropods to include in figures based on which are on 
#the highest proportion of leaves in both visual and lab surveys
lab16_bs_rel = data.frame(lab16_bs_rel)
lab16_vis_rel = data.frame(lab16_vis_rel)
bs_arth_rank = lab16_bs_rel[order(lab16_bs_rel$proportion, decreasing = T),] #can i use a forloop in this situation? how?
vis_arth_rank = lab16_vis_rel[order(lab16_vis_rel$proportion, decreasing = T),] #can i use a forloop in this situation? how?
#choosing to display COLE, DIPT, AUCH, all in the top 5 most commonly seen orders in both vis and bs, and in the "bird food" category, and LEPL

#merge lab vis and bs data for comparison of survey types within the lab, group arths not chosen for display into "other"
lab_rel = data.frame(left_join(bs_arth_rank, vis_arth_rank, by = "arthCode"))
names(lab_rel) = c("arthCode", "bs", "vis")
#group less common arthropods together for plotting #
lab_rel$arthCode = ifelse(lab_rel$arthCode == "LEPL", "LEPL",
                          ifelse(lab_rel$arthCode == "COLE", "COLE",
                                 ifelse(lab_rel$arthCode == "AUCH", "AUCH",
                                        ifelse(lab_rel$arthCode == "DIPT", "DIPT", "OTHR"))))
vis_oth = data.frame(lab_rel %>% select(arthCode, vis) %>% group_by(arthCode) %>% summarize(vis = sum(vis)))
bs_oth = data.frame(lab_rel %>% select(arthCode, bs) %>% group_by(arthCode) %>% summarize(bs = sum(bs)))
lab_selected = left_join(vis_oth, bs_oth, by = "arthCode")

#merge relative arth numbers into one dataframe (for volunteer/lab comparison)
rel = left_join(vol15_rel, vol16_rel, by = "arthCode")
names(rel) = c("arthCode", "mean_vol15", "mean_vol16")
rel1 = left_join(rel, lab15_rel, by = "arthCode")
names(rel1) = c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15")
arth_rel = left_join(rel1, lab16_bs_rel, by = "arthCode")
names(arth_rel) = c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15", "mean_lab16_bs")

#find (# of surveys per day for each year/surveyor type)
uniq_vol15 = data.frame(table(vol15_sub$julianday, vol15_sub$circle, vol15_sub$survey)) %>% filter(Freq > 0) 
surv_vol15 = length(uniq_vol15$Freq)
uniq_vol16 = data.frame(table(vol16_sub$julianday, vol16_sub$circle, vol16_sub$survey)) %>% filter(Freq > 0) 
surv_vol16 = length(uniq_vol16$Freq)
uniq_lab15 = data.frame(table(lab15_sub$julianday, lab15_sub$circle, lab15_sub$survey)) %>% filter(Freq > 0) 
surv_lab15 = length(uniq_lab15$Freq)
uniq_lab16_bs = data.frame(table(lab16_bs_sub$julianday, lab16_bs_sub$circle, lab16_bs_sub$survey)) %>% filter(Freq > 0) 
surv_lab16_bs = length(uniq_lab16_bs$Freq)

#calculate mean number of each type of arthropods seen for each year/surveyor type
vol15_means= vol15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol15))
vol16_means= vol16_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_vol16))
lab15_means= lab15_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab15))
lab16_bs_means= lab16_bs_sub %>% group_by(arthCode) %>% dplyr::summarize(mean_count = (sum(count)/surv_lab16_bs))

#merge all means into one dataframe
join = left_join(vol15_means, vol16_means, by = "arthCode")
names(join) = c("arthCode", "mean_vol15", "mean_vol16")
join1 = left_join(join, lab15_means, by = "arthCode")
names(join1) =c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15")
arth_means = left_join(join1, lab16_bs_means, by = "arthCode")
names(arth_means) = c("arthCode", "mean_vol15", "mean_vol16", "mean_lab15", "mean_lab16_bs")

#plot relative and mean arth frequencies
## Example code:
# barplot(counts, main="Car Distribution by Gears and VS",
# xlab="Number of Gears", col=c("darkblue","red"),
# legend = rownames(counts), beside=TRUE) 
par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))

lab_selected1 = as.matrix(lab_selected) #create matrix w/o arth code?
barplot(lab_selected1, legend = lab_selected$arthCode, las = 2, cex.names = .7, 
        ylab = "Proportion of Arthopods Seen") #this needs to be cleaned up

arth_means_m = as.matrix(arth_means) #create matrix w/o arth code?
barplot(arth_means_m, legend = arth_means$arthCode, las = 2, cex.names = .7, 
        ylab = "Mean Arths Observed per Survey")


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

