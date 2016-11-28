#Plots for CC paper: Absolute arth densities and relative arth densities 
source("data_cleaning.R")

#subset lab data to the the circles regularly surveyed by volunteers
lab = filter(labdata.pr, circle %in% c("1", "2", "3", "4", "5", "6", "7", "8"))

#find distribution of surveys by date in each year
vol15 = filter(volunteer.pr, year == 2015) 
vol15_dates = vol15 %>% group_by(julianday) %>% tally
vol16 = filter(volunteer.pr, year == 2016) 
vol16_dates = vol16 %>% group_by(julianday) %>% tally

lab15 = lab %>% filter(year == 2015)
lab15_dates = lab15 %>% group_by(julianday) %>% tally
lab16 = lab %>% filter(year == 2016) 
lab16_dates = lab16 %>% group_by(julianday) %>% tally

#plots for sampling windows and sampling effort
par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
plot(vol15_dates$julianday, vol15_dates$n, xlim = c(130, 280))
plot(lab15_dates$julianday, lab15_dates$n, xlim = c(130, 280))
plot(vol16_dates$julianday, vol16_dates$n, xlim = c(130, 280))
plot(lab16_dates$julianday, lab16_dates$n, xlim = c(130, 280))

#summarize the average density by order per tree #check to ensure there are no survey days with 2 surveys/day
uniq_vol15= vol15 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count)) 
uniq_vol16= vol16 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
uniq_lab15= lab15 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
uniq_lab16= lab16 %>% group_by(julianday, circle, survey) %>% dplyr::summarize(sum_count = sum(count))
