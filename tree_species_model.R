#Tree species model
source("data_cleaning.R")

#Create 1 dataset with all data from PR/NCBG 2015 & 2016
lab.triangle = rbind(labdata.pr, labdata.bg)

#add unique identifier column for surveys
lab.triangle$identifier = paste0(lab.triangle$site, lab.triangle$circle, lab.triangle$survey, lab.triangle$date)

#subset into beatsheets and visual surveys
bs = lab.triangle[lab.triangle$surveyType == "Beat_Sheet", ]
vis = lab.triangle[lab.triangle$surveyType == "Visual", ]

#group by unique surveys and summarize arthropod density
vis_grouped = vis %>% group_by(site, circle, survey, date)
vis_count = dplyr::summarise(vis_grouped, sum(count))
vis_count$identifier = paste0(vis_count$site, vis_count$circle, vis_count$survey, vis_count$date)
vis_biomass = dplyr::summarise(vis_grouped, sum(biomass))
vis_biomass$identifier = paste0(vis_biomass$site, vis_biomass$circle, vis_biomass$survey, vis_biomass$date)

#merge to add tree species data to summary data
merge(vis_count, lab )
#create list of unique surveys
uniq_surv = unique(labdata.triangle[, c("site", "circle", "survey", "date")])
uniq_surv$identifier = paste0(uniq_surv$site, uniq_surv$date, uniq_surv$circle, uniq_surv$survey)

#
