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
names(vis_count) = c("site", "circle", "survey", "date", "sum_count", "identifier")
vis_biomass = dplyr::summarise(vis_grouped, sum(biomass))
vis_biomass$identifier = paste0(vis_biomass$site, vis_biomass$circle, vis_biomass$survey, vis_biomass$date)
names(vis_count) = c("site", "circle", "survey", "date", "sum_count", "identifier")

#merge to add tree species data to summary data
vis_plants = unique(vis[, c("identifier", "plantSp")]) #there are 16 observations that have multiple plant species for the same surveys- need to go through and find these and correct
#vis_plants1 = data.frame(table(vis_plants))
#vis_plants2 = vis_plants1[vis_plants1$Freq> 0,]
vis_count_final= merge(vis_count, vis_plants, by.x = "identifier", by.y = "identifier", all.x = T) #this has larger dimensions than vis_count because of extra plant species
vis_biomass_final= merge(vis_biomass, vis_plants, by.x = "identifier", by.y = "identifier", all.x = T)


#create list of unique surveys
#uniq_surv = unique(labdata.triangle[, c("site", "circle", "survey", "date")])
#uniq_surv$identifier = paste0(uniq_surv$site, uniq_surv$date, uniq_surv$circle, uniq_surv$survey)

#Modeling
lm.count = lm(sum_count ~ plantSp, data=vis_count_final) #so many different capitalizations of plant species, need to merge with plant species master lists for PR and NCBG

