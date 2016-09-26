#Tree species model
source("data_cleaning.R")

#read in data
all_surveyTrees <- read.csv("data/tbl_surveyTrees.csv", header=T)

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
names(vis_biomass) = c("site", "circle", "survey", "date", "sum_biomass", "identifier")

#add year column to summarized arth densities
vis_year = unique(vis[, c("identifier", "year")])
vis_count1 = merge(vis_count, vis_year, by.x = "identifier", by.y = "identifier")
vis_biomass1 = merge(vis_biomass, vis_year, by.x="identifier", by.y="identifier")

#Get list of tree species for each year for NCBG & PR
all_surveyTrees_triangle = all_surveyTrees[all_surveyTrees$siteID %in% c("117", "8892356"),]
trees_2016 = all_surveyTrees_triangle
trees_2016$year = "2016"
trees_2015 = all_surveyTrees_triangle
trees_2015$year = "2015"

#Make changes to 2015 tree species to reflect species that were changed in 2016
trees_2015$plantSpecies = as.character(trees_2015$plantSpecies)
trees_2015[11, 4] = "Devil's walkingstick"  #changed 117 3A
trees_2015[34, 4] = "Tuliptree" #changed 117 7D
trees_2015[77, 4] = "Mapleleaf viburnum" #changed 8892356 4B
trees_2015[95, 4] = "Sugar maple" #changed 8892356 7E
trees_2015$plantSpecies = as.factor(trees_2015$plantSpecies)

#add trees 2014
trees_2014 = trees_2015
trees_2014$year = "2014"

#create one dataframe with tree species for 2015 & 2016
trees_both = rbind(trees_2015, trees_2016)

#merge to add tree species data to summary data
vis_count1$loc_ID = paste0("site", "circle", "survey", "year")
vis_count_final = merge(vis_count1, trees_both, by.x = "identifier", by.y = "identifier", all.x = T) #have to create identifier that works for both of these dfs 
vis_biomass_final = merge(vis_biomass1, trees_both, by.x = "identifier", by.y = "identifier", all.x = T)

#Modeling
lm.count = lm(sum_count ~ plantSp, data=vis_count_final) #so many different capitalizations of plant species, need to merge with plant species master lists for PR and NCBG

