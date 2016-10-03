#Tree species model
source("data_cleaning.R")

#load packages
library(agricolae)
library(ggplot2)

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
vis_biomass = dplyr::summarise(vis_grouped, sum(biomass)) #biomass estimates only exist for ~ half of unique surveys. could populate fields w/zero count w/ zeros, but what about others
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

#create one dataframe with tree species for 2014, 2015 & 2016
trees_all = rbind(trees_2014, trees_2015, trees_2016)
trees_all$loc_ID = paste0(trees_all$site, trees_all$circle, trees_all$survey, trees_all$year)

#merge to add tree species data to summary data
vis_count1$loc_ID = paste0(vis_count1$site, vis_count1$circle, vis_count1$survey, vis_count1$year)
vis_biomass1$loc_ID = paste0(vis_biomass1$site, vis_biomass1$circle, vis_biomass1$survey, vis_biomass1$year)


count_merged = merge(vis_count1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T) #have to create identifier that works for both of these dfs 
biomass_merged = merge(vis_biomass1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T)

count_merged1 = dplyr::select(count_merged, site, circle.x, survey.x, year.x, sum_count, plantSpecies)
names(count_merged1) = c("site", "circle", "survey", "year", "sum_count", "plantSpecies")

biomass_merged1 = dplyr::select(biomass_merged, site, circle.x, survey.x, year.x, sum_biomass, plantSpecies)
names(biomass_merged1) = c("site", "circle", "survey", "year", "sum_biomass", "plantSpecies")

#Find 10 most common tree species to use in analysis 
trees = select(count_merged1, plantSpecies )
trees_freq = data.frame(table(trees))
trees_ordered= trees_freq[order(trees_freq$Freq, decreasing = T),] 
common_trees = trees_ordered[1:10,]

#Only use surveys conducted on 10 most common tree species
count_common = dplyr::filter(count_merged1, plantSpecies %in% common_trees$trees)
biomass_common = dplyr::filter(biomass_merged1, plantSpecies %in% common_trees$trees)

#Calculate mean arthropod density for each tree species
#count_grouped_sp = count_common %>% group_by(plantSpecies)
#count_means = dplyr::summarise(count_grouped_sp, mean(sum_count)) #check one of these to confirm that the mean is divided by the number of occurrences
#names(count_means) = c("plantSpecies", "mean_dens")

#biomass_grouped_sp = biomass_common %>% group_by(plantSpecies)
#biomass_means = dplyr::summarise(biomass_grouped_sp, mean(sum_biomass))
#names(biomass_means) = c("plantSpecies", "mean_biomass")

#AOV models
#count_means$plantSpecies= as.character(count_means$plantSpecies)
#aov.count = aov(mean_dens ~ plantSpecies, data=count_means) 
#aov.biomass = aov(mean_biomass ~ plantSpecies, data=biomass_means) #getting error, not sure what it means

aov.raw = aov(sum_count ~ plantSpecies, data=count_common) #I think this is how I'm supposed to do it
lm.raw = lm(sum_count ~ plantSpecies, data=count_common)

TukeyHSD(aov.raw)
HSD_raw<- HSD.test(lm.raw, "plantSpecies")

#Create dataframe with results of HSD test
groups.df = data.frame(HSD_raw$groups)
means.df = data.frame(HSD_raw$means)
means.ordered = means.df[order(means.df$sum_count, decreasing = T),]
plotting = cbind(groups.df, means.ordered)
plotting = dplyr::select(plotting, -sum_count)
names(plotting)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#Plot HSD results
barplot(plotting$means, names.arg=plotting$tree_sp, las=2, ylab="mean arth density", 
        main = "Mean Arth Density by Tree Species", ylim = c(0,5), cex.names=.55, 
        cex.axis = .75, 
        col = c("red", "red", "purple", "purple", "purple", "purple", "purple", "purple", "blue", "blue"))
        
#outliers have not been removed- should they be?

    


