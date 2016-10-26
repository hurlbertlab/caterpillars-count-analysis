#Tree species model
source("data_cleaning.R")

#load packages
library(agricolae)

#read in data
all_surveyTrees <- read.csv("data/tbl_surveyTrees.csv", header=T)


#Create 1 dataset with all data from PR/NCBG 2015 & 2016
lab.triangle = rbind(labdata.pr, labdata.bg)

#Remove large arth abservations (to do or not to do?)
lab.triangle <- lab.triangle[!(lab.triangle$count > 10),]

#subset for  visual surveys
vis_tri = lab.triangle[lab.triangle$surveyType == "Visual", ]

#merge triangle visual surveys with list of accurate triangle plant species
vis_tri$loc_ID = paste0(vis_tri$site, vis_tri$circle, vis_tri$survey, vis_tri$year)
vis_tri1 = merge(vis_tri, trees_tri, by.x= "loc_ID", by.y= "loc_ID", all.x=TRUE)
vis_tri2 = dplyr::select(vis_tri1, -clean_plantSp, -circle.y, -survey.y, -year.y, -notes.y)
names(vis_tri2) = c("loc_ID", "surveyID", "userID", "site", "survey", "circle", "date", "julianday", "plantSp", "herbivory", 
               "arthropod", "arthCode", "length", "count", "notes", "surveyType", "leafCount", "wetLeaves", "year", 
               "biomass", "siteID", "clean_plantSp")

#Get list of tree species for each year for NCBG & PR
all_surveyTrees_triangle = all_surveyTrees[all_surveyTrees$siteID %in% c("117", "8892356"),]
trees_2016 = all_surveyTrees_triangle
trees_2016$year = "2016"
trees_2015 = all_surveyTrees_triangle
trees_2015$year = "2015"

#add trees 2014
trees_2014 = trees_2015
trees_2014$year = "2014"

#create one dataframe with tree species for 2014, 2015 & 2016
trees_tri = rbind(trees_2014, trees_2015, trees_2016)
trees_tri$loc_ID = paste0(trees_tri$site, trees_tri$circle, trees_tri$survey, trees_tri$year)

#adjust for surveys that were moved to different trees in different years
trees_tri$plantSpecies = as.character(trees_tri$plantSpecies)
trees_tri$plantSpecies[trees_tri$loc_ID %in% c("1173A2015", "1173A2014")] = "Devil's walkingstick" 
trees_tri$plantSpecies[trees_tri$loc_ID %in% c("1177D2015", "1177D2014")] = "Tuliptree" 
trees_tri$plantSpecies[trees_tri$loc_ID %in% c("88923564B2015", "88923564B2014")] = "Mapleleaf viburnum" 
trees_tri$plantSpecies[trees_tri$loc_ID %in% c("88923567E2015", "88923567E2014")] = "Sugar maple"  
trees_tri$plantSpecies = as.factor(trees_tri$plantSpecies)

#merge tree sp with corrections with vis_tri
vis_tri1 = merge(vis_tri, trees_tri, by.x= "loc_ID", by.y= "loc_ID", all.x=TRUE)
vis_tri2 = dplyr::select(vis_tri1, -loc_ID, -clean_plantSp, -circle.y, -survey.y, -year.y, -siteID)
names(vis_tri2) = c("surveyID", "userID", "site", "survey", "circle", "date", "julianday", "plantSp", "herbivory", 
                    "arthropod", "arthCode", "length", "count", "notes.y", "notes.x", "surveyType", "leafCount", "wetLeaves", "year", 
                    "biomass", "clean_plantSp")

#merge triangle surveys and appalachian surveys
vis_app = cleandata.app[cleandata.app$surveyType=="Visual",]
vis = rbind(vis_tri2,vis_app)

#add unique identifier column for surveys
vis$identifier = paste0(vis$site, vis$circle, vis$survey, vis$date)
vis$loc_ID = paste0(vis$site, vis$circle, vis$survey, vis$year)

#create unique tree-locations list for all trees, 2010-2016, app and triangle
trees_all = unique(vis[,c("clean_plantSp", "loc_ID")])


#group by unique surveys and summarize arthropod density
vis_grouped = vis %>% group_by(site, circle, survey, date)
vis_count = dplyr::summarise(vis_grouped, sum(count))
vis_count$identifier = paste0(vis_count$site, vis_count$circle, vis_count$survey, vis_count$date)
names(vis_count) = c("site", "circle", "survey", "date", "sum_count", "identifier")
vis_count = data.frame(vis_count)

vis_biomass = dplyr::summarise(vis_grouped, sum(biomass)) #biomass estimates only exist for ~ half of unique surveys. could populate fields w/zero count w/ zeros, but what about others
vis_biomass$identifier = paste0(vis_biomass$site, vis_biomass$circle, vis_biomass$survey, vis_biomass$date)
names(vis_biomass) = c("site", "circle", "survey", "date", "sum_biomass", "identifier")
vis_biomass = data.frame(vis_biomass)

#add year column to summarized arth densities
vis_year = unique(vis[, c("identifier", "year")])
vis_count1 = merge(vis_count, vis_year, by.x = "identifier", by.y = "identifier")
vis_biomass1 = merge(vis_biomass, vis_year, by.x="identifier", by.y="identifier")

#merge to add tree species data to summary data
vis_count1$loc_ID = paste0(vis_count1$site, vis_count1$circle, vis_count1$survey, vis_count1$year)
vis_biomass1$loc_ID = paste0(vis_biomass1$site, vis_biomass1$circle, vis_biomass1$survey, vis_biomass1$year)

count_merged = merge(vis_count1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T) 
biomass_merged = merge(vis_biomass1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T)

count_merged1 = dplyr::select(count_merged, site, circle, survey, year, sum_count, clean_plantSp)
names(count_merged1) = c("site", "circle", "survey", "year", "sum_count", "plantSpecies")

biomass_merged1 = dplyr::select(biomass_merged, site, circle, survey, year, sum_biomass, clean_plantSp)
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

aov.raw = aov(sum_count ~ plantSpecies, data=count_common) 
lm.raw = lm(sum_count ~ plantSpecies, data=count_common)

HSD_raw<- HSD.test(lm.raw, "plantSpecies")

#Create dataframe with results of HSD test
groups.df = data.frame(HSD_raw$groups)
means.df = data.frame(HSD_raw$means)
means.ordered = means.df[order(means.df$sum_count, decreasing = T),]
plotting = cbind(groups.df, means.ordered)
plotting = dplyr::select(plotting, -sum_count)
names(plotting)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#Plot HSD results
par(mar=c(6,4,4,4))
barplot(plotting$means, names.arg=plotting$tree_sp, las=2, ylab="mean arth density", 
        main = "Mean Arth Density by Tree Species", ylim = c(0,5), cex.names=.65, 
        cex.axis = .75, 
        col = c("red", "red", "purple", "purple", "purple", "purple", "purple", "purple", "blue", "blue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=4, plotting$M)



#Huge standard deviation: bar.err(HSD_raw$means, variation="SD") 

lotsabugs=dplyr::filter(lab.triangle, count > 10)
lotsabugs1= select(lotsabugs, count, plantSp)
   

