#Tree species model
source("cleaning_scripts/data_cleaning.R")

#load packages
library(agricolae)
library(tidyr)
library(dplyr)

#read in data
all_surveyTrees = read.csv("data/arths_survey/tbl_surveyTrees.csv", header=F)
leaf_app = read.table("data/arths_survey/LeafAreaDatabase_20131126.txt", header=T, sep= '\t', quote="\"", fill = T, stringsAsFactors = FALSE)
leaf_tri = read.table("data/arths_survey/LeafPhotoAreaDatabase_CC.txt", header=T, sep= '\t', quote="\"", fill = T)     #this is only a small subset of leaf area for the triangle for the most common trees
plant_codes = read.csv("data/arths_survey/USA&AppalachianTrees_2016.csv", stringsAsFactors = F, header=T)

#organizes column headers
names(all_surveyTrees) = c("siteID", "circle", "survey", "plantSpecies")

#Create 1 dataset with all data from PR/NCBG 2015 & 2016
lab.triangle = rbind(labdata.pr, labdata.bg)

#subset for  visual surveys
vis_tri = lab.triangle[lab.triangle$surveyType == "Visual", ]

#add loc_ID column to specify unique trees for each year
vis_tri$loc_ID = paste0(vis_tri$site, vis_tri$circle, vis_tri$survey, vis_tri$year)

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
vis_tri2 = dplyr::select(vis_tri1, -loc_ID, -clean_plantSp, -circle.y, -survey.y, -year.y, -siteID, -date)
names(vis_tri2) = c("surveyID", "userID", "site", "survey", "circle", "date", "julianday", "plantSp", "herbivory", 
                    "arthropod", "arthCode", "length", "count", "notes.y", "notes.x", "surveyType", "leafCount", "wetLeaves", "year", 
                    "biomass", "clean_plantSp")

#merge triangle surveys and appalachian surveys
vis_app = cleandata.app %>% dplyr::filter(surveyType =="Visual") %>% dplyr::select(-date) %>% dplyr::rename(date = date2)
vis = rbind(vis_tri2, vis_app)

#add unique identifier column for surveys
vis$identifier = paste0(vis$site, vis$circle, vis$survey, vis$date)
vis$loc_ID = paste0(vis$site, vis$circle, vis$survey, vis$year)

#create unique tree-locations list for all trees, 2010-2016, app and triangle
trees_all = unique(vis[,c("clean_plantSp", "loc_ID")])

#subset to bird food and caterpillars as groups that are being affected by bottom-up
birdfood = c('ARAN', 'AUCH', 'COLE', 'DIPT', 'HETE', 'LEPL', 'ORTH', 'LEPA')
vis_food = filter(vis, arthCode %in% birdfood)
vis_caterpillar = filter(vis, arthCode == "LEPL")

#group by unique surveys and summarize arthropod density
vis_food_count = vis_food %>% group_by(site, circle, survey, date) %>% summarise(sum(count))
vis_food_count$identifier = paste0(vis_food_count$site, vis_food_count$circle, vis_food_count$survey, vis_food_count$date)
names(vis_food_count) = c("site", "circle", "survey", "date", "sum_count", "identifier")
vis_food_count = data.frame(vis_food_count)

vis_caterpillar_count = vis_caterpillar %>% group_by(site, circle, survey, date) %>% summarise(sum(count))
vis_caterpillar_count$identifier = paste0(vis_caterpillar_count$site, vis_caterpillar_count$circle, vis_caterpillar_count$survey, vis_caterpillar_count$date)
names(vis_caterpillar_count) = c("site", "circle", "survey", "date", "sum_count", "identifier")
vis_caterpillar_count = data.frame(vis_caterpillar_count)

#vis_biomass = dplyr::summarise(vis_grouped, sum(biomass)) #biomass estimates only exist for ~ half of unique surveys. could populate fields w/zero count w/ zeros, but what about others
#vis_biomass$identifier = paste0(vis_biomass$site, vis_biomass$circle, vis_biomass$survey, vis_biomass$date)
#names(vis_biomass) = c("site", "circle", "survey", "date", "sum_biomass", "identifier")
#vis_biomass = data.frame(vis_biomass)

#add year column to summarized arth densities
vis_year = unique(vis[, c("identifier", "year")])
vis_food_count1 = merge(vis_food_count, vis_year, by.x = "identifier", by.y = "identifier")
vis_caterpillar_count1 = merge(vis_caterpillar_count, vis_year, by.x = "identifier", by.y = "identifier")

#merge to add tree species data to summary data for bird food and catepillars
vis_food_count1$loc_ID = paste0(vis_food_count1$site, vis_food_count1$circle, vis_food_count1$survey, vis_food_count1$year)
food_count_merged = merge(vis_food_count1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T) 
food_count_merged1 = dplyr::select(food_count_merged, site, circle, survey, year, sum_count, clean_plantSp)
names(food_count_merged1) = c("site", "circle", "survey", "year", "sum_count", "plantSpecies")

vis_caterpillar_count1$loc_ID = paste0(vis_caterpillar_count1$site, vis_caterpillar_count1$circle, vis_caterpillar_count1$survey, vis_caterpillar_count1$year)
caterpillar_count_merged = merge(vis_caterpillar_count1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T) 
caterpillar_count_merged1 = dplyr::select(caterpillar_count_merged, site, circle, survey, year, sum_count, clean_plantSp)
names(caterpillar_count_merged1) = c("site", "circle", "survey", "year", "sum_count", "plantSpecies")


#Find 10 most common tree species to use in analysis for each set of locations
#you need to ask about for loops!!!
triangle = c(117, 8892356)
appalachians = c(6391028, 6391108, 2704132, 2704111, 6391138, 6391006, 8892036, 8204240, 8204206, 8892025, 6303105, 6303117,
                 6390615, 8290339, 6390644, 8290344, 8890236, 8890517, 8890009, 8890029, 8890538, 8890223, 8892130, 8892242,
                 8892217, 8890721, 8890745, 8892305, 8892346, 8892112, 6302205, 8290243, 6390627, 8204219, 6390944, 6390909)
#both sets (in case it is needed later)
trees = food_count_merged1 %>% select(plantSpecies) # this will need to be redone
trees_freq = data.frame(table(trees))
trees_ordered = trees_freq[order(trees_freq$Freq, decreasing = T),] 
trees_ordered1 = filter(trees_ordered, trees !="UNID") # remove unidentified tree species
common_trees = trees_ordered1[1:10,]
#just Appalachians
trees_app = food_count_merged1 %>% filter(site %in% appalachians) %>% dplyr::select(plantSpecies) 
trees_freq_app = data.frame(table(trees_app))
trees_ordered_app = trees_freq_app[order(trees_freq_app$Freq, decreasing = T),] 
trees_ordered1_app = filter(trees_ordered_app, trees_app !="UNID") # remove unidentified tree species
common_trees_app = trees_ordered1_app[1:10,]
#just triangle
trees_tri = food_count_merged1 %>% filter(site %in% triangle) %>% dplyr::select(plantSpecies) 
trees_freq_tri = data.frame(table(trees_tri))
trees_ordered_tri = trees_freq_tri[order(trees_freq_tri$Freq, decreasing = T),] 
trees_ordered1_tri = filter(trees_ordered_tri, trees_tri !="UNID") # remove unidentified tree species
common_trees_tri = trees_ordered1_tri[1:10,]

#find 24 most common tree species to use in graphing extremes (these each have at least 100 surveys)
common_24 = trees_ordered1[1:24,]

#get avg leaf area for each species
leaf_app$LeafArea_pixels = as.numeric(leaf_app$LeafArea_pixels)
leaf_app$RefArea_pixels = as.numeric(leaf_app$RefArea_pixels)
leaf_app$RefArea_cm2 = as.numeric(leaf_app$RefArea_cm2)
leaf_app$TreeSpecies = as.factor(leaf_app$TreeSpecies)
leaf_app$leaf_area_cm2 = (leaf_app$LeafArea_pixels/leaf_app$RefArea_pixels)*(leaf_app$RefArea_cm2)
leaf_app_clean = leaf_app[leaf_app$leaf_area_cm2 != is.na(leaf_app$leaf_area_cm2), ]

#merge leaf areas from appalachians to subset of triangle
leaf_app_clean1 = dplyr::select(leaf_app_clean, TreeSpecies, leaf_area_cm2)
names(leaf_app_clean1) = c("TreeCodes", "leaf_area_cm2")
leaf_tri1 = dplyr::select(leaf_tri, TreeCode, Leaf.Area..cm.2.)
names(leaf_tri1) = c("TreeCodes", "leaf_area_cm2")
all_leaves = rbind(leaf_app_clean1, leaf_tri1)
  
leaves_grouped =  group_by(all_leaves, TreeCodes) 
leaves_sp = dplyr::summarize(leaves_grouped, (mean(leaf_area_cm2)))

#merge avg leaf area app/species with common names 
leaves_sp1 = merge(leaves_sp, plant_codes, by.x = "TreeCodes", by.y = "TreeCode", all.x=T)
leaves_sp1 = as.data.frame(leaves_sp1)
leaves_sp1 = dplyr::select(leaves_sp1, -TreeCodes, -TreeSciName, -Notes)

names(leaves_sp1) = c("avg_leaf_area_cm2", "ComName")

#are the NA TreeSpecies typos or missing? (appear to be typos, will leave b/c just need leaf approximation)
missingnames = c("ACPU", "ACRV", "ACSU","ARSP", "ASCA", "ASH", "CADRE","CAKYA", "CALA","CARYA"
                 ,"COPL","ELUM","FLAV","GUAR","GUSP","HALA","ILAM","LIAM","LIST","MAER", "NYSA", 
                 "NYSS", "OYAR", "POPS", "PRUNIS", "PUMO", "QUAO","QUMI", "RASPBERRY", "RH MA", "RHAR",
                 "SAAB","SYOR","TICO","TORA")
#for (code in missingnames) {
#  print(sum(leaf_app$TreeSpecies == code))}

#merge avg leaf area with arth density data
food_count_merged2 = merge(food_count_merged1, leaves_sp1, by.x = "plantSpecies", by.y="ComName", all.x = T)
food_count_merged2 = dplyr::select(food_count_merged2, plantSpecies, site, circle, survey, year, sum_count, avg_leaf_area_cm2)

caterpillar_count_merged2 = merge(caterpillar_count_merged1, leaves_sp1, by.x = "plantSpecies", by.y="ComName", all.x = T)
caterpillar_count_merged2 = dplyr::select(caterpillar_count_merged2, plantSpecies, site, circle, survey, year, sum_count, avg_leaf_area_cm2)

#normalize arth density by avg area
food_count_merged2$count_norm = (food_count_merged2$sum_count/food_count_merged2$avg_leaf_area_cm2)*mean(food_count_merged2$avg_leaf_area_cm2, na.rm=TRUE)
caterpillar_count_merged2$count_norm = (caterpillar_count_merged2$sum_count/caterpillar_count_merged2$avg_leaf_area_cm2)*mean(caterpillar_count_merged2$avg_leaf_area_cm2, na.rm=TRUE)

#log transform 
food_count_merged2$count_log10 = log10(food_count_merged2$count_norm+.01) 
caterpillar_count_merged2$count_log10 = log10(caterpillar_count_merged2$count_norm+.01) 

#subset to surveys conducted in appalachians or triangle
food_count_tri= dplyr::filter(food_count_merged2, site %in% triangle)
caterpillar_count_tri = dplyr::filter(caterpillar_count_merged2, site %in% triangle)
food_count_app = dplyr::filter(food_count_merged2, site %in% appalachians)
caterpillar_count_app = dplyr::filter(caterpillar_count_merged2, site %in% appalachians)

#Only use surveys conducted on 10 most common tree species in each location
#all locations
#count_common = dplyr::filter(count_merged2, plantSpecies %in% common_trees$trees)
#just app
count_common_app_food = dplyr::filter(food_count_merged2, plantSpecies %in% common_trees_app$trees_app)
count_common_app_caterpillar = dplyr::filter(caterpillar_count_merged2, plantSpecies %in% common_trees_app$trees_app)
#just tri
count_common_tri_food = dplyr::filter(food_count_merged2, plantSpecies %in% common_trees_tri$trees_tri)
count_common_tri_caterpillar = dplyr::filter(caterpillar_count_merged2, plantSpecies %in% common_trees_tri$trees_tri)
#biomass_common = dplyr::filter(biomass_merged2, plantSpecies %in% common_trees$trees)

#Plot HSD results

#bird food in the appalachians
lm.log_app_food = lm(count_log10 ~ plantSpecies, data= count_common_app_food)
HSD_log_app_food<- HSD.test(lm.log_app_food, "plantSpecies")

#Create dataframe with results of HSD test
groups.log.df_app_food = data.frame(HSD_log_app_food$groups)
means.log.df_app_food = data.frame(HSD_log_app_food$means)
means.log.ordered_app_food = means.log.df_app_food[order(means.log.df_app_food$count_log10, decreasing = T),]
plotting.log_app_food = cbind(groups.log.df_app_food, means.log.ordered_app_food)
plotting.log_app_food = dplyr::select(plotting.log_app_food, -count_log10)
names(plotting.log_app_food)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#caterpillar in the appalachians
lm.log_app_caterpillar = lm(count_log10 ~ plantSpecies, data= count_common_app_caterpillar)
HSD_log_app_caterpillar<- HSD.test(lm.log_app_caterpillar, "plantSpecies")

#Create dataframe with results of HSD test
groups.log.df_app_caterpillar = data.frame(HSD_log_app_caterpillar$groups)
means.log.df_app_caterpillar = data.frame(HSD_log_app_caterpillar$means)
means.log.ordered_app_caterpillar = means.log.df_app_caterpillar[order(means.log.df_app_caterpillar$count_log10, decreasing = T),]
plotting.log_app_caterpillar = cbind(groups.log.df_app_caterpillar, means.log.ordered_app_caterpillar)
plotting.log_app_caterpillar = dplyr::select(plotting.log_app_caterpillar, -count_log10)
names(plotting.log_app_caterpillar)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#food in the triangle
lm.log_tri_food = lm(count_log10 ~ plantSpecies, data= count_common_tri_food)
HSD_log_tri_food<- HSD.test(lm.log_tri_food, "plantSpecies")

#Create dataframe with results of HSD test
groups.log.df_tri_food = data.frame(HSD_log_tri_food$groups)
means.log.df_tri_food = data.frame(HSD_log_tri_food$means)
means.log.ordered_tri_food = means.log.df_tri_food[order(means.log.df_tri_food$count_log10, decreasing = T),]
plotting.log_tri_food = cbind(groups.log.df_tri_food, means.log.ordered_tri_food)
plotting.log_tri_food = dplyr::select(plotting.log_tri_food, -count_log10)
names(plotting.log_tri_food)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#caterpillar in the triangle
lm.log_tri_caterpillar = lm(count_log10 ~ plantSpecies, data= count_common_tri_caterpillar)
HSD_log_tri_caterpillar<- HSD.test(lm.log_tri_caterpillar, "plantSpecies")

#Create dataframe with results of HSD test
groups.log.df_tri_caterpillar = data.frame(HSD_log_tri_caterpillar$groups)
means.log.df_tri_caterpillar = data.frame(HSD_log_tri_caterpillar$means)
means.log.ordered_tri_caterpillar = means.log.df_tri_caterpillar[order(means.log.df_tri_caterpillar$count_log10, decreasing = T),]
plotting.log_tri_caterpillar = cbind(groups.log.df_tri_caterpillar, means.log.ordered_tri_caterpillar)
plotting.log_tri_caterpillar = dplyr::select(plotting.log_tri_caterpillar, -count_log10)
names(plotting.log_tri_caterpillar)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#Plot HSD results
par(mfrow = c(2, 2), mar=c(7,4,3,3))
barplot(plotting.log_app_food$means, names.arg=plotting.log_app_food$tree_sp, las=2, ylab="log mean arth density", 
              ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
              col = c("darkblue", "darkblue", "blue", "deepskyblue3", "deepskyblue2", "deepskyblue1", "deepskyblue1", "deepskyblue1", "deepskyblue1", "aliceblue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_food$M)

#caterpillar appalachians
barplot(plotting.log_app_caterpillar$means, names.arg=plotting.log_app_caterpillar$tree_sp, las=2, ylab="log mean arth density", 
         ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = c("darkblue", "darkblue", "blue", "deepskyblue3", "deepskyblue2", "deepskyblue1", "deepskyblue1", "deepskyblue1", "deepskyblue1", "aliceblue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_app_caterpillar$M)

#bird food triangle
barplot(plotting.log_tri_food$means, names.arg=plotting.log_tri_food$tree_sp, las=2, ylab="log mean arth density", 
        ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = c("darkblue", "darkblue", "blue", "deepskyblue3", "deepskyblue2", "deepskyblue1", "deepskyblue1", "deepskyblue1", "deepskyblue1", "aliceblue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_food$M)

#caterpillar triangle
barplot(plotting.log_tri_caterpillar$means, names.arg=plotting.log_tri_caterpillar$tree_sp, las=2, ylab="log mean arth density", 
         ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = c("darkblue", "darkblue", "blue", "deepskyblue3", "deepskyblue2", "deepskyblue1", "deepskyblue1", "deepskyblue1", "deepskyblue1", "aliceblue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log_tri_caterpillar$M)

#max and min average normalized values for each tree species
app_food_range= count_common_app_food %>%
               group_by(plantSpecies) %>%
               dplyr::summarize(mean = mean(count_norm)) %>%
               filter(plantSpecies != "Carolina silverbell") %>%
               dplyr::summarize(min = min(mean), max = max(mean))

app_caterpillar_range = count_common_app_caterpillar %>%
                       group_by(plantSpecies) %>%
                       dplyr::summarize(mean = mean(count_norm)) %>%
                       filter(plantSpecies != "Carolina silverbell") %>%
                       dplyr::summarize(min = min(mean), max = max(mean))
tri_food_range = count_common_tri_food %>%
                group_by(plantSpecies) %>%
                dplyr::summarize(mean = mean(count_norm)) %>%
                filter(plantSpecies != "Pin oak") %>%
                dplyr::summarize(min = min(mean), max = max(mean))
tri_caterpillar_range = count_common_tri_caterpillar %>%
                       group_by(plantSpecies) %>%
                       dplyr::summarize(mean = mean(count_norm)) %>%
                       filter(plantSpecies != "Pin oak") %>%
                       dplyr::summarize(min = min(mean), max = max(mean))







