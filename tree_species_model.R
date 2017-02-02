#Tree species model
source("data_cleaning.R")

#load packages
library(agricolae)
library(tidyr)
library(dplyr)



#read in data
all_surveyTrees = read.csv("data/tbl_surveyTrees.csv", header=F)
leaf_app = read.table("data/LeafAreaDatabase_20131126.txt", header=T, sep= '\t', quote="\"", fill = T, stringsAsFactors = FALSE)
leaf_tri = read.table("data/LeafPhotoAreaDatabase_CC.txt", header=T, sep= '\t', quote="\"", fill = T)     #this is only a small subset of leaf area for the triangle for the most common trees
plant_codes = read.csv("USA&AppalachianTrees_2016.csv", stringsAsFactors = F, header=T)

#organizes column headers
names(all_surveyTrees) = c("siteID", "circle", "survey", "plantSpecies")

#Create 1 dataset with all data from PR/NCBG 2015 & 2016
lab.triangle = rbind(labdata.pr, labdata.bg)

#subset for  visual surveys
vis_tri1 = lab.triangle[lab.triangle$surveyType == "Visual", ]

#subset to bird food
birdfood = c('ARAN', 'AUCH', 'COLE', 'DIPT', 'HETE', 'LEPL', 'ORTH')
vis_tri = filter(vis_tri1, arthCode %in% birdfood)

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

#vis_biomass = dplyr::summarise(vis_grouped, sum(biomass)) #biomass estimates only exist for ~ half of unique surveys. could populate fields w/zero count w/ zeros, but what about others
#vis_biomass$identifier = paste0(vis_biomass$site, vis_biomass$circle, vis_biomass$survey, vis_biomass$date)
names(vis_biomass) = c("site", "circle", "survey", "date", "sum_biomass", "identifier")
#vis_biomass = data.frame(vis_biomass)

#add year column to summarized arth densities
vis_year = unique(vis[, c("identifier", "year")])
vis_count1 = merge(vis_count, vis_year, by.x = "identifier", by.y = "identifier")
#vis_biomass1 = merge(vis_biomass, vis_year, by.x="identifier", by.y="identifier")

#merge to add tree species data to summary data
vis_count1$loc_ID = paste0(vis_count1$site, vis_count1$circle, vis_count1$survey, vis_count1$year)
#vis_biomass1$loc_ID = paste0(vis_biomass1$site, vis_biomass1$circle, vis_biomass1$survey, vis_biomass1$year)

count_merged = merge(vis_count1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T) 
biomass_merged = merge(vis_biomass1, trees_all, by.x = "loc_ID", by.y = "loc_ID", all.x = T)

count_merged1 = dplyr::select(count_merged, site, circle, survey, year, sum_count, clean_plantSp)
names(count_merged1) = c("site", "circle", "survey", "year", "sum_count", "plantSpecies")

#biomass_merged1 = dplyr::select(biomass_merged, site, circle, survey, year, sum_biomass, clean_plantSp)
names(biomass_merged1) = c("site", "circle", "survey", "year", "sum_biomass", "plantSpecies")

#Find 10 most common tree species to use in analysis 
trees = select(count_merged1, plantSpecies)
trees_freq = data.frame(table(trees))
trees_ordered = trees_freq[order(trees_freq$Freq, decreasing = T),] 
trees_ordered1 = filter(trees_ordered, trees !="UNID") # remove unidentified tree species
common_trees = trees_ordered1[1:10,]

#find 24 most common tree species to use in graphing extremes (these each have at least 100 surveys)
trees = select(count_merged1, plantSpecies)
trees_freq = data.frame(table(trees))
trees_ordered = trees_freq[order(trees_freq$Freq, decreasing = T),] 
trees_ordered1 = filter(trees_ordered, trees !="UNID") # remove unidentified tree species
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
leaves_sp = dplyr::summarize(leaves_grouped, (avg_leaf_area_cm2 = mean(leaf_area_cm2)))

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
for (code in missingnames) {
  print(sum(leaf_app$TreeSpecies == code))}

#merge avg leaf area with arth density data
count_merged2 = merge(count_merged1, leaves_sp1, by.x = "plantSpecies", by.y="ComName", all.x = T)
count_merged2 = dplyr::select(count_merged2, plantSpecies, site, circle, survey, year, sum_count, avg_leaf_area_cm2)

#biomass_merged2 = merge(biomass_merged1, leaves_sp1, by.x = "plantSpecies", by.y="ComName", all.x = T)
#biomass_merged2 = dplyr::select(biomass_merged2, plantSpecies, site, circle, survey, year, sum_biomass, avg_leaf_area_cm2)


#normalize arth density by avg area
count_merged2$count_norm = (count_merged2$sum_count/count_merged2$avg_leaf_area_cm2)*mean(count_merged2$avg_leaf_area_cm2, na.rm=TRUE)
#biomass_merged2$count_norm = (biomass_merged2$sum_biomass/biomass_merged2$avg_leaf_area_cm2)*mean(biomass_merged2$avg_leaf_area_cm2, na.rm=TRUE)

#log transform 
count_merged2$count_log10 = log10(count_merged2$count_norm+.01) 
#biomass_merged2$biomass_log10 = log10(biomass_merged2$biomass_norm+.01) 

#Only use surveys conducted on 10 most common tree species
count_common = dplyr::filter(count_merged2, plantSpecies %in% common_trees$trees)
#biomass_common = dplyr::filter(biomass_merged2, plantSpecies %in% common_trees$trees)

#HSD model- raw data
lm.raw = lm(count_norm ~ plantSpecies, data=count_common)

HSD_raw<- HSD.test(lm.raw, "plantSpecies")

#Create dataframe with results of HSD test
groups.df = data.frame(HSD_raw$groups)
means.df = data.frame(HSD_raw$means)
means.ordered = means.df[order(means.df$count_norm, decreasing = T),]
plotting = cbind(groups.df, means.ordered)
plotting = dplyr::select(plotting, -count_norm)
names(plotting)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")
write.csv(plotting, 'data/tree_sp_mean_density.csv', row.names = F)

#Plot HSD results

# NOTE!!! should merge color info in based on 'M' designation rather than
# specifying it manually as done below. If the data/analysis changes,
# you might forget to change the colors appropriately!
par(mar=c(6,4,4,4))
barplot(plotting$means, names.arg=plotting$tree_sp, las=2, ylab="mean arth density", 
        main = "Mean Arthropod Density by Tree Species", ylim = c(0,10), cex.names=.65, 
        cex.axis = .75, 
        col = c("red", "red", "purple", "purple", "purple", "purple", "purple", "purple", "blue", "blue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=4, plotting$M)

#Do HSD model- log transformed data
lm.log = lm(count_log10 ~ plantSpecies, data=count_common)
HSD_log<- HSD.test(lm.log, "plantSpecies")

#Create dataframe with results of HSD test
groups.log.df = data.frame(HSD_log$groups)
means.log.df = data.frame(HSD_log$means)
means.log.ordered = means.log.df[order(means.log.df$count_log10, decreasing = T),]
plotting.log = cbind(groups.log.df, means.log.ordered)
plotting.log = dplyr::select(plotting.log, -count_log10)
names(plotting.log)=c("tree_sp", "means", "M", "std", "r", "Min", "Max")

#Plot HSD results
par(mfrow = c(1, 1), mar=c(7,4,3,3))
barplot(plotting.log$means, names.arg=plotting.log$tree_sp, las=2, ylab="log mean arth density", 
        main = "Mean Arth Density by Tree Species", ylim = c(-.3,.4), cex.names=.65, cex.axis = .75, 
        col = c("darkblue", "darkblue", "blue", "deepskyblue3", "deepskyblue2", "deepskyblue1", "deepskyblue1", "deepskyblue1", "deepskyblue1", "aliceblue"))
text(x=seq(from=.7, to= 11.5 ,by=1.2), y=.35, plotting.log$M)
#-----------------------------------------------------------------------test for how often you see an arthropod density at least this extreme-----------------------
#create columns with Y/N data for certain arth density thresholds
count_merged3 = count_merged2
count_merged3$percent40 = ifelse(count_merged3$sum_count >= 40, "Yes", "No")
count_merged3$percent10 = ifelse(count_merged3$sum_count >= 10, "Yes", "No")
count_merged3$percent5 = ifelse(count_merged3$sum_count >= 5, "Yes", "No")
count_merged3$percent1 = ifelse(count_merged3$sum_count >= 1, "Yes", "No")

#get percentage of each tree species that meet the density criteria for each threshold 
percent40 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc40 = sum(percent40=="Yes")/length(percent40))
percent40 = data.frame(percent40)
percent40$not40 = 1-percent40$perc40

percent10 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc10=sum(percent10=="Yes")/length(percent10))
percent10 = data.frame(percent10)
percent10$not10 = 1-percent10$perc10

percent5 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc5 = sum(percent5=="Yes")/length(percent5))
percent5 = data.frame(percent5)
percent5$not5 = 1-percent5$perc5

percent1 = count_merged3 %>% group_by(plantSpecies) %>% dplyr::summarise(perc1 = sum(percent1=="Yes")/length(percent1))
percent1 = data.frame(percent1)
percent1$not1 = 1-percent1$perc1

#create df with all threshold percentages for all species
percent40.10 = merge(percent40, percent10, by="plantSpecies")
percent40.10.5 = merge(percent40.10, percent5, by = "plantSpecies")
extremes_bysp = merge(percent40.10.5, percent1, by = "plantSpecies")

#merge threshold percentages with avg leaf areas per species
extremes_bysp1 = merge(extremes_bysp, leaves_sp1, by.x = "plantSpecies", by.y= "ComName") #decrease in trees here- why???? 29 out of 52 possible -> ones we don't have leaf areas for?
extremes_24 = dplyr::filter(extremes_bysp1, plantSpecies %in% common_24$trees) #missing two of these-> why?

#merge plant codes back in for plotting purposes
extremes_241=merge(extremes_24, plant_codes, by.x = "plantSpecies", by.y = "ComName", all.x = TRUE)
extremes_241 = dplyr::select(extremes_241, -TreeSciName, -Notes)
#plot relationship between likelihood of seeing an extreme arth dens and leaf area 
#(tree species above and below the curve tell us which species are good quality, whcih species are poorer quality)

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
lm.40 =lm(extremes_241$perc40 ~ extremes_241$avg_leaf_area_cm2)
p40=plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc40, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=40 Arth", cex.axis = .6)
abline(lm.40)

plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc10, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=10 Arth")
lm.10 =lm(extremes_241$perc10 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.10)

par(mfrow = c(1, 1), mar = c(5, 5, 1, 1))
plot5=plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc5, ylim=c(0,.6), xlab = "Mean Leaf Area (cm2)", ylab = "Proportion of Surveys >=5 Arth", col = "aquamarine", pch=16)
lm.5 =lm(extremes_241$perc5 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.5)
text(extremes_241$avg_leaf_area_cm2, extremes_241$perc5, labels = extremes_241$TreeCode, cex = .7)


plot(extremes_241$avg_leaf_area_cm2, extremes_241$perc1, ylim=c(0,1), xlab = "Mean Leaf Area (cm2)", ylab = "Percent of Surveys >=1 Arth")
lm.1 =lm(extremes_241$perc1 ~ extremes_241$avg_leaf_area_cm2)
abline(lm.1)

#chi squared data reshaping
table40 = table(count_merged3$percent40, count_merged3$plantSpecies)
table10 = table(count_merged3$percent10, count_merged3$plantSpecies)
table5 = table(count_merged3$percent5, count_merged3$plantSpecies)
table1 = table(count_merged3$percent1, count_merged3$plantSpecies)
chisq.test(table40)
chisq.test(table10)
chisq.test(table5)
chisq.test(table1)

#so the above results tell me that the number of arthropods seen at each threshold is dependent on 
#tree species (is this true for all tree species)?





