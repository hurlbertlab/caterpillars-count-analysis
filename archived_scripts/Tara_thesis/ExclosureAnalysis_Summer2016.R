#Analysis of Exclosure Data Summer 2016
#Open necessary packages
source("analysis_scripts/summary_functions.r")
library(tidyr)
library(dplyr)
library(stringr)
library(coin)
library(lattice)

# Read in data
all_surveys <- read.csv('data/arthropods/tbl_surveys.csv', header=F)
all_orders <- read.csv('data/arthropods/tbl_orders.csv', header=F)
all_surveyTrees <- read.csv("data/trees/tbl_surveyTrees.csv", header=T)

# Rename columns and remove unecessary ones
names(all_surveys) = c("surveyID", "siteID", "userID", 
                       "circle", "survey", "timeStart", 
                       "timeSubmit", "temperatureMin", "temperatureMax",
                       "siteNotes", "plantSpecies", "herbivory", 
                       "leavePhoto", "isValid", "Status",
                       "surveyType", "leafCount", "source")
names(all_orders) = c("orderID", "surveyID", "orderArthropod", 
                      "orderLength", "orderNotes", "orderCount",
                      "insectPhoto", "timeStamp", "isValid")
names(all_surveyTrees) = c("siteID", "circle", "survey", "surveyTrees")

all_surveys1= dplyr::select(all_surveys, -timeSubmit, -Status, -leavePhoto, -source)
all_orders1 = dplyr::select(all_orders, -insectPhoto, -timeStamp, -isValid)

#Change surveyTrees to Character 
all_surveyTrees$surveyTrees<-as.character(all_surveyTrees$surveyTrees)

#Add columns with arth order count and official plant species
all_data <- merge (all_surveys1, all_orders1, 
                   by.x = "surveyID", 
                   by.y = "surveyID", all.x= TRUE)
###Create Identifier Column for Specific Survey locations
all_data$identifier <- paste0(all_data$siteID, 
                              all_data$circle, 
                              all_data$survey)
all_surveyTrees$identifier <- paste0(all_surveyTrees$siteID, 
                          all_surveyTrees$circle, 
                          all_surveyTrees$survey)
all_data1 <- merge(all_data, all_surveyTrees, 
                   by.x = "identifier", 
                   by.y= "identifier", all.x= TRUE)
all_data2 <- dplyr::select(all_data1, -siteID.y, -circle.y, -survey.y, -plantSpecies) 
names(all_data2) <- c("identifier", "surveyID", "siteID", "userID", 
                      "circle", "survey","timeStart","temperatureMin", 
                      "temperatureMax","siteNotes","herbivory","isValid",
                      "surveyType","leafCount","orderID","orderArthropod",
                      "orderLength", "orderNotes",
                      "orderCount", "surveyTrees")

#Change herbivory to percent values (midpoint of range for Categories 1-3, 37.5 for 4)
all_data2$percent_herb <- ifelse(all_data2$herbivory==0, "0",
                          ifelse(all_data2$herbivory==1, "2.5",
                          ifelse(all_data2$herbivory==2, "7.5",
                          ifelse(all_data2$herbivory==3, "17.5", 
                          ifelse(all_data2$herbivory==4, "37.5", NA)))))

#Change Percent Herbivory to a numeric vector
all_data2$percent_herb <- as.numeric(all_data2$percent_herb)                          

#Add date column
all_data2$date = as.character(as.POSIXlt(word(all_data2$timeStart, 1, sep = " "), format = "%Y-%m-%d"))                        

#Identify Exclosure Surveys 
exclosures <-filter(all_data2, grepl("EXCLOSURE", siteNotes))
exclosures$TrapType <- "VFX"
exclosures$identifier <- paste0(exclosures$siteID, exclosures$circle, exclosures$survey)

#Identify visual control surveys, identify paired surveys and remove 
#beat sheet surveys)
visual_surveys <- filter(all_data2, leafCount == "50")
ex_pairs_allvisuals <- filter(visual_surveys, identifier 
                            %in% exclosures$identifier)


#Create dataframe with surveys from paired controls and exclosures on dates exclosures 
#were surveyed
ex_pairs <-filter(ex_pairs_allvisuals, grepl("2016-05-11", timeStart) |
                                       grepl("2016-05-12", timeStart) | 
                                       grepl("2016-05-16", timeStart) | 
                                       grepl("2016-05-18", timeStart) |
                                       grepl("2016-06-23", timeStart) | 
                                       grepl("2016-06-24", timeStart))
#Add column marking exclosures
ex_pairs1 <- merge(ex_pairs, exclosures, by.x = "orderID", 
                                         by.y= "orderID",
                                         all.x = TRUE)
ex_pairs2 <- dplyr::select(ex_pairs1, -date.y, -identifier.y, -surveyID.y, -siteID.y, -userID.y, 
                    -circle.y, -survey.y, -timeStart.y, -temperatureMin.y,
                    -temperatureMax.y, -siteNotes.y, -herbivory.y, -isValid.y, 
                    -surveyType.y, -leafCount.y, -orderArthropod.y, -orderLength.y, 
                    -orderNotes.y, -orderCount.y, -surveyTrees.y, -percent_herb.y, -date.y)
names(ex_pairs2) <- c("OrderID", "identifier", "surveyID", "siteID", "userID", "circle",
                      "survey", "timeStart", "temperatureMin", "temperatureMax",
                      "siteNotes", "herbivory", "isValid",
                      "surveyType", "leafCount", "orderArthropod", "orderLength",
                      "orderNotes", "orderCount", "surveyTrees", "percent_herb", "date", "TrapType")
#Mark visual surveys
ex_pairs2["TrapType"][is.na(ex_pairs2["TrapType"])] <- "VF"

#Mark surveys without arthropod observations as 0s
ex_pairs2["orderCount"][is.na(ex_pairs2["orderCount"])] <- 0


#Add Visit Number for exclosure sampling days
ex_pairs2$VisitNumber <- ifelse(ex_pairs2$date=="2016-05-11", "1",
                         ifelse(ex_pairs2$date=="2016-05-12", "1",
                         ifelse(ex_pairs2$date=="2016-05-16", "2",
                         ifelse(ex_pairs2$date=="2016-05-18", "2", 
                         ifelse(ex_pairs2$date=="2016-06-23", "3",
                         ifelse(ex_pairs2$date=="2016-06-24", "3",  NA))))))

#Summarise observations for 3 food types (start 2012 reshaping analysis code)
#Summarise Observations for All Arthropods (Arthropods greater than 2 mm)
grouped_all <- ex_pairs2 %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_all <- dplyr::summarise(grouped_all, sum(orderCount)) 
total_all$surveyID<- paste0(total_all$siteID, total_all$VisitNumber,
                            total_all$circle, total_all$survey, total_all$TrapType)


#Summarise Observations for Relevant Orders great than or equal to 5 mm ("Bird food Arthropods") 
food_arthropods <- filter(ex_pairs2, orderLength > "4" & orderArthropod %in% c("Caterpillars (Lepidoptera larvae)", #not sure this line of code is working.. missing 26 caterpillars obvs from 11B
                                                       "Beetles (Coleoptera)", 
                                                       "Spiders (Araneae; NOT daddy longlegs!)", 
                                                       "True Bugs (Heteroptera)", 
                                                       "Grasshoppers, Crickets (Orthoptera)", 
                                                       "Leaf hoppers and Cicadas (Auchenorrhyncha)"))
grouped_food <- food_arthropods %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_food <- dplyr::summarise(grouped_food, sum(orderCount))
total_food$surveyID<- paste0(total_food$siteID, total_food$VisitNumber,
                                total_food$circle, total_food$survey,total_food$TrapType)

#Summarise Observations for Caterpillars
caterpillar <-filter(ex_pairs2, orderArthropod == "Caterpillars (Lepidoptera larvae)")
grouped_caterpillar <- caterpillar %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_caterpillar <- dplyr::summarise(grouped_caterpillar, sum(orderCount))
total_caterpillar$surveyID<- paste0(total_caterpillar$siteID, total_caterpillar$VisitNumber,
                                total_caterpillar$circle, total_caterpillar$survey,total_caterpillar$TrapType)

#Summarise Observations for Orders not of Interest to Birds or individuals < 5 mm
notfood_orders = c("Ants (Formicidae)", "Bees and Wasps (Hymenoptera, excluding ants)", "Daddy longlegs (Opiliones)", "Flies (Diptera)", "Moths, Butterflies (Lepidoptera)")
notfood <- filter(ex_pairs2, orderArthropod %in% notfood_orders & orderLength < 5 | orderArthropod %in% notfood_orders | orderLength<5)
grouped_notfood <- notfood %>% group_by(TrapType, siteID, circle, survey, VisitNumber)
total_notfood <- dplyr::summarise(grouped_notfood, sum(orderCount))
total_notfood$surveyID<- paste0(total_notfood$siteID, total_notfood$VisitNumber,
                                total_notfood$circle, total_notfood$survey,total_notfood$TrapType)

#Create Unique Surveys Dataframe
unique_surveys<-unique(ex_pairs2[, c("TrapType", "siteID", "circle", "survey", "VisitNumber")])
unique_surveys$surveyID<- paste0(unique_surveys$siteID, unique_surveys$VisitNumber, 
                                    unique_surveys$circle, unique_surveys$survey,unique_surveys$TrapType)
unique_surveys_count <- data.frame(table(unique_surveys[, c("TrapType", "siteID", 
                                                               "circle", "survey", 
                                                               "VisitNumber", "surveyID")]))
unique_surveys_count = unique_surveys_count[unique_surveys_count$Freq> 0,]

#Remove 3 surveys from NCBG 1A/1B where exclosure location was moved after Visit1 (prior to exclosure being put on)
unique_corrected = dplyr::filter(unique_surveys_count, surveyID !="889235611BVF", surveyID!="889235621BVF" & surveyID!="889235631BVF")


#Create 3 New Dataframes Merging List of Unique Surveys with Summary Observations 
#for Each Unique Survey for Each of the 3 Food Types
all_abundance <- merge(unique_corrected, total_all,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
all_abundance1<- dplyr::select(all_abundance, -Freq, -TrapType.y, -siteID.y, -circle.y,
                        -survey.y, -VisitNumber.y, -surveyID)
names(all_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_all")
all_abundance1["total_all"][is.na(all_abundance1["total_all"])] <- 0


food_abundance <- merge(unique_corrected, total_food,
                       by.x="surveyID",
                       by.y = "surveyID", 
                       all.x = TRUE)
food_abundance1<- dplyr::select(food_abundance, -Freq, -TrapType.y, -siteID.y, -circle.y,
                         -survey.y, -VisitNumber.y, -surveyID)
names(food_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_food")
food_abundance1["total_food"][is.na(food_abundance1["total_food"])] <- 0


caterpillar_abundance <- merge(unique_corrected, total_caterpillar,
                        by.x="surveyID",
                        by.y = "surveyID", 
                        all.x = TRUE)
caterpillar_abundance1<- dplyr::select(caterpillar_abundance, -Freq, -TrapType.y, -siteID.y, 
                         -circle.y,  -survey.y, -VisitNumber.y, -surveyID)
names(caterpillar_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_caterpillar")
caterpillar_abundance1["total_caterpillar"][is.na(caterpillar_abundance1["total_caterpillar"])] <- 0

notfood_abundance <- merge(unique_corrected, total_notfood,
                               by.x="surveyID",
                               by.y = "surveyID", 
                               all.x = TRUE)
notfood_abundance1<- dplyr::select(notfood_abundance, -Freq, -TrapType.y, -siteID.y, 
                                -circle.y,  -survey.y, -VisitNumber.y, -surveyID)
names(notfood_abundance1) <- c("TrapType","siteID", "circle", "survey", "VisitNumber", "total_notfood")
notfood_abundance1["total_notfood"][is.na(notfood_abundance1["total_notfood"])] <- 0

#Spread Visit 1 and Visit 3 for 3 Food Type Datasets and Create Difference Column to Format for Wilcox Test
all_time <- spread(all_abundance1, VisitNumber, total_all)
names(all_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
all_time$visit_dif<-all_time$Visit3-all_time$Visit2

food_time <- spread(food_abundance1, VisitNumber, total_food)
names(food_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
food_time$visit_dif<-food_time$Visit3-food_time$Visit2

caterpillar_time <- spread(caterpillar_abundance1, VisitNumber, total_caterpillar)
names(caterpillar_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
caterpillar_time$visit_dif<-caterpillar_time$Visit3-caterpillar_time$Visit2

notfood_time <- spread(notfood_abundance1, VisitNumber, total_notfood)
names(notfood_time) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
notfood_time$visit_dif<-notfood_time$Visit3-notfood_time$Visit2

#Run wilcox_test (Difference of Difference, parallel to 2012 Comparison)
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time)
wilcox_test(visit_dif ~ TrapType, data=notfood_time) 

#Run analyses on dif between end arth density b/w treatment and control (independent of 2012 comparison)
wilcox_test(Visit3~ TrapType, data=food_time)
wilcox_test(Visit3~ TrapType, data=caterpillar_time)
wilcox_test(Visit3~ TrapType, data=notfood_time)

##Reshape dataframe for visualization
food_time1 <- dplyr::select(food_time, -Visit1, -Visit2, -visit_dif)
food_V3 <- food_time %>% dplyr::select(-Visit1, -Visit2, -visit_dif) %>% spread(TrapType, Visit3)
names(food_V3) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
food_V3$Visit3Dif <- (food_V3$Visit3VFX - food_V3$Visit3VF)
food_V3$identifier <- paste0(food_V3$siteID, food_V3$circle, food_V3$survey)
food_final<-merge(food_V3, all_surveyTrees, by.x = "identifier", by.y="identifier")
food_final <- dplyr::select(food_final, -siteID.y, -circle.y, -survey.y)
names(food_final)<- c("identifier", "siteID", "circle", "survey", "Visit3VF", "Visit3VFX", "Visit3Dif", "treeSp")

caterpillar_time1 <- dplyr::select(caterpillar_time, -Visit1, -Visit2, -visit_dif)
caterpillar_V3<- spread(caterpillar_time1, TrapType, Visit3)
names(caterpillar_V3) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
caterpillar_V3$Visit3Dif <- caterpillar_V3$Visit3VFX - caterpillar_V3$Visit3VF
caterpillar_V3$identifier <- paste0(caterpillar_V3$siteID, caterpillar_V3$circle, caterpillar_V3$survey)
caterpillar_final<-merge(caterpillar_V3, all_surveyTrees, by.x = "identifier", by.y="identifier")
caterpillar_final <- dplyr::select(caterpillar_final, -siteID.y, -circle.y, -survey.y)
names(caterpillar_final)<- c("identifier", "siteID", "circle", "survey", "Visit3VF", "Visit3VFX", "Visit3Dif", "treeSp")

##Visualize exclosure data vs. control data
#Boxplot of difference in final arth densities by site
boxplot(food_final$Visit3Dif ~food_final$siteID, main="Arth Density for Relevant Orders by Site", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)
boxplot(caterpillar_final$Visit3Dif ~caterpillar_final$siteID, main="Caterpillar Density by Site", ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)

#Boxplot of difference in final arth densities by tree species
boxplot(food_final$Visit3Dif ~food_final$treeSp, main="Arth Density for Relevant Orders by Tree Species", 
        ylab="FinalVisit Difference in Arth Density (treatment-control)", new=T)
boxplot(caterpillar_final$Visit3Dif ~caterpillar_final$treeSp, main="Caterpillar Density by Tree Species", 
        ylab="FinalVisit Difference in Caterpillar Density (treatment-control)", new=T)

#Boxplot of average arth densities for exclosures and controls
boxplot(food_final$Visit3VF, main="selected Arthropod Density by Treatment")
boxplot(food_final$Visit3VFX, new=F)

#Remove outliers (observations were more than 5 arthropods were seen on a survey)
#food_no_outliers <-filter(food_final, Visit3VF < 6 & Visit3VFX <6)
caterpillar_no_outliers <- filter(caterpillar_final, Visit3VF < 6 & Visit3VFX < 6)

#Boxplot of average bird food and caterpillar dnesity by tree species for exclosures vs controls (outliers removed)
par(mfrow = c(2,2))
boxplot(food_final$Visit3VFX ~ food_final$treeSp, main = "Bird Food, Exclosure", 
        ylab = "# arthropods per survey", cex.axis = .4)
boxplot(food_final$Visit3VF ~ food_final$treeSp, main = "Bird Food, Control", 
        ylab = "# arthropods per survey", cex.axis = .4)
boxplot(caterpillar_no_outliers$Visit3VFX ~ caterpillar_no_outliers$treeSp, main = "Caterpillars, Exclosure", 
        ylab = "# arthropods per survey", cex.axis = .4)
boxplot(caterpillar_final$Visit3VF ~ caterpillar_final$treeSp, main = "Caterpillars, Control", 
        ylab = "# arthropods per survey", cex.axis = .4, ylim=c(0,2.0))



#****************Analyze change in herbivory**********
unique_herbivory<-unique(ex_pairs2[, c("TrapType", "siteID", "circle", "survey", "VisitNumber", "percent_herb")])
unique_herbivory_V3 <- filter(unique_herbivory, VisitNumber=="3")
unique_herbivory_V3 <- spread(unique_herbivory_V3, TrapType, percent_herb)

#Spread Visit 1 and Visit 3 for Herbivory and Create Difference Column to Format for Wilcox Test
herb <- spread(unique_herbivory, VisitNumber, percent_herb)
names(herb) = c('TrapType','siteID', "circle", "survey", "Visit1", "Visit2", "Visit3")
herb$visit_dif<-herb$Visit3-herb$Visit2
herb$TrapType <- as.factor(herb$TrapType)

#Run Wilcox test
wilcox_test(visit_dif ~ TrapType, data=herb)

#Summarise observations for 2016 herbivory by date, and site
visual_surveys_clean<- visual_surveys[!(visual_surveys$surveyID %in% exclosures$surveyID), ]
visual_surveys_clean <-filter(visual_surveys_clean, grepl("2016", date))
grouped_site <- visual_surveys_clean %>% group_by(siteID, date)
mean_site_herb <- (summarise(grouped_site, mean(percent_herb)))
mean_site_herb<-data.frame(mean_site_herb)
mean_site_herb$julianday = yday(mean_site_herb$date)
mean_site_PR <- filter(mean_site_herb, siteID=="117")
mean_site_BG <- filter(mean_site_herb, siteID=="8892356")

#Summarise observations for 2016 herbivory by date, site, and tree species
grouped_species <- visual_surveys_clean %>% group_by(siteID, date, surveyTrees)

#Histogram of Total Arth Density on 3rd Visit & Herbivory
par(mfcol=c(2, 2))
par(mfrow = c(2, 2), mar=c(6,4,4,4))
histogram(food_final$Visit3VF, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(food_final$Visit3VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,25), xlab="Arthropod density (# of arthropods)", col="slate blue2")
histogram(unique_herbivory_V3$VF, type="count",
          ylab="Number of Surveys", ylim=c(0,15), xlab="Percent Herbivory", col="green2")
histogram(unique_herbivory_V3$VFX, type="count",
          ylab="Number of Surveys", ylim=c(0,15), xlab="Percent Herbivory", col="green2")


#Visualizing Herbivory
plot(mean_site_PR$julianday, mean_site_PR$mean.percent_herb.)
plot.default(mean_site_BG$julianday, mean_site_BG$mean.percent_herb.)
#plot.default(mean_species_herb$julianday, mean_species_herb$mean.percent_herb., ylab=="Mean Percent Herbivory", xlab=="Julian Day")

#plot 

##Bird Food Group Reshaping for histograms 
#have column for differences between V3 and V2 for VF and VFX, and then column that is "difference of differences" difference in those columns
food_time1_t <- select(food_time, -Visit1, -Visit3,  -visit_dif)
food_Visit2_t<- spread(food_time1_t, TrapType, Visit2)
names(food_Visit2_t) <- c("siteID", "circle", "survey", "Visit2VF", "Visit2VFX")
food_Visit2_t$ID <- paste(food_Visit2_t$siteID, food_Visit2_t$circle, food_Visit2_t$survey)

food_time2_t <- select(food_time, -Visit1, -Visit2, -visit_dif)
food_Visit3_t<- spread(food_time2_t, TrapType, Visit3)
names(food_Visit3_t) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
food_Visit3_t$ID <- paste(food_Visit3_t$siteID, food_Visit3_t$circle, food_Visit3_t$survey)

food_time3_t <- merge(food_Visit2_t, food_Visit3_t, by.x="ID", by.y="ID")
food_time4_t <- select(food_time3_t, -siteID.y, -circle.y, -survey.y, -ID)
names(food_time4_t) <- c("siteID", "circle", "survey", "Visit2VF", "Visit2VFX", "Visit3VF", "Visit3VFX")

food_time4_t$VF_dif<- food_time4_t$Visit3VF-food_time4_t$Visit2VF
food_time4_t$VFX_dif<- food_time4_t$Visit3VFX-food_time4_t$Visit2VFX
food_time4_t$VFX_VF_dif<-food_time4_t$VFX_dif-food_time4_t$VF_dif

##Caterpillar Food Group Reshaping
caterpillar_time1_t <- select(caterpillar_time, -Visit1, -Visit3,  -visit_dif)
caterpillar_Visit2_t<- spread(caterpillar_time1_t, TrapType, Visit2)
names(caterpillar_Visit2_t) <- c("siteID", "circle", "survey", "Visit2VF", "Visit2VFX")
caterpillar_Visit2_t$ID <- paste(caterpillar_Visit2_t$siteID, caterpillar_Visit2_t$circle, caterpillar_Visit2_t$survey)

caterpillar_time2_t <- select(caterpillar_time, -Visit1, -Visit2, -visit_dif)
caterpillar_Visit3_t<- spread(caterpillar_time2_t, TrapType, Visit3)
names(caterpillar_Visit3_t) <- c("siteID", "circle", "survey", "Visit3VF", "Visit3VFX")
caterpillar_Visit3_t$ID <- paste(caterpillar_Visit3_t$siteID, caterpillar_Visit3_t$circle, caterpillar_Visit3_t$survey)

caterpillar_time3_t <- merge(caterpillar_Visit2_t, caterpillar_Visit3_t, by.x="ID", by.y="ID")
caterpillar_time4_t <- select(caterpillar_time3_t, -siteID.y, -circle.y, -survey.y, -ID)
names(caterpillar_time4_t) <- c("siteID", "circle", "survey", "Visit2VF", "Visit2VFX", "Visit3VF", "Visit3VFX")

caterpillar_time4_t$VF_dif<- caterpillar_time4_t$Visit3VF-caterpillar_time4_t$Visit2VF
caterpillar_time4_t$VFX_dif<- caterpillar_time4_t$Visit3VFX-caterpillar_time4_t$Visit2VFX
caterpillar_time4_t$VFX_VF_dif<-caterpillar_time4_t$VFX_dif-caterpillar_time4_t$VF_dif

#Run wilcox_test (Difference of Difference, parallel to 2012 Comparison)
wilcox_test(visit_dif ~ TrapType, data=all_time)
wilcox_test(visit_dif ~ TrapType, data=food_time)
wilcox_test(visit_dif ~ TrapType, data=caterpillar_time)
wilcox_test(visit_dif ~ TrapType, data=notfood_time) 

#Run analyses on dif between end arth density b/w treatment and control (independent of 2012 comparison)
wilcox.test(food_final$Visit3VF, food_final$Visit3VFX, paired =TRUE)
wilcox.test(caterpillar_final$Visit3VF, caterpillar_final$Visit3VFX, paired=TRUE)
