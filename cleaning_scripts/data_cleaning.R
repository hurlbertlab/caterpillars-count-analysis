################################################
# Script for reading in and cleaning data from
# Caterpillars Count! data exported from the
# phpMyAdmin project site.

# Data include:
#
# tbl_surveys: date, site, time, and temperature of all survey events
#
# tbl_orders: arthropod observed, their length, and count associated with
#             each survey event

# Load required libraries

library(dplyr)
library(lubridate)
library(stringr)

#Source summary_functions.r
source("analysis_scripts/summary_functions.r")

# UserIDs of Hurlbert Lab data collectors
labgroupusers = c(69, 75, 130, 131, 132, 136, 158, 159, 189, 191, 300, 301, 302)



# Read in data
tempsurveys = read.csv('data/arthropods/tbl_surveys.csv', header=F, stringsAsFactors = F)
survclean = read.csv('data/arthropods/tbl_surveys_clean.csv', header=F, stringsAsFactors = F)


# At the moment, there is no difference between tbl_orders and tbl_orders_clean
orders = read.csv('data/arthropods/tbl_orders.csv', header=F, stringsAsFactors = F)
#ordclean = read.csv('data/arthropods/tbl_orders_clean.csv', header=F, stringsAsFactors = F)

names(tempsurveys) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                   'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                   'herbivory', 'photo', 'isValid', 'status', 'surveyType',
                   'leafCount', 'source')
names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                  'count', 'photo', 'time', 'isValid')

names(survclean) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                     'tempMin', 'tempMax', 'plantSp', 'herbivory', 'modified', 
                     'modNotes', 'surveyType', 'leafCount')
#names(ordclean) = c('recordID', 'surveyID', 'arthropod', 'length', 'count',
#                    'modified', 'modNotes', 'status')

# Fix surveyType field based on known historical survey methods.
# (data entry via web as of 2017 does not store surveyType info)
# --all surveys prior to 2015 are visual surveys
# --in 2015, "BEAT_SHEET" is in the siteNotes field (only done at 117 & 8892356)
# --in 2016, all surveys at 882351 and all surveys by volunteers at 117 were Beat_Sheet
# --in 2016, surveys at 117 and 8892356 by labgroupusers were Beat_Sheet if leafCount != 50
# --in 2017, all surveys at 8892364, 8892366, 8892367 and 8892368 are Beat_Sheet (but entered via web)

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) < 2015] = 'Visual'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2015 &
                         grepl("BEAT SHEET", tempsurveys$notes)] = 'Beat_Sheet'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2015 &
                         !grepl("BEAT SHEET", tempsurveys$notes)] = 'Visual'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2016 &
                         tempsurveys$site == 8892351] = 'Beat_Sheet'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2016 &
                         tempsurveys$site == 117 &
                         !tempsurveys$userID %in% labgroupusers] = 'Beat_Sheet'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2016 &
                         tempsurveys$site %in% c(117, 8892356) &
                         tempsurveys$userID %in% labgroupusers &
                         tempsurveys$leafCount != 50] = 'Beat_Sheet'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2016 &
                         tempsurveys$site %in% c(117, 8892356) &
                         tempsurveys$userID %in% labgroupusers &
                         tempsurveys$leafCount == 50] = 'Visual'

tempsurveys$surveyType[tempsurveys$surveyID == 23258] = 'Beat_Sheet'

tempsurveys$surveyType[substr(tempsurveys$dateStart, 1, 4) == 2017 &
                         tempsurveys$site %in% c(8892364, 8892366, 8892367, 8892368)] = 'Beat_Sheet'


# Pull out the leafCount from the notes for 2015 beat sheet surveys
leafCount2015bs = tempsurveys$notes[grep("BEAT SHEET", tempsurveys$notes)] %>%
  word(-1, sep = "BEAT SHEET; ") %>%
  word(-1, sep = "= ") %>%
  word(-1, sep = "Leaves  ") %>%
  word(-1, sep = "Leaves=") %>%
  word(1, sep = ";") %>%
  word(1, sep = ",") %>%
  gsub(pattern = " ", replacement = "") %>%
  gsub(pattern = "\n", replacement = "") %>%
  gsub(pattern = "Unknown", replacement = NA) %>%
  gsub(pattern = "unknown", replacement = NA) %>%
  as.numeric()

tempsurveys$leafCount[grep("BEAT SHEET", tempsurveys$notes)] = leafCount2015bs



## Cleaning other known issues

tempsurveys$isValid[tempsurveys$surveyID %in% 
                      c(20962:21041, #double-entered records
                        26189:26207, #individual re-did surveys in reverse order immediately after
                        26142, #random empty survey in October
                        20790,  #tent caterpillar cluster recorded by volunteers but avoided by us
                        27319, 27322, 27359, 27369, 27506:27508, 27763, #C. Lim's app/pending surveys lost arth records
                        21031, 26114
                        )] = 0

# Fixing date typo for 9 surveys
tempsurveys$dateStart[tempsurveys$site == 117] = 
  gsub("2016-07-08", "2016-07-09", tempsurveys$dateStart[tempsurveys$site == 117])

survclean$dateStart[survclean$site == 117] = 
  gsub("2016-07-08", "2016-07-09", survclean$dateStart[survclean$site == 117])


# Only include valid entries in surveys without 'delete' in notes and not from Test sites
oksurveys = tempsurveys$surveyID[tempsurveys$isValid == 1 & 
                                   tempsurveys$status != 'invalid' &
                                   !grepl("delete", tolower(tempsurveys$notes)) &
                                   !grepl("no leaves", tolower(tempsurveys$notes)) &
                                   !grepl("dead tree", tolower(tempsurveys$notes)) &
                                   !tempsurveys$site %in% c(8892357, 8892363)]

# Filter and merge notes, surveyType, and leafCount fields back in
surveys = survclean %>% 
  filter(surveyID %in% oksurveys) %>%
  select(-surveyType, -leafCount) %>%
  left_join(tempsurveys[, c('surveyID', 'notes', 'surveyType', 'leafCount')], by = 'surveyID') %>%
  rename(sitenotes = notes)


# Fix a few missing dates (based on checking old MS Access database)
surveys$datetime = as.POSIXct(surveys$dateStart, format = "%Y-%m-%d %H:%M:%S")
surveys$date = as.POSIXct(word(surveys$dateStart, 1, sep = " "), format = "%Y-%m-%d")
surveys$time = format(surveys$datetime, "%H:%M")
surveys$date[surveys$surveyID %in% c(6497, 6499, 6501, 6502, 6503, 6504, 6507)] = "2011-06-05"
surveys$date[surveys$surveyID == 9992] = "2011-06-01"
surveys$date[surveys$site == 8892351 & surveys$dateStart == "0000-00-00 00:00:00"] = "2016-06-10"

#convert survey letters to upper case to avoid duplicates
surveys$survey = toupper(surveys$survey)

# Merge orders and surveys table
orders2 = left_join(surveys, orders, by = 'surveyID') %>%
  mutate(julianday = yday(date), date = as.character(date)) %>%
  select(surveyID, userID, site, circle, survey, date, time.x, julianday,
                      plantSp, herbivory, arthropod, length,
                      count, sitenotes, notes, surveyType, leafCount) %>%
  rename(time = time.x, bugnotes = notes) %>%
  filter(arthropod != "Leaf Roll" | is.na(arthropod))

# Clean arthropod names and then add column with arthopod order code
orders2$arthropod[orders2$arthropod == "Bees and Wasps (Hymenoptera excluding ants)"] = 
  "Bees and Wasps (Hymenoptera, excluding ants)"
orders2$arthropod[orders2$arthropod == "Leaf Hoppers and Cicadas (Auchenorrhyncha)"] = 
  "Leaf hoppers and Cicadas (Auchenorrhyncha)"
orders2$arthropod[orders2$arthropod == "Butterflies and Moths (Lepidoptera adult)"] = 
  "Moths, Butterflies (Lepidoptera)"
orders2$arthropod[orders2$arthropod == "OTHER (describe in Notes)"] = 
  "Other (describe in Notes)"
orders2$arthropod[orders2$arthropod == "Unidentified"] = 
  "UNIDENTIFIED (describe in Notes)"

# Change records of termites to "Other"; users did not correctly identify
orders2$arthropod[orders2$arthropod == "Termites (Isoptera)"] = 
  "Other (describe in Notes)"

#Change NAs in counts to 0s, and NAs in arthropods to none- mobile phone submissions don't recognize "nones"
orders2$arthropod[is.na(orders2$arthropod)] <- "NONE"
orders2$count[is.na(orders2$count)] <- 0
orders2$length[is.na(orders2$length) & orders2$arthropod == 'NONE'] <- 0


arthcodes = read.csv('data/arthropods/arth_codes.csv', header=T,
                     stringsAsFactors = FALSE) %>%
  select(ArthCode, DataName) %>%
  rename(arthCode = ArthCode, arthropod = DataName)

joindata <- left_join(orders2, arthcodes, by = 'arthropod') %>%
  select(surveyID, userID, site, circle, survey, date,time, julianday,
                       plantSp,herbivory,arthropod,arthCode,length,
                       count, sitenotes, bugnotes, surveyType, leafCount) %>%
  arrange(date)

# Add a column indicating if the leaves were wet
tempwet <- sort(c(grep("wet leaves", joindata$sitenotes), grep("Wet leaves", joindata$sitenotes), 
                  grep("very dewy", joindata$sitenotes), grep("Wet Leaves", joindata$sitenotes)))
joindata$wetLeaves = rep('no', nrow(joindata))
joindata$wetLeaves[tempwet] = 'yes'

# Add a year column
joindata$year = substring(joindata$date, 1, 4)

# Change arthCodes to class character
joindata$arthCode = as.character(joindata$arthCode)


#Removing exclosure trees (only 2016)    
cleandata <-filter(joindata, !(grepl("EXCLOSURE", sitenotes)))

#--------------------------------------------------------------------------------

## Calculating biomass and adding this as a column to the cleandata

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log10(a)
reg.data.temp <- read.csv('data/arthropods/arth_regression.csv', header = T, 
                          stringsAsFactors = F, sep = ',')
# Calculate a (the coefficient)
reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)

# Create list of arthropod orders (by code)
arthlist <- arthcodes$arthCode[arthcodes$arthCode %in% reg.data.temp$arthCode] # arthcodes from data_cleaning.R

# Merge reg.data.temp and arthlist so NAs will be calculated
reg.data <- merge(reg.data.temp, arthcodes, by = 'arthCode', all = T)

# Create empty biomass vector
cleandata$biomass = NA

# For loop for calculating biomass for each observation
for (ord in arthlist) {
  b = reg.data$slope[reg.data$arthCode == ord]
  a = reg.data$coefficient[reg.data$arthCode == ord]
  biomass = (a*(cleandata$length[cleandata$arthCode == ord])^b)*(cleandata$count[cleandata$arthCode == ord])
  cleandata$biomass[cleandata$arthCode == ord] <- biomass
}

#---------------------------------------------------------------------------------

## Clean Tree sp names (pre-2015 no master table for tree sp names)
# (this cleaning is mostly unnecessary since "true" species names are now being
#  merged in from tbl_surveytrees.csv)
cleandata$plantSp = tolower(trimws(cleandata$plantSp))
plantFreq = cleandata %>% count(plantSp) %>% arrange(desc(n)) %>% data.frame()
cleandata$clean_plantSp = cleandata$plantSp

cleandata$clean_plantSp[cleandata$plantSp %in% c("acer saccharum", "sugar mapl",
            "sugar maplee", "sugar maplee", "sygar naoke")] = "sugar maple"
cleandata$clean_plantSp[cleandata$plantSp == "american holly "] = "american holly"
cleandata$clean_plantSp[cleandata$plantSp %in% c("carpinus caroliniana",
            "mucslewood", "muscewood", "muscle wood", "musclewood", "musclewood?")] = "american hornbeam"
cleandata$clean_plantSp[cleandata$plantSp %in% c("boxelder")] = "box elder"
cleandata$clean_plantSp[cleandata$plantSp %in% c("arsp", "devil's walking stick", 
            "devil's-walking-sticl", "devils claw", "devils walking stick", "devils-walkingstick")] = "devil's walkingstick"
cleandata$clean_plantSp[cleandata$plantSp == "butternut hickory"] = "bitternut hickory"
cleandata$clean_plantSp[cleandata$plantSp %in% c("acarolina ash")] = "carolina ash"
cleandata$clean_plantSp[cleandata$plantSp == "chalk mape"] = "chalk maple"
cleandata$clean_plantSp[cleandata$plantSp %in% c("dwarf paw paw", "baby paw paw")] = "dwarf pawpaw"
cleandata$clean_plantSp[cleandata$plantSp %in% c("boxelder")] = "box elder"
cleandata$clean_plantSp[cleandata$plantSp %in% c("maple-leaved viburnum",  
            "mapleleaf-viburnum", "maple-leaf viburnum")] = "mapleleaf viburnum"
cleandata$clean_plantSp[cleandata$plantSp %in% c("sour wood")] = "sourwood"
cleandata$clean_plantSp[cleandata$plantSp %in% c("spice bush")] = "spicebush"
cleandata$clean_plantSp[cleandata$plantSp %in% c("liquidambar", "sweetgum", "sweet gun")] = "sweet gum"
cleandata$clean_plantSp[cleandata$plantSp %in% c("tulip tree", "tuliptree")] = "tulip poplar"
cleandata$clean_plantSp[cleandata$plantSp %in% c("alternate-leafed dogwood")] = "alternate-leaved dogwood"
cleandata$clean_plantSp[cleandata$surveyID == 17644] = "winged elm"
cleandata$clean_plantSp[cleandata$site == 117 & cleandata$circle == 4 & cleandata$survey == 'D' &
                          cleandata$year <= 2016] = "black willow"

cleandata$clean_plantSp[cleandata$plantSp == 'alder'] = "hazel alder"
cleandata$clean_plantSp[cleandata$plantSp %in% c("american basswood","basswood")] = "american basswood"
cleandata$clean_plantSp[cleandata$plantSp %in% c("american beech ","beech", "american beech c")] = "american beech"
cleandata$clean_plantSp[cleandata$plantSp == "bladdernut"] = "american bladdernut"
cleandata$clean_plantSp[cleandata$plantSp %in% c("black willow ", "willow", "wilow")] = "black willow"
cleandata$clean_plantSp[cleandata$plantSp %in% c("carolina silver", "carolina silver ",  "carolina silver bell", "silverbell", "sliverbell")] = "carolina silverbell"
cleandata$clean_plantSp[cleandata$plantSp %in% c("fringe tree", "fringe" )] = "fringetree"
cleandata$clean_plantSp[cleandata$plantSp %in% c("paw-paw", "pawpaw")] = "paw paw"
cleandata$clean_plantSp[cleandata$plantSp %in% c("persimmon", "perssimon", "permisson")] = "common persimmon"
cleandata$clean_plantSp[cleandata$plantSp %in% c("redbud", "eastern red bud ", "eastern red bud ", "eastern red bud")] = "eastern redbud"
cleandata$clean_plantSp[cleandata$plantSp %in% c("hazelnut", "hazlenut")] = "american hazelnut"
cleandata$clean_plantSp[cleandata$plantSp =="mulberry"] = "white mulberry"
cleandata$clean_plantSp[cleandata$plantSp %in% c("sycamore")] = "american sycamore"
cleandata$clean_plantSp[cleandata$plantSp %in% c("","dead tree", "delete me", "frsp", "gfg", "list", "beat sheet", 
                                                       "no data entry", "no entry", "not listed", "plant", 
                                                       "quer","rhsp", 
                                                       "station", "staton", "statton", "sweet elm", 
                                                       "syor", "test", "unid. rubus", "unidentified", 
                                                       "unidentified ", "unid. rubus, (often recorded as \"rusp\")", 
                                                       "unidentified sp", "vasp")] = "unidentified"


# Getting master list of tree species by survey (mainly from historical data where there are discrepancies)
# This simply associates the tree species most frequently associated with each survey.
# If there was a tie (i.e., a survey was labeled 'Red maple' twice and 'Red oak' twice)
# then it is excluded from this table. 
# Perhaps by examining additional evidence (e.g. userID of data entry, other notes)
# we can assign tree species to these surveys in the future, but presumably
# some fraction of them the tree species id will be lost to time.

#recentsurveytrees = read.csv('data/trees/tbl_surveyTrees.csv', stringsAsFactors = F, header = F)
#names(recentsurveytrees) = c('site', 'circle', 'survey', 'plantSp')

#apptrees = cleandata %>% 
#  select(site, circle, survey, date, clean_plantSp) %>% 
#  unique() %>% 
#  count(site, circle, survey, clean_plantSp) %>% 
#  arrange(site, circle, survey, desc(n)) %>% 
#  mutate(rank = rank(desc(n), ties.method = 'average')) %>% 
#  filter(rank==1, !site %in% unique(recentsurveytrees$site)) %>% 
#  select(-rank, -n) %>%
#  rename(plantSp = clean_plantSp) %>%
#  data.frame()

#surveytrees = rbind(recentsurveytrees, apptrees)
#names(surveytrees)[4] = 'realPlantSp'
#write.csv(surveytrees, 'data/trees/all_surveytrees.csv', row.names = F)
surveytrees = read.csv('data/trees/all_surveytrees.csv', header=T, stringsAsFactors = F)


cleandata2 = left_join(cleandata, surveytrees) %>%
  select(surveyID, userID, site, circle, survey, date, time, julianday, year,
         surveyType, leafCount, realPlantSp, herbivory, arthropod, arthCode, 
         length, count, biomass, wetLeaves, sitenotes, bugnotes)


# Go through surveys which have had tree species changes, and change realPlantSp as needed
oldtrees = read.csv('data/trees/tbl_old_surveyTrees.csv', stringsAsFactors = F)
names(oldtrees) = c('oldTree_ID', 'siteID', 'circle', 'survey', 'dateBegin', 'dateEnd', 'plantspecies', 'Notes')

for (i in 1:nrow(oldtrees)) {
  cleandata2$realPlantSp[cleandata2$siteID == oldtrees$siteID[i] &
                           cleandata2$circle == oldtrees$circle[i] &
                           cleandata2$survey == oldtrees$survey[i] &
                           as.Date(cleandata2$date) > as.Date(oldtrees$dateBegin[i]) &
                           as.Date(cleandata2$date) < as.Date(oldtrees$dateEnd[i])] = oldtrees$plantspecies[i]
}

#----------------------------------------------------------------------------------------
# Leaf area data

all_surveyTrees = read.csv("data/trees/tbl_surveyTrees.csv", header=F)
plant_codes = read.csv("data/trees/USA&AppalachianTrees_2016.csv", stringsAsFactors = F, header=T)

# Leaf area by species, site, date and station
area = read.table("data/trees/LeafAreaDatabase.txt", header=T, 
                      sep= '\t', quote="\"", fill = T, stringsAsFactors = FALSE) %>%
  mutate(area_cm2 = (LeafArea_pixels/RefArea_pixels)*(RefArea_cm2)) %>%
  filter(!is.na(area_cm2)) %>%
  left_join(plant_codes[, c('TreeCode', 'ComName')], by = 'TreeCode') %>%
  select(site, date, circle, survey, ComName, area_cm2)
  
# Leaf area by species, site and date
sitedate_area = area %>%
  group_by(site, date, ComName) %>%
  summarize(area_cm2 = mean(area_cm2))

# Leaf area by species and site
site_area = area %>%
  group_by(site, ComName) %>%
  summarize(area_cm2 = mean(area_cm2))

# Leaf area by species and site
species_area = area %>%
  group_by(ComName) %>%
  summarize(area_cm2 = mean(area_cm2)) %>% 
  arrange(desc(area_cm2)) %>% 
  data.frame()

# 1) Where possible merge by site, 
bysurvey = left_join(cleandata, area[, c('site', 'date', 'circle', 'survey', 'area_cm2')])


#--------------------------------------------------------------------------

# Data collected by the Hurlbert Lab group in the Triangle
labdata = filter(cleandata2, userID %in% labgroupusers)

# Subsetting cleandata now that it has the biomass column included
labdata.pr <- labdata[labdata$site == 117,]
labdata.bg <- labdata[labdata$site == 8892356,]

cleandata.app <-cleandata2[cleandata2$year %in% c(2010,2011,2012),]

cleandata.pr <- cleandata2[cleandata2$site == 117,]
cleandata.bg <- cleandata2[cleandata2$site == 8892356,]

amsurvey.pr <- surveySubset(labdata.pr, subset = "visual am")
pmsurvey.pr <- surveySubset(labdata.pr, subset = "visual pm")
beatsheet.pr <- surveySubset(labdata.pr, subset = "beat sheet")
volunteer.pr <- surveySubset(cleandata.pr, subset = "volunteer")

amsurvey.bg <- surveySubset(labdata.bg, subset = "visual am")
beatsheet.bg <- surveySubset(labdata.bg, subset = "beat sheet")

