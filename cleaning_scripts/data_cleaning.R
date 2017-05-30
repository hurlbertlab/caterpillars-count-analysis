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

# Only include valid entries in surveys
oksurveys = tempsurveys$surveyID[tempsurveys$isValid == 1 & tempsurveys$status != 'invalid']

# Filter and merge notes field back in
surveys = survclean %>% 
  filter(surveyID %in% oksurveys) %>%
  left_join(tempsurveys[, c('surveyID', 'notes')], by = 'surveyID') %>%
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
  select(surveyID, userID, site, survey, circle, date, time.x, julianday,
                      plantSp,herbivory, arthropod, length,
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


arthcodes = read.csv('data/arthropods/arth_codes.csv', header=T,
                     stringsAsFactors = FALSE) %>%
  select(ArthCode, DataName) %>%
  rename(arthCode = ArthCode, arthropod = DataName)

joindata <- left_join(orders2, arthcodes, by = 'arthropod') %>%
  select(surveyID, userID,site, survey, circle, date,time, julianday,
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


# Cleaning beat sheets (PR and BG) and isolating # leaves into a new column

# Beat sheet designation was in notes field prior to 2016
beatsheet_pre2016 <- joindata[grep("BEAT SHEET", joindata$sitenotes), ] 

# In 2016, surveyType was not getting recorded when entered from website, so 
# beat sheet is designated by either the surveyType field when entered by app or
# by a leaf count that differed from the 50 expected of a visual survey.
# (Participants were told to record 49 or 51 if the number of leaves in a beat
# sheet survey truly was 50.)
beatsheet_post2016 <- joindata[((joindata$leafCount != "50") & (joindata$year>= "2016")) | (joindata$surveyType=="Beat_Sheet"),] 
beatsheet_post2016 <- beatsheet_post2016[!is.na(beatsheet_post2016$surveyID),]

# Pull out the leafCount from the notes
leavesNumTemp0 <- word(beatsheet_pre2016$sitenotes, -1, sep = "BEAT SHEET; ")
leavesNumTemp <- word(leavesNumTemp0, -1, sep = "= ")
leavesNumTemp1 <- word(leavesNumTemp, -1, sep = "Leaves  ")
leavesNumTemp2 <- word(leavesNumTemp1, -1, sep = "Leaves=")
leavesNumTemp3 <- word(leavesNumTemp2, 1, sep = ";")
leavesNumTemp4 <- word(leavesNumTemp3, 1, sep = ",")
leavesNumTemp5 <- gsub(" ", "", leavesNumTemp4)
leavesNumTemp6 <- gsub("\n", "", leavesNumTemp5)
leavesNumTemp7 <- gsub("Unknown", NA, leavesNumTemp6)
leavesNumTemp8 <- gsub("unknown", NA, leavesNumTemp7)
beatsheet_pre2016$leafCount <- as.numeric(leavesNumTemp8)
beatsheet<-rbind(beatsheet_pre2016, beatsheet_post2016)
beatsheet$surveyType <- "Beat_Sheet"
visual <- joindata[!joindata$surveyID %in% beatsheet$surveyID, ]
cleandata <- rbind(visual, beatsheet)
cleandata$surveyType[cleandata$surveyType != "Beat_Sheet"] <- "Visual"

#Removing exclosure trees (only 2016)    
cleandata <-filter(cleandata, !(grepl("EXCLOSURE", sitenotes)))

#--------------------------------------------------------------------------------

## Calculating biomass and adding this as a column to the cleandata

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log(a)
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
cleandata$plantSp = tolower(cleandata$plantSp)
plantFreq=data.frame(table(dplyr::select(cleandata, plantSp)))
cleandata$clean_plantSp = cleandata$plantSp

cleandata$clean_plantSp = gsub("acer saccharum", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sugar mapl", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sugar maple ", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sugar maplee", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sugar maplee ", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sygar naoke", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("american holly ", "american holly", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("carpinus caroliniana", "american hornbeam", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("american hornbeam ", "american hornbeam", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("boxelder", "box elder", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("arsp", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("devil's walking stick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("devil's-walking-sticl", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("devils claw", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("devils walking stick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("devils-walkingstick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("butternut hickory", "bitternut hickory", cleandata$clean_plantSp) 
cleandata$clean_plantSp = gsub("box elder ", "box elder", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("acarolina ash", "carolina ash", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("carolina ash ", "carolina ash", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("chalk mape", "chalk maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("dwarf paw paw", "dwarf pawpaw", cleandata$clean_plantSp) 
cleandata$clean_plantSp = gsub("baby paw paw", "dwarf pawpaw", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("flowering dogwood ", "flowering dogwood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("maple-leaved viburnum", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("mapleleaf viburnum ", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("mapleleaf-viburnum", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("maple-leaf viburnum ", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("mucslewood", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("muscewood", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("musclewood ", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("muscle wood ", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("musclewood?", "musclewood", cleandata$clean_plantSp) #musclewood? won't be removed
cleandata$clean_plantSp = gsub("red maple ", "red maple", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("red oak ", "red oak", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sour wood", "sourwood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sourwood ", "sourwood", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("spice bush", "spicebush", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("liquidambar", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sweetgum ", "sweet gum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sweet gun", "sweet gum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sweetgum", "sweet gum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("sweet gum ", "sweet gum", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("spicebush ", "spicebush", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("tulip poplar ", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp = gsub("tulip tree", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp = gsub("tuliptree", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp = gsub("alternate-leafed dogwood", "alternate-leaved dogwood", cleandata$clean_plantSp)  
cleandata$clean_plantSp = gsub("elum", "elm", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("chinkapin", "white oak", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("white oak ", "white oak", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("unidentified elm", "unid. elm", cleandata$clean_plantSp)
cleandata$clean_plantSp = gsub("unid willow", "unid. willow", cleandata$clean_plantSp)

cleandata$clean_plantSp[cleandata$clean_plantSp == 'alder'] = "hazel alder"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("american basswood","basswood")] = "american basswood"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("american beech ","beech", "american beech c")] = "american beech"
cleandata$clean_plantSp[cleandata$clean_plantSp == "bladdernut"] = "american bladdernut"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("black willow ", "willow", "wilow")] = "black willow"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("carolina silver", "carolina silver ",  "carolina silver bell", "silverbell", "sliverbell")] = "carolina silverbell"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("fringe tree", "fringe" )] = "fringetree"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("paw-paw", "pawpaw")] = "paw paw"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("persimmon", "perssimon", "permisson")] = "common persimmon"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("redbud", "eastern red bud ", "eastern red bud ", "eastern red bud")] = "eastern redbud"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("hazelnut", "hazlenut")] = "american hazelnut"
cleandata$clean_plantSp[cleandata$clean_plantSp =="mulberry"] = "white mulberry"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("sycamore")] = "american sycamore"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("","dead tree", "delete me", "frsp", "gfg", "list", "beat sheet", 
                                                       "no data entry", "no entry", "not listed", "plant", 
                                                       "musclewood (possible id: sweetgum)", "musclewood? maple is correct id",
                                                       "musclewood?, sweetgumcorrect id, i think. ", "musclewood?", 
                                                       "musclewood? maple is correct id i believe. ", 
                                                       "musclewood? maple is correct id i believe. ", 
                                                       "musclewood?, sweetgum correct id, i think.", "quer","rhsp", 
                                                       "station", "staton", "statton", "sweet elm", 
                                                       "sweetgum? maple i think is correct id", 
                                                       "sweetgum? musclewoodis correct id i believe", 
                                                       "musclewood(possible id: sweetgum)", 
                                                       "sweetgum? maple is correct id i think.", 
                                                       "sweetgum? maple is correct id i think.", 
                                                       "sweetgum? musclewood correct id",  
                                                       "sweetgum? musclewoodcorrect id ", 
                                                       "sweetgum? musclewoodis correct id ",
                                                       "sweetgum? musclewood is correct id", 
                                                       "sweetgum? musclewood is correct id ", 
                                                       "sweetgum? musclewood is correct id i believe", 
                                                       "musclewood?, sweetgum correct id, i think. ", 
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
#write.csv(surveytrees, 'data/all_surveytrees.csv', row.names = F)
surveytrees = read.csv('data/all_surveytrees.csv', header=T, stringsAsFactors = F)


cleandata2 = left_join(cleandata, surveytrees) %>%
  select(surveyID, userID, site, circle, survey, date, time, julianday, year,
         surveyType, leafCount, realPlantSp, herbivory, arthropod, arthCode, 
         length, count, biomass, wetLeaves, sitenotes, bugnotes)

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

