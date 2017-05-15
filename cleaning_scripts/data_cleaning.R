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
labgroupusers = c(69, 130, 131, 132, 136, 158, 159, 189, 191)



# Read in data
tempsurveys = read.csv('data/arthropods/tbl_surveys.csv', header=F, stringsAsFactors = F)
orders = read.csv('data/arthropods/tbl_orders.csv', header=F, stringsAsFactors = F)

names(tempsurveys) = c('surveyID', 'site', 'userID', 'circle', 'survey', 'dateStart',
                   'dateSubmit', 'tempMin', 'tempMax', 'notes', 'plantSp',
                   'herbivory', 'photo', 'isValid', 'status', 'surveyType',
                   'leafCount', 'source')
names(orders) = c('recordID', 'surveyID', 'arthropod', 'length', 'notes',
                  'count', 'photo', 'time', 'isValid')

# Only include valid entries in surveys
surveys = tempsurveys[tempsurveys$isValid == 1,]

# Convert 'survey' field to character from factor
surveys$survey = as.character(surveys$survey)

# Fix a few missing dates (based on checking old MS Access database)
surveys$datetime = as.POSIXlt(surveys$dateStart, format = "%Y-%m-%d %H:%M:%S")
surveys$date = as.POSIXlt(word(surveys$dateStart, 1, sep = " "), format = "%Y-%m-%d")
surveys$time = format(surveys$datetime, "%H:%M")
surveys$date[surveys$surveyID %in% c(6497, 6499, 6501, 6502, 6503, 6504, 6507)] = "2011-06-05"
surveys$date[surveys$surveyID == 9992] = "2011-06-01"
surveys$date[surveys$site == 8892351 & surveys$dateStart == "0000-00-00 00:00:00"] = "2016-06-10"

#convert survey letters to upper case to avoid duplicates
surveys$survey = toupper(surveys$survey)

# Merge orders and surveys table
orders2 = merge(surveys, orders, by = 'surveyID', sort = FALSE, all.x = TRUE)
orders2$julianday = yday(orders2$date)
orders2$date = as.character(orders2$date)

orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date', 'time.x', 'julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x', 'surveyType', 'leafCount')]
names(orders3)[names(orders3) == 'time.x'] = 'time'

# Clean arthropod names and then add column with arthopod order code
orders3$arthropod[orders3$arthropod == "Bees and Wasps (Hymenoptera excluding ants)"] = 
  "Bees and Wasps (Hymenoptera, excluding ants)"
orders3$arthropod[orders3$arthropod == "Leaf Hoppers and Cicadas (Auchenorrhyncha)"] = 
  "Leaf hoppers and Cicadas (Auchenorrhyncha)"
orders3$arthropod[orders3$arthropod == "Butterflies and Moths (Lepidoptera adult)"] = 
  "Moths, Butterflies (Lepidoptera)"
orders3$arthropod[orders3$arthropod == "OTHER (describe in Notes)"] = 
  "Other (describe in Notes)"
orders3$arthropod[orders3$arthropod == "Unidentified"] = 
  "UNIDENTIFIED (describe in Notes)"

# Change records of termites to "Other"; users did not correctly identify
orders3$arthropod[orders3$arthropod == "Termites (Isoptera)"] = 
  "Other (describe in Notes)"

#Change NAs in counts to 0s, and NAs in arthropods to none- mobile phone submissions don't recognize "nones"
orders3["arthropod"][is.na(orders3["arthropod"])] <- "NONE"
orders3["count"][is.na(orders3["count"])] <- 0


# Remove all records where the Order is "Leaf Roll"
orders4 = orders3[orders3$arthropod != "Leaf Roll",]

arthcodes = read.csv('data/arthropods/arth_codes.csv', header=T)
arthcodes1 = arthcodes[, c('ArthCode', 'DataName')]
names(arthcodes1) = c('arthCode', 'arthropod')
cleandata <- merge(orders4, arthcodes1, by = 'arthropod', all.x = TRUE, sort = FALSE)
cleandata <- cleandata[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','time', 'julianday',
                       'plantSp','herbivory','arthropod','arthCode','length',
                       'count','notes.y','notes.x', 'surveyType', 'leafCount')]
cleandata <- cleandata[order(cleandata$date),]

# Add a column indicating if the leaves were wet
tempwet <- sort(c(grep("wet leaves", cleandata$notes.x), grep("Wet leaves", cleandata$notes.x), 
                  grep("very dewy", cleandata$notes.x), grep("Wet Leaves", cleandata$notes.x)))
cleandata$wetLeaves = rep('no', nrow(cleandata))
cleandata$wetLeaves[tempwet] = 'yes'

# Add a year column
cleandata$year = substring(cleandata$date, 1, 4)

# Change arthCodes to class character
cleandata$arthCode = as.character(cleandata$arthCode)

 #Take out large caterpillar colonies
#cleandata <- cleandata[!(cleandata$arthCode == "LEPL" & cleandata$count > 10),]
 #or
#cleandata$count[cleandata$arthCode == "LEPL" & cleandata$count > 5] = 5

# Cleaning beat sheets (PR and BG) and isolating # leaves into a new column

# Beat sheet designation was in notes field prior to 2016
beatsheet_pre2016 <- cleandata[grep("BEAT SHEET", cleandata$notes.x), ] 

# In 2016, surveyType was not getting recorded when entered from website, so 
# beat sheet is designated by either the surveyType field when entered by app or
# by a leaf count that differed from the 50 expected of a visual survey.
# (Participants were told to record 49 or 51 if the number of leaves in a beat
# sheet survey truly was 50.)
beatsheet_post2016 <- cleandata[((cleandata$leafCount != "50") & (cleandata$year>= "2016")) | (cleandata$surveyType=="Beat_Sheet"),] 
beatsheet_post2016 <- beatsheet_post2016[!is.na(beatsheet_post2016$surveyID),]

# Pull out the leafCount from the notes
leavesNumTemp0 <- word(beatsheet_pre2016$notes.x, -1, sep = "BEAT SHEET; ")
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
cleandata2 <- cleandata[!cleandata$surveyID %in% beatsheet$surveyID, ]
cleandata <- rbind(cleandata2, beatsheet)
cleandata$surveyType[cleandata$surveyType != "Beat_Sheet"] <- "Visual"

#--------------------------------------------------------------------------------

## Calculating biomass and adding this as a column to the cleandata

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log(a)
reg.data.temp <- read.csv('data/arthropods/arth_regression.csv', header = T, sep = ',')
# Calculate a (the coefficient)
reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)

# Create list of arthropod orders (by code)
arthlist <- arthcodes$ArthCode[arthcodes$ArthCode %in% reg.data.temp$arthCode] # arthcodes from data_cleaning.R

# Merge reg.data.temp and arthlist so NAs will be calculated
reg.data <- merge(reg.data.temp, arthcodes, by.x = 'arthCode', by.y = 'ArthCode', all = T)

# Create empty biomass vector
cleandata$biomass = NA

# For loop for calculating biomass for each observation
for (ord in arthlist) {
  b = reg.data$slope[reg.data$arthCode == ord]
  a = reg.data$coefficient[reg.data$arthCode == ord]
  biomass = (a*(cleandata$length[cleandata$arthCode == ord])^b)*(cleandata$count[cleandata$arthCode == ord])
  cleandata$biomass[cleandata$arthCode == ord] <- biomass
}

# Orders with regression data:
regorders <- as.vector(reg.data.temp$arthCode)

# Clean Tree sp names (pre-2015 no master table for tree sp names)
cleandata$plantSp = tolower(cleandata$plantSp)
plantFreq=data.frame(table(dplyr::select(cleandata, plantSp)))
cleandata$clean_plantSp = cleandata$plantSp

cleandata$clean_plantSp = gsub("acer saccharum", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sugar mapl", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sugar maple ", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sugar maplee", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sugar maplee ", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sygar naoke", "sugar maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("american holly ", "american holly", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("carpinus caroliniana", "american hornbeam", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("american hornbeam ", "american hornbeam", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("boxelder", "box elder", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("arsp", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("devil's walking stick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("devil's-walking-sticl", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("devils claw", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("devils walking stick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("devils-walkingstick", "devil's walkingstick", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("butternut hickory", "bitternut hickory", cleandata$clean_plantSp) 
cleandata$clean_plantSp= gsub("box elder ", "box elder", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("acarolina ash", "carolina ash", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("carolina ash ", "carolina ash", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("chalk mape", "chalk maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("dwarf paw paw", "dwarf pawpaw", cleandata$clean_plantSp) 
cleandata$clean_plantSp= gsub("baby paw paw", "dwarf pawpaw", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("flowering dogwood ", "flowering dogwood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("maple-leaved viburnum", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("mapleleaf viburnum ", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("mapleleaf-viburnum", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("maple-leaf viburnum ", "mapleleaf viburnum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("mucslewood", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("muscewood", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("musclewood ", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("muscle wood ", "musclewood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("musclewood?", "musclewood", cleandata$clean_plantSp) #musclewood? won't be removed
cleandata$clean_plantSp= gsub("red maple ", "red maple", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("red oak ", "red oak", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sour wood", "sourwood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sourwood ", "sourwood", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("spice bush", "spicebush", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("liquidambar", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sweetgum ", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sweet gun", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sweet gum", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("sweet gum ", "sweetgum", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("spicebush ", "spicebush", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("tulip poplar ", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp= gsub("tulip tree", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp= gsub("tuliptree", "tulip poplar", cleandata$clean_plantSp) 
cleandata$clean_plantSp= gsub("alternate-leafed dogwood", "alternate-leaved dogwood", cleandata$clean_plantSp)  
cleandata$clean_plantSp= gsub("elum", "elm", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("chinkapin", "white oak", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("white oak ", "white oak", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("unidentified elm", "unid. elm", cleandata$clean_plantSp)
cleandata$clean_plantSp= gsub("unid willow", "unid. willow", cleandata$clean_plantSp)

cleandata$clean_plantSp[cleandata$clean_plantSp == 'alder'] = "hazel alder"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("american basswood","basswood")] = "american basswood"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("american beech ","beech", "american beech c")] = "american bladdernut"
cleandata$clean_plantSp[cleandata$clean_plantSp == "bladdernut"] = "american bladdernut"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("black willow ", "willow", "wilow")] = "black willow"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("carolina silver", "carolina silver ",  "carolina silver bell", "silverbell", "sliverbell")] = "carolina silverbell"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("fringe tree", "fringe" )] = "fringetree"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("paw-paw", "paw paw")] = "common persimmon"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("persimmon", "perssimon", "permisson")] = "common persimmon"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("redbud", "eastern red bud ", "eastern red bud ", "eastern red bud")] = "eastern redbud"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("hazelnut", "hazlenut")] = "american hazelnut"
cleandata$clean_plantSp[cleandata$clean_plantSp =="mulberry"] = "white mulberry"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("sycamore")] = "american sycamore"
cleandata$clean_plantSp[cleandata$clean_plantSp %in% c("","dead tree", "delete me", "frsp", "gfg", "list", "beat sheet", "no data entry", "no entry", "not listed", "plant", 
                                                       "musclewood (possible id: sweetgum)", "musclewood? maple is correct id","musclewood?, sweetgumcorrect id, i think. ",
                                                       "musclewood?", "musclewood? maple is correct id i believe. ", "musclewood? maple is correct id i believe. ", "musclewood?, sweetgum correct id, i think.", 
                                                       "quer","rhsp", "station", "staton", "statton", "sweet elm", "sweetgum? maple i think is correct id", "sweetgum? musclewoodis correct id i believe", "musclewood(possible id: sweetgum)", 
                                                       "sweetgum? maple is correct id i think.", "sweetgum? maple is correct id i think.", "sweetgum? musclewood correct id",  "sweetgum? musclewoodcorrect id ", "sweetgum? musclewoodis correct id ",
                                                       "sweetgum? musclewood is correct id", "sweetgum? musclewood is correct id ", "sweetgum? musclewood is correct id i believe", "musclewood?, sweetgum correct id, i think. ","syor", "test",
                                                       "unid. rubus", "unidentified", "unidentified ", "unid. rubus, (often recorded as \"rusp\")", "unidentified sp", "vasp")] = "unidentified"
  
#Removing exclosure trees (only 2016)    
cleandata <-filter(cleandata, !(grepl("EXCLOSURE", notes.x)))

#----------------------------------------------------------------------------------------
# Leaf area data

all_surveyTrees = read.csv("data/trees/tbl_surveyTrees.csv", header=F)
plant_codes = read.csv("data/trees/USA&AppalachianTrees_2016.csv", stringsAsFactors = F, header=T)

# Leaf area by species, site, date and station
area = read.table("data/trees/LeafAreaDatabase.txt", header=T, 
                      sep= '\t', quote="\"", fill = T, stringsAsFactors = FALSE) %>%
  mutate(area_cm2 = (LeafArea_pixels/RefArea_pixels)*(RefArea_cm2)) %>%
  filter(!is.na(area_cm2)) %>%
  left_join(plant_codes[, c('TreeCode', 'ComName')], by = c('TreeSpecies' = 'TreeCode')) %>%
  select(Site, Date, Station, ComName, area_cm2)
  
# Leaf area by species, site and date
sitedate_area = area %>%
  group_by(Site, Date, ComName) %>%
  summarize(area_cm2 = mean(area_cm2))

# Leaf area by species and site
site_area = area %>%
  group_by(Site, ComName) %>%
  summarize(area_cm2 = mean(area_cm2))

# Leaf area by species and site
species_area = area %>%
  group_by(ComName) %>%
  summarize(area_cm2 = mean(area_cm2))

# Now need to merge in leaf area info by
# --species, site, station, and date when possible
# --or use mean leaf area




#--------------------------------------------------------------------------

# Taking out caterpillar colonies
cleandata[cleandata$count > 10 & cleandata$arthCode == 'LEPL',]$count = 10

# Data collected by the Hurlbert Lab group in the Triangle
labdata = filter(cleandata, userID %in% labgroupusers)

# Subsetting cleandata now that it has the biomass column included
labdata.pr <- labdata[labdata$site == 117,]
labdata.bg <- labdata[labdata$site == 8892356,]

cleandata.app <-cleandata[cleandata$year %in% c(2010,2011,2012),]

cleandata.pr <- cleandata[cleandata$site == 117,]
cleandata.bg <- cleandata[cleandata$site == 8892356,]

amsurvey.pr <- surveySubset(labdata.pr, subset = "visual am")
pmsurvey.pr <- surveySubset(labdata.pr, subset = "visual pm")
beatsheet.pr <- surveySubset(labdata.pr, subset = "beat sheet")
volunteer.pr <- surveySubset(cleandata.pr, subset = "volunteer")

amsurvey.bg <- surveySubset(labdata.bg, subset = "visual am")
beatsheet.bg <- surveySubset(labdata.bg, subset = "beat sheet")

