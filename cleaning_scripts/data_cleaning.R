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
                      count, notes.y , notes.x, surveyType, leafCount) %>%
  rename(time = time.x) %>%
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


arthcodes = read.csv('data/arthropods/arth_codes.csv', header=T) %>%
  select(ArthCode, DataName) %>%
  rename(arthCode = ArthCode, arthropod = DataName)

joindata <- left_join(orders2, arthcodes, by = 'arthropod') %>%
  select(surveyID, userID,site, survey, circle, date,time, julianday,
                       plantSp,herbivory,arthropod,arthCode,length,
                       count,notes.y,notes.x, surveyType, leafCount) %>%
  arrange(date)

# Add a column indicating if the leaves were wet
tempwet <- sort(c(grep("wet leaves", joindata$notes.x), grep("Wet leaves", joindata$notes.x), 
                  grep("very dewy", joindata$notes.x), grep("Wet Leaves", joindata$notes.x)))
joindata$wetLeaves = rep('no', nrow(joindata))
joindata$wetLeaves[tempwet] = 'yes'

# Add a year column
joindata$year = substring(joindata$date, 1, 4)

# Change arthCodes to class character
joindata$arthCode = as.character(joindata$arthCode)


# Cleaning beat sheets (PR and BG) and isolating # leaves into a new column

# Beat sheet designation was in notes field prior to 2016
beatsheet_pre2016 <- joindata[grep("BEAT SHEET", joindata$notes.x), ] 

# In 2016, surveyType was not getting recorded when entered from website, so 
# beat sheet is designated by either the surveyType field when entered by app or
# by a leaf count that differed from the 50 expected of a visual survey.
# (Participants were told to record 49 or 51 if the number of leaves in a beat
# sheet survey truly was 50.)
beatsheet_post2016 <- joindata[((joindata$leafCount != "50") & (joindata$year>= "2016")) | (joindata$surveyType=="Beat_Sheet"),] 
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
visual <- joindata[!joindata$surveyID %in% beatsheet$surveyID, ]
cleandata <- rbind(visual, beatsheet)
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

#---------------------------------------------------------------------------------

## Clean Tree sp names (pre-2015 no master table for tree sp names)

cleandata$plantSp = tolower(cleandata$plantSp)
plantFreq=data.frame(table(dplyr::select(cleandata, plantSp)))

cleandata$clean_plantSp <- ifelse(cleandata$plantSp %in% c("acer saccharum", "sugar mapl", "sugar maple", "sugar maple "),"Sugar maple", 
    ifelse (cleandata$plantSp %in% c("alder", "hazel alder"), "Hazel alder",
    ifelse (cleandata$plantSp %in% c("american basswood", "basswood"), "American basswood",
    ifelse (cleandata$plantSp %in% c("american beech", "american beech ", "beech"), "American beech",
    ifelse (cleandata$plantSp %in% c("american bladdernut", "bladdernut"), "American bladdernut",
    ifelse (cleandata$plantSp %in% c("american chestnut"),"American chestnut",
    ifelse (cleandata$plantSp %in% c("american holly", "american holly "), "American holly",
    ifelse (cleandata$plantSp %in% c("american hornbeam", "american hornbeam ", "carpinus caroliniana"), "American hornbeam", 
    ifelse (cleandata$plantSp %in% c("american sycamore"), "American sycamore",
    ifelse (cleandata$plantSp %in% c("arsp", "devil's walking stick", "devil's walkingstick","devil's-walking-sticl", "devils claw", "devils walking stick", "devils-walkingstick"), "Devil's walkingstick",
    ifelse (cleandata$plantSp %in% c("bear huckleberry"), "Bear huckleberry",
    ifelse (cleandata$plantSp %in% c("bitternut hickory", "butternut hickory"), "Bitternut hickory",
    ifelse (cleandata$plantSp %in% c("black gum"), "Black gum",
    ifelse (cleandata$plantSp %in% c("black oak"), "Black oak",
    ifelse (cleandata$plantSp %in% c("black willow", "black willow ", "willow", "wilow"), "Black willow",
    ifelse (cleandata$plantSp %in% c("box elder", "box elder "), "Box elder",
    ifelse (cleandata$plantSp %in% c("buffalo nut"), "Buffalo nut",
    ifelse (cleandata$plantSp %in% c("carolina ash", "carolina ash "), "Carolina ash",
    ifelse (cleandata$plantSp %in% c("carolina silver", "carolina silver bell", "carolina silverbell", "silverbell"), "Carolina silverbell",
    ifelse (cleandata$plantSp %in% c("chalk maple"), "Chalk maple",
    ifelse (cleandata$plantSp %in% c("chestnut oak"), "Chestnut oak",
    ifelse (cleandata$plantSp %in% c("common persimmon", "persimmon", "perssimon"), "Common persimmon",  
    ifelse (cleandata$plantSp %in% c("dwarf paw paw", "dwarf pawpaw", "baby paw paw"), "Dwarf pawpaw",   
    ifelse (cleandata$plantSp %in% c("eastern red bud", "eastern red bud ", "eastern redbud", "redbud"), "Eastern redbud",
    ifelse (cleandata$plantSp %in% c("flowering dogwood", "flowering dogwood "), "Flowering dogwood",
    ifelse (cleandata$plantSp %in% c("fraser magnolia"), "Fraser magnolia",
    ifelse (cleandata$plantSp %in% c("fringe tree", "fringetree"), "Fringetree",
    ifelse (cleandata$plantSp %in% c("great rhododendron"), "Great rhododendron",
    ifelse (cleandata$plantSp %in% c("hazelnut", "hazlenut"), "American hazelnut",
    ifelse (cleandata$plantSp %in% c("maple-leaved viburnum", "mapleleaf viburnum", "mapleleaf viburnum ", "mapleleaf-viburnum", "maple-leaf viburnum "), "Mapleleaf viburnum",
    ifelse (cleandata$plantSp %in% c("mountain laurel"), "Mountain laurel",
    ifelse (cleandata$plantSp %in% c("mucslewood", "muscewood", "muscle wood ", "musclewood"), "American hornbeam",
    ifelse (cleandata$plantSp %in% c("mulberry", "white mulberry"), "White mulberry", 
    ifelse (cleandata$plantSp %in% c("paw paw", "paw-paw", "pawpaw"), "Pawpaw", 
    ifelse (cleandata$plantSp %in% c("pin oak"), "Pin oak", 
    ifelse (cleandata$plantSp %in% c("red maple", "red maple "), "Red maple", 
    ifelse (cleandata$plantSp %in% c("red oak", "red oak "), "Red oak", 
    ifelse (cleandata$plantSp %in% c("sassafras"), "Sassafras", 
    ifelse (cleandata$plantSp %in% c("serviceberry"), "Serviceberry", 
    ifelse (cleandata$plantSp %in% c("sour wood", "sourwood", "sourwood "), "Sourwood", 
    ifelse (cleandata$plantSp %in% c("spice bush", "spicebush", "spicebush "), "Spicebush",
    ifelse (cleandata$plantSp %in% c("striped maple"), "Striped maple", 
    ifelse (cleandata$plantSp %in% c("sweet birch"), "Sweet birch",
    ifelse (cleandata$plantSp %in% c("sweet gum", "sweet gum ", "sweet gun", "sweetgum"), "Sweet gum",
    ifelse (cleandata$plantSp %in% c("tulip poplar", "tulip poplar ", "tulip tree", "tuliptree", "tuliptree "), "Tuliptree", 
    ifelse (cleandata$plantSp %in% c("winged elm"), "Winged elm",  
    ifelse (cleandata$plantSp %in% c("witch hazel"), "Witch hazel", 
    ifelse (cleandata$plantSp %in% c("yellow birch"), "Yellow birch", 
    ifelse (cleandata$plantSp %in% c("yellow buckeye"), "Yellow buckeye", "UNID")))))))))))))))))))))))))))))))))))))))))))))))))
   # ifelse (cleandata$plantSp %in% c("alternate-leafed dogwood", "american elm","black cherry", "black locust","cucumber magnolia", "elm","elum","beat sheet", "chalk willow", "chinkapin", "flame azalea", "frsp", "highbush blueberry", "hop hornbeam", "list", "maple", "mountain holly" 
                                    #   "mountain maple", "musclewood (possible id: sweetgum)", "musclewood? maple is correct id", 
                                    #   "musclewood?, sweetgum correct id, i think.","pignut hickory" "pin cherry", "quer","rhsp", "russian olive", "shagbark hickory", "station", "staton", "sweetgum? maple i think is correct id",
                                     #  "sweetgum? maple is correct id i think.", "sweetgum? maple is correct id i think.", "sweetgum? musclewood correct id", 
                                     #  "sweetgum? musclewood is correct id", "sweetgum? musclewood is correct id i believe", "sweet pepper bush", "sycamore", "syor", "unid willow", 
                                     #  "unid. ash", "unid. cherry", "unid. elm", "unid. hickory", "unid. rhododendron", "unid. rubus", "unidentified", "unidentified ", 
                                     #  "unidentified elm", "unidentified sp", "vasp", "white ash", white oak"), "other", "NA")
     
  
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
  summarize(area_cm2 = mean(area_cm2)) %>% 
  arrange(desc(area_cm2)) %>% 
  data.frame()

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

