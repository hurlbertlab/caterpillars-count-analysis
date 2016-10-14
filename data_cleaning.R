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
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)

#Source summary_functions.r
source("summary_functions.r")

# UserIDs of Hurlbert Lab data collectors
labgroupusers = c(69, 130, 131, 132, 136, 158, 159, 189, 191)


# Set working directory
# setwd('c:/git/caterpillars-count-analysis')

# Read in data
tempsurveys = read.csv('data/tbl_surveys.csv', header=F, stringsAsFactors = F)
orders = read.csv('data/tbl_orders.csv', header=F, stringsAsFactors = F)

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
surveys$date = as.character(as.POSIXlt(word(surveys$dateStart, 1, sep = " "), format = "%Y-%m-%d"))
surveys$date[surveys$surveyID %in% c(6497, 6499, 6501, 6502, 6503, 6504, 6507)] = "2011-06-05"
surveys$date[surveys$surveyID == 9992] = "2011-06-01"
surveys$date[surveys$site == 8892351 & surveys$dateStart == "0000-00-00 00:00:00"] = "2016-06-10"

# Create effortByDay dataframe for use in summary functions
effortByDay = data.frame(table(surveys[, c('site', 'date')]))
names(effortByDay) = c('site', 'date', 'numSurveys')
effortByDay = effortByDay[effortByDay$numSurveys!=0, ]
effortByDay$date = as.POSIXlt(effortByDay$date, format = "%Y-%m-%d")
effortByDay$julianday = yday(effortByDay$date)
tempyear <- substring(effortByDay$date, 1, 4)
effortByDay$year = tempyear 

# Merge orders and surveys table
orders2 = merge(surveys, orders, by = 'surveyID', sort = FALSE)
orders2$julianday = yday(orders2$date)

orders3 = orders2[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
                      'plantSp','herbivory','arthropod','length',
                      'count','notes.y','notes.x', 'surveyType', 'leafCount')]

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

# Remove all records where the Order is "Leaf Roll"
orders3 = orders3[orders3$arthropod != "Leaf Roll",]

arthcodes = read.csv('arth_codes.csv', header=T)
arthcodes1 = arthcodes[, c('ArthCode', 'DataName')]
names(arthcodes1) = c('arthCode', 'arthropod')
cleandata <- merge(orders3, arthcodes1, by = 'arthropod', all.x = TRUE, sort = FALSE)
cleandata <- cleandata[, c('surveyID', 'userID','site', 'survey', 'circle', 'date','julianday',
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
reg.data.temp <- read.csv('arth_regression.csv', header = T, sep = ',')
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

#Clean Tree sp names (pre-2015 no master table for tree sp names)
cleandata$plantSp = tolower(cleandata$plantSp)
plantFreq=data.frame(table(dplyr::select(cleandata, plantSp)))


#if cleandata$plantSp %in% c("Acer saccharum", "Sugar mapl", "Sugar maple", "Sugar Maple", "Sugar Maple ")
#{cleandata$clean_plantSp == "Sugar maple"
#} else if cleandata$plantSp %in% c("alder", "hazel alder") {cleandata$clean_plantSp == "Hazel alder"
#} else if cleandata$plantSp %in% c("alternate-leafed dogwood") {cleandata$clean_plantSp =="Alternate-leafed dogwood"
#} else if cleandata$plantSp %in% c("american basswood", "basswood") {cleandata$clean_plantSp == "American basswood"
#} else if cleandata$plantSp %in% c("american beech", "american beech ", "beech") {cleandata$clean_plantSp == "American beech"
#} else if cleandata$plantSp %in% c("american bladdernut", "bladdernut") {cleandata$clean_plantSp == "American bladdernut"
#} else if cleandata$plantSp %in% c("american chestnut") {cleandata$clean_plantSp == "American chestnut"
#} else if cleandata$plantSp %in% c("american elm") {cleandata$clean_plantSp == "American elm"
#} else if cleandata$plantSp %in% c("american holly", "american holly ") {cleandata$clean_plantSp == "American holly"
#} else if cleandata$plantSp %in% c("american hornbeam", "american hornbeam ", "carpinus caroliniana") {cleandata$clean_plantSp == "American hornbeam" 
#} else if cleandata$plantSp %in% c("american sycamore") {cleandata$clean_plantSp == "American sycamore"
#} else if cleandata$plantSp %in% c("arsp", "devil's walking stick", "devil's walkingstick","devil's-walking-sticl", 
                                   "devils claw", "devils walking stick", "devils-walkingstick") 
                                    {cleandata$clean_plantSp == "Devil's walkingstick"
#} else if cleandata$plantSp %in% c("bear huckleberry") {cleandata$clean_plantSp == "Bear huckleberry"
#} else if cleandata$plantSp %in% c("bitternut hickory", "butternut hickory") {cleandata$clean_plantSp == "Bitternut hickory"
#} else if cleandata$plantSp %in% c("black cherry") {cleandata$clean_plantSp == "Black cherry"
#} else if cleandata$plantSp %in% c("black gum") {cleandata$clean_plantSp == "Black gum"
#} else if cleandata$plantSp %in% c("black locust") {cleandata$clean_plantSp == "Black locust"
#} else if cleandata$plantSp %in% c("black oak") {cleandata$clean_plantSp == "Black oak"
#} else if cleandata$plantSp %in% c("black willow", "black willow ", "willow", "wilow") {cleandata$clean_plantSp == "Black willow"
#} else if cleandata$plantSp %in% c("box elder", "box elder ") {cleandata$clean_plantSp == "Box elder"
#} else if cleandata$plantSp %in% c("buffalo nut") {cleandata$clean_plantSp == "Buffalo nut"
#} else if cleandata$plantSp %in% c("carolina ash", "carolina ash ") {cleandata$clean_plantSp == "Carolina ash"
#} else if cleandata$plantSp %in% c("carolina silver", "carolina silver bell", "carolina silverbell", "silverbell") {cleandata$clean_plantSp == "Carolina silverbell"
#} else if cleandata$plantSp %in% c("chalk maple") {cleandata$clean_plantSp == "Chalk maple"
#} else if cleandata$plantSp %in% c("chestnut oak") {cleandata$clean_plantSP == "Chesnut oak"
#} else if cleandata$plantSp %in% c("common persimmon", "persimmon", "perssimon") {cleandata$clean_plantSP == "Common persimmon"  
#} else if cleandata$plantSp %in% c("cucumber magnolia") {cleandata$clean_plantSP == "Cucumber magnolia"
#} else if cleandata$plantSp %in% c("dwarf paw paw", "dwarf pawpaw", "baby paw paw") {cleandata$clean_plantSp == "Dwarf pawpaw"   
#} else if cleandata$plantSp %in% c("eastern red bud", "eastern red bud ", "eastern redbud", "redbud") {cleandata$clean_plantSp == "Eastern redbud"
#} else if cleandata$plantSp %in% c("flame azalea") {cleandata$clean_plantSp == "Flame azalea"
#} else if cleandata$plantSp %in% c("flowering dogwood", "flowering dogwood ") {cleandata$clean_plantSp == "Flowering dogwood"
#} else if cleandata$plantSp %in% c("fraser magnolia") {cleandata$clean_plantSp == "Fraser magnolia"
#} else if cleandata$plantSp %in% c("fringe tree", "fringetree") {cleandata$clean_plantSp == "Fringetree"
#} else if cleandata$plantSp %in% c("flame azalea") {cleandata$clean_plantSp == "Flame azalea"
#} else if cleandata$plantSp %in% c("great rhododendron") {cleandata$clean_plantSp == "Great rhododendron"
#} else if cleandata$plantSp %in% c("hazelnut", "hazlenut") {cleandata$clean_plantSp == "American hazelnut"
#} else if cleandata$plantSp %in% c("highbush blueberry") {cleandata$clean_plantSp == "Highbush blueberry"
#} else if cleandata$plantSp %in% c("hop hornbeam") {cleandata$clean_plantSp == "Hop hornbeam"
#} else if cleandata$plantSp %in% c("maple-leaved viburnum", "mapleleaf viburnum", "mapleleaf viburnum ", "mapleleaf-viburnum", "maple-leaf viburnum ") {cleandata$clean_plantSp == "Mapleleaf viburnum"
#} else if cleandata$plantSp %in% c("mountain holly") {cleandata$clean_plantSp == "Mountain holly"
#} else if cleandata$plantSp %in% c("mountain laurel") {cleandata$clean_plantSp == "Mountain laurel"
#} else if cleandata$plantSp %in% c("mucslewood", "muscewood", "muscle wood ", "musclewood") {cleandata$clean_plantSp == "Mucslewood"
#} else if cleandata$plantSp %in% c("mulberry", "white mulberry") {cleandata$clean_plantSp == "Mulberry" 
#} else if cleandata$plantSp %in% c("paw paw", "paw-paw", "pawpaw") {cleandata$clean_plantSp == "Pawpaw" 
#} else if cleandata$plantSp %in% c("pignut hickory") {cleandata$clean_plantSp == "Pignut hickory"
#} else if cleandata$plantSp %in% c("pin cherry") {cleandata$clean_plantSp == "Pin cherry" 
#} else if cleandata$plantSp %in% c("pin oak") {cleandata$clean_plantSp == "Pin oak" 
#} else if cleandata$plantSp %in% c("red maple", "red maple ") {cleandata$clean_plantSp == "Red maple" 
#} else if cleandata$plantSp %in% c("red oak", "red oak ") {cleandata$clean_plantSp == "Red oak" 
#} else if cleandata$plantSp %in% c("russian olive") {cleandata$clean_plantSp == "Russian olive"   
#} else if cleandata$plantSp %in% c("sassafras") {cleandata$clean_plantSp == "Sassafras" 
#} else if cleandata$plantSp %in% c("serviceberry") {cleandata$clean_plantSp == "Serviceberry" 
#} else if cleandata$plantSp %in% c("shagbark hickory") {cleandata$clean_plantSp == "Shagbark hickory" 
#} else if cleandata$plantSp %in% c("sour wood", "sourwood", "sourwood ") {cleandata$clean_plantSp == "Sourwood" 
#} else if cleandata$plantSp %in% c("spice bush", "spicebush", "spicebush ") {cleandata$clean_plantSp == "Spicebush"
#} else if cleandata$plantSp %in% c("striped maple") {cleandata$clean_plantSp == "Striped maple" 
#} else if cleandata$plantSp %in% c("sugar mapl", "sugar maple", "sugar maple ", "sygar naoke") {cleandata$clean_plantSp == "Sugar maple"
#} else if cleandata$plantSp %in% c("sweet birch") {cleandata$clean_plantSp == "Sweet birch"
#} else if cleandata$plantSp %in% c("sweet gum", "sweet gum ", "sweet gun", "sweetgum") {cleandata$clean_plantSp == "Sweet gum"
#} else if cleandata$plantSp %in% c("sweet pepper bush") {cleandata$clean_plantSp == "Sweet pepperbush" 
#} else if cleandata$plantSp %in% c("tulip poplar", "tulip poplar ", "tulip tree", "tuliptree", "tuliptree ") {cleandata$clean_plantSp == "Tuliptree" 
#} else if cleandata$plantSp %in% c("white ash") {cleandata$clean_plantSp == "White ash"  
#} else if cleandata$plantSp %in% c("white oak", "white oak ") {cleandata$clean_plantSp == "White oak" 
#} else if cleandata$plantSp %in% c("winged elm") {cleandata$clean_plantSp == "Winged elm"  
#} else if cleandata$plantSp %in% c("witch hazel") {cleandata$clean_plantSp == "Witch hazel" 
#} else if cleandata$plantSp %in% c("yellow birch") {cleandata$clean_plantSp == "Yellow birch" 
#} else if cleandata$plantSp %in% c("yellow buckeye") {cleandata$clean_plantSp == "Yellow buckeye" 
#} else if cleandata$plantSp %in% c("elm","elum","beat sheet", "chalk willow", "chinkapin","frsp", "list", "maple", 
                                   "mountain maple", "musclewood (possible id: sweetgum)", "musclewood? maple is correct id", 
                                   "musclewood?, sweetgum correct id, i think.", "quer","rhsp", "station", "staton", "sweetgum? maple i think is correct id",
                                   "sweetgum? maple is correct id i think.", "sweetgum? maple is correct id i think.", "sweetgum? musclewood correct id", 
                                   "sweetgum? musclewood is correct id", "sweetgum? musclewood is correct id i believe", "sycamore", "syor", "unid willow", 
                                   "unid. ash", "unid. cherry", "unid. elm", "unid. hickory", "unid. rhododendron", "unid. rubus", "unidentified", "unidentified ", 
                                   "unidentified elm", "unidentified sp", "vasp") 
#} else  ) {cleandata$clean_plantSp == "NA"  
  
#Removing exclosure trees (only 2016)    
cleandata <-filter(cleandata, !(grepl("EXCLOSURE", notes.x)))

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

