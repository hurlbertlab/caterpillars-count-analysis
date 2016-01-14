
setwd('c:/git/caterpillars-count-analysis')

source('C:/git/caterpillars-count-analysis/summary_functions.r')

source('C:/git/caterpillars-count-analysis/data_cleaning.R')

source('C:/git/caterpillars-count-analysis/395_paper_plots.R')

source('C:/git/caterpillars-count-analysis/arth_analyses.R')

# Prairie Ridge Summer 2014 data

cleandata.pr2014 <- cleandata[cleandata$site == 117 & cleandata$year == 2014,]

# Only user ID's included in this data are 69 (24), 75 (3), and 129 (349)

# Need to make am survey subsetting more general in summary functions script.

# For now:

# 2014 visual survey (is clean data ready?)
#visualsurvey2014 = cleandata.pr2014[!grepl("BEAT SHEET", cleandata.pr2014$notes.x),]

# 2014 beat sheet
#beatsheet2014 = cleandata.pr2014[grep("BEAT SHEET", cleandata.pr2014$notes.x),]

# Plot mean density (all arthropods)

par(mfrow = c(1, 1))
meanDensityByDay(cleandata.pr2014, ordersToInclude = "All", inputYear = 2014, inputSite = 117, plot = T, new = T, lwd = 2, main = '2014 Surveys')

# Plot mean density of 6 orders of arthropods together

arthplot(cleandata.pr2014, ebd = effortByDay, 117, 2014)

# Julian days seem really off?