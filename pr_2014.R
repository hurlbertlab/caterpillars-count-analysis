# Prairie Ridge Summer 2014 data

cleandata.pr2014 <- cleandata[cleandata$site == 117 & cleandata$year == 2014,]

# Only user ID's included in this data are 69 (24), 75 (3), and 129 (349)

# Need to make am survey subsetting more general in summary functions script.

# For now:

# 2014 visual survey (is clean data ready?)
visualsurvey2014 = cleandata.pr2014[!grepl("BEAT SHEET", cleandata.pr2014$notes.x),]

# 2014 beat sheet