# Script for calculating biomass and adding this as column to the cleandata

# Source summary_functions.r and data_cleaning.R if have not already

# Create empty biomass vector
cleandata$biomass = NA

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log(a)
reg.data.temp <- read.csv('arth_regression.csv', header = T, sep = ',')
# Calculate a (the coefficient)
reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)

# Create list of arthropod orders (by code)
arthlist <- as.vector(arthcodes$ArthCode) # arthcodes from data_cleaning.R

# Merge reg.data.temp and arthlist so NAs will be calculated
reg.data <- merge(reg.data.temp, arthcodes, by.x = 'arthCode', by.y = 'ArthCode', all = T)

# For loop for calculating biomass for each observation
for (ord in arthlist) {
  b = reg.data[reg.data$arthCode == ord,]$slope
  a = reg.data[reg.data$arthCode == ord,]$coefficient
  cleandata$biomass[cleandata$arthCode == ord] <- (a*(cleandata$length[cleandata$arthCode == ord])^(b))*(cleandata$count[cleandata$arthCode == ord])
}

# Orders with regression data:
regorders <- as.vector(reg.data.temp$arthCode)

# Subsetting cleandata now that is has biomass
cleandata.pr <- cleandata[cleandata$site == 117 & cleandata$year == 2015,]
cleandata.bg <- cleandata[cleandata$site == 8892356 & cleandata$year == 2015,]

amsurvey.pr <- surveySubset(cleandata.pr, subset = "visual am", minLength = 5)
pmsurvey.pr <- surveySubset(cleandata.pr, subset = "visual pm", minLength = 5)
beatsheet.pr <- surveySubset(cleandata.pr, subset = "beat sheet", minLength = 5)
volunteer.pr <- surveySubset(cleandata.pr, subset = "volunteer", minLength = 5)

amsurvey.bg <- surveySubset(cleandata.bg, subset = "visual am", minLength = 5)
beatsheet.bg <- surveySubset(cleandata.bg, subset = "beat sheet", minLength = 5)

# Plotting biomass average per day
meanDensityByDay(surveyData = amsurvey.pr, ordersToInclude = regorders, minLength = 5, 
                 inputSite = 117, inputYear = 2015, plot = T, plotVar = 'meanBiomass')
meanDensityByDay(surveyData = pmsurvey.pr, ordersToInclude = regorders, minLength = 5, 
                 inputSite = 117, inputYear = 2015, plot = T, plotVar = 'meanBiomass')
meanDensityByDay(surveyData = beatsheet.pr, ordersToInclude = regorders, minLength = 5, 
                 inputSite = 117, inputYear = 2015, plot = T, plotVar = 'meanBiomass')
meanDensityByDay(surveyData = volunteer.pr, ordersToInclude = regorders, minLength = 5, 
                 inputSite = 117, inputYear = 2015, plot = T, plotVar = 'meanBiomass')


