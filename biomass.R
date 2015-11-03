# Script for calculating biomass and adding this as column to the cleandata

# Create empty biomass column
cleandata$biomass = NA

# y = a(x)^b
# Read in arthropod regression data with slope = b and intercept = log(a)
reg.data.temp <- read.csv('arth_regression.csv', header = T, sep = ',')
# Calculate a (the coefficient)
reg.data.temp$coefficient <- 10^(reg.data.temp$intercept)

# Create list of arthropod orders (by code)
arthlist <- arthcodes$ArthCode # arthcodes from data_cleaning.R

# Merge reg.data.temp and arthlist so NAs will be calculated
reg.data <- merge(reg.data.temp, arthcodes, by.x = 'arthCode', by.y = 'ArthCode', all = T)

# For loop for calculating biomass for each observation
for (ord in arthlist) {
  b = reg.data[reg.data$arthCode == ord,]$slope
  a = reg.data[reg.data$arthCode == ord,]$coefficient
  cleandata$biomass[cleandata$arthCode == ord,] = (a*(cleandata$length[cleandata$arthCode == ord,])^(b))
}