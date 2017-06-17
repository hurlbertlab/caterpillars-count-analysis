# Data currently in GoogleDoc here

# Use the 'gsheet' package by maxconway for reading into R

#library(devtools)
#devtools::install_github("maxconway/gsheet")

library(gsheet)
library(lubridate)
library(dplyr)

# URL copied from address bar
frassBackup = function(open = F) {
  require(gsheet)
  url = "https://docs.google.com/spreadsheets/d/1RwXzwhHUbP0m5gKSOVhnKZbS1C_NrbdfHLglIVCzyFc/edit#gid=1479231778"
  data = gsheet2tbl(url)

  # Write a copy
  write.csv(data, paste('data/arthropods/frass_', Sys.Date(), '.csv', sep = ''),
          row.names = F)
  if (open) { return (data) }
}


# Function that takes a date field (formatted as %m/%d/%Y) and a time field
# (hh:mm in 24h time), converts the date to julian day and adds the fractional
# day represented by the hours and minutes
julianDayTime = function(date, hour_min) {
  require(lubridate)
  jday = yday(date)
  temp = sapply(strsplit(hour_min, ":"), function(x) {
    x = as.numeric(x)
    x[1] + x[2]/60
  })
  output = jday + temp/24
  return(output)
}



# Convert date class
data$Date.Set = as.Date(data$Date.Set, format = "%m/%d/%Y")
data$Date.Collected = as.Date(data$Date.Collected, format = "%m/%d/%Y")
data$Year = format(data$Date.Collected, "%Y")
data$jday.Set = julianDayTime(data$Date.Set, data$Time.Set)
data$jday.Collected = julianDayTime(data$Date.Collected, data$Time.Collected)

data$frass.mg.d = data$Frass.mass..mg./(data$jday.Collected - data$jday.Set)
data$frass.no.d = data$Frass.number/(data$jday.Collected = data$jday.Set)

data$jday = floor(data$jday.Collected)

meanfrass = data %>%
  group_by(Site, Year, jday) %>%
  summarize(mass = mean(frass.mg.d, na.rm=T),
            density = mean(frass.no.d, na.rm=T))

write.csv(meanfrass, "data/arthropods/frass_by_day_2015-2017.csv", row.names = F)

frassplot = function(frassdata, site, year, color = 'black', new = T, var = 'mass',...) {
  temp = filter(frassdata, Site == site, Year == year)
  if (new & var == 'mass') {
    plot(temp$jday, temp$mass, xlab = "Julian day", ylab = "Mean frass (mg / trap / day)",
         type = 'l', col = color, ...)
  } else if (!new & var == 'mass') {
    points(temp$jday, temp$mass, type = 'l', col = color, ...)
  } else if (new & var == 'density') {
    plot(temp$jday, temp$density, xlab = "Julian day", ylab = "Mean frass (no. / trap / day)",
         type = 'l', col = color, ...)
  } else if (!new & var == 'density') {
    points(temp$jday, temp$density, type = 'l', col = color, ...)
  }
}

frassplot(meanfrass, "Botanical Garden", 2015, 'blue', new = T, var = 'mass', ylim = c(0, 6), lwd = 3)
frassplot(meanfrass, "Botanical Garden", 2016, 'red', new = F, var = 'mass', lwd = 3)
frassplot(meanfrass, "Prairie Ridge", 2015, 'green', new = F, var = 'mass', lwd = 3)

frassplot(meanfrass, "Botanical Garden", 2015, 'blue', new = T, var = 'density', ylim = c(0, 0.35), lwd = 3)
frassplot(meanfrass, "Botanical Garden", 2016, 'red', new = F, var = 'density', lwd = 3)
frassplot(meanfrass, "Prairie Ridge", 2015, 'green', new = F, var = 'density', lwd = 3)

frassplot(meanfrass, "Botanical Garden", 2017, 'red', new =T, var = 'density', lwd = 3)
frassplot(meanfrass, "Botanical Garden", 2017, 'red', new =T, var = 'mass', lwd = 3)


prlep15.bsden = meanDensityByDay(beatsheet.pr, ordersToInclude = "LEPL", inputYear = 2015,
                                   inputSite = 117, jdRange = c(138,197), outlierCount = 10000,
                                   plot = F, plotVar = 'meanDensity')
bsGfit = fitG(prlep15.bsden$julianday, prlep15.bsden$meanDensity, 
              weighted.mean(prlep15.bsden$julianday, prlep15.bsden$meanDensity),
              14, 200)  

prlep15.frden = filter(meanfrass, Site == "Prairie Ridge", Year == 2015, jday >= 138, jday <=197)

frGfit = fitG(prlep15.frden$jday, prlep15.frden$density, 
              weighted.mean(prlep15.frden$jday, prlep15.frden$density),
              14, 200)  




par(new=T)
frassplot(meanfrass, "Prairie Ridge", 2015, 'green', new = T, var = 'density', lwd = 3, xlim = c(139, 206))

lepbs15 = 

bsG = fitG(series$julianday, series[, plotVar], 
           weighted.mean(series$julianday, series[, plotVar]),
           14, 200)
