# Exploring Hubbard Brook caterpillar dataset

# Load libraries
library(lubridate)
library(dplyr)

dat = read.csv('leps.csv', header=T, stringsAsFactors = F)
dat$date = as.character(as.POSIXlt(dat$date, format = '%m/%d/%Y'))
dat$year = as.numeric(substr(dat$date, 1, 4))

# cats by year, plot, and day
plotsbyday = dat %>% group_by(year, plot, yearday) %>% 
  summarize(total = sum(number.lep), biomass = sum(lepbio.mass.mg.))

# cats by year and day (lumping over plot)
byday = dat %>% group_by(year, yearday) %>% 
  summarize(total = sum(number.lep), biomass = sum(lepbio.mass.mg.))

# Hubbard Brook plots by year
pdf('hubbardbrook_by_year.pdf', height = 8, width = 10)
par(mfrow = c(3, 4), mgp = c(2.5, 1, 0), mar = c(2, 2, 1, 1),
    oma = c(3, 3, 0, 0))
for (y in unique(byday$year)) {
  temp = filter(byday, year == y)
  plot(temp$yearday, temp$total, type = 'l', 
       xlab = "", ylab = "",
       lwd = 3, main = y)
}
mtext("Julian day", 1, outer = TRUE, line = 1)
mtext("Caterpillar density", 2, outer = TRUE, line = 1)
dev.off()

pdf('hubbardbrook_by_plotyear.pdf', height = 8, width = 10)
par(mfrow = c(3, 4), mgp = c(2.5, 1, 0), mar = c(2, 2, 1, 1),
    oma = c(3, 3, 0, 0))
n = 0
for (y in unique(plotsbyday$year)) {
  for (p in unique(plotsbyday$plot)) {
    temp = filter(byday, year == y)
    plot(temp$yearday, temp$total, type = 'l', 
         xlab = "", ylab = "",
         lwd = 3, main = paste(y, ", Plot ", p, sep = ""))
    n = n+1
    if (n%%12 == 0) {
      mtext("Julian day", 1, outer = TRUE, line = 1)
      mtext("Caterpillar density", 2, outer = TRUE, line = 1)
    }
  }
}
dev.off()






plot(byday$yearday, byday$biomass, type = "n", xlab = "Julian day", ylab = "Caterpillar density", ylim = c(0, 300))
sapply(min(byday$year):max(byday$year), function(x) {temp = subset(byday, year==x); points(temp$yearday, temp$total, type = 'l', lwd = 2, col = 'gray90') } )

plot(plotsbyday$yearday, plotsbyday$biomass, type = "n", xlab = "Julian day", ylab = "Caterpillar density", ylim = c(0, 60))
for (y in unique(plotsbyday$year)) {
  for (p in unique(plotsbyday$plot)) {
    temp = filter(plotsbyday, year == y, plot == p)
    points(temp$yearday, temp$total, type = 'l', col='gray80')
  }
}

