# Need to run data_cleaning.r first

# Caterpillars by day at Prairie Ridge for 2016
lepsbyday = meanDensityByDay(amsurvey.pr, effort = effortByDay, ordersToInclude = 'LEPL', minLength = 5, inputSite=117, inputYear=2016, plot=T)

jds = yday(as.Date(c("2016-05-15", "2016-06-01", "2016-06-15", "2016-07-01", '2016-07-15'), format = "%Y-%m-%d"))

# Hatching and fledging dates for house wren nest at Prairie Ridge
birddates = yday(as.Date(c('2016-05-30','2016-06-15'), format = "%Y-%m-%d"))

pdf('z:/talks/2016/NAOC/PR_cats_and_howr.pdf', height = 4, width = 5)
par(mgp = c(2.5, 1, 0))
plot(lepsbyday$julianday, 100*lepsbyday$meanDensity, type = 'l', xlab = '', ylab = 'Caterpillar density', las = 1, xaxt = "n", col = 'limegreen', lwd = 3)
axis(1, at = jds, labels = F)
mtext(c('May-15', 'Jun-1', 'Jun-15', 'Jul-1', 'Jul-15'), 1, at = jds, line = 1)

abline(v = birddates, lty = c('dotted', 'dashed'), lwd = 3, col = 'purple')
dev.off()
