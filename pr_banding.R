##############################           #######
# Prairie Ridge Banding Data #           # TEH #
##############################           #######

# Organization and analysis of Prairie Ridge bird banding data received July 7, 2015.

setwd('c:/git/bird-phenology')

# Read in data
bands = read.csv('julybands.csv')
# For reference:
# names(bands)
# "Band.Number"                "Species"                    "Age"                       
# "How.Aged"                   "Sex"                        "How.Sexed"                 
# "Skull"                      "Cloacal.Protuberance"       "Brood.Patch"               
# "Fat.Score"                  "Body.Molt"                  "Flight.Feather.Molt"       
# "Wing.Chord"                 "Bird.Weight"                "Bird.Status"               
# "Banding.Date"               "Net.Nest.Cavity.Designator" "Location"                  
# "Remarks"                    "Left.Leg.Color.1"           "Left.Leg.Color.2"          
# "R.Leg.Color.1"              "R.Leg.Color.2" 

# Rename columns
names(bands) <- c('bandNum', 'species', 'age', 'howAged', 'sex', 'howSexed', 'skull', 
                   'cloacalProtuberance', 'broodPatch', 'fatScore', 'bodyMolt', 'flightMolt',
                   'wingChord', 'weight', 'status', 'bandingDate', 'netNestCavityDesig',
                   'location', 'remarks', 'lLegCol1', 'lLegCol2', 'rLegCol1', 'rLegCol2')

# Change banding date to date class
bands$bandingDate <- as.Date(bands$bandingDate, '%m/%d/%Y')

# Add a year column and julian day
tempyear <- substring(bands$bandingDate, 1, 4)
bands$year = tempyear
bands$julianDay <- yday(bands$bandingDate)
bands <- bands[, c('bandNum', 'species', 'age', 'howAged', 'sex', 'howSexed', 'skull', 
                           'cloacalProtuberance', 'broodPatch', 'fatScore', 'bodyMolt', 'flightMolt',
                           'wingChord', 'weight', 'status', 'bandingDate', 'year', 'julianDay', 'netNestCavityDesig',
                           'location', 'remarks', 'lLegCol1', 'lLegCol2', 'rLegCol1', 'rLegCol2')]


# 2014 & 2015 hatch year %, based on Dr. Hurlbert's "hy_maps_phenology" script
bands2015 = subset(bands, (year == 2015 | year == 2014) & location == 'PRRI')
par(mar = c(5,5,1,1), mgp = c(3,1,0), las = 1, cex.lab = 1.5, cex.axis = 1.25)
pr.hy = aggregate(bands2015$age, by=list(bands2015$julianDay), function(x) sum(x=='HY')/length(x)) # don't quite understand the function part of this
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 2, col = 'plum', lty = 2, lwd = 3)


# Including hatch year points and mean arthropod density on same plot
# First, open and run 'summary_function.r' (in git caterpillars-count-analysis folder)

# Hatch year and LEPL mean density
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1.25, col = 'plum', lty = 2, lwd = 3)
par(new = T)
plot(PRam$julianday, PRam$meanDensity, col = "blue", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310))
lines(PRbs$julianday, PRbs$meanDensity, col = "orange")
lines(PRpm$julianday, PRpm$meanDensity, col = "red")
lines(PRvol$julianday, PRvol$meanDensity, col = "green")
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
axis(side=4) # Need to adjust data before setting range
mtext("Caterpillar Mean Density", side=4, cex = 1.25, las = 0)
title("% Hatch Year and Caterpillar Mean Density", line = 1)

# Hatch year and fraction of surveys where at least one LEPL was recorded
par(mar=c(5, 5, 1, 1) + 1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1.25, col = 'plum', lty = 2, lwd = 3)
par(new = T)
plot(PRam$julianday, PRam$fracSurveys, col = "blue", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310))
lines(PRbs$julianday, PRbs$fracSurveys, col = "orange")
lines(PRpm$julianday, PRpm$fracSurveys, col = "red")
lines(PRvol$julianday, PRvol$fracSurveys, col = "green")
legend("topleft", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys'),lwd = 2, lty = 'solid', 
       col = c('blue', 'orange', 'red', 'green'))
axis(side=4) # Need to adjust data before setting range
mtext("Fraction of Surveys Caterpillars were Present", side=4, cex = 1.25, las = 0, line = 3)
title("% Hatch Year and Caterpillar Presence", line = 1)



# Messing
plot(bands2015$bandingDate, bands2015$broodPatch)

