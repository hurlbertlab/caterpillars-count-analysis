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
################################################################################################
# Set wd back to caterpillars-count-analysis: setwd("c:/git/caterpillars-count-analysis")
# Open and run 'summary_function.r' (in git caterpillars-count-analysis folder) 

# Run the functions for plots based on LEPL only

# Hatch year and LEPL mean density
# Before running, choose method of removing caterpillar colony outliers
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs$julianday, PRbs$meanDensity, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRam$julianday, PRam$meanDensity, col = "blue", lwd = 2)
lines(PRpm$julianday, PRpm$meanDensity, col = "red", lwd = 2)
lines(PRvol$julianday, PRvol$meanDensity, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', '% hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Caterpillar Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("% Hatch Year and Caterpillar Mean Density", line = 1)
# (plotted beat sheet first since this graph has the highest peak)

# Hatch year and fraction of surveys where at least one LEPL was recorded
# First replace NA under 'fracSurveys' with zero (no fraction calculated when no caterpillars surveyed):
PRam$fracSurveys[is.na(PRam$fracSurveys)] = 0
PRbs$fracSurveys[is.na(PRbs$fracSurveys)] = 0
PRpm$fracSurveys[is.na(PRpm$fracSurveys)] = 0
PRvol$fracSurveys[is.na(PRvol$fracSurveys)] = 0
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs$julianday, PRbs$fracSurveys, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRam$julianday, PRam$fracSurveys, col = "blue", lwd = 2)
lines(PRpm$julianday, PRpm$fracSurveys, col = "red", lwd = 2)
lines(PRvol$julianday, PRvol$fracSurveys, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', '% hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Fraction of Surveys Caterpillars were Present", side=4, cex = 1.5, las = 0, line = 3)
title("% Hatch Year and Caterpillar Presence", line = 1)
# (plotted beat sheet first since this graph has the highest peak)

# Run the functions for plots based on all arthropods

# Hatch year and all arthropod mean density
# Note that original decision on caterpillar colonies will affect this as well
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRvol.all$julianday, PRvol.all$meanDensity, col = "green", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRbs.all$julianday, PRbs.all$meanDensity, col = "orange", lwd = 2)
lines(PRpm.all$julianday, PRpm.all$meanDensity, col = "red", lwd = 2)
lines(PRam.all$julianday, PRam.all$meanDensity, col = "blue", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', '% hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("% Hatch Year and Arthropod Mean Density", line = 1)
# (plotted volunteer surveys first since this graph has the highest peak)

# Run the functions for plots based on selected arthropods

# Hatch year and selected arthropod (LEPL, ORTH, ARAN) mean density
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "% Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs.mult$julianday, PRbs.mult$meanDensity, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRam.mult$julianday, PRam.mult$meanDensity, col = "blue", lwd = 2)
lines(PRpm.mult$julianday, PRpm.mult$meanDensity, col = "red", lwd = 2)
lines(PRvol.mult$julianday, PRvol.mult$meanDensity, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', '% hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Selected Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("% Hatch Year and Selected Arthropod Mean Density", line = 1)
# (plotted beat sheet first since this graph has the highest peak, but other graphs much lower
# so need readjusting)



# Messing
plot(bands2015$bandingDate, bands2015$broodPatch)

