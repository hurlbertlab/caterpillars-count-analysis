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

# Read in migration form data
migform = read.csv('BBS-migration.csv')
migform = migform[, c(1,3)]

# Subset based on year
bands1415 = subset(bands, (year == 2015 | year == 2014) & location == 'PRRI' & julianDay %in% 120:310)
# Or separate years
bands14 = subset(bands, year == 2014 & location == 'PRRI' & julianDay %in% 120:310)
bands15 = subset(bands, year == 2015 & location == 'PRRI' & julianDay %in% 120:310)

# Merge migration type data with banding data
bandsmig = merge(bands1415, migform, by = 'species', all.x = T)
# Or separate years, #### ONLY RUN ONE IF PLAN TO SEPARATE: ####
bandsmig = merge(bands14, migform, by = 'species', all.x = T)
bandsmig = merge(bands15, migform, by = 'species', all.x = T)

# Subset based on migration type
# nt = neotropical migrant, sd = short distance migrant, r = permanent resident
bandsallm = bandsmig[(bandsmig$migration == 'nt'|bandsmig$migration == 'sd'),]
bandsnt = bandsmig[bandsmig$migration == 'nt',]
bandssd = bandsmig[bandsmig$migration == 'sd',]
bandsr = bandsmig[bandsmig$migration == 'r',]

# 2014 & 2015 hatch year fraction, based on Dr. Hurlbert's "hy_maps_phenology" script
par(mar = c(5,5,1,1), mgp = c(3,1,0), las = 1, cex.lab = 1.5, cex.axis = 1.25)
pr.hy = aggregate(bands1415$age, by=list(bands1415$julianDay), function(x) sum(x=='HY')/length(x))
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 2, col = 'plum', lty = 2, lwd = 3)
# Separate years, 2014:
par(mar = c(5,5,1,1), mgp = c(3,1,0), las = 1, cex.lab = 1.5, cex.axis = 1.25)
pr.hy = aggregate(bands14$age, by=list(bands14$julianDay), function(x) sum(x=='HY')/length(x))
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 2, col = 'plum', lty = 2, lwd = 3)
# Separate years, 2015:
par(mar = c(5,5,1,1), mgp = c(3,1,0), las = 1, cex.lab = 1.5, cex.axis = 1.25)
pr.hy = aggregate(bands15$age, by=list(bands15$julianDay), function(x) sum(x=='HY')/length(x))
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 2, col = 'plum', lty = 2, lwd = 3)

# Graph the subsets based on migration type
par(mar = c(5,5,1,1), mgp = c(3,1,0), las = 1, cex.lab = 1.5, cex.axis = 1.25)
pr.hynt = aggregate(bandsnt$age, by=list(bandsnt$julianDay), function(x) sum(x=='HY')/length(x))
plot(pr.hynt$Group.1, pr.hynt$x, xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.1), cex = 2, col = 'brown', lty = 1, lwd = 3, type = 'l')
pr.hysd = aggregate(bandssd$age, by=list(bandssd$julianDay), function(x) sum(x=='HY')/length(x))
points(pr.hysd$Group.1, pr.hysd$x, col = 'green3', type = 'l', lwd = 3)
pr.hyr = aggregate(bandsr$age, by=list(bandsr$julianDay), function(x) sum(x=='HY')/length(x))
points(pr.hyr$Group.1, pr.hyr$x, col = 'lightslateblue', type = 'l', lwd = 3)
legend("topleft", c('neotropical', 'short distance', 'resident'),lwd = 2, lty = 'solid', 
       col = c('brown', 'green3', 'lightslateblue'))

####################
# Adding parabolas #
####################

jd = c(120:310)

# All migration types
hy.lm = lm(x ~ Group.1 + I(Group.1^2), data = pr.hy)
hy.predict = hy.lm$coefficients[1] + hy.lm$coefficients[2]*jd + hy.lm$coefficients[3]*jd^2
points(jd, hy.predict, type = 'l', col = 'plum')

# Neotropical migrants
hynt.lm = lm(x ~ Group.1 + I(Group.1^2), data = pr.hynt)
nt.predict = hynt.lm$coefficients[1] + hynt.lm$coefficients[2]*jd + hynt.lm$coefficients[3]*jd^2
points(jd, nt.predict, type = 'l', col = 'brown')

# Short distance migrants
hysd.lm = lm(x ~ Group.1 + I(Group.1^2), data = pr.hysd)
sd.predict = hysd.lm$coefficients[1] + hysd.lm$coefficients[2]*jd + hysd.lm$coefficients[3]*jd^2
points(jd, sd.predict, type = 'l', col = 'green3')

# Permanent residents
hyr.lm = lm(x ~ Group.1 + I(Group.1^2), data = pr.hyr)
r.predict = hyr.lm$coefficients[1] + hyr.lm$coefficients[2]*jd + hyr.lm$coefficients[3]*jd^2
points(jd, r.predict, type = 'l', col = 'lightslateblue')

# Calculating peaks
jd[hy.predict == max(hy.predict)] # all migrant type peak
jd[nt.predict == max(nt.predict)] # neotropical migrant peak
jd[sd.predict == max(sd.predict)] # short distance migrant peak
jd[r.predict == max(r.predict)] # permanent resident peak

#######################################################################
# Including hatch year points and mean arthropod density on same plot #
#######################################################################
# Set wd back to caterpillars-count-analysis: setwd("c:/git/caterpillars-count-analysis")
# Open and run 'summary_function.r' (in git caterpillars-count-analysis folder) 

# Run the functions for plots based on LEPL only

# Hatch year and LEPL mean density
# Before running, choose method of removing caterpillar colony outliers
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs$julianday, PRbs$meanDensity, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRam$julianday, PRam$meanDensity, col = "blue", lwd = 2)
lines(PRpm$julianday, PRpm$meanDensity, col = "red", lwd = 2)
lines(PRvol$julianday, PRvol$meanDensity, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', 'fraction hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Caterpillar Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("Fraction Hatch Year and Caterpillar Mean Density", line = 1)
# (plotted beat sheet first since this graph has the highest peak)

# Hatch year and fraction of surveys where at least one LEPL was recorded
# First replace NA under 'fracSurveys' with zero (no fraction calculated when no caterpillars surveyed):
PRam$fracSurveys[is.na(PRam$fracSurveys)] = 0
PRbs$fracSurveys[is.na(PRbs$fracSurveys)] = 0
PRpm$fracSurveys[is.na(PRpm$fracSurveys)] = 0
PRvol$fracSurveys[is.na(PRvol$fracSurveys)] = 0
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs$julianday, PRbs$fracSurveys, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRam$julianday, PRam$fracSurveys, col = "blue", lwd = 2)
lines(PRpm$julianday, PRpm$fracSurveys, col = "red", lwd = 2)
lines(PRvol$julianday, PRvol$fracSurveys, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', 'fraction hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Fraction of Surveys Caterpillars were Present", side=4, cex = 1.5, las = 0, line = 3)
title("Fraction Hatch Year and Caterpillar Presence", line = 1)
# (plotted beat sheet first since this graph has the highest peak)

# Run the functions for plots based on all arthropods

# Hatch year and all arthropod mean density
# Note that original decision on caterpillar colonies will affect this as well
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.3), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRvol.all$julianday, PRvol.all$meanDensity, col = "green", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), lwd = 2)
lines(PRbs.all$julianday, PRbs.all$meanDensity, col = "orange", lwd = 2)
lines(PRpm.all$julianday, PRpm.all$meanDensity, col = "red", lwd = 2)
lines(PRam.all$julianday, PRam.all$meanDensity, col = "blue", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', 'fraction hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4) # Need to adjust data before setting range
mtext("Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("Fraction Hatch Year and Arthropod Mean Density", line = 1)
# (plotted volunteer surveys first since this graph has the highest peak)

# Run the functions for plots based on selected arthropods

# Hatch year and selected arthropod (LEPL, ORTH, ARAN, COLE, HEMI) mean density
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.1), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs.mult$julianday, PRbs.mult$meanDensity, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), ylim = c(0.3, 1.4), lwd = 2)
lines(PRam.mult$julianday, PRam.mult$meanDensity, col = "blue", lwd = 2)
lines(PRpm.mult$julianday, PRpm.mult$meanDensity, col = "red", lwd = 2)
lines(PRvol.mult$julianday, PRvol.mult$meanDensity, col = "green", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'lab pm surveys', 'volunteer surveys', 'fraction hatch year'),
       lwd = c(2,2,2,2,4), lty = c(1,1,1,1,3), col = c('blue', 'orange', 'red', 'green', 'plum'))
axis(side=4)
mtext("Selected Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("Fraction Hatch Year and Selected Arthropod Mean Density", line = 1)

# Getting a better comparison with mismatching Julian Days
birdday <- unique(pr.hy$Group.1)
arthday <- unique(PRbs.mult$julianday)
julianall1 <- c(birdday, arthday)
julianall2 <- julianall1[order(julianall1)]
julianall <- c(julianall2)
pr.hyFull <- merge(pr.hy, julianall, by.x = 'Group.1', by.y = 'julianall', all = T)
# still working on merging errors



###################
# For final paper #
###################

# Selected arthropod mean density compared to fraction hatch year plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(pr.hy$Group.1, pr.hy$x, type = 'l', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 1.1), cex = 1, cex.lab = 1.5, col = 'plum', lty = 3, lwd = 4)
par(new = T)
plot(PRbs.mult$julianday, PRbs.mult$meanDensity, col = "orange", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), ylim = c(0.3, 2.1), lwd = 2)
lines(PRam.mult$julianday, PRam.mult$meanDensity, col = "blue", lwd = 2)
legend("topright", c('lab am surveys', 'lab beat sheet', 'fraction hatch year'),
       lwd = c(2,2,4), lty = c(1,1,3), col = c('blue', 'orange', 'plum'))
axis(side=4)
mtext("Selected Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("Fraction Hatch Year and Selected Arthropod Mean Density", line = 1)

# Comparison of 5 parabolas (3 migrant type hatch year fractions, 2 types of arthropod surveying)
# Migrant type plots
par(mar=c(5, 4, 4, 4) + 0.1)
plot(jd, nt.predict, type = 'l', col = 'brown', xlab = "Julian Day", ylab = "Fraction Hatch Year Birds",
     xlim = c(120, 310), ylim = c(0, 0.9), cex = 1, cex.lab = 1.5, lty = 3, lwd = 4)
points(jd, sd.predict, type = 'l', col = 'green3', lty = 3, lwd = 4)
points(jd, r.predict, type = 'l', col = 'lightslateblue', lty = 3, lwd = 4)
# Create parabolas from arthropod data
am.lm = lm(meanDensity ~ julianday + I(julianday^2), data = PRam.mult)
am.predict = am.lm$coefficients[1] + am.lm$coefficients[2]*jd + am.lm$coefficients[3]*jd^2
bs.lm = lm(meanDensity ~ julianday + I(julianday^2), data = PRbs.mult)
bs.predict = bs.lm$coefficients[1] + bs.lm$coefficients[2]*jd + bs.lm$coefficients[3]*jd^2
# Plot arthropod parabolas
par(new = T)
plot(jd, am.predict, col = "blue", type = 'l', axes = FALSE, 
     bty = "n", xlab = "", ylab = "", xlim = c(120, 310), ylim = c(0.3, 1.8), lwd = 3)
points(jd, bs.predict, type = 'l', col = 'orange', lwd = 3)
# Add legend, axes
legend("bottomright", c('neotropical bird', 'short distance bird', 'resident bird', 
                     'lab am arthropods', 'beat sheet arthropods'), cex = 1,
       lwd = c(4,4,4,3,3), lty = c(3,3,3,1,1), col = c('brown', 'green3', 'lightslateblue', 'blue', 'orange'))
axis(side=4)
mtext("Selected Arthropod Mean Density", side=4, cex = 1.5, las = 0, line = 3)
title("Parabolic Distribution of Fraction Hatch Year and Selected Arthropod Mean Density", line = 1)

# Calculating peaks for table
jd[hy.predict == max(hy.predict)] # all migrant type peak
jd[nt.predict == max(nt.predict)] # neotropical migrant peak
jd[sd.predict == max(sd.predict)] # short distance migrant peak
jd[r.predict == max(r.predict)] # permanent resident peak
jd[am.predict == max(am.predict)] # am arth survey peak
jd[bs.predict == max(bs.predict)] # beat sheet arth survey peak
# Making a table (need to fix):
labels <- c('Neotropical migrant', 'Short distance migrant', 'Permanent resident', 
            'AM survey arthropods', 'Beat sheet arthropods')
peaks <- c(259,237,214,171,165)
peaktable <- table(labels, peaks)
mosaicplot(peakplot)

#######################
# Testing correlation #
#######################

cordata <- data.frame(jd)
cordata$nt <- nt.predict
cordata$sd <- sd.predict
cordata$r <- r.predict
cordata$am <- am.predict
cordata$bs <- bs.predict

cor(cordata$am, cordata$bs) # very correlated, just going to use BS moving forward)

# LA = 'lag-adjusted' (for BS)
nt.predictLA = hynt.lm$coefficients[1] + hynt.lm$coefficients[2]*(jd+94) + hynt.lm$coefficients[3]*(jd+94)^2
sd.predictLA = hysd.lm$coefficients[1] + hysd.lm$coefficients[2]*(jd+72) + hysd.lm$coefficients[3]*(jd+72)^2
r.predictLA = hyr.lm$coefficients[1] + hyr.lm$coefficients[2]*(jd+49) + hyr.lm$coefficients[3]*(jd+49)^2
cordata$ntLA <- nt.predictLA
cordata$sdLA <- sd.predictLA
cordata$rLA <- r.predictLA

cor(cordata$bs, cordata$ntLA)
cor(cordata$bs, cordata$sdLA)
cor(cordata$bs, cordata$rLA)

# TW = two week adjustment
nt.predictTW = hynt.lm$coefficients[1] + hynt.lm$coefficients[2]*(jd+14) + hynt.lm$coefficients[3]*(jd+14)^2
sd.predictTW = hysd.lm$coefficients[1] + hysd.lm$coefficients[2]*(jd+14) + hysd.lm$coefficients[3]*(jd+14)^2
r.predictTW = hyr.lm$coefficients[1] + hyr.lm$coefficients[2]*(jd+14) + hyr.lm$coefficients[3]*(jd+14)^2
cordata$ntTW <- nt.predictTW
cordata$sdTW <- sd.predictTW
cordata$rTW <- r.predictTW

cor(cordata$bs, cordata$ntTW)
cor(cordata$bs, cordata$sdTW)
cor(cordata$bs, cordata$rTW)

# OM = one month adjustment
nt.predictOM = hynt.lm$coefficients[1] + hynt.lm$coefficients[2]*(jd+30) + hynt.lm$coefficients[3]*(jd+30)^2
sd.predictOM = hysd.lm$coefficients[1] + hysd.lm$coefficients[2]*(jd+30) + hysd.lm$coefficients[3]*(jd+30)^2
r.predictOM = hyr.lm$coefficients[1] + hyr.lm$coefficients[2]*(jd+30) + hyr.lm$coefficients[3]*(jd+30)^2
cordata$ntOM <- nt.predictOM
cordata$sdOM <- sd.predictOM
cordata$rOM <- r.predictOM

cor(cordata$bs, cordata$ntOM)
cor(cordata$bs, cordata$sdOM)
cor(cordata$bs, cordata$rOM)



# Messing
plot(bands2015$bandingDate, bands2015$broodPatch)

