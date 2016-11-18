# Experimental plots just to see what's happening

# Run summary_functions.R and data_cleaning.R first

# Taking out caterpillar colonies
amsurvey.pr <- amsurvey.pr[!(amsurvey.pr$arthCode == "LEPL" & amsurvey.pr$count > 10),]
amsurvey.bg <- amsurvey.bg[!(amsurvey.bg$arthCode == "LEPL" & amsurvey.bg$count > 10),]
labdata.pr <- labdata.pr[!(labdata.pr$arthCode == "LEPL" & labdata.pr$count > 10),]
labdata.bg <- labdata.bg[!(labdata.bg$arthCode == "LEPL" & labdata.bg$count > 10),]

#----Prairie Ridge: Create mean density datasets for cats and bird food, both years, both methods, both sites----
# Don't have volunteer beat sheet data yet
multorders <- c('LEPL', 'ORTH', 'ARAN','COLE', 'HEMI')

PR.LEPL15vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                             ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                             minLength = 5)
PR.LEPL16vis = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                             minLength = 5)

PR.BIRD15vis = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                             ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                             minLength = 5)
PR.BIRD16vis = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                             minLength = 5)

# Botanical Garden
BG.LEPL15vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',], 
                             ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, 
                             minLength = 5)
BG.LEPL16vis = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, 
                             minLength = 5)

BG.BIRD15vis = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',], 
                             ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = F, 
                             minLength = 5)
BG.BIRD16vis = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = F, 
                             minLength = 5)

# Beat sheet
# Prairie Ridge
PR.LEPL15bts = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Beat_Sheet',], 
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                                minLength = 5)
PR.LEPL16bts = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Beat_Sheet',],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                                minLength = 5)

PR.BIRD15bts = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Beat_Sheet',], 
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                                minLength = 5)
PR.BIRD16bts = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Beat_Sheet',],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                                minLength = 5)

# Botanical Garden
BG.LEPL15bts = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Beat_Sheet',], 
                                ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = F, 
                                minLength = 5)
BG.LEPL16bts = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Beat_Sheet',],  
                                ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = F, 
                                minLength = 5)

BG.BIRD15bts = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Beat_Sheet',], 
                                ordersToInclude = multorders, inputYear = 2015, inputSite = 8892356, plot = F, 
                                minLength = 5)
BG.BIRD16bts = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Beat_Sheet',],  
                                ordersToInclude = multorders, inputYear = 2016, inputSite = 8892356, plot = F, 
                                minLength = 5)


if (0) {

#----Prairie Ridge By Year Plot Comparisons----

#pdf(file = "year_comp_PR.pdf", 12, 8)
#par(mfrow = c(2,3))

PR.LEPL15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',], 
                             ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.31), ylab = "Mean density", main = 'PR LEPL')
PR.LEPL16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'red', minLength = 5, lwd = 2, ylim = c(0,.3))
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))


PR.ORTH15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                           ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = T, 
                           plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                           xlim = c(130,208), ylim = c(0,.4), ylab = "Mean density", main = 'PR ORTH')
PR.ORTH16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                           ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = T, 
                           plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))


PR.COLE15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                             ordersToInclude = "COLE", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.4), ylab = "Mean density", main = 'PR COLE')
PR.COLE16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "COLE", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))


PR.ARAN15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                             ordersToInclude = "ARAN", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.3), ylab = "Mean density", main = 'PR ARAN')
PR.ARAN16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "ARAN", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))


PR.AUCH15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                             ordersToInclude = "AUCH", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.7), ylab = "Mean density", main = 'PR AUCH')
PR.AUCH16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "AUCH", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))


PR.HETE15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                             ordersToInclude = "HETE", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.16), ylab = "Mean density", main = 'PR HETE')
PR.HETE16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "HETE", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

#dev.off()

PR.DIPT15 = meanDensityByDay(amsurvey.pr[amsurvey.pr$surveyType == 'Visual',],  
                             ordersToInclude = "DIPT", inputYear = 2015, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.35), ylab = "Mean density", main = 'PR DIPT')
PR.DIPT16 = meanDensityByDay(labdata.pr[labdata.pr$surveyType == 'Visual',],  
                             ordersToInclude = "DIPT", inputYear = 2016, inputSite = 117, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

#----Botanical Garden By Year Plot Comparisons----

#pdf(file = "year_comp_BG.pdf", 12, 8)
#par(mfrow = c(2,3))

BG.LEPL15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.6), ylab = "Mean density", main = 'BG LEPL')
BG.LEPL16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

BG.ORTH15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "ORTH", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.25), ylab = "Mean density", main = 'BG ORTH')
BG.ORTH16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "ORTH", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

BG.COLE15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "COLE", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.4), ylab = "Mean density", main = 'BG COLE')
BG.COLE16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "COLE", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

BG.ARAN15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "ARAN", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.4), ylab = "Mean density", main = 'BG ARAN')
BG.ARAN16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "ARAN", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

BG.AUCH15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "AUCH", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.3), ylab = "Mean density", main = 'BG AUCH')
BG.AUCH16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "AUCH", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

BG.HETE15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "HETE", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.25), ylab = "Mean density", main = 'BG HETE')
BG.HETE16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "HETE", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

#dev.off()

BG.DIPT15 = meanDensityByDay(amsurvey.bg[amsurvey.bg$surveyType == 'Visual',],  
                             ordersToInclude = "DIPT", inputYear = 2015, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = T, color = 'blue', minLength = 5, lwd = 2,
                             xlim = c(130,208), ylim = c(0,.6), ylab = "Mean density", main = 'BG DIPT')
BG.DIPT16 = meanDensityByDay(labdata.bg[labdata.bg$surveyType == 'Visual',],  
                             ordersToInclude = "DIPT", inputYear = 2016, inputSite = 8892356, plot = T, 
                             plotVar = 'meanDensity', new = F, color = 'red', minLength = 5, lwd = 2)
legend("topleft", c('2015', '2016'), lwd = 2, col = c('blue', 'red'))

} # end if (0)

#----addchangevec function----
# Function that returns the mean of change between days for a 
# dataset created with the meanDensityByDay function

addchangevec = function(dataset) 
  
{

# Create empty change between days vector
changevec <- c()
jdays = dataset$julianday

# For loop for calculating change between days
for (day in head(jdays, -1)) {
  tempchange = dataset[dataset$julianday == jdays[match(day,jdays) + 1],]$meanDensity - dataset[dataset$julianday == day,]$meanDensity
  changevec <- c(changevec, tempchange)
} # end for loop

#print(changevec)
abschange <- abs(changevec)
mean(abschange)

} # end function

if (0) {
  
# Have to create these first:
# Make dataset of average change between days values
PR_2015 <- c(addchangevec(PR.LEPL15), addchangevec(PR.ORTH15), addchangevec(PR.COLE15), 
             addchangevec(PR.ARAN15), addchangevec(PR.AUCH15), 
             addchangevec(PR.DIPT15), addchangevec(PR.HETE15))
PR_2016 <- c(addchangevec(PR.LEPL16), addchangevec(PR.ORTH16), addchangevec(PR.COLE16), 
             addchangevec(PR.ARAN16), addchangevec(PR.AUCH16),
             addchangevec(PR.DIPT16), addchangevec(PR.HETE16))
BG_2015 <- c(addchangevec(BG.LEPL15), addchangevec(BG.ORTH15), addchangevec(BG.COLE15), 
             addchangevec(BG.ARAN15), addchangevec(BG.AUCH15),  
             addchangevec(BG.DIPT15), addchangevec(BG.HETE15))
BG_2016 <- c(addchangevec(BG.LEPL16), addchangevec(BG.ORTH16), addchangevec(BG.COLE16), 
             addchangevec(BG.ARAN16), addchangevec(BG.AUCH16),  
             addchangevec(BG.DIPT16), addchangevec(BG.HETE16))
changeByDay <- data.frame(rbind(PR_2015, PR_2016, BG_2015, BG_2016))
names(changeByDay) <- c('LEPL', 'ORTH', 'COLE', 'ARAN', 'AUCH', 'DIPT', 'HETE')



par(mfrow = c(1,1), oma = c(1,1,1,1), mar = c(4,4,2,2))
colors <- c('red', 'red3', 'blue', 'blue4')
barplot(as.matrix(changeByDay), ylab = "Average change in density between surveys", 
        xlab = "Arthropod", beside=TRUE, col=colors, border = NA, bty = "L")
par(xpd = TRUE)
legend('topright', 
       legend = c("PR 2015", "PR 2016", "BG 2015", "BG 2016"), 
       fill = c('red', 'red3', 'blue', 'blue4'), border = NA)

} # end if (0) #2




