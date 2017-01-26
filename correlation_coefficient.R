# Correlation coefficients between trained and citizen scientist estimates over the year
# Weekly average values

setwd('C:/git/caterpillars-count-analysis')
source('summary_functions.r')
source('data_cleaning.R')

# 2015 Visual LEPL
PR.LEPL15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5)
PR.LEPL15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5)
PR.LEPL15 = merge(PR.LEPL15.sci[,c(1,6)], PR.LEPL15.cs[,c(1,6)], by = 'week', all = F)
names(PR.LEPL15) = c('week', 'sci', 'cs')
LEPL15r = cor(PR.LEPL15$sci, PR.LEPL15$cs)


# 2016 Beat Sheet LEPL
PR.LEPL16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5)
PR.LEPL16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5)
PR.LEPL16 = merge(PR.LEPL16.sci[,c(1,6)], PR.LEPL16.cs[,c(1,6)], by = 'week', all = F)
names(PR.LEPL16) = c('week', 'sci', 'cs')
LEPL16r = cor(PR.LEPL16$sci, PR.LEPL16$cs)

# 2015 Visual ORTH
PR.ORTH15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5)
PR.ORTH15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5)
PR.ORTH15 = merge(PR.ORTH15.sci[,c(1,6)], PR.ORTH15.cs[,c(1,6)], by = 'week', all = F)
names(PR.ORTH15) = c('week', 'sci', 'cs')
ORTH15r = cor(PR.ORTH15$sci, PR.ORTH15$cs)

# 2016 Beat Sheet ORTH
PR.ORTH16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5)
PR.ORTH16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = "ORTH", inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.ORTH16 = merge(PR.ORTH16.sci[,c(1,6)], PR.ORTH16.cs[,c(1,6)], by = 'week', all = F)
names(PR.ORTH16) = c('week', 'sci', 'cs')
ORTH16r = cor(PR.ORTH16$sci, PR.ORTH16$cs)

# 2015 Visual BIRD
PR.BIRD15.sci = meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                                  ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                                  plotVar = 'fracSurveys', new = T, minLength = 5)
PR.BIRD15.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2015, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F)
PR.BIRD15 = merge(PR.BIRD15.sci[,c(1,6)], PR.BIRD15.cs[,c(1,6)], by = 'week', all = F)
names(PR.BIRD15) = c('week', 'sci', 'cs')
BIRD15r = cor(PR.BIRD15$sci, PR.BIRD15$cs)

# 2016 Beat Sheet BIRD
PR.BIRD16.sci = meanDensityByWeek(beatsheet.pr[beatsheet.pr$surveyType == 'Beat_Sheet' & beatsheet.pr$julianday %in% c(134:204),], 
                                   ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                                   plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 2,
                                   xlim = c(20,30), ylim = c(0,.6), ylab = "", main = '')
PR.BIRD16.cs = meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                                 ordersToInclude = multorders, inputYear = 2016, inputSite = 117, plot = F, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 2, lty = 2)
PR.BIRD16 = merge(PR.BIRD16.sci[,c(1,6)], PR.BIRD16.cs[,c(1,6)], by = 'week', all = F)
names(PR.BIRD16) = c('week', 'sci', 'cs')
BIRD16r = cor(PR.BIRD16$sci, PR.BIRD16$cs)

cordat <- data.frame(year = rep(c(2015,2016), times = 3), 
                     arth = c('LEPL', 'LEPL', 'ORTH', 'ORTH', 'BIRD', 'BIRD'),
                     method = rep(c('visual', 'beat_sheet'), times = 3), 
                     cor = c(LEPL15r, LEPL16r, ORTH15r, ORTH16r, BIRD15r, BIRD16r))


# Histograms of the times surveys were done

par(mfrow = c(2,4), mar = c(4,4,3,2), oma = c(5,5,3,3))

labvs15 = amsurvey.pr[amsurvey.pr$year == 2015,]
hist(as.numeric(substr(labvs15$date, 12, 13)), xlab = 'Hour', main = 'Lab Visual 2015')

vol15 = volunteer.pr[volunteer.pr$year == 2015,]
hist(as.numeric(substr(vol15$date, 12, 13)), xlab = 'Hour', main = 'Vol Visual 2015')

labbs15 = beatsheet.pr[beatsheet.pr$year == 2015,]
hist(as.numeric(substr(labbs15$date, 12, 13)), xlab = 'Hour', main = 'Lab BS 2015')

labpm15 = pmsurvey.pr[pmsurvey.pr$year == 2015,]
hist(as.numeric(substr(labpm15$date, 12, 13)), xlab = 'Hour', main = 'Lab PM 2015')

labvs16 = labdata.pr[labdata.pr$year == 2016 & labdata.pr$surveyType == 'Visual',]
hist(as.numeric(substr(labvs16$date, 12, 13)), xlab = 'Hour', main = 'Lab Visual 2016')

vol16 = volunteer.pr[volunteer.pr$year == 2016,]
hist(as.numeric(substr(vol16$date, 12, 13)), xlab = 'Hour', main = 'Vol BS 2016')

labbs16 = beatsheet.pr[beatsheet.pr$year == 2016,]
hist(as.numeric(substr(labbs16$date, 12, 13)), xlab = 'Hour', main = 'Lab BS 2016')





# Checking how similar pm lab is to am lab (2015)
par(mfrow=c(1,1), mar = c(4,4,2,2), oma = c(1,1,1,1))
amlab <- meanDensityByWeek(amsurvey.pr[amsurvey.pr$surveyType == 'Visual' & amsurvey.pr$julianday %in% c(134:204),], 
                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                 plotVar = 'fracSurveys', new = T, minLength = 5, ylim = c(0,0.12), main = 'By week')
pmlab <- meanDensityByWeek(pmsurvey.pr[pmsurvey.pr$surveyType == 'Visual' & pmsurvey.pr$julianday %in% c(134:204),], 
                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                  plotVar = 'fracSurveys', new = F, minLength = 5, col = 'blue')
pmvol <- meanDensityByWeek(volunteer.pr[volunteer.pr$julianday %in% c(134:204),],  
                  ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, plot = T, 
                  plotVar = 'fracSurveys', new = F, minLength = 5, col = 'red')
legend("topleft", c('am lab', 'pm lab', 'pm volunteers'), lty = 1, col = c('black', 'blue', 'red'))
mergedlab <- merge(amlab[,c(1,6)], pmlab[,c(1,6)], by = 'week', all = F)
names(mergedlab) = c('week', 'am', 'pm')
cor(mergedlab$am, mergedlab$pm)
