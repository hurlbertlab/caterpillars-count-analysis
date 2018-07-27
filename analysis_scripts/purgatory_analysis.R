# Summary of sampling effort by site

# Analysis that spans old and new database; will be obsolete very soon

library(dplyr)
library(tidyr)
library(lubridate)


# read in data
plant = read.csv('data/arthropods/Plant.csv', header = T, quote = '\"')
survey = read.csv('data/arthropods/Survey.csv', header = T, quote = '\"')
site = read.csv('data/arthropods/Site.csv', header = T, quote = '\"')
arth = read.csv('data/arthropods/ArthropodSighting.csv', header = T, quote = '\"')

surv = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>% 
  select(Name, LocalDate) %>%
  group_by(Name) %>%
  mutate(n_surveys = n(),
         n_dates = n_distinct(LocalDate)) %>%
  select(Name, n_surveys, n_dates) %>%
  arrange(desc(n_dates)) %>%
  unique() %>%
  data.frame()


PR2018vislepdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
  filter(Name == 'Prairie Ridge Ecostation', ObservationMethod == 'Visual') %>%
  mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
         julianday = yday(LocalDate)) %>%
  left_join(arth, by = c('ID' = 'SurveyFK')) %>%
  select(LocalDate, julianday, Code, Group, Length, Quantity) %>%
  filter(Group == 'caterpillar', Length >= 5) %>%
  distinct(julianday, Code) %>%
  count(julianday) %>%
  mutate(fracSurveys = 100*n/60)

BG2018vislepdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
  filter(Name == 'NC Botanical Garden', ObservationMethod == 'Visual') %>%
  mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
         julianday = yday(LocalDate)) %>%
  left_join(arth, by = c('ID' = 'SurveyFK')) %>%
  select(LocalDate, julianday, Code, Group, Length, Quantity) %>%
  filter(Group == 'caterpillar', Length >= 5) %>%
  distinct(julianday, Code) %>%
  count(julianday) %>%
  mutate(fracSurveys = 100*n/40)

UNC2018vislepdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
  filter(Name == 'UNC Chapel Hill Campus', ObservationMethod == 'Visual') %>%
  mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
         julianday = yday(LocalDate)) %>%
  left_join(arth, by = c('ID' = 'SurveyFK')) %>%
  select(LocalDate, julianday, Code, Group, Length, Quantity) %>%
  filter(Group == 'caterpillar', Length >= 5) %>%
  distinct(julianday, Code) %>%
  count(julianday) %>%
  mutate(fracSurveys = 100*n/60)


# From sampling_effort_analysis.r ******************************************************

# PRAIRIE RIDGE
col1 = 'blueviolet'
col2 = rgb(140/255, 176/255, 122/255)
col3 = rgb(52/255, 212/255, 52/255)
col4 = rgb(0, 100/255, 0)



#PR.LEPL15.day = meanDensityByDay(amsurvey.pr, 
#                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
#                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
#                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 5, las = 1,
#                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillar Occurrence", xlab = 'Julian day',
#                                 main = 'Prairie Ridge Ecostation', col = col1)
# upon inspection, use jdRange = c(135, 194)

pdf('output/plots/PR_caterpillars_2016-2018.pdf', height = 4, width = 5)
par(tck = -.01, mar = c(4, 5, 2, 1), mgp = c(2.5, .5, 0), cex.lab = 1.5)

beg_jd = 134
end_jd = 205

PR.LEPL16.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 3, las = 1, xaxt = 'n',
                                 xlim = c(beg_jd, end_jd), ylim = c(0,25), ylab = "Caterpillar Frequency", xlab = 'Date',
                                 main = 'Prairie Ridge Ecostation', col = col2)
# upon inspection, use jdRange = c(132, 189)

PR.LEPL17.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2017, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 3, las = 1,
                                 xlim = c(beg_jd, end_jd), ylim = c(0,25), ylab = "Caterpillars", 
                                 main = '', col = col3)

points(PR2018vislepdata$julianday, PR2018vislepdata$fracSurveys, type = 'l', lwd = 4, col = col4)

legend("topleft", legend = 2016:2018, col = c(col2, col3, col4), text.col = c(col2, col3, col4), lwd = 3)
axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("May 15", "Jun 1", "Jun 15", "Jul1", "Jul15"), at = c(136, 153, 167, 183, 197), side = 1, line = .5)
dev.off()


# NCBG
beg_jd15 = 136
end_jd15 = 206
beg_jd16 = 147
end_jd16 = 194


pdf('output/plots/BG_caterpillars_2015-2018.pdf', height = 4, width = 5)
par(tck = -.01, mar = c(4, 5, 2, 1), mgp = c(2.5, .5, 0), cex.lab = 1.5)

BG.LEPL15.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 5, las = 1, xaxt = "n",
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillar Occurrence", xlab = 'Date',
                                 main = 'NC Botanical Garden', col = 'blueviolet')
# upon inspection, use jdRange = c(135, 194)

BG.LEPL16.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,22), ylab = "Caterpillars", 
                                 main = 'NC Botanical Garden', col = 'blue')
# upon inspection, use jdRange = c(132, 189)

BG.LEPL17.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2017, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,22), ylab = "Caterpillars", 
                                 main = '', col = 'skyblue')

points(BG2018vislepdata$julianday, BG2018vislepdata$fracSurveys, type = 'l', lwd = 5, col = 'lightpink')

legend("topright", legend = 2015:2018, col = c('blueviolet', 'blue', 'skyblue', 'lightpink'), lwd = 5)
axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("May 15", "Jun 1", "Jun 15", "Jul1", "Jul15"), at = c(136, 153, 167, 183, 197), side = 1, line = .5)

dev.off()


# UNC Campus

pdf('output/plots/UNC_caterpillars_2018.pdf', height = 4, width = 5)
par(tck = -.01, mar = c(4, 5, 2, 1), mgp = c(2.5, .5, 0), cex.lab = 1.5)

plot(UNC2018vislepdata$julianday, UNC2018vislepdata$fracSurveys, type = 'l', lwd = 4, col = col4,
     xaxt = 'n', ylab = 'Caterpillar Frequency', xlab = 'Date', main = 'UNC Campus', xlim = c(130, 200))

axis(1, at = c(136, 153, 167, 183, 197), labels = F, tck = -.02)
mtext(c("May 15", "Jun 1", "Jun 15", "Jul1", "Jul15"), at = c(136, 153, 167, 183, 197), side = 1, line = .5)
dev.off()



# Prairie Ridge by arthropod group
# Make sure datasets placed into function do not have minLength already subsetted out (?) need to check on this
meanDensityByDay2 = function(surveyData, # merged dataframe of surveys and orders tables
                            ordersToInclude = 'All',       # which arthropod orders to calculate density for (codes)
                            byTreeSpecies = FALSE, # do we want to calculate densities separately for each tree?
                            minLength = 0,         # minimum arthropod size to include 
                            inputSite,
                            inputYear,
                            jdRange = c(1,365),
                            outlierCount = 10000,
                            plot = F,
                            plotVar = 'meanDensity', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                            new = T,
                            color = 'black',
                            ...)                  

{
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  firstFilter = surveyData %>%
    filter(Name %in% inputSite, year %in% inputYear, 
           julianday >= jdRange[1], julianday <= jdRange[2])
  
  effortByDay = firstFilter %>%
    distinct(surveyID, julianday) %>%
    count(julianday)
  
  arthCount = firstFilter %>%
    filter(length >= minLength, 
           count < outlierCount, 
           arthCode %in% ordersToInclude) %>%
    group_by(julianday) %>%
    summarize(totalCount = sum(count, na.rm = T),
              totalBiomass = sum(biomass, na.rm = T),
              numSurveysGTzero = length(unique(surveyID[count > 0]))) %>% 
    right_join(effortByDay, by = 'julianday') %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, totalBiomass = 0, numSurveysGTzero = 0) %>%
    mutate(meanDensity = totalCount/n,
           meanBiomass = totalBiomass/n,
           fracSurveys = 100*numSurveysGTzero/n) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianday, arthCount[, plotVar], type = 'l', 
         col = color, ...)
  } else if (plot & new==F) {
    points(arthCount$julianday, arthCount[, plotVar], type = 'l', col = color, ...)
  }
  return(arthCount)
}




PR2018visspiderdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
  filter(Name == 'Prairie Ridge Ecostation', ObservationMethod == 'Visual') %>%
  mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
         julianday = yday(LocalDate)) %>%
  left_join(arth, by = c('ID' = 'SurveyFK')) %>%
  select(LocalDate, julianday, Code, Group, Length, Quantity) %>%
  filter(Group == 'spider', Length >= 5) %>%
  distinct(julianday, Code) %>%
  count(julianday) %>%
  mutate(fracSurveys = 100*n/60)
