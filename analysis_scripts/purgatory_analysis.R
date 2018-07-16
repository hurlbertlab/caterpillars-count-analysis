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



# From sampling_effort_analysis.r ******************************************************

# PRAIRIE RIDGE

PR.LEPL15.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillar Occurrence", xlab = 'Julian day',
                                 main = 'Prairie Ridge Ecostation', col = 'blueviolet')
# upon inspection, use jdRange = c(135, 194)

PR.LEPL16.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '', col = 'blue')
# upon inspection, use jdRange = c(132, 189)

PR.LEPL17.day = meanDensityByDay(amsurvey.pr, 
                                 ordersToInclude = "LEPL", inputYear = 2017, inputSite = 117, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '', col = 'skyblue')

points(PR2018vislepdata$julianday, PR2018vislepdata$fracSurveys, type = 'l', lwd = 5, col = 'lightpink')

legend("topright", legend = 2015:2018, col = c('blueviolet', 'blue', 'skyblue', 'lightpink'), lwd = 5)


# NCBG

BG.LEPL15.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2015, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = T, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd15, end_jd15), ylim = c(0,30), ylab = "Caterpillar Occurrence", xlab = 'Julian day',
                                 main = 'NC Botanical Garden', col = 'blueviolet')
# upon inspection, use jdRange = c(135, 194)

BG.LEPL16.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2016, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '', col = 'blue')
# upon inspection, use jdRange = c(132, 189)

BG.LEPL17.day = meanDensityByDay(amsurvey.bg, 
                                 ordersToInclude = "LEPL", inputYear = 2017, inputSite = 8892356, 
                                 jdRange = c(1,365), outlierCount = 10000, plot = T, 
                                 plotVar = 'fracSurveys', new = F, minLength = 5, lwd = 5, las = 1,
                                 xlim = c(beg_jd16, end_jd16), ylim = c(0,30), ylab = "Caterpillars", 
                                 main = '', col = 'skyblue')

points(BG2018vislepdata$julianday, BG2018vislepdata$fracSurveys, type = 'l', lwd = 5, col = 'lightpink')

legend("topright", legend = 2015:2018, col = c('blueviolet', 'blue', 'skyblue', 'lightpink'), lwd = 5)
