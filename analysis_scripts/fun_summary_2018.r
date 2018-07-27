library(dplyr)
library(lubridate)
library(wesanderson)

url = 'https://caterpillarscount.unc.edu/iuFYr1xREQOp2ioB5MHvnCTY39UHv2/'

today = strsplit(as.character(Sys.time()), " ")[[1]][1]

plant = read.csv(paste(url, today, "_Plant.csv", sep = ""), header = T, quote = '\"')
survey = read.csv(paste(url, today, "_Survey.csv", sep = ""), header = T, quote = '\"')
site = read.csv(paste(url, today, "_Site.csv", sep = ""), header = T, quote = '\"')
arth = read.csv(paste(url, today, "_ArthropodSighting.csv", sep = ""), header = T, quote = '\"')
user = read.csv(paste(url, today, "_User.csv", sep = ""), header = T, quote = '\"')


surv = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>% 
  
  select(LocalDate, ObservationMethod, WetLeaves, PlantSpecies, NumberOfLeaves, )
  data.frame()

arthdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
    left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
    filter(Name %in% c('Prairie Ridge Ecostation', 'NC Botanical Garden', 'UNC Chapel Hill Campus')) %>%
    mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
           julianday = yday(LocalDate),
           year = substring(LocalDate, 1, 4)) %>%
    left_join(arth, by = c('ID' = 'SurveyFK')) %>%
    filter(Length >= 5) %>% 
    select(Name, LocalDate, julianday, year, Code, Group, Length, Quantity, UserFKOfObserver)
    
arthcount = arthdata %>% count(Group)

arthOther = data.frame(group = arthcount$Group, group2 = c('ant', 'other', 'other', 'beetle', 'caterpillar',
                                                           'other', 'fly', 'grasshopper', 'leafhopper', 
                                                           'other', 'other', 'spider', 'truebugs', 'other'))

arthCols = data.frame(group2 = c('ant', 'beetle', 'caterpillar', 'fly', 'grasshopper', 'leafhopper',
                                 'other', 'spider', 'truebugs'),
                      col = wes_palette(9, name = "Zissou1", type = "continuous"))


pies = arthdata %>%
  left_join(arthOther, by = c('Group' = 'group')) %>%
  group_by(Name, group2) %>%
  summarize(total = sum(Quantity))

pdf('output/plots/pie_comparison.pdf', height = 3, width = 9)
par(mfrow = c(1, 3))
pie(pies$total[pies$Name == 'NC Botanical Garden'], labels = pies$group2[1:9], 
    main = 'NC Botanical Garden', col = arthcols)

pie(pies$total[pies$Name == 'Prairie Ridge Ecostation'], labels = pies$group2[1:9], 
    main = 'Prairie Ridge', col = arthcols)

pie(pies$total[pies$Name == 'UNC Chapel Hill Campus'], labels = pies$group2[1:9], 
    main = 'UNC', col = arthcols)
dev.off()

# Leps
lepdata = left_join(survey, plant, by = c('PlantFK' = 'ID')) %>%
  left_join(site[, c('ID', 'Name')], by = c('SiteFK' = 'ID')) %>%
  filter(Name %in% c('Prairie Ridge Ecostation', 'NC Botanical Garden', 'UNC Chapel Hill Campus')) %>%
  mutate(LocalDate = as.Date(as.character(LocalDate), format = "%Y-%m-%d"),
         julianday = yday(LocalDate)) %>%
  left_join(arth, by = c('ID' = 'SurveyFK')) %>%
  filter(Group == 'caterpillar') %>% 
  group_by(UserFKOfObserver) %>%
  summarize(tot = sum(Quantity))