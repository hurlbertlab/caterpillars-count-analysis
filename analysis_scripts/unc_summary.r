# Quick analysis of UNC Chapel Hill campus data

library(dplyr)


plants = read.csv('data/arthropods/Plant.csv', header = T)
surveys = read.csv('data/arthropods/Survey.csv', header = T)
arths = read.csv('data/arthropods/ArthropodSighting.csv', header = T)
sites = read.csv('data/arthropods/Site.csv', header = T)

# UNC Chapel Hill Campus is SiteFK == 60

unc = surveys %>% 
  filter(PlantFK %in% plants$ID[plants$SiteFK == 60]) %>%
  left_join(arths, by = c('ID' = 'SurveyFK')) %>%
  left_join(plants, by = c('PlantFK' = 'ID')) %>%
  select(ID, Circle, Code, Species, LocalDate, ObservationMethod, 
         AverageLeafLength, Group, Length, Quantity, Notes.x, Notes.y)

arths = unique(unc$Group[!is.na(unc$Group)])
arthsCols = data.frame(arth = arths,
                       color = rainbow(length(arths)))
arthsCols$color = as.character(arthsCols$color)

plantCols = data.frame(Plant = arthsByPlant$Species[1:10],
                       plantCol = rainbow(10))
plantCols$plantCol = as.character(plantCols$plantCol)

plantCount = count(unc, Species) %>%
  filter(n > 0) %>% 
  arrange(desc(n))

arthCount = count(unc, Group) %>%
  filter(n > 0) %>%
  arrange(desc(n))



arthsByPlant = group_by(unc, Species) %>%
  summarize(totalDensity = sum(Quantity, na.rm = T),
            numSurveys = length(ID)) %>%
  mutate(arthPerSurvey = totalDensity/numSurveys) %>%
  arrange(desc(arthPerSurvey))
  
arthGroupsByPlant = group_by(unc, Species, Group) %>%
  summarize(totalDensity = sum(Quantity, na.rm = T)) %>%
  left_join(arthsByPlant[, c('Species', 'numSurveys')]) %>%
  mutate(arthPerSurvey = totalDensity/numSurveys) %>%
  left_join(arthsCols, by = c('Group' = 'arth')) %>%
  left_join(plantCols, by = c('Species' = 'Plant')) %>%
  arrange(desc(arthPerSurvey))


cats = arthGroupsByPlant %>%
  filter(Group == 'caterpillar')

sugmap = filter(arthGroupsByPlant, Species == 'Sugar maple') %>%
  filter(!is.na(Group))

blkchr = filter(arthGroupsByPlant, Species == 'Black cherry') %>%
  filter(!is.na(Group))


# Plots
pdf('output/plots/unc_arthDensity_byPlant.pdf', height = 5, width = 6)
par(mar = c(6, 4, 1, 1))
barplot(arthsByPlant$arthPerSurvey[1:10], names.arg = arthsByPlant$Species[1:10], 
        las = 2, col = rainbow(10))
dev.off()

pdf('output/plots/unc_pies.pdf', height = 6, width = 6)
par(mfrow = c(2, 2))
pie(sugmap$totalDensity, col = sugmap$color, labels = sugmap$Group, cex = .5)
pie(blkchr$totalDensity, col = blkchr$color, labels = blkchr$Group, cex = .5)

pie(cats$totalDensity, col = cats$plantCol, labels = cats$Species, cex = .5)
