#all the number of leaves surveyed and when they were surveyed is the same across all years (4000 leaves every two weeks)
#beech and sugar maple, but viburnum and striped maple were only surveyed 1996-1997
#0s are not included in the dataset, so means aren't meaningful

#load libraries
library(dplyr)

#read in data
btbw = read.csv('data/HubbardBrookData/BTBW_data_Dryad.csv', header=T, stringsAsFactors = F)
dat = read.csv('data/HubbardBrookData/Copy of leps.csv', header=T, stringsAsFactors = F)
singer = read.csv("data/SingerData.csv", stringsAsFactors = F)

#source data
source("tree_species_model.R")

#improve format/put tree species names into common names
dat$date = as.character(as.POSIXlt(dat$date, format = '%m/%d/%y'))
dat$year = as.numeric(substr(dat$date, 1, 4))
dat$week = floor(dat$yearday/7)+1
dat$tree.name = ifelse(dat$tree.spec == 1, "American beech",
                ifelse(dat$tree.spec == 2, "Sugar maple",
                ifelse(dat$tree.spec == 3, "Striped maple",
                ifelse(dat$tree.spec == 4, "Viburnum", NA))))
singer$hostplantspecies = gsub("Hammamelis virginiana", "Hamamelis virginiana", singer$hostplantspecies) #incorrect spelling
plant_codes1 = dplyr::rename(plant_codes, hostplantspecies = TreeSciName)
singer1 = singer %>% left_join(plant_codes1, by = "hostplantspecies") %>% select(-TreeCode, -Notes)

#remove data from 1994 (because it had far more caterpillars than any other year) & 1996 & 1997 b/c they only have 1 plot
goodyears = c("1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1995")
dat1 = dplyr::filter(dat, year %in% goodyears)

#number of sets of surveys conducted/year (says on website it should be 4-6 but this is not reflected in these counts) 
hubbard_dates = dat %>% dplyr::select(date, year, plot) %>% dplyr::distinct() 
plot1_dates = hubbard_dates %>% dplyr::filter(plot == 1) %>% dplyr::count(year) 
plot2_dates = hubbard_dates %>% dplyr::filter(plot == 2) %>% dplyr::count(year) 
plot3_dates = hubbard_dates %>% dplyr::filter(plot == 3) %>% dplyr::count(year) 
plot4_dates = hubbard_dates %>% dplyr::filter(plot == 4) %>% dplyr::count(year) 

#check out weird years/plots
two_92 = dat %>% dplyr::filter(year == 1992 & plot == 2)
three_89 = dat %>% dplyr::filter(year == 1989 & plot == 3) #only 6 dates, one is left over
four_89 = dat %>% dplyr::filter(year == 1989 & plot == 4)

unique_hub = data.frame(table(dat %>% dplyr::select(year, yearday, plot, count, grid.letter))) 
unique_hub1 = dplyr::filter(unique_hub, Freq > 0)

# calculate total # of leaves surveyed for each tree species over the course of all years we are looking at
unique_hub = dplyr::filter(data.frame(table(dat %>% dplyr:: select (year, plot, count))), Freq > 0)
numvisits = unique_hub %>% dplyr::count(year, plot)
numvisits$visits = numvisits$n*80 #number of visits in that plot in that year times the number of surveys/visit (4000 leaves surveyed/visit = 80 x 50 leaf surveys)
total_surveys = numvisits %>% dplyr::summarize(sum_visits = sum(visits)) %>% dplyr::filter(year %in% goodyears) %>% dplyr::summarize(sum(sum_visits)) 
total_surveys1 = 15040 #needs to be automated

#global means for normal years (beech and sugar maple) #are there the same number of beech and sugar maple trees? or is there metadata?
#same number of beech and sugar maple
hubbard_means = dat1 %>% dplyr::rename(ComName = tree.name) %>% group_by(ComName) %>% 
  dplyr::summarize(sum_count = sum(number.lep)/total_surveys1, biomass = sum(lepbio.mass.mg.)/total_surveys1) 
hubbard_ranks = hubbard_means[order(hubbard_means$sum_count, decreasing = T),]
hubbard_ranks_sel = hubbard_ranks %>% filter(ComName %in% c("Sugar maple", "American beech"))
hubbard_ranks_sel = cbind(hubbard_ranks = rownames(hubbard_ranks_sel), hubbard_ranks_sel)

#subset dataset to only include 96/97 data to see if there is effect on beech and sugar maple amounts
sumbysp_67 = dat1 %>% filter(year %in% c("1996", "1997") & plot =="1") %>% group_by(tree.name) %>% 
  dplyr::summarize(total_count = sum(number.lep), biomass = sum(lepbio.mass.mg.)) 

#find out if each row is a unique survey -> nope
surveys = select(dat, plot, grid.letter, grid.number, date, tree.spec)
surveysct = data.frame(table(surveys)) %>% filter(Freq>0)

#Find patterns in Singer Data 
#include only trees without a treatment

singer_means = singer1 %>% filter(treatment == "unbagged") %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(generalist.density.m2)) 
singer_means$ComName = gsub("Northern red oak", "Red oak", singer_means$ComName)
singer_ranks = singer_means[order(singer_means$mean_cat_dens, decreasing = T),]
singer_ranks = cbind(singer_ranks = rownames(singer_ranks), singer_ranks)
singer_ranks$ComName = gsub("Northern red oak", "Red oak", singer_ranks$ComName)

#CC & Appalachian data
#add column with just caterpillar data
vis$cats_count = ifelse(vis$arthCode == "LEPL", vis$count, 0)
vis_tri2$cats_count = ifelse(vis_tri2$arthCode == "LEPL", vis_tri2$count, 0)
vis_app$cats_count = ifelse(vis_app$arthCode == "LEPL", vis_app$count, 0)

# all southern surveys together group by unique surveys and summarize by caterpillar density
#vis_norm = filter(vis, cats_count <= 5) #use if you want to exclude big colonies
south_means = vis %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count))
south_ranks = south_means[order(south_means$mean_cat_dens, decreasing = T),]
south_relevant = filter(south_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple"))
south_relevant = cbind(south_ranks = rownames(south_relevant), south_relevant)

#triangle surveys group by unique surveys and summarize by caterpillar density
tri_means = vis_tri2 %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count))
tri_ranks = tri_means[order(tri_means$mean_cat_dens, decreasing = T),]
tri_relevant = data.frame(filter(tri_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple")))
tri_relevant = cbind(tri_ranks = rownames(tri_relevant), tri_relevant)
tri_relevant$tri_ranks = as.character(tri_relevant$tri_ranks)

#appalachian surveys broken down into middle appalachia (VA) and southern appalachia(NC, TN, SC, GA)
va_means = vis_app %>% filter(grepl("88", site)) %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count))
va_ranks = va_means[order(va_means$mean_cat_dens, decreasing = T),]
va_relevant = data.frame(filter(va_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple")))
va_relevant = cbind(va_ranks = rownames(va_relevant), va_relevant)
va_relevant$va_ranks = as.character(va_relevant$va_ranks)

sa_means = vis_app %>% filter(!grepl("88", site)) %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count))
sa_ranks = sa_means[order(sa_means$mean_cat_dens, decreasing = T),]
sa_relevant = filter(sa_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple"))
sa_relevant = cbind(sa_ranks = rownames(sa_relevant), sa_relevant)
sa_relevant$sa_ranks = as.character(sa_relevant$sa_ranks)

# Visualize rankings #double check rankings for hubbard brook
# convert rownames (ranking numbers) to a column (y-axis of figure)
# merge together the 3 data sets by tree species
all_ranks = south_relevant %>% left_join(singer_ranks, by = "ComName") %>% left_join(hubbard_ranks_sel, by = "ComName") %>% left_join(tri_relevant,  by = "ComName") %>% left_join(va_relevant,  by = "ComName") %>% left_join(sa_relevant,  by = "ComName")


#create columns that allow for numeric plotting
all_ranks$singer_cat = 1
all_ranks$tri_cat = 2
all_ranks$va_cat = 3 
all_ranks$sa_cat = 4
all_ranks$hubbard_cat = 5
plot(all_ranks$singer_cat, all_ranks$singer_ranks, xlim = c(.2,3.6), ylim = c(0,6), type = "n", xlab = "Location", ylab = "Relative Rank of Caterpillar Abund.") # needs work
  text(all_ranks$singer_cat, as.numeric(all_ranks$singer_ranks)-3, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$tri_cat, all_ranks$tri_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$va_cat, all_ranks$va_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$sa_cat, all_ranks$sa_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$hubbard_cat, all_ranks$hubbard_ranks, labels = all_ranks$ComName, cex = .6)

#--------------------------variance partitioning analysis of tree sp and site on caterpillar density----------------
means1 = merge(va_means, sa_means, by = "ComName") 
means1_r = rename(means1,va_cat = mean_cat_dens.x, sa_cat = mean_cat_dens.y)
means2 = merge(means1_r, tri_means, by = "ComName", all.x = T)
means2_r = rename(means2, tri_cat = mean_cat_dens)
means3 = merge(means2_r, singer_means, all.x = T)
means3_r = rename(means3, singer_cat = mean_cat_dens)
means4 = merge(means3_r, hubbard_means, all.x = T)
all_means = means4 %>% rename(hubbard_cat = sum_count) %>% select(-biomass)

all_means_site = dplyr::gather(all_means, site,  mean_cat, va_cat, sa_cat, tri_cat, singer_cat, hubbard_cat)

#linear models
lm.site = lm(mean_cat ~ site, data = all_means_site)
lm.species = lm(mean_cat ~ ComName, data = all_means_site) #what does it mean that the adjusted r2 is nothing?
lm.site.species = lm(mean_cat ~ site + ComName, data = all_means_site)

#variance partitioning
#a = variance uniquely explained by site
#b = variance explained by site and species together
#c = variance uniquely explained by site and species
#d = variance explained by neither


a = summary(lm.site.species)$r.squared - summary(lm.species)$r.squared
b = summary(lm.site)$r.squared - a
c = summary(lm.site.species)$r.squared - summary(lm.site)$r.squared
d = 1-summary(lm.site.species)$r.squared



