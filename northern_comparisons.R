setwd("~/Desktop/Lab")
#all the number of leaves surveyed and when they were surveyed is the same across all years (4000 leaves every two weeks)
#beech and sugar maple, but viburnum and striped maple were only surveyed 1996-1997
#0s are not included in the dataset, so means aren't meaningful
#

#load libraries
library(dplyr)
#read in data
btbw = read.csv('HubbardBrookData/BTBW_data_Dryad.csv', header=T, stringsAsFactors = F)
dat = read.csv('HubbardBrookData/Copy of leps.csv', header=T, stringsAsFactors = F)
singer = read.csv("SingerData/SingerData.csv", stringsAsFactors = F)

#source data
setwd("~/Desktop/caterpillars-count-analysis")
source("tree_species_model.R")


dat$date = as.character(as.POSIXlt(dat$date, format = '%m/%d/%y'))
dat$year = as.numeric(substr(dat$date, 1, 4))
dat$week = floor(dat$yearday/7)+1
dat$tree.name = ifelse(dat$tree.spec == 1, "American beech",
                ifelse(dat$tree.spec == 2, "Sugar maple",
                ifelse(dat$tree.spec == 3, "Striped maple",
                ifelse(dat$tree.spec == 4, "Viburnum", NA))))


#remove data from 1994 because it had far more caterpillars than any other year
dat1 = dplyr::filter(dat, year!="1994")

#global means for normal years (beech and sugar maple) #are there the same number of beech and sugar maple trees? or is there metadata?
#same number of beech and sugar maple
sumbysp = dat1 %>% group_by(tree.name) %>% 
  dplyr::summarize(sum_count = sum(number.lep), biomass = sum(lepbio.mass.mg.)) 
hubbard_ranks = sumbysp[order(sumbysp$sum_count, decreasing = T),]
#subset dataset to only include 96/97 data to see if there is effect on beech and sugar maple amounts
sumbysp_67 = dat1 %>% filter(year %in% c("1996", "1997") & plot =="1") %>% group_by(tree.name) %>% 
  dplyr::summarize(total_count = sum(number.lep), biomass = sum(lepbio.mass.mg.)) 

#find out if each row is a unique survey -> nope
surveys = select(dat, plot, grid.letter, grid.number, date, tree.spec)
surveysct = data.frame(table(surveys)) %>% filter(Freq>0)

#Find patterns in Singer Data 
#include only trees without a treatment
singer_means = singer %>% filter(treatment == "unbagged") %>% group_by(hostplantspecies) %>% summarize(mean_cat_dens = mean(generalist.density.m2)) 
singer_ranks = singer_means[order(singer_means$mean_cat_dens, decreasing = T),]

#CC & Appalachian data
#add column with just caterpillar data
vis$cats_count = ifelse(vis$arthCode == "LEPL", vis$count, 0)

#group by unique surveys and summarize by caterpillar density
vis_norm = filter(vis, cats_count <= 5)
south_means = vis_norm %>% group_by(clean_plantSp) %>% dplyr::summarize(mean_cat_dens = mean(cats_count))
south_ranks = south_means[order(south_means$mean_cat_dens, decreasing = T),]
south_relevant = filter(south_ranks, clean_plantSp %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple", "Striped maple"))



