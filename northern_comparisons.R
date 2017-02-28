#all the number of leaves surveyed and when they were surveyed is the same across all years (4000 leaves every two weeks)
#beech and sugar maple, but viburnum and striped maple were only surveyed 1996-1997
#0s are not included in the dataset, so means aren't meaningful

#load libraries
library(dplyr)

#read in data
btbw = read.csv('data/HubbardBrookData/BTBW_data_Dryad.csv', header=T, stringsAsFactors = F)
dat = read.csv('data/HubbardBrookData/Copy of leps.csv', header=T, stringsAsFactors = F)
singer = read.csv("data/SingerData.csv", stringsAsFactors = F)
temperature = read.csv("data/prism_temp_northerncomp.csv")
precipitation = read.csv("data/prism_precip_northerncomp.csv")

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
singer1$ComName = gsub("Northern red oak", "Red oak", singer1$ComName)

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

# calculate total # of leaves surveyed for each tree species over the course of all years we are looking at
unique_hub = dplyr::filter(data.frame(table(dat %>% dplyr:: select (year, plot, count))), Freq > 0)
numvisits = unique_hub %>% dplyr::count(year, plot)
numvisits$visits = numvisits$n*80 #number of visits in that plot in that year times the number of surveys/visit (4000 leaves surveyed/visit = 80 x 50 leaf surveys)
total_surveys = numvisits %>% dplyr::summarize(sum_visits = sum(visits)) %>% dplyr::filter(year %in% goodyears) %>% dplyr::summarize(sum(sum_visits)) 
total_surveys1 = 15040 #needs to be automated

#global means for normal years (beech and sugar maple) #are there the same number of beech and sugar maple trees? or is there metadata?
#same number of beech and sugar maple
hubbard_means = dat1 %>% dplyr::rename(ComName = tree.name) %>% group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = sum(number.lep)/total_surveys1, biomass = sum(lepbio.mass.mg.)/total_surveys1) 
hubbard_ranks = hubbard_means[order(hubbard_means$sum_count, decreasing = T),]
hubbard_ranks_sel = hubbard_ranks %>% filter(ComName %in% c("Sugar maple", "American beech"))
hubbard_ranks_sel = cbind(hubbard_ranks = rownames(hubbard_ranks_sel), hubbard_ranks_sel)

#hubbard means by site
hub_means_sites = dat1 %>% dplyr::rename(ComName = tree.name) %>% group_by(ComName, plot) %>% 
  dplyr::summarize(mean_cat_dens = sum(number.lep)/total_surveys1) 

#subset dataset to only include 96/97 data to see if there is effect on beech and sugar maple amounts
sumbysp_67 = dat1 %>% filter(year %in% c("1996", "1997") & plot =="1") %>% group_by(tree.name) %>% 
  dplyr::summarize(total_count = sum(number.lep), biomass = sum(lepbio.mass.mg.)) 

#Find patterns in Singer Data 
#include only trees without a treatment
singer_means = singer1 %>% filter(treatment == "unbagged") %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(generalist.density.m2)) 
singer_ranks = singer_means[order(singer_means$mean_cat_dens, decreasing = T),]
singer_ranks = cbind(singer_ranks = rownames(singer_ranks), singer_ranks)
singer_ranks$ComName = gsub("Northern red oak", "Red oak", singer_ranks$ComName)

#singer means by site 
singer_means_sites = singer1 %>% filter(treatment == "unbagged") %>% group_by(ComName, site) %>% dplyr::summarize(mean_cat_dens = mean(generalist.density.m2)) 

#CC & Appalachian data
#add column with just caterpillar data
vis_tri2$cats_count = ifelse(vis_tri2$arthCode == "LEPL", vis_tri2$count, 0)
vis_app$cats_count = ifelse(vis_app$arthCode == "LEPL", vis_app$count, 0)

#add column with caterpillars > 10
vis_tri2$cats_count_10 = ifelse(vis_tri2$arthCode == "LEPL" & vis_tri2$length >= 10, vis_tri2$count, 0)
vis_app$cats_count_10 = ifelse(vis_app$arthCode == "LEPL" & vis_app$length >= 10, vis_app$count, 0) # this is just spitting out the same result as w/o the length

# all southern surveys together group by unique surveys and summarize by caterpillar density
vis_all = rbind(vis_tri2, vis_app)
south_means = vis_all %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))
south_ranks = south_means[order(south_means$mean_cat_dens, decreasing = T),]
south_relevant = filter(south_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple"))
south_relevant = cbind(south_ranks = rownames(south_relevant), south_relevant)

#triangle surveys group by unique surveys and summarize by caterpillar density
tri_means = vis_tri2 %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))
tri_ranks = tri_means[order(tri_means$mean_cat_dens, decreasing = T),]
tri_relevant = data.frame(filter(tri_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple")))
tri_relevant = cbind(tri_ranks = rownames(tri_relevant), tri_relevant)
tri_relevant$tri_ranks = as.character(tri_relevant$tri_ranks)
tri_means_sites = vis_tri2 %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName, site) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))


#appalachian surveys broken down into middle appalachia (VA) and southern appalachia(NC, TN, SC, GA)
va_sites = vis_app %>% filter(grepl("88", site))
uniq_va = unique(va_sites$site)
va_means = va_sites %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))
va_ranks = va_means[order(va_means$mean_cat_dens, decreasing = T),]
va_relevant = data.frame(filter(va_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple")))
va_relevant = cbind(va_ranks = rownames(va_relevant), va_relevant)
va_relevant$va_ranks = as.character(va_relevant$va_ranks)
va_means_sites = va_sites %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName, site) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))

sa_sites = vis_app %>% filter(!grepl("88", site))
uniq_sa = unique(sa_sites$site)
sa_means = sa_sites %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))
sa_ranks = sa_means[order(sa_means$mean_cat_dens, decreasing = T),]
sa_relevant = filter(sa_ranks, ComName %in% c("Red oak","Red maple", "Witch hazel", "Sweet birch", "American beech", "Sugar maple"))
sa_relevant = cbind(sa_ranks = rownames(sa_relevant), sa_relevant)
sa_relevant$sa_ranks = as.character(sa_relevant$sa_ranks)
sa_means_sites = sa_sites %>% dplyr::rename(ComName = clean_plantSp) %>% group_by(ComName, site) %>% dplyr::summarize(mean_cat_dens = mean(cats_count_10))


# Visualize rankings #double check rankings for hubbard brook
# convert rownames (ranking numbers) to a column (y-axis of figure)
# merge together the 3 data sets by tree species
all_ranks = south_relevant %>% left_join(singer_ranks, by = "ComName") %>% left_join(hubbard_ranks_sel, by = "ComName") %>% left_join(tri_relevant,  by = "ComName") %>% left_join(va_relevant,  by = "ComName") %>% left_join(sa_relevant,  by = "ComName")


#create columns that allow for numeric plotting
all_ranks$sing = 1
all_ranks$tri = 2
all_ranks$va = 3 
all_ranks$sa = 4
all_ranks$hub = 5
plot(all_ranks$sing, all_ranks$singer_ranks, xlim = c(.2,3.6), ylim = c(0,6), type = "n", xlab = "Location", ylab = "Relative Rank of Caterpillar Abund.") # needs work
  text(all_ranks$sing, as.numeric(all_ranks$singer_ranks)-3, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$tri, all_ranks$tri_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$va, all_ranks$va_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$sa, all_ranks$sa_ranks, labels = all_ranks$ComName, cex = .6)
  text(all_ranks$hub, all_ranks$hubbard_ranks, labels = all_ranks$ComName, cex = .6)

#--------------------------variance partitioning analysis of tree sp and site on caterpillar density----------------
#spread
#merge means from all regions into one df
#means1 = merge(va_means_sites, sa_means_sites, by = "ComName") 
#means2 = merge(means1_r, tri_means_sites, by = "ComName", all.x = T)
#means2_r = rename(means2, tri = mean_cat_dens)
#means3 = merge(means2_r, singer_means_sites, all.x = T)
#means3_r = rename(means3, sing = mean_cat_dens)
#means4 = merge(means3_r, hubbard_means, all.x = T)
#all_means = means4 %>% rename(hub = sum_count) %>% dplyr::select(-biomass)
#add column to each regional dataframe to identify after the bind
va_means_sites$region = "va"
sa_means_sites$region = "sa"
tri_means_sites$region = "tri"
singer_means_sites$region = "sing"
hub_means_sites$region = "hub"

#create one dataframe with all densities
hub_means_sites = rename(hub_means_sites, site = plot)
hub_means_sites$site = as.character(hub_means_sites$site)
va_sa = rbind(va_means_sites, sa_means_sites)
va_sa_tri = rbind(va_sa, tri_means_sites)
va_sa_tri$site = as.character(va_sa_tri$site)
va_sa_tri_singer = rbind(va_sa_tri, singer_means_sites)
all_regions = rbind(va_sa_tri, hub_means_sites)



#remove meaningless "UNID" tree sp
all_means1= filter(all_means, ComName != "UNID")

#gather into 3 columns (ComName, site, caterpillar density)
all_means_region = tidyr::gather(all_means1, region,  mean_cat, va, sa, tri, sing, hub)

#------------summarize climate data by region----------------------------------------------------------------
clim = merge(precipitation, temperature, by= "X")
climate = rename(clim, site_region=X) #no location data for sites for brook & singer, sites close to each other & at low elevation, so same temp & precip used
climate$region = ifelse(climate$site_region %in% uniq_va, "va",
                    ifelse(climate$site_region %in% uniq_sa, "sa",
                           ifelse(climate$site_region %in% c("Botanical Garden", "Prairie Ridge"), "tri",
                                  ifelse(climate$site_region == "Hubbard Brook", "hub",
                                         ifelse(climate$site_region == "Singer", "sing", NA)))))
clim_summary = data.frame(climate %>% group_by(region) %>% summarize(ppt_may = mean(ppt_normals_may), ppt_june = mean(ppt_normals_june), ppt_july = mean(ppt_normals_july),
                                                     temp_may = mean(temp_normals_may), temp_june = mean(temp_normals_june), temp_july = mean(temp_normals_july)))
# merge climate data with caterpillar density data for variance partitioning
region_complete = merge(all_means_region, clim_summary, by = "region", all.x = T)

region_complete$total_precip = region_complete$ppt_may + region_complete$ppt_june + region_complete$ppt_july

#linear models
lm.region = lm(mean_cat ~ region, data = region_complete)
lm.species = lm(mean_cat ~ ComName, data = region_complete) #what does it mean that the adjusted r2 is nothing?
lm.precip = lm(mean_cat ~ total_precip, data = region_complete)
lm.region.species = lm(mean_cat ~ region + ComName, data = region_complete)
lm.region.precip = lm(mean_cat ~ region + total_precip, data = region_complete )
lm.species.precip = lm(mean_cat ~ ComName + total_precip, data = region_complete)
lm.all = lm(mean_cat ~ region + ComName + total_precip, data = region_complete)

#potential variables currently not included
lm.maytemp = lm(mean_cat ~ temp_may, data = region_complete)
lm.junetemp = lm(mean_cat ~ temp_june, data = region_complete)
lm.julytemp = lm(mean_cat ~ temp_july, data = region_complete)

#variance partitioning
#a(1) = variance uniquely explained by region
#b(1) = variance uniquely explained by species
#c(1) = variance uniquely explained by net precipitation may-july
#d(1) = variance explained by both region and species
#e(1) = variance explained by both region and precip
#f(1) = variance explained by both species and precip
#g(1) = variance explained by none of the 3
#h(1) = variance explained by all of the 3

a = summary(lm.all)$r.squared - summary(lm.species.precip)$r.squared
b = summary(lm.all)$r.squared - summary(lm.region.precip)$r.squared
c = summary(lm.all)$r.squared - summary(lm.region.species)$r.squared
dh = summary(lm.region)$r.squared + summary(lm.species)$r.squared - summary(lm.region.species)$r.squared
eh = summary(lm.region)$r.squared + summary(lm.precip)$r.squared - summary(lm.region.precip)$r.squared
hf = summary(lm.species)$r.squared + summary(lm.precip)$r.squared - summary(lm.species.precip)$r.squared
e = summary(lm.region)$r.squared - dh - a
f = summary(lm.species)$r.squared -dh - b
d = summary(lm.region.species)$r.squared - hf - e - c - b
h = hf - f
g = 1 - summary(lm.all)$r.squared



#how to do this... you have 5 site



