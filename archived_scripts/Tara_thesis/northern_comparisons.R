## Data cleaning and Variance Partitioning Analyses for Regional Comparisons Section of Tara's Thesis
# Thesis version
# For Hubbard Brook:
# All the number of leaves surveyed and when they were surveyed is the same across all years (4000 leaves every two weeks)
# Beech and sugar maple, but viburnum and striped maple were only surveyed 1996-1997
# 0s are not included in the dataset, so means aren't meaningful

#load libraries
library(dplyr)

#read in data
btbw = read.csv('data/birds/BTBW_data_Dryad.csv', header=T, stringsAsFactors = F)
dat = read.csv('data/arthropods/caterpillars_hubbardbrook.csv', header=T, stringsAsFactors = F)
singer = read.csv("data/arthropods/SingerData.csv", stringsAsFactors = F)
temperature = read.csv("data/environmental/prism_temp_northerncomp.csv")
precipitation = read.csv("data/environmental/prism_precip_northerncomp.csv")

#source data
source("archived_scripts/Tara_thesis/tree_species_model.R")
source("archived_scripts/Tara_thesis/data_analysis.R")

#improve format/put tree species names into common names
dat$date = as.character(as.POSIXlt(dat$date, format = '%m/%d/%y'))
dat$year = as.numeric(substr(dat$date, 1, 4))
dat$week = floor(dat$yearday/7)+1
dat$tree.name = ifelse(dat$tree.spec == 1, "american beech", # as outlined in hubbard brook metadata
                       ifelse(dat$tree.spec == 2, "sugar maple",
                              ifelse(dat$tree.spec == 3, "striped maple",
                                     ifelse(dat$tree.spec == 4, "viburnum", NA))))
dat$plot = gsub(1, "Hubbard Brook", dat$plot)
dat$plot = gsub(2, "Moosilauke", dat$plot)
dat$plot = gsub(3, "Russell", dat$plot)
dat$plot = gsub(4, "Stinson", dat$plot)
singer$hostplantspecies = gsub("Hammamelis virginiana", "Hamamelis virginiana", singer$hostplantspecies) #incorrect spelling
plant_codes1 = dplyr::rename(plant_codes, hostplantspecies = TreeSciName)
singer1 = singer %>% 
  left_join(plant_codes1, by = "hostplantspecies") %>% 
  dplyr::select(-TreeCode, -Notes)
singer1$ComName = gsub("Northern red oak", "Red oak", singer1$ComName)

#remove data from 1994 (because it had far more caterpillars than any other year) & 1996 & 1997 b/c they only have 1 plot
goodyears = c("1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1995")
dat1 = dplyr::filter(dat, year %in% goodyears)

#number of sets of surveys conducted/year (says on website it should be 4-6 but this is not reflected in these counts) 
hubbard_dates = dat %>% 
  dplyr::select(date, year, plot) %>% 
  dplyr::distinct() 
plot1_dates = hubbard_dates %>% 
  dplyr::filter(plot == "Hubbard Brook") %>% 
  dplyr::count(year) 
plot2_dates = hubbard_dates %>% 
  dplyr::filter(plot == "Moosilauke") %>% 
  dplyr::count(year) 
plot3_dates = hubbard_dates %>% 
  dplyr::filter(plot == "Russell") %>% 
  dplyr::count(year) 
plot4_dates = hubbard_dates %>% 
  dplyr::filter(plot == "Stinson") %>% 
  dplyr::count(year) 

#check out weird years/plots
two_92 = dat %>% 
  dplyr::filter(year == 1992 & plot == "Moosilauke")
three_89 = dat %>% 
  dplyr::filter(year == 1989 & plot == "Russell") #only 6 dates, one is left over
four_89 = dat %>% 
  dplyr::filter(year == 1989 & plot == "Stinson")

# calculate total # of leaves surveyed for each tree species over the course of all years we are looking at
unique_hub = dplyr::filter(data.frame(table(dat %>% dplyr:: select (year, plot, count))), Freq > 0)
numvisits = unique_hub %>% 
  dplyr::count(year, plot)
numvisits$visits = numvisits$n*80 #number of visits in that plot in that year times the number of surveys/visit (4000 leaves surveyed/visit = 80 x 50 leaf surveys)
total_surveys = numvisits %>% 
  dplyr::summarize(sum_visits = sum(visits)) %>% 
  dplyr::filter(year %in% goodyears) %>% 
  dplyr::summarize(sum(sum_visits)) 
total_surveys1 = 15040 #needs to be automated
surveyspersite=total_surveys1/4

#global means for normal years (beech and sugar maple) #are there the same number of beech and sugar maple trees? or is there metadata?
#same number of beech and sugar maple
#rename hubbard sites more intuitively
hubbard_means = dat1 %>% 
  dplyr::rename(ComName = tree.name) %>% 
  group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = sum(number.lep)/total_surveys1, biomass = sum(lepbio.mass.mg.)/total_surveys1) 
hubbard_ranks = hubbard_means[order(hubbard_means$mean_cat_dens, decreasing = T),]
hubbard_ranks_sel = hubbard_ranks %>% 
  filter(ComName %in% c("sugar maple", "american beech"))
hubbard_ranks_sel = cbind(hubbard_ranks = rownames(hubbard_ranks_sel), hubbard_ranks_sel)

#hubbard means by site
hub_means_sites = data.frame(dat1 %>% dplyr::rename(ComName = tree.name, site=plot) %>% 
                               group_by(ComName, site) %>% 
                               dplyr::summarize(mean_cat_dens = sum(number.lep)/(surveyspersite))) 


#subset dataset to only include 96/97 data to see if there is effect on beech and sugar maple amounts
sumbysp_67 = dat1 %>% 
  filter(year %in% c("1996", "1997") & plot =="1") %>% 
  group_by(tree.name) %>% 
  dplyr::summarize(total_count = sum(number.lep), biomass = sum(lepbio.mass.mg.)) 

#Find patterns in Singer Data 
#include only trees without a treatment
singer_means = singer1 %>% 
  filter(treatment == "unbagged") %>% 
  group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = mean(X.generalists*50/numberlvs)) #calculate number of caterpillars that would appear on a 50 leaf count (x.generalists is # of caterpillars over total leaf area (estimated by avg leaf area * number of leaves))
singer_uniqsurv_sites = singer1 %>%
                        filter(treatment == "unbagged") %>%  #although singer already is in a unique survey format, the surveys have to be normalized to what they would be if a 50 leaf survey were being conducted
                        group_by(date, hostID) %>%
                        dplyr::summarize(mean_cat_dens = mean(X.generalists*50/numberlvs))
  
singer_ranks = singer_means[order(singer_means$mean_cat_dens, decreasing = T),]
singer_ranks = cbind(singer_ranks = rownames(singer_ranks), singer_ranks)
singer_ranks$ComName = gsub("northern red oak", "red oak", singer_ranks$ComName)

#singer means by site 
singer_means_sites = data.frame(singer1 %>% filter(treatment == "unbagged") %>% 
                                  group_by(ComName, site) %>% 
                                  summarize(mean_cat_dens = mean(X.generalists*50/numberlvs))) 

#CC & Appalachian data
#add column with just caterpillar data
vis_tri$cats_count = ifelse(vis_tri$arthCode == "LEPL", vis_tri$count, 0)
vis_app$cats_count = ifelse(vis_app$arthCode == "LEPL", vis_app$count, 0)

#add column with caterpillars > 10
vis_tri$cats_count_10 = ifelse(vis_tri$arthCode == "LEPL" & vis_tri$length >= 10, vis_tri$count, 0)
vis_app$cats_count_10 = ifelse(vis_app$arthCode == "LEPL" & vis_app$length >= 10, vis_app$count, 0) # this is just spitting out the same result as w/o the length

# all southern surveys together 
vis_all = rbind(vis_tri, vis_app)
south_means = vis_all %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
south_ranks = south_means[order(south_means$mean_cat_dens, decreasing = T),]
south_relevant = filter(south_ranks, ComName %in% c("red oak","red maple", "witch hazel", "sweet birch", "american beech", "sugar maple"))
south_relevant = cbind(south_ranks = rownames(south_relevant), south_relevant)

# triangle surveys group by unique surveys and summarize by caterpillar density
tri_means = vis_tri %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
tri_ranks = tri_means[order(tri_means$mean_cat_dens, decreasing = T),]
tri_relevant = data.frame(filter(tri_ranks, ComName %in% c("red oak","red maple", "witch hazel", "sweet birch", "american beech", "sugar maple")))
tri_relevant = cbind(tri_ranks = rownames(tri_relevant), tri_relevant)
tri_relevant$tri_ranks = as.character(tri_relevant$tri_ranks)
tri_means_sites = vis_tri %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName, site) %>%
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
tri_uniqsurv_sites= vis_tri %>% 
                    group_by(site, survey, circle, date) %>%
                    summarise(sum(cats_count_10))
total_caterpillars_12 <- (dplyr::summarise(grouped_caterpillars_12, sum(Abundance))) 


## Appalachian surveys broken down into middle appalachia (VA) and southern appalachia (NC, TN, SC, GA)
# filter to site/tree sp combos with at least 10 surveys to reduce uncertainty
#surveys_app$site_tree = paste0(surveys_app$site, surveys_app$realPlantSp)
#vis_app$site_tree = paste0(vis_app$site, vis_app$tree)

va_sites = vis_app %>% 
  filter(grepl("88", site)) #88 is the BBS state code for VA
uniq_va = unique(va_sites$site)
va_means = va_sites %>%
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName)  %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
va_ranks = va_means[order(va_means$mean_cat_dens, decreasing = T),]
va_relevant = data.frame(filter(va_ranks, ComName %in% c("red oak","red maple", "witch hazel", "sweet birch", "american beech", "sugar maple")))
va_relevant = cbind(va_ranks = rownames(va_relevant), va_relevant)
va_relevant$va_ranks = as.character(va_relevant$va_ranks)
va_means_sites = va_sites %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName, site) %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
va_uniqsurv_sites= va_sites %>%     #caterpillars greater than 10 mm per unique survey  
  group_by(site, survey, circle, date) %>%
  summarise(sum_by_leaf = sum(cats_count_10))

sa_sites = vis_app %>% filter(!grepl("88", site)) #all BBS sites excluding those in VA
uniq_sa = unique(sa_sites$site)
sa_means = sa_sites %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName) %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
sa_ranks = sa_means[order(sa_means$mean_cat_dens, decreasing = T),]
sa_relevant = filter(sa_ranks, ComName %in% c("red oak","red maple", "witch hazel", "sweet birch", "american beech", "sugar maple"))
sa_relevant = cbind(sa_ranks = rownames(sa_relevant), sa_relevant)
sa_relevant$sa_ranks = as.character(sa_relevant$sa_ranks)
sa_means_sites = sa_sites %>% 
  dplyr::rename(ComName = realPlantSp) %>% 
  group_by(ComName, site) %>% 
  dplyr::summarize(mean_cat_dens = mean(cats_count_10))
sa_uniqsurv_sites= sa_sites %>%     #caterpillars greater than 10 mm per unique survey  
  group_by(site, survey, circle, date) %>%
  summarise(sum(cats_count_10))


# Visualize rankings #double check rankings for hubbard brook
# convert rownames (ranking numbers) to a column (y-axis of figure)
# merge together the 3 data sets by tree species
all_ranks = south_relevant %>% 
  left_join(singer_ranks, by = "ComName") %>% 
  left_join(hubbard_ranks_sel, by = "ComName") %>%
  left_join(tri_relevant,  by = "ComName") %>% 
  left_join(va_relevant,  by = "ComName") %>% 
  left_join(sa_relevant,  by = "ComName")


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
#add column to each regional dataframe to identify after the bind
va_means_sites$region = "va"
sa_means_sites$region = "sa"
tri_means_sites$region = "tri"
singer_means_sites$region = "sing"
hub_means_sites$region = "hub"

#create one dataframe with all densities
va_sa = rbind(va_means_sites, sa_means_sites)
va_sa_tri = rbind(va_sa, tri_means_sites)
va_sa_tri$site = as.character(va_sa_tri$site)
va_sa_tri = data.frame(va_sa_tri)
va_sa_tri_singer = rbind(va_sa_tri, singer_means_sites)
all_sites = data.frame(rbind(va_sa_tri_singer, hub_means_sites))

#remove meaningless "UNID" tree sp
all_sites = filter(all_sites, ComName != "UNID")

#change site names for PR and NCBG to match those in the climate dataframe
all_sites$site = gsub(117, "Prairie Ridge", all_sites$site) 
all_sites$site = gsub("6303Prairie Ridge", 6303117, all_sites$site)
all_sites$site = gsub(8892356, "Botanical Garden", all_sites$site)


##------------summarize climate data by site/region----------------##
clim = merge(precipitation, temperature, by= "X")
climate = dplyr::rename(clim, site=X) #no location data for sites for brook & singer, sites close to each other & at low elevation, so same temp & precip used
clim_summary = climate %>%
  group_by(site) %>%
  summarize(annual_ppt = sum(ppt_normals_1, ppt_normals_2, ppt_normals_3, ppt_normals_4, ppt_normals_5, ppt_normals_6, ppt_normals_7, ppt_normals_8, ppt_normals_9, ppt_normals_10, ppt_normals_11, ppt_normals_12),
            avg_summer_tmp = mean(temp_normals_may, temp_normals_june, temp_normals_july)) %>%
  data.frame() 
clim_summary$region = ifelse(climate$site %in% uniq_va, "va",
                             ifelse(climate$site %in% uniq_sa, "sa",
                                    ifelse(climate$site %in% c("Botanical Garden", "Prairie Ridge"), "tri",
                                           ifelse(climate$site %in% c("Hubbard Brook", "Moosilauke", "Russell", "Stinson"), "hub",
                                                  ifelse(climate$site %in% c("C", "H", "M"), "sing", NA)))))



# merge climate data with caterpillar density data for variance partitioning
region_complete = merge(all_sites, clim_summary, by = "site", all.x = T)
region_complete1 = region_complete %>% 
  dplyr::select(-region.y) %>%
  rename(region = region.x)

#merge leaf area data (created in data_cleaning) & normalize 
region_normalized = left_join(region_complete1, species_area, by = "ComName") #this has 350 rows vs 346 in region_complete1 because Sassafrass occurs twice in leaves_sp1, one w/ a leaf area, 1 w/ a NA. fix later, won't affect analysis because it will get filtered out
region_normalized1 = filter(region_normalized, area_cm2 != "NA")
region_normalized1$cat_normalized = ((region_normalized1$mean_cat_dens)/(region_normalized1$area_cm2))*mean(region_normalized1$area_cm2)

bysite = region_normalized1 %>% 
         group_by(region, ComName) %>%
         summarize(cat_dens_region=mean(mean_cat_dens), annual_ppt = mean(annual_ppt), avg_summer_tmp = mean(avg_summer_tmp)) %>%
         data.frame()

byregion = region_normalized1 %>% 
  group_by(region, ComName) %>%
  summarize(cat_dens_region=mean(mean_cat_dens), annual_ppt = mean(annual_ppt), avg_summer_tmp = mean(avg_summer_tmp)) %>%
  data.frame()


## VP 1- between region and tree sp (by region)
#a = variance uniquely explained by region		  
#b = variance explained by region and species together		  
#c = variance uniquely explained by species		
#d = variance explained by neither
rlm.region = lm(cat_dens_region ~ region, data = byregion)
rlm.species = lm(cat_dens_region ~ ComName, data = byregion) #what does it mean that the adjusted r2 is nothing?
rlm.region.species = lm(cat_dens_region ~ region + ComName, data = byregion)

a = summary(rlm.region.species)$r.squared - summary(rlm.species)$r.squared 
b = summary(rlm.region)$r.squared - a		
c = summary(rlm.region.species)$r.squared - summary(rlm.region)$r.squared		 
d = 1-summary(rlm.region.species)$r.squared

#VP 2- between climate and tree sp (by region)
#a2 = variance uniquely explained by climate		  
#b2 = variance explained by climate and species together		  
#c2 = variance uniquely explained by species		
#d2 = variance explained by neither
lm.climate = lm(cat_dens_region ~ annual_ppt + avg_summer_tmp, data = byregion)
lm.species = lm(cat_dens_region ~ ComName, data = byregion)
lm.climate.species = lm(cat_dens_region ~ ComName + annual_ppt + avg_summer_tmp, data = byregion)

a2 = summary(lm.climate.species)$r.squared - summary(lm.species)$r.squared
b2 = summary(lm.climate)$r.squared - a2		
c2 = summary(lm.climate.species)$r.squared - summary(lm.climate)$r.squared		 
d2 = 1-summary(lm.climate.species)$r.squared

#VP 3 (between precip and temp)
#a3 = variance uniquely explained by precip		  
#b3 = variance explained by precip and temp together (climate)		  
#c3 = variance uniquely explained by temp		
#d3 = variance explained by neither
lm.climate.int = lm(cat_dens_region ~ annual_ppt*avg_summer_tmp, data = byregion)   #examining the interaction between climate and tree species 
lm.precip = lm(cat_dens_region ~ annual_ppt, data = byregion)
lm.temp = lm(cat_dens_region ~ avg_summer_tmp, data = byregion)

a3 = summary(lm.climate)$r.squared - summary(lm.temp)$r.squared #greater than total variance explained by 		 
b3 = summary(lm.precip)$r.squared - a3		
c3 = summary(lm.climate)$r.squared - summary(lm.precip)$r.squared		 
d3 = 1-summary(lm.climate)$r.squared







