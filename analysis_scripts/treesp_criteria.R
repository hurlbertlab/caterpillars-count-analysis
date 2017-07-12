## Determine which tree species should be included analysis on the basis of
        # 1) Total # of unique surveys on a single tree sp over time (independent of site)
        # 2) Total # of sites a tree sp. occurs at
        # 3) # of regions tree sp occurs at (2 is enough if includes singer or hub, 3 otherwise)
#hubraw1 -> raw data for hubbard
#singer1 -> raw data for singer
#vis_tri -> raw triangle data
#vis_app -> raw appalachian -> includes both va and sa
source("archived_scripts/Tara_thesis/northern_comparisons.R")
source("analysis_scripts/summary_functions.R")

# want to perform calculations on average caterpillar densities. will do caterpillars/m2 (ie/10000 cm2)
    # singer already has this


# hub surveys: total # of caterpillars on a unique survey is already summarized in the raw data
#group by plot, year, week, grid.letter, grid.number, tree.spec
hubraw_uniq = hubraw1 %>%  
  filter(lep.length >= 10) %>%
  select(plot, date, tree.name) %>%
  rename(ComName = tree.name,  site = plot)
hubraw_uniq$region = "hub"


#singer surveys : total # of caterpillars on a unique survey is already summarized in the raw data
#filter to remove bagged singer surveys (treatment = bagged)
singerraw_uniq = singer1 %>%    
  filter(treatment == "unbagged") %>%
  select(site, date, ComName)
singerraw_uniq$ComName = tolower(singerraw_uniq$ComName)
singerraw_uniq$region = "sing"

 

#to get unique vis_tri & vis_app surveys #since most of these are 0s, it's weird that the # of surveys is reduced so much by summarize (usually there is only 1 survey record...) - no actually this is possible, b/c there might be multiple entries for other arth types
vis_tri_uniq = vis_tri %>%
  select(site, circle, survey, date, realPlantSp) %>%
  distinct() %>%
  dplyr::select(-circle, -survey) %>%
  dplyr::rename(ComName = realPlantSp)
vis_tri_uniq$ComName = tolower(vis_tri_uniq$ComName)
vis_tri_uniq$region = "tri"

vis_app_uniq = vis_app %>%
  select(site, circle, survey, date, realPlantSp) %>%
  distinct() %>%
  dplyr::select(-circle, -survey) %>%
  dplyr::rename(ComName = realPlantSp)

va_sites_uniq = vis_app_uniq %>% 
  filter(grepl("88", site))
va_sites_uniq$region = "va"

sa_sites_uniq = vis_app_uniq %>% 
  filter(!grepl("88", site)) #all BBS sites excluding those in VA
sa_sites_uniq$region = "sa"


#find total # of sites a tree sp occurs at 
# 1) create a dataframe with all of the unique surveys
  # each original data frame must have a unique identifier for each site, a date, and a region component
# 2) get list of total # of sites a tree sp. occurs at by group.by tree.sp, site, distinct()
# 3) get list of total # of surveys a tree sp. has overall by group.by, tree.sp, site, date
# 4) get list of total # of regins a s

aa1 = rbind(hubraw_uniq, singerraw_uniq)
aa2 = rbind(aa1, vis_tri_uniq)
aa3 = rbind(aa2, va_sites_uniq)
allraw_uniq = rbind(aa3, sa_sites_uniq)

totalspecies_sites = allraw_uniq %>% 
 distinct(site, ComName) %>%
 count(ComName) %>%
 data.frame()

totalspecies_sites = totalspecies_sites[order(totalspecies_sites$n, decreasing = T),]
enough_sites = filter(totalspecies_sites, n >= 4) #tuliptree and tulip poplar are the same... figure out where the discripeancy is

totalspecies_surveys = allraw_uniq %>% #this is not doing total unique surveys, it's doing total dates/sites at which a survey was conducted 
  distinct(site, date, ComName) %>%
  count(ComName) %>%
  data.frame()
  
totalspecies_surveys = totalspecies_surveys[order(totalspecies_surveys$n, decreasing = T),] 
enough_surveys = filter(totalspecies_surveys, n >= 25) #tuliptree and tulip poplar are the same... figure out where the discripeancy is

totalspecies_regions = allraw_uniq %>%
  distinct(region, ComName) %>%
  count(ComName) %>%
  data.frame()

totalspecies_regions = totalspecies_regions[order(totalspecies_regions$n, decreasing = T),] 
enough_regions = filter(totalspecies_regions, n >= 3 | ComName %in% singerraw_uniq$ComName)


# create a list of trees that fulfill these expectations
all_trees = unique(allraw_uniq$ComName)
all_trees1 = all_trees[all_trees %in% enough_sites$ComName] 
all_trees2 = all_trees1[all_trees1 %in% enough_surveys$ComName]
analysis_trees = all_trees2[all_trees2 %in% enough_regions$ComName]
  