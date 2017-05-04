## Linear Models For effects of Average Summer Temperature and Net Annual Precipitation on Caterpillar Density Across Regions
# WARNING- this is not an actual number of caterpillars observed, this is the number 
# observed per 50 leaf survey X THE LEAF AREA OF THAT TREE SPECIES IN CM
source("archived_scripts/Tara_thesis/northern_comparisons.R")
hubbard_no_trees = dat1 %>% dplyr::rename(ComName = tree.name, site=plot) 
hubbard_no_trees1 = merge(hubbard_no_trees, leaves_sp1)
hubbard_no_trees2 = hubbard_no_trees1 %>%
                    group_by(site) %>%
                    dplyr::summarize(mean_cat_dens = sum(number.lep*avg_leaf_area_cm2)/(surveyspersite)) %>%
                    data.frame()

singer_no_trees = singer1 %>% filter(treatment == "unbagged")
singer_no_trees1 = merge(singer_no_trees, leaves_sp1)
singer_no_trees2 =  singer_no_trees1 %>% 
                    group_by(site) %>% 
                    dplyr::summarize(mean_cat_dens = mean(X.generalists*50*avg_leaf_area_cm2/numberlvs)) %>% #calculate number of caterpillars that would appear on a 50 leaf given tree sp area count
                    data.frame()
nona =filter(leaves_sp1, avg_leaf_area_cm2 !="NA")
sa_no_trees = sa_sites %>%  #get unique surveys
              dplyr::rename(ComName = clean_plantSp) %>% 
              group_by(site, date, survey, circle, ComName) %>%
              summarize(sum_cat=sum(cats_count_10)) %>%
              data.frame()
sa_no_trees1 = merge(sa_no_trees, leaves_sp1)
sa_no_trees2 = sa_no_trees1 %>%
               dplyr::filter(avg_leaf_area_cm2 != "NA") %>%
               group_by(site) %>%
               dplyr::summarize(mean_cat_dens = mean(sum_cat*avg_leaf_area_cm2)) %>%
               data.frame()

va_no_trees = va_sites %>%  #get unique surveys
  dplyr::rename(ComName = clean_plantSp) %>% 
  group_by(site, date, survey, circle, ComName) %>%
  summarize(sum_cat=sum(cats_count_10)) %>%
  data.frame()
va_no_trees1 = merge(va_no_trees, leaves_sp1)
va_no_trees2 = va_no_trees1 %>%
               dplyr::filter(avg_leaf_area_cm2 != "NA") %>%
               group_by(site) %>%
               dplyr::summarize(mean_cat_dens = mean(sum_cat*avg_leaf_area_cm2)) %>%
               data.frame()

tri_no_trees = vis_tri2 %>%  #get unique surveys
  dplyr::rename(ComName = clean_plantSp) %>% 
  group_by(site, date, survey, circle, ComName) %>%
  summarize(sum_cat=sum(cats_count_10)) %>%
  data.frame()
tri_no_trees1 = merge(tri_no_trees, leaves_sp1)
tri_no_trees2 = tri_no_trees1 %>%
  dplyr::filter(avg_leaf_area_cm2 != "NA") %>%
  group_by(site) %>%
  dplyr::summarize(mean_cat_dens = mean(sum_cat*avg_leaf_area_cm2)) %>%
  data.frame()
               
hs=rbind(hubbard_no_trees2, singer_no_trees2)
hss=rbind(hs, sa_no_trees2)
hssv = rbind(hss, va_no_trees2)
all_no_trees = rbind(hssv, tri_no_trees2)

all_no_trees$site = gsub(117, "Prairie Ridge", all_no_trees$site) 
all_no_trees$site = gsub("6303Prairie Ridge", 6303117, all_no_trees$site)
all_no_trees$site = gsub(8892356, "Botanical Garden", all_no_trees$site)

all_no_trees_clim = merge(all_no_trees, clim_summary)

lm.precip1 = lm(mean_cat_dens ~ annual_ppt, data = all_no_trees_clim)
lm.temp1 = lm(mean_cat_dens ~ avg_summer_tmp, data = all_no_trees_clim)


              
              
              