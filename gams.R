# GAMs

# Eventually will use this data for each GAM
source('C:/git/caterpillars-count-analysis/phenology_thesisplots.R')

# mgcv package has existing gam functions
library(mgcv)

findMaxJD <- function(lp_formatted, # dataset from phenology_thesisplots.R by week
                      year, # 2015 or 2015
                      site, # 117 or 8892356
                      species) # caterpillars, orthopterans, or bird food
  
{ # start function

# use an already subsetted dataset (like PR.LEPL15.bs)
names(lp_formatted) = c('WEEKNO', 'numSurveys', 'totalCount', 'numSurveysGTzero', 'meanDensity',
                        'COUNT')
lp_formatted$YEAR = year
lp_formatted$SITE = site
lp_formatted$trimWEEKNO = 1:length(lp_formatted$WEEKNO)
lp_formatted$SPECIES = species


#for storing results
lp_gams<-list()

#loop through years, GAM for each year, plot flight curve
for (i in 1:length(unique(lp_formatted$YEAR))) {
  
  lp_yr<-lp_formatted[lp_formatted$YEAR==unique(lp_formatted$YEAR)[i],]
  
  lp_gams[[i]] <- try(gam(COUNT~ s(trimWEEKNO,bs="cr"),data=lp_yr,family=betar(link="logit")), silent=TRUE)
  gam.check(lp_gams[[i]])
  
  jds = data.frame(trimWEEKNO = seq(20, 30, by = .1428))
  
  lp_yr[,"FITTED"]<-predict.gam(lp_gams[[i]], newdata = lp_formatted, type="response")
  lp_yr[,"COUNT_IMPUTED"] <- lp_yr$COUNT
  lp_yr[is.na(lp_yr$COUNT),"COUNT_IMPUTED"] <- lp_yr$FITTED[is.na(lp_yr$COUNT)]
  
  
  # Define the flight curve from the fitted values (this is one flight curve per year for all site)
  site_sums = aggregate(lp_yr$FITTED, by = list(SITE = lp_yr$SITE), FUN = sum)
  
  # Rename sum column
  names(site_sums)[names(site_sums) == "x"]  = "SITE_YR_FSUM"
  # Add data to sp_data data.frame (ensure merge does not sort the data!)
  lp_yr = merge(lp_yr, site_sums, by = c("SITE"), all = TRUE, sort = FALSE)
  # Calculate normalised values
  lp_yr[,"NM"] = lp_yr$FITTED/lp_yr$SITE_YR_FSUM
  
  #sp_data_filled <- lp_data
  
  
  flight_curve <- data.frame(species=lp_yr$SPECIES, year=lp_yr$YEAR, WEEKNO=lp_yr$WEEKNO,
                             WEEKNO_adj=lp_yr$trimWEEKNO, 
                             nm=lp_yr$NM)[!duplicated(paste(lp_yr$YEAR,lp_yr$WEEKNO,sep="_")),]
  
  flight_curve <- flight_curve[order(flight_curve$WEEKNO),]
  
  #Plot flight curve (phenology) and original count data
  par(mfrow = c(1,1))
  plot(flight_curve$WEEKNO,flight_curve$nm,type='b', main = paste(flight_curve$species[1], 
                              unique(lp_formatted$YEAR)[i]), ylim = c(0, max(lp_yr$COUNT)))
  #points(flight_curve$WEEKNO,flight_curve$nm,col='red')
  
  points(lp_yr$WEEKNO, lp_yr$COUNT, lty = 1, col = 'red')
  
}


maxweek <- flight_curve$WEEKNO[flight_curve$nm == max(flight_curve$nm)] # to find max
jday = (maxweek*7)+4 # julian day of the middle of the week
return(jday)

} # end function

# Prairie Ridge

findMaxJD(PR.LEPL15.vis, 2015, 117, 'caterpillars') # warning
findMaxJD(PR.LEPL15.bs, 2015, 117, 'caterpillars')
findMaxJD(PR.LEPL16.vis, 2016, 117, 'caterpillars') # warning
findMaxJD(PR.LEPL16.bs, 2016, 117, 'caterpillars') # warning

findMaxJD(PR.ORTH15.vis, 2015, 117, 'orthopterans') # warning
findMaxJD(PR.ORTH15.bs, 2015, 117, 'orthopterans') # warning
findMaxJD(PR.ORTH16.vis, 2016, 117, 'orthopterans') # warning
findMaxJD(PR.ORTH16.bs, 2016, 117, 'orthopterans') 

findMaxJD(PR.BIRD15.vis, 2015, 117, 'bird food') 
findMaxJD(PR.BIRD15.bs, 2015, 117, 'bird food')
findMaxJD(PR.BIRD16.vis, 2016, 117, 'bird food') 
findMaxJD(PR.BIRD16.bs, 2016, 117, 'bird food') 

# Botanical Garden

findMaxJD(BG.LEPL15.vis, 2015, 8892356, 'caterpillars') 
findMaxJD(BG.LEPL15.bs, 2015, 8892356, 'caterpillars')
findMaxJD(BG.LEPL16.vis, 2016, 8892356, 'caterpillars') 
findMaxJD(BG.LEPL16.bs, 2016, 8892356, 'caterpillars') 

findMaxJD(BG.ORTH15.vis, 2015, 8892356, 'orthopterans') 
findMaxJD(BG.ORTH15.bs, 2015, 8892356, 'orthopterans') 
findMaxJD(BG.ORTH16.vis, 2016, 8892356, 'orthopterans') 
findMaxJD(BG.ORTH16.bs, 2016, 8892356, 'orthopterans') 

findMaxJD(BG.BIRD15.vis, 2015, 8892356, 'bird food') 
findMaxJD(BG.BIRD15.bs, 2015, 8892356, 'bird food')
findMaxJD(BG.BIRD16.vis, 2016, 8892356, 'bird food') 
findMaxJD(BG.BIRD16.bs, 2016, 8892356, 'bird food') 


