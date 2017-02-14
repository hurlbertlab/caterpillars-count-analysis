# GAMs


# use an already subsetted dataset (like PR.LEPL15.bs)
lp_formatted <- PR.LEPL15.bs
names(lp_formatted) = c('DAYNO', 'numSurveys', 'totalCount', 'numSurveysGTzero', 'meanDensity',
                        'COUNT')
lp_formatted$YEAR = 2015
lp_formatted$SITE = 117
lp_formatted$trimDAYNO = 1:length(lp_formatted$DAYNO)


#for storing results
lp_gams<-list()

#loop through years, GAM for each year, plot flight curve
for (i in 1:length(unique(lp_formatted$YEAR))) {
  
  lp_yr<-lp_formatted[lp_formatted$YEAR==unique(lp_formatted$YEAR)[i],]
  
  lp_gams[[i]] <- try(gam(COUNT~ s(trimDAYNO,bs="cr") + SITE,data=lp_yr,family = poisson(link="log")), silent=TRUE)
  gam.check(lp_gams[[i]])
  
  lp_yr[,"FITTED"]<-predict.gam(lp_gams[[i]], newdata = lp_yr, type="response")
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
  
  
  flight_curve <- data.frame(species=lp_yr$SPECIES, year=lp_yr$YEAR, week=lp_yr$WEEK, DAYNO=lp_yr$DAYNO,DAYNO_adj=lp_yr$trimDAYNO, nm=lp_yr$NM)[!duplicated(paste(lp_yr$YEAR,lp_yr$DAYNO,sep="_")),]
  
  flight_curve <- flight_curve[order(flight_curve$DAYNO),]
  
  #Plot flight curve (phenology) and original count data
  
  plot(flight_curve$DAYNO,flight_curve$nm,type='l', main = paste(flight_curve$species[1], unique(lp_formatted$YEAR)[i]))
  points(flight_curve$DAYNO,flight_curve$nm,col='red')
  
  scalar1<-max(flight_curve$nm)/max(lp_yr$COUNT[is.na(lp_yr$COUNT)==F])
  for (j in 1:length(levels(lp_yr$SITE))) {
    site_j<-levels(lp_yr$SITE)[j]
    points(lp_yr$DAYNO[lp_yr$SITE==site_j],lp_yr$COUNT[lp_yr$SITE==site_j]*scalar1,col=rainbow(length(levels(lp_yr$SITE)))[j], pch=16)
  }
  
}