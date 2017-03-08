# Modified from filtered_logistics.r script from Hurlbert Liang 2012
# Use either sampling_pr or sampling_bg (from eBird.R script)
# Still have errors to work out

library(bbmle) ##has function 'mle2' which is a wrapper for the base function 'optim'
require(agricolae)

setwd('c:/git/caterpillars-count-analysis')

sampling_pr <- read.csv('c:/git/caterpillars-count-analysis/data/ebird_sampling_pr.csv')
sampling_bg <- read.csv('c:/git/caterpillars-count-analysis/data/ebird_sampling_bg.csv')
obs_pr <- read.csv('c:/git/caterpillars-count-analysis/data/ebird_obs_pr.csv')
obs_bg <- read.csv('c:/git/caterpillars-count-analysis/data/ebird_obs_bg.csv')


#---- CREATE PRAIRIE RIDGE DATASET ----

par(mfrow=c(3,2)) #mai=c(0.3,0.6,0.25,0.5), las=1,xaxs="r")
splist = c('Passerina cyanea', 'Vireo olivaceus', 'Geothlypis trichas', 'Polioptila caerulea')

inflection.pt.output=c()
singlesp = c()
singlesp1yr = c()

for(sp in 1:4) { par(mfrow=c(3,2))
 for (yr in 2006:2016) {                   # 
#for(i in 1:(nrow(long)-1)) {         # no lat long needed because datasets already subsetted
#for(j in 1:(nrow(lat)-1)) {          # so only need year and sp

#sampling effort

t.samp=subset(sampling_pr, sampling_pr$Year.Collected==yr)
obs_pr$Scientific.Name=as.character(obs_pr$Scientific.Name)
t.obs.NA1s = subset(obs_pr, obs_pr$Year==yr & obs_pr$Scientific.Name==as.character(splist[sp]))
t.obs.NA1s$Observation.Count = as.numeric(as.character(t.obs.NA1s$Observation.Count))

#((nrow(inf_jds1[inf_jds1$prop>0,]))>=30)==T
if(nrow(t.samp)>0 & nrow(t.obs.NA1s)>0 & (length(unique(t.obs.NA1s$JulianDay))>=30)==T){    #       nrow(t.samp)>49 &
  effort.by.day = aggregate(t.samp$Lat.Long, list(t.samp$Year.Collected, t.samp$JulianDay), function(x) length(unique(x)))
  names(effort.by.day)=c('Year','JulianDay','Num.Unique.locs')            #number of unique locs in sampling per day
  window.days = 60:180     #the days without records are discarded, and here we're adding the 0's back in to replace the NA's
  temp.data1 = merge(effort.by.day,as.data.frame(window.days),by.x='JulianDay',by.y='window.days',all=T)
  temp.data1[is.na(temp.data1$Num.Unique.locs),'Num.Unique.locs'] = 0
  
  #observations
  
  t.obs.NA1s[is.na(t.obs.NA1s$Observation.Count)==T,'Observation.Count'] = 1                                    #give all the ones without obs counts a value of 1
  combos = do.call('paste', t.obs.NA1s[c('Scientific.Name','Latitude','Longitude','Year','JulianDay')])         #paste these columns together
  max.by.siteday = aggregate(t.obs.NA1s$Observation.Count, by=list(combos), function(x) max(x))                 #take the max observation count 
  max.by.siteday.split = as.data.frame(matrix(unlist(strsplit(max.by.siteday$Group.1, " ")), ncol=6, byrow=T))  #unsplit them
  MAX.by.siteday.split = data.frame(do.call('paste', max.by.siteday.split[c('V1','V2')]), do.call('paste',max.by.siteday.split[c('V3','V4')]), max.by.siteday.split$V3, max.by.siteday.split$V4, max.by.siteday.split$V5, max.by.siteday.split$V6)
  pre.max.by.siteday = cbind(MAX.by.siteday.split, max.by.siteday$x)         
  names(pre.max.by.siteday) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count')     #<-Added in Lat and Long separately too
  pre.max.by.siteday[,'Year'] = as.numeric(as.character(pre.max.by.siteday[,'Year']))
  pre.max.by.siteday[,'JulianDay'] = as.numeric(as.character(pre.max.by.siteday[,'JulianDay']))
  sorted.max.by.siteday = pre.max.by.siteday[order(pre.max.by.siteday$Scientific.Name, pre.max.by.siteday$Year, pre.max.by.siteday$JulianDay),]         #<-Now with Lat.Long added in.
  
  hmm = cbind(sorted.max.by.siteday, 1)
  names(hmm) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count','uniq.count')
  combos2 = do.call('paste', hmm[c('Latitude','Longitude','Year','JulianDay')])
  gee = aggregate(hmm$uniq.count, by=list(combos2), sum)
  goe = as.data.frame(matrix(unlist(strsplit(gee$Group.1, " ")), ncol=4, byrow=T))
  gob = data.frame(goe$V1, goe$V2, goe$V3, goe$V4)
  goc = cbind(gob, gee$x)
  names(goc) = c('Latitude','Longitude','Year','JulianDay','Num.of.uniq.locs')
  combos3 = do.call('paste', goc[c('Year','JulianDay')])
  loc = aggregate(goc$Num.of.uniq.locs, by=list(combos3), sum)
  Loc = as.data.frame(matrix(unlist(strsplit(loc$Group.1, " ")), ncol=2, byrow=T))
  locc = cbind(Loc, loc$x)
  names(locc) = c('Year','JulianDay','Num.uniq.locs')
  locc[,'Year'] = as.numeric(as.character(locc[,'Year']))
  locc[,'JulianDay'] = as.numeric(as.character(locc[,'JulianDay']))
  locc[,'Num.uniq.locs'] = as.numeric(as.character(locc[,'Num.uniq.locs']))
  
  window.days2 = as.data.frame(as.matrix(window.days, ncol=1, byrow=T))
  temp.data2 = merge(locc[locc$Year==yr,],window.days2,by.x='JulianDay',by.y='V1',all=T)
  temp.data2[is.na(temp.data2$Num.uniq.locs),'Num.uniq.locs'] = 0
  
  temp.data2 = temp.data2[temp.data2$JulianDay<=180 & temp.data2$JulianDay>=60, ]         ##<<Change both here if want to change window of days seen<<##
  temp.data1 = temp.data1[temp.data1$JulianDay<=180 & temp.data1$JulianDay>=60, ]
  
  temp.data2$prop = temp.data2$Num.uniq.locs/temp.data1$Num.Unique.locs
  
  #plotting
  plot(temp.data2$JulianDay, temp.data2$prop,ylim=c(0,1),xlab='JulianDay',ylab='Prop of Uniq Locs',main=paste(as.character(splist[sp])), xlim=c(60,180), pch=16,type='o')
  text(min(temp.data2$JulianDay)+5, 0.9, yr, cex=1.5)
  #text(160, 0.9, as.character(splist[sp]))
  
  # FUNCTION FOR DRAWING A LOGISTIC CURVE GIVEN LOGISTIC FIT COEFFICIENTS
  exp.mod<-function(coes,jd){
    Asym<-plogis(coes[1])
    xmid<-coes[2]
    scal<-coes[3]
    Asym/(1 + exp((xmid - jd)/scal))
  }
  
  #mle fitting : BEGIN FITTING AND MAXIMUM LIKELIHOOD ESTIMATION OF LOGISTIC CURVE
  ll.exp.con<-function(Asym,xmid,scal){
    if(xmid>max(temp.data2$JulianDay)){
      nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
        1000 *(abs(max(temp.data2$JulianDay)-xmid))^2        #<-make it huge if it veers outside of constraints of jd
    }
    else{
      if(xmid<min(temp.data2$JulianDay)){
        nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
          1000 *(abs(min(temp.data2$JulianDay)-xmid))^2
      }
      else{
        nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
      }}
    nll
  }
  nll<-numeric()
  xmids<-seq(60,180,20)
  coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))
  
  for(xm in 1:length(xmids)){
    guess <- list(Asym=.6,xmid=xmids[xm],scal=1)
    fit.exp.con<- mle2(ll.exp.con, start = guess, method = "Nelder-Mead",skip.hessian=T)
    coef.mat[xm,]<-coef(fit.exp.con)
    Asym<-coef(fit.exp.con)[1]
    xmid<-coef(fit.exp.con)[2]
    scal<-coef(fit.exp.con)[3]
    nll[xm]<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
  }
  best.coef<-coef.mat[order(nll)[1],] ##only takes coef from the model with the smallest neg.log.likelihood
  
  #Functions - calculate "confidence intervals" on the esimate of arrival date based on julian days at which the best fit logistic
  #            predicts 2.5% and 97.5% of the occupancy of the asymptotic value.
  ad.LL = function(params) {
    Asym = plogis(params[1])
    xmid = params[2]
    scal = params[3]
    occupancy = Asym/(1 + exp((xmid - 60:180)/scal))
    LL2.5 = max(which(occupancy <= 0.025*Asym)) + 59
  }
  ad.UL = function(params) {
    Asym = plogis(params[1])
    xmid = params[2]
    scal = params[3]
    occupancy = Asym/(1 + exp((xmid - 60:180)/scal))
    LL97.5 = min(which(occupancy >= 0.975*Asym)) + 59
  }
  
  lowerconf = ad.LL(best.coef)
  upperconf = ad.UL(best.coef)
  
  #ADD BEST FIT LOGISTIC CURVE TO PLOT
  lines(temp.data2$JulianDay,exp.mod(best.coef,temp.data2$JulianDay),col='blue') ##model result
  abline(v=best.coef[2], col='red')
  
  temp.data2$prop[is.nan(temp.data2$prop)==T] = 0                                                     
  temp.jd = temp.data2$JulianDay#[is.nan(temp.data2$prop)==F]
  temp.prop = temp.data2$prop     #[is.nan(temp.data2$prop)==F]
  temp.yr = rep(temp.data2$Year, length(temp.prop))
  
  x=lm(exp.mod(best.coef, temp.data2$JulianDay)~temp.prop)
  
  singlesp1yr = c(as.character(splist[sp]), yr, best.coef[2], lowerconf, upperconf)
  singlesp = rbind(singlesp, singlesp1yr)
  
}}
  inflection.pt.output = rbind(inflection.pt.output, singlesp)
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, exp.mod(best.coef, temp.data2$JulianDay), summary(x)$r.squared, long[i,], lat[j,], long[i+1,], lat[j+1,]))
  
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, long[i,], lat[j,]))
}

# CHECK ON THESE WARNING MESSAGES

# Use this code after running sampling_pr and obs_pr initial datasets through:
inflection_pr <- unique(inflection.pt.output) 
inflection_pr <- data.frame(inflection_pr) # warning message is ok?
names(inflection_pr) <- c('scientific_name', 'year', 'inflection_pt', 'lowerconf', 'upperconf')
inflection_pr$year = as.numeric(as.character(inflection_pr$year))
inflection_pr$inflection_pt = as.numeric(as.character(inflection_pr$inflection_pt))
inflection_pr$scientific_name = as.character(inflection_pr$scientific_name)
inflection_pr$lowerconf = as.numeric(as.character(inflection_pr$lowerconf))
inflection_pr$upperconf = as.numeric(as.character(inflection_pr$upperconf))
inflection_pr$confint = inflection_pr$upperconf - inflection_pr$lowerconf





#---- CREATE BOTANICAL GARDEN DATASET ----

par(mfrow=c(3,2)) #mai=c(0.3,0.6,0.25,0.5), las=1,xaxs="r")
splist = c('Passerina cyanea', 'Vireo olivaceus', 'Geothlypis trichas', 'Polioptila caerulea')

inflection.pt.output=c()
singlesp = c()
singlesp1yr = c()

for(sp in 1:4) { par(mfrow=c(3,2))
  for (yr in 2006:2016) {                   # 
    #for(i in 1:(nrow(long)-1)) {         # no lat long needed because datasets already subsetted
    #for(j in 1:(nrow(lat)-1)) {          # so only need year and sp
    
    #sampling effort
    
    t.samp=subset(sampling_bg, sampling_bg$Year.Collected==yr)
    obs_bg$Scientific.Name=as.character(obs_bg$Scientific.Name)
    t.obs.NA1s = subset(obs_bg, obs_bg$Year==yr & obs_bg$Scientific.Name==as.character(splist[sp]))
    t.obs.NA1s$Observation.Count = as.numeric(as.character(t.obs.NA1s$Observation.Count))
    
    #((nrow(inf_jds1[inf_jds1$prop>0,]))>=30)==T
    if(nrow(t.samp)>0 & nrow(t.obs.NA1s)>0 & (length(unique(t.obs.NA1s$JulianDay))>=30)==T){    #       nrow(t.samp)>49 &
      effort.by.day = aggregate(t.samp$Lat.Long, list(t.samp$Year.Collected, t.samp$JulianDay), function(x) length(unique(x)))
      names(effort.by.day)=c('Year','JulianDay','Num.Unique.locs')            #number of unique locs in sampling per day
      window.days = 60:180     #the days without records are discarded, and here we're adding the 0's back in to replace the NA's
      temp.data1 = merge(effort.by.day,as.data.frame(window.days),by.x='JulianDay',by.y='window.days',all=T)
      temp.data1[is.na(temp.data1$Num.Unique.locs),'Num.Unique.locs'] = 0
      
      #observations
      
      t.obs.NA1s[is.na(t.obs.NA1s$Observation.Count)==T,'Observation.Count'] = 1                                    #give all the ones without obs counts a value of 1
      combos = do.call('paste', t.obs.NA1s[c('Scientific.Name','Latitude','Longitude','Year','JulianDay')])         #paste these columns together
      max.by.siteday = aggregate(t.obs.NA1s$Observation.Count, by=list(combos), function(x) max(x))                 #take the max observation count 
      max.by.siteday.split = as.data.frame(matrix(unlist(strsplit(max.by.siteday$Group.1, " ")), ncol=6, byrow=T))  #unsplit them
      MAX.by.siteday.split = data.frame(do.call('paste', max.by.siteday.split[c('V1','V2')]), do.call('paste',max.by.siteday.split[c('V3','V4')]), max.by.siteday.split$V3, max.by.siteday.split$V4, max.by.siteday.split$V5, max.by.siteday.split$V6)
      pre.max.by.siteday = cbind(MAX.by.siteday.split, max.by.siteday$x)         
      names(pre.max.by.siteday) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count')     #<-Added in Lat and Long separately too
      pre.max.by.siteday[,'Year'] = as.numeric(as.character(pre.max.by.siteday[,'Year']))
      pre.max.by.siteday[,'JulianDay'] = as.numeric(as.character(pre.max.by.siteday[,'JulianDay']))
      sorted.max.by.siteday = pre.max.by.siteday[order(pre.max.by.siteday$Scientific.Name, pre.max.by.siteday$Year, pre.max.by.siteday$JulianDay),]         #<-Now with Lat.Long added in.
      
      hmm = cbind(sorted.max.by.siteday, 1)
      names(hmm) = c('Scientific.Name','Lat.Long','Latitude','Longitude','Year','JulianDay','Observation.Count','uniq.count')
      combos2 = do.call('paste', hmm[c('Latitude','Longitude','Year','JulianDay')])
      gee = aggregate(hmm$uniq.count, by=list(combos2), sum)
      goe = as.data.frame(matrix(unlist(strsplit(gee$Group.1, " ")), ncol=4, byrow=T))
      gob = data.frame(goe$V1, goe$V2, goe$V3, goe$V4)
      goc = cbind(gob, gee$x)
      names(goc) = c('Latitude','Longitude','Year','JulianDay','Num.of.uniq.locs')
      combos3 = do.call('paste', goc[c('Year','JulianDay')])
      loc = aggregate(goc$Num.of.uniq.locs, by=list(combos3), sum)
      Loc = as.data.frame(matrix(unlist(strsplit(loc$Group.1, " ")), ncol=2, byrow=T))
      locc = cbind(Loc, loc$x)
      names(locc) = c('Year','JulianDay','Num.uniq.locs')
      locc[,'Year'] = as.numeric(as.character(locc[,'Year']))
      locc[,'JulianDay'] = as.numeric(as.character(locc[,'JulianDay']))
      locc[,'Num.uniq.locs'] = as.numeric(as.character(locc[,'Num.uniq.locs']))
      
      window.days2 = as.data.frame(as.matrix(window.days, ncol=1, byrow=T))
      temp.data2 = merge(locc[locc$Year==yr,],window.days2,by.x='JulianDay',by.y='V1',all=T)
      temp.data2[is.na(temp.data2$Num.uniq.locs),'Num.uniq.locs'] = 0
      
      temp.data2 = temp.data2[temp.data2$JulianDay<=180 & temp.data2$JulianDay>=60, ]         ##<<Change both here if want to change window of days seen<<##
      temp.data1 = temp.data1[temp.data1$JulianDay<=180 & temp.data1$JulianDay>=60, ]
      
      temp.data2$prop = temp.data2$Num.uniq.locs/temp.data1$Num.Unique.locs
      
      #plotting
      plot(temp.data2$JulianDay, temp.data2$prop,ylim=c(0,1),xlab='JulianDay',ylab='Prop of Uniq Locs',main=paste(as.character(splist[sp])), xlim=c(60,180), pch=16,type='o')
      text(min(temp.data2$JulianDay)+5, 0.9, yr, cex=1.5)
      #text(160, 0.9, as.character(splist[sp]))
      
      # FUNCTION FOR DRAWING A LOGISTIC CURVE GIVEN LOGISTIC FIT COEFFICIENTS
      exp.mod<-function(coes,jd){
        Asym<-plogis(coes[1])
        xmid<-coes[2]
        scal<-coes[3]
        Asym/(1 + exp((xmid - jd)/scal))
      }
      
      #mle fitting : BEGIN FITTING AND MAXIMUM LIKELIHOOD ESTIMATION OF LOGISTIC CURVE
      ll.exp.con<-function(Asym,xmid,scal){
        if(xmid>max(temp.data2$JulianDay)){
          nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
            1000 *(abs(max(temp.data2$JulianDay)-xmid))^2        #<-make it huge if it veers outside of constraints of jd
        }
        else{
          if(xmid<min(temp.data2$JulianDay)){
            nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE)) +
              1000 *(abs(min(temp.data2$JulianDay)-xmid))^2
          }
          else{
            nll<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
          }}
        nll
      }
      nll<-numeric()
      coef.mat<-matrix(NA,ncol=3,nrow=length(xmids))
      xmids<-seq(60,180,20)
      for(xm in 1:length(xmids)){
        guess <- list(Asym=.6,xmid=xmids[xm],scal=1)
        fit.exp.con<- mle2(ll.exp.con, start = guess, method = "Nelder-Mead",skip.hessian=T)
        coef.mat[xm,]<-coef(fit.exp.con)
        Asym<-coef(fit.exp.con)[1]
        xmid<-coef(fit.exp.con)[2]
        scal<-coef(fit.exp.con)[3]
        nll[xm]<- -sum( dbinom(temp.data2$Num.uniq.locs,size=temp.data1$Num.Unique.locs,prob=sapply(temp.data2$JulianDay,function(jd) plogis(Asym)/(1 + exp((xmid - jd)/(scal)))),log=TRUE))
      }
      
      
      
      best.coef<-coef.mat[order(nll)[1],] ##only takes coef from the model with the smallest neg.log.likelihood
      
      #Functions - calculate "confidence intervals" on the esimate of arrival date based on julian days at which the best fit logistic
      #            predicts 2.5% and 97.5% of the occupancy of the asymptotic value.
      ad.LL = function(params) {
        Asym = plogis(params[1])
        xmid = params[2]
        scal = params[3]
        occupancy = Asym/(1 + exp((xmid - 60:180)/scal))
        LL2.5 = max(which(occupancy <= 0.025*Asym)) + 59
      }
      ad.UL = function(params) {
        Asym = plogis(params[1])
        xmid = params[2]
        scal = params[3]
        occupancy = Asym/(1 + exp((xmid - 60:180)/scal))
        LL97.5 = min(which(occupancy >= 0.975*Asym)) + 59
      }
      
      lowerconf = ad.LL(best.coef)
      upperconf = ad.UL(best.coef)
      
      #ADD BEST FIT LOGISTIC CURVE TO PLOT
      lines(temp.data2$JulianDay,exp.mod(best.coef,temp.data2$JulianDay),col='blue') ##model result
      abline(v=best.coef[2], col='red')
      
      temp.data2$prop[is.nan(temp.data2$prop)==T] = 0                                                     
      temp.jd = temp.data2$JulianDay#[is.nan(temp.data2$prop)==F]
      temp.prop = temp.data2$prop     #[is.nan(temp.data2$prop)==F]
      temp.yr = rep(temp.data2$Year, length(temp.prop))
      
      x=lm(exp.mod(best.coef, temp.data2$JulianDay)~temp.prop)
      
      singlesp1yr = c(as.character(splist[sp]), yr, best.coef[2], lowerconf, upperconf)
      singlesp = rbind(singlesp, singlesp1yr)
      
    }}
  inflection.pt.output = rbind(inflection.pt.output, singlesp)
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, exp.mod(best.coef, temp.data2$JulianDay), summary(x)$r.squared, long[i,], lat[j,], long[i+1,], lat[j+1,]))
  
  #inflection.pt.output = rbind(inflection.pt.output, cbind(as.character(splist[sp]), yr, best.coef[1], best.coef[2], best.coef[3], temp.jd, temp.prop, long[i,], lat[j,]))
}

# CHECK ON THESE WARNING MESSAGES

# Use this code after running sampling_bg and obs_bg initial datasets through:
inflection_bg <- unique(inflection.pt.output) 
inflection_bg <- data.frame(inflection_bg) # warning message is ok?
names(inflection_bg) <- c('scientific_name', 'year', 'inflection_pt', 'lowerconf', 'upperconf')
inflection_bg$year = as.numeric(as.character(inflection_bg$year))
inflection_bg$inflection_pt = as.numeric(as.character(inflection_bg$inflection_pt))
inflection_bg$scientific_name = as.character(inflection_bg$scientific_name)
inflection_bg$lowerconf = as.numeric(as.character(inflection_bg$lowerconf))
inflection_bg$upperconf = as.numeric(as.character(inflection_bg$upperconf))
inflection_bg$confint = inflection_bg$upperconf - inflection_bg$lowerconf


