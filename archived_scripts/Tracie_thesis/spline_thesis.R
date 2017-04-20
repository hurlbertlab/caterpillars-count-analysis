# Script for fitting splines to arthropod phenology data.
# See phenology_thesisplots.R to get datasets 
# (but any CC dataset that has been run through the meanDensityByWeek function should work)

par(mfrow = c(1,1), mar = c(4,4,2,2), oma = c(1,1,1,1))

source('archived_scripts/Tracie_thesis/phenology_thesisplots.R')

splinearth <- function(dataset, # enter dataset
                       degreesf) # enter degrees of freedom
{ # start function

  dataset$julianday = (dataset$week - 1)*7 + 4
  spline = smooth.spline(dataset$julianday, dataset$fracSurveys, df = degreesf)
  plot(dataset$julianday, dataset$fracSurveys, type = 'l', col = 'black', lwd = 1,
      xlab = 'Julian day', ylab = 'Occurrence (fraction of surveys)')
  spl.pred = predict(spline, 135:204)
  lines(predict(spline, sort(c(seq(135, 204, by = 1), unique(dataset$julianday)))), lwd = 2, col = 'darkorchid1')
  abline(v=spl.pred$x[spl.pred$y==max(spl.pred$y)], col='plum', lwd = 2, lty = 2)
  points(x = spl.pred$x[spl.pred$y==max(spl.pred$y)], y = max(spl.pred$y), pch = 8, cex = 2, lwd = 3, col = 'chocolate2') # estimation of y
  return(spl.pred$x[spl.pred$y==max(spl.pred$y)])
  
} # end function

datalist = c("PR.BIRD15.bs", "PR.BIRD15.vis", "PR.BIRD16.bs", "PR.BIRD16.vis", "PR.LEPL15.bs", "PR.LEPL15.vis", 
       "PR.LEPL16.bs", "PR.LEPL16.vis", "PR.ORTH15.bs", "PR.ORTH15.vis", "PR.ORTH16.bs", "PR.ORTH16.vis",
       "BG.BIRD15.bs", "BG.BIRD15.vis", "BG.BIRD16.bs", "BG.BIRD16.vis", "BG.LEPL15.bs", "BG.LEPL15.vis", 
       "BG.LEPL16.bs", "BG.LEPL16.vis", "BG.ORTH15.bs", "BG.ORTH15.vis", "BG.ORTH16.bs", "BG.ORTH16.vis")

# Prairie Ridge
PR.LEPL15.vis.max = splinearth(PR.LEPL15.vis, 5)
PR.LEPL15.bs.max = splinearth(PR.LEPL15.bs, 5) # weird?
PR.LEPL16.vis.max = splinearth(PR.LEPL16.vis, 5)
PR.LEPL16.bs.max = splinearth(PR.LEPL16.bs, 5) # weird
PR.ORTH15.vis.max = splinearth(PR.ORTH15.vis, 5)
PR.ORTH15.bs.max = splinearth(PR.ORTH15.bs, 5)
PR.ORTH16.vis.max = splinearth(PR.ORTH16.vis, 5)
PR.ORTH16.bs.max = splinearth(PR.ORTH16.bs, 5)
PR.BIRD15.vis.max = splinearth(PR.BIRD15.vis, 5)
PR.BIRD15.bs.max = splinearth(PR.BIRD15.bs, 5)
PR.BIRD16.vis.max = splinearth(PR.BIRD16.vis, 5)
PR.BIRD16.bs.max = splinearth(PR.BIRD16.bs, 5)

# Botanical Garden
BG.LEPL15.vis.max = splinearth(BG.LEPL15.vis, 5)
BG.LEPL15.bs.max = splinearth(BG.LEPL15.bs, 5)
BG.LEPL16.vis.max = splinearth(BG.LEPL16.vis, 5)
BG.LEPL16.bs.max = splinearth(BG.LEPL16.bs, 5)
BG.ORTH15.vis.max = splinearth(BG.ORTH15.vis, 5) # weird?
BG.ORTH15.bs.max = splinearth(BG.ORTH15.bs, 5)
BG.ORTH16.vis.max = splinearth(BG.ORTH16.vis, 5)
BG.ORTH16.bs.max = splinearth(BG.ORTH16.bs, 5) # weird?
BG.BIRD15.vis.max = splinearth(BG.BIRD15.vis, 5)
BG.BIRD15.bs.max = splinearth(BG.BIRD15.bs, 5)
BG.BIRD16.vis.max = splinearth(BG.BIRD16.vis, 5)
BG.BIRD16.bs.max = splinearth(BG.BIRD16.bs, 5)



