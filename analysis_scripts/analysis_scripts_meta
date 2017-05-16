# Caterpillars Count! Repository Notes - Analysis Scripts


## arth_analyses.R

-creates 6-panel plots of mean density over Julian day for LEPL, ORTH, HETE, ARAN, AUCH
-compares peaks between orders for LEPL-ORTH, LEPL-AUCH, and ORTH-AUCH

## arthsize.R

-plots average length of arths over Julian day, includes: ARAN, AUCH, COLE, DIPT, LEPL, OPIL, ORTH, and HETE (both sites in 2015)
-may need to change what original datasets are plugged in
-group plots of the lm for all arths, both sites in 2015

## correlation_coefficient.R

-calculates correlation coefficients between citizen scientists and trained scientists for LEPL, ORTH, and BIRD food in 2015 across both survey methods
-need to define multorders for bird food category

## eBird.R

-takes raw eBird data, cleans it and subsets it to specific locations: Prairie Ridge, NCBG, Hubbard Brook
-have to read in files from own computer, will change
-can make scripts more generalizable to any eBird data

## estimating_peaks.R

-currently not working, investigating
-maxjdStat function
-aveMaxPerCircleNum function
-splines
-quadratic fits

## fracsurveys_phenology.R

-phenology plots, including the one for overall paper and for Tracie’s 495 paper
-how to calculate Gaussian fits to phenology curves
-used to contain phenological shifts (Figure 4 in Tracie’s 495 paper) but those were updated for honors thesis (see Tracie’s honors thesis folder)
-last section (Figure 4 Phenology Plot) may need to be deleted – sourcing scripts that no longer exist

## modis_10year.R

-downloads data and calculates greenup date for 2000-2016 using average EVI at Prairie Ridge, NCBG, and Hubbard Brook
-small adjustments to change or add years
-more generalizable than modis_temp.R

## modis_temp.R

-Script for downloading MODIS EVI data and temp data – specific sites and years
-Download MODIS data: can adjust year, location, and type of data downloaded – currently set to PR and BG 2015 and 2016
-Calculates average EVI across area
-Plots greenup curves
-Reads in and cleans temperature data (from weather stations)
-Calculate growing degree days with NCBG temp data
-Combined plotting

## NC_caterpillar_plots.r

-plots PR and BG cats 2015 and 2016, mean density by day and by week
-plots average phenology of two sites for those years, creates pdf

## phenology_plot.R

-problems – same as/similar to fracsurveys_phenology.R

## prism_10year.R

-Downloads daily average temp data from PRISM
-Can adjust what dates are downloaded (downloads for whole earth)
-Can adjust what lat-longs are subsetted out
-Organizes temperature data that has been downloaded (adjust where data is being pulled from)
-Reads in the newly created clean csv, example given from repo
-gddcalc function: takes a dataset with year, date, jday column
-Calculate reference Julian day at 1000 GDDs to use for comparison, plots
-for loop to combine data from different sites, plotting
