################################################
# Script for conducting basic analyses based on 
# Caterpillars Count! data exported from the
# phpMyAdmin project site.

# Data include:
#
# tbl_surveys: date, site, time, and temperature of all survey events
#
# tbl_orders: arthropod observed, their length, and count associated with
#             each survey event


# Read in data
surveys = read.csv('data/tbl_surveys.csv', header=F)
orders = read.csv('data/tbl_orders.csv', header=F)