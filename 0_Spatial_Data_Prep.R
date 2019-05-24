#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Initial Spatial Analysis
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/24/2019
#Purpose: Conduct Initial Spatial Analyis for SWAT-DSF Anlaysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some Notes on Data -- 
#  We are starting this anlaysis with the following data
#  -Polygon shapes of Wetland Subshed boundaries (from Jones et al., 2018)
#  -Raster files of actual & potential wetland area (from Jones et al., 2018)
#  -Stage to area AND stage to volume relationships for each wetland

#Goal: The end product should be a csv file of with collumns of: 
# (1) BasinID, (2-3) Actual and potential stroage capacity, 
# (4-5) Actual and potential perimeter, and (6) land use

#1.0 Setup workspace------------------------------------------------------------
#Clear memory
remove(list=ls())

#Call relevant packages
library(tidyverse)
library(sp)
library(raster)

#define relevant directories

