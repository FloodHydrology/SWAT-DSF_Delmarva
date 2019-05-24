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
library(sf)
library(raster)
library(parallel)

#define relevant directories
data_dir<-'/nfs/njones-data/Research Projects/SWAT-DSF_Delmarva/SpatialData/greensboro_Jones2018/'
list.files(data_dir)

#Download Relevant Files
df<-read_csv(paste0(data_dir,"inundate.csv"))
basin.shp<-st_read(paste0(data_dir,'greens_basin.shp'))
actual.grd<-raster(paste0(data_dir,'actual_area.tif'))
potential.grd<-raster(paste0(data_dir,'potential_area.tif'))

#2.0 Estimate subshed area------------------------------------------------------
basin.shp$area<-st_area(basin.shp, by_element = T)
a<-tibble(BasinID=basin.shp$BasinID, 
          watershed_area = basin.shp$area)

#3.0 Estimate perimeter---------------------------------------------------------
#Create function to estimate perimeter of wetland in each subshed
perimeter_fun<-function(BasinID){
  
  #Select Basin Shp
  basin<-basin.shp[basin.shp$BasinID==BasinID,]
  
  #add buffer to shape equal to 1 cell
  basin<-st_buffer(basin, res(actual.grd)[1])
  
  #crop raster shape
  actual<-raster::crop(actual.grd, basin)
  actual<-raster::mask(actual, basin)
  actual[is.na(actual)==T]<-0
  potential<-raster::crop(potential.grd, basin)
  potential<-raster::mask(potential, basin)
  potential[is.na(potential)==T]<-0
  
  #Create filter to estimate sum of perimeter for each cell
  f<-matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
  actual<-focal(actual, f, function(x) 4 - sum(x))*actual
  potential<-focal(potential, f, function(x) 4 - sum(x))*potential
  
  #Create output
  tibble(BasinID, 
         perimeter_actual = cellStats(actual,  sum, na.rm=T), 
         perimeter_potential = cellStats(potential, sum, na.rm=T))
}

#Execute function
p<-mclapply(X = basin.shp$BasinID, 
            FUN=perimeter_fun, 
            mc.cores = detectCores()) %>%
  bind_rows(.)

#4.0 Estimate storage capacity--------------------------------------------------
df<-df %>%
  dplyr::select(basin.id, depth_ditch,area_ditch,volume_ditch,
         depth_wetland,area_wetland,volume_wetland) %>%
  rename(BasinID         = basin.id, 
         depth_actual    = depth_ditch, 
         depth_potential = depth_wetland, 
         area_actual     = area_ditch, 
         area_potential  = area_wetland, 
         volume_actual   = volume_ditch, 
         volume_potential=volume_wetland) %>%
  left_join(., a) %>% left_join(., p)

#Export
write.csv(df, paste0(data_dir, "wetland_info.csv"))
  

