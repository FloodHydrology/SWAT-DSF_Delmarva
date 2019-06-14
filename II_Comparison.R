#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Restored vs Baseline Scenarios
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 6/13/2019
#Purpose: Compare restored and baseline model simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1.0 Setup workspace------------------------------------------------------------
#Clear memory
remove(list=ls())

#call required packages
library(tidyverse)
library(lubridate)
library(data.table)

#Define Relevant Directories
scratch_dir<-"C:\\ScratchWorkspace\\"
swat_dir<-"C:\\ScratchWorkspace/SWAT_Files/"

#download giw info file
df<-read_csv(paste0(swat_dir, "wetland_info.csv"))

#2.0 Setup temp directory to execute SWAT---------------------------------------
#2.1 Setup Environment----------------------------------------------------------
#Create temp directory
temp_dir<-paste0(scratch_dir, "GWAT\\")
dir.create(temp_dir)

#Copy 7zip exacutable into temp directory
file.copy("C:\\Program Files/7-Zip/7z.exe", 
          temp_dir, 
          overwrite = T)

#2.2 Arrange simulation files---------------------------------------------------
#Create setup_fun to unzip TxtInOut files and copy SWAT_Executable 
setup_fun<-function(new_dir,io_zip, pause=F){
  #Set working dir and create TxtInOut file
  setwd(temp_dir)
  dir.create(new_dir)
  dir.create(paste0(new_dir,"/TxtInOut"))
  
  #Unip txt in out file
  system(paste0(
    #Call 7zip executable
    "7z.exe",
    #Give execute command
    " e ",
    #Point to zip file
    swat_dir, 
    #Point to specific zip file
    io_zip,
    #Export to generic TxtInOutFile
    " -o", temp_dir,"\\",new_dir,"\\TxtInOut\\"), 
    wait=pause)
  
  #Copy Exacutable
  file.copy(from = paste0(swat_dir,"swat11052018WriteGIWOutput.exe"), 
            to = paste0(temp_dir,"\\",new_dir,"\\TxtInOut\\swat_dsf.exe"), 
            overwrite = T)
}

#Execute setup_fun
setup_fun("baseline_1","baseline_simulations\\TxtInOut1.zip")
setup_fun("baseline_2","baseline_simulations\\TxtInOut276.zip")
setup_fun("baseline_3","baseline_simulations\\TxtInOut286.zip")
setup_fun("potential_1","potential_simulations\\TxtInOut1.zip")
setup_fun("potential_2","potential_simulations\\TxtInOut276.zip")
setup_fun("potential_3","potential_simulations\\TxtInOut286.zip", pause=T)

#3.0 Execute SWAT Simulations---------------------------------------------------
setwd(paste0(temp_dir,"\\baseline_1\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\baseline_2\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\baseline_3\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\potential_1\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\potential_2\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\potential_3\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = T)

#4.0 Download and organzie data-------------------------------------------------
#reset working directory
setwd(scratch_dir)

#4.1 Organzie baseline files----------------------------------------------------
#create function to identify undrained GIWs in each simulation
fun<-function(n){
  
  #read .giw file in question
  giw<-read_table(paste0(temp_dir,"\\baseline_1\\",files$files[n]))
  
  #subset df to values in giw tibble
  temp<-df %>% select(BasinID, depth_potential) %>% rename(BASINID = BasinID)
  
  #left join temp and df to add "depth_potential" info
  giw<-left_join(giw, temp, by="BASINID")
  
  #Remove ditched GIWs
  giw<-giw %>%
    filter(MAXDEPTHMM==(depth_potential*1000)) %>%
    select(GLOBALHRUNUM, BASINID)
  
  #Export tibble
  giw
}

#apply function
files<-tibble(files = list.files(paste0(temp_dir,"baseline_1"), recursive = T)) %>%
  filter(str_detect(files, ".giw")) %>%
  filter(!str_detect(files, "output"))
giw_baseline<-lapply(seq(1,nrow(files)), fun) %>% bind_rows(.)
remove(files)

#gather info from baseline sim
baseline1<-read_csv(paste0(temp_dir, "baseline_1\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="1")
baseline2<-read_csv(paste0(temp_dir, "baseline_2\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="2")
baseline3<-read_csv(paste0(temp_dir, "baseline_3\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="3")
baseline<-bind_rows(baseline1,baseline2,baseline3) %>% rename(GLOBALHRUNUM = GlobalHRUNumber)
remove(baseline1, baseline2, baseline3)  

#Limit to wetlands [use data.table for merge b/c of size]
baseline<-as.data.table(baseline)
setkey(baseline, GLOBALHRUNUM)
giw_baseline<-as.data.table(giw_baseline)
setkey(giw_baseline, GLOBALHRUNUM)
baseline<-merge(baseline, giw_baseline, all.x=F)
baseline<-as_tibble(baseline)

#4.2 Organzie potential files---------------------------------------------------
#create function to identify undrained GIWs in each simulation
fun<-function(n){
  
  #read .giw file in question
  giw<-read_table(paste0(temp_dir,"\\potential_1\\",files$files[n]))
  
  #subset df to values in giw tibble
  temp<-df %>% select(BasinID, depth_potential) %>% rename(BASINID = BasinID)
  
  #left join temp and df to add "depth_potential" info
  giw<-left_join(giw, temp, by="BASINID")
  
  #Remove ditched GIWs
  giw<-giw %>%
    filter(MAXDEPTHMM==(depth_potential*1000)) %>%
    select(GLOBALHRUNUM, BASINID)
  
  #Export tibble
  giw
}

#apply function
files<-tibble(files = list.files(paste0(temp_dir,"potential_1"), recursive = T)) %>%
  filter(str_detect(files, ".giw")) %>%
  filter(!str_detect(files, "output"))
giw_potential<-lapply(seq(1,nrow(files)), fun) %>% bind_rows(.)
remove(files)

#gather info from potential sim
potential1<-read_csv(paste0(temp_dir, "potential_1\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="1")
potential2<-read_csv(paste0(temp_dir, "potential_2\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="2")
potential3<-read_csv(paste0(temp_dir, "potential_3\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="3")
potential<-bind_rows(potential1,potential2,potential3) %>% rename(GLOBALHRUNUM = GlobalHRUNumber)
remove(potential1, potential2, potential3)  

#Limit to wetlands [use data.table for merge b/c of size]
potential<-as.data.table(potential)
setkey(potential, GLOBALHRUNUM)
giw_potential<-as.data.table(giw_potential)
setkey(giw_potential, GLOBALHRUNUM)
potential<-merge(potential, giw_potential, all.x=F)
potential<-as_tibble(potential) %>% rename(GLOBALHRUNUM = GlobalHRUNumber)
remove(potential1, potential2, potential3)  

#4.3 Save backup----------------------------------------------------------------
save.image("backup.RData")

#5.0 Exploratory Analysis-------------------------------------------------------
#5.1 Estimate change metrics----------------------------------------------------
df<-df %>%
  mutate(area_diff        = area_potential      - area_actual, 
         volume_diff      = volume_potential    - volume_actual,
         perimeter_diff   = perimeter_potential - perimeter_actual) %>%
  select(BasinID, area_diff, volume_diff, perimeter_diff)

#5.1 Area anlaysis--------------------------------------------------------------
#Estimate dynamic mean area accross the simulations
area_baseline<-baseline %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWSurfaceAreaM2) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  summarise(area=mean(GIWSurfaceAreaM2, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(area=mean(area, na.rm=T))

area_potential<-potential %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWSurfaceAreaM2) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  summarise(area=mean(GIWSurfaceAreaM2, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(area=mean(area, na.rm=T))

area_dynamic<-
  left_join(area_potential %>% rename(area_potential=area), 
            area_baseline %>% rename(area_baseline=area)) %>%
  mutate(area_baseline = if_else(is.na(area_baseline), 
                                 0, 
                                 area_baseline)) %>%
  filter(area_potential>100) %>%
  mutate(mean_area_diff = area_potential-area_baseline) %>%
  select(BASINID, mean_area_diff)

#Combine
area<-left_join(area_dynamic, df%>%rename(BASINID=BasinID)) 

#5.2 Inundation Duration Anlaysis-----------------------------------------------
#Estimate inundation duration accross the simulations
duration_baseline<-baseline %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWStageMM) %>%
  filter(GIWStageMM>0) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  count(GIWStageMM) %>% select(sim, BASINID, n) %>%
  group_by(BASINID) %>%
  summarise(duration=sum(n, na.rm=T)/8)

duration_potential<-potential %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWStageMM) %>%
  filter(GIWStageMM>0) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  count(GIWStageMM) %>% select(sim, BASINID, n) %>%
  group_by(BASINID) %>%
  summarise(duration=sum(n, na.rm=T)/8)

duration<-
  left_join(duration_potential %>% rename(duration_potential= duration), 
            duration_baseline  %>% rename(duration_baseline = duration)) %>%
  mutate(duration_baseline = if_else(is.na(duration_baseline), 
                                 0, 
                                 duration_baseline)) %>%
  filter(duration_potential>30) %>%
  mutate(diff_duration = duration_potential-duration_baseline) %>%
  select(BASINID, diff_duration)

#Combine static and dynamic measures
duration<-left_join(duration, df%>%rename(BASINID=BasinID)) 

#5.3 Residence Time-------------------------------------------------------------
tr_baseline<-baseline %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWStageMM, GIWVolumeM3, GIWSpillageM3, LatqM3, GIWEvapOutM3) %>%
  filter(GIWStageMM>0) %>%
  #Estimate measure of mean tr for each data
  mutate(tr_daily = GIWVolumeM3/(GIWSpillageM3+LatqM3+GIWEvapOutM3)) %>%
  group_by(sim, BASINID) %>%
  summarise(tr_daily=mean(tr_daily, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(tr_daily=mean(tr_daily, na.rm=T))

tr_potential<-potential %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWStageMM, GIWVolumeM3, GIWSpillageM3, LatqM3, GIWEvapOutM3) %>%
  filter(GIWStageMM>0) %>%
  #Estimate measure of mean tr for each data
  mutate(tr_daily = GIWVolumeM3/(GIWSpillageM3+LatqM3+GIWEvapOutM3)) %>%
  group_by(sim, BASINID) %>%
  summarise(tr_daily=mean(tr_daily, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(tr_daily=mean(tr_daily, na.rm=T))

tr<-left_join(tr_potential %>% rename(tr_potential = tr_daily), 
              tr_baseline  %>% rename(tr_baseline  = tr_daily)) %>%
  mutate(tr_baseline = if_else(is.na(tr_baseline), 
                                     0, 
                               tr_baseline)) %>%
  filter(tr_potential!=Inf) %>%
  mutate(diff_tr = tr_potential-tr_baseline) %>%
  select(BASINID, diff_tr)

#Combine static and dynamic measures
tr<-left_join(tr, df%>%rename(BASINID=BasinID)) 

#6.0 Some plotting--------------------------------------------------------------
#6.1 Dynamic vs static restoration estimates------------------------------------
#Setup plotting space
par(mfrow=c(3,3))
par(mar=c(3,3.2,0.35,0.25))
par(mgp=c(1.4,0.5,0)) 
par(ps=12)
par(cex.lab=14♦/12)
par(cex.axis=10/12)

#area plots
plot(area$mean_area_diff~area$area_diff, pch=19, col="dark red", ylab = "Change in Mean Wetland Area", xlab="Change in Max Wetland Area")
  abline(h=0, lty=2, col="grey30", lwd=2)
  abline(v=0, lty=2, col="grey30", lwd=2)

plot(area$mean_area_diff~area$volume_diff, pch=19, col="dark red", ylab = "Change in Mean Wetland Area", xlab="Change in Storage Capacity")
  abline(h=0, lty=2, col="grey30", lwd=2)
  abline(v=0, lty=2, col="grey30", lwd=2)

plot(area$mean_area_diff~area$perimeter_diff, pch=19, col="dark red", ylab = "Change in Mean Wetland Area", xlab="Change in Shorline Length")
  abline(h=0, lty=2, col="grey30", lwd=2)
  abline(v=0, lty=2, col="grey30", lwd=2)

#duration plots
plot(duration$diff_duration~duration$area_diff, pch=19, col="dark blue", ylab = "Change in Hydroperiod", xlab="Change in Max Wetland Area")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)

plot(duration$diff_duration~duration$volume_diff, pch=19, col="dark blue", ylab = "Change in Hydroperiod", xlab="Change in Storage Capacity")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)

plot(duration$diff_duration~duration$perimeter_diff, pch=19, col="dark blue", ylab = "Change in Hydroperiod", xlab="Change in Shorline Length")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)


#residence time plots
plot(tr$diff_tr~tr$area_diff, pch=19, col="dark green", ylab = "Change in Res Time", xlab="Change in Max Wetland Area", log="y")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)

plot(tr$diff_tr~tr$volume_diff, pch=19, col="dark green", ylab = "Change in Res Time", xlab="Change in Storage Capacity", log="y")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)

plot(tr$diff_tr~tr$perimeter_diff, pch=19, col="dark green", ylab = "Change in Res Time", xlab="Change in Shorline Length",  log="y")
abline(h=0, lty=2, col="grey30", lwd=2)
abline(v=0, lty=2, col="grey30", lwd=2)



