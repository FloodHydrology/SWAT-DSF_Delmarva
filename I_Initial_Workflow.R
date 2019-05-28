#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Demo SWAT-DSF Workflow
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/22/2019
#Purpose: Develop worflow for SWAT-DSF simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#0.0 Psuedo Code----------------------------------------------------------------
#Need to check code where you dich the wetlands.  It feels like something is off, b/c 
#the contemporary simulation results in almost no weltands??? That can't be right. 
#Worst case scenario -- not recreating the HRU's with SWAT is killing us.  Best case, I messed
#something up in section 2.3 below. 

#for now, I"m going to rerun the "everythign is restored scenario" and look at the outputs. 

#1.0 Setup workspace------------------------------------------------------------
#Clear memory
remove(list=ls())

#call required packages
library(data.table)
library(lubridate)
library(tidyverse)


#Define Relevant Directories
scratch_dir<-"C:\\ScratchWorkspace/"
swat_dir<-"C:\\ScratchWorkspace/SWAT_Files/"

#download giw info file
df<-read_csv(paste0(swat_dir, "wetland_info.csv"))

#2.0 Create function to run SWAT-DSF--------------------------------------------

#2.1 Setup Environment----------------------------------------------------------
#Create temp directory
temp_dir<-do.call(paste0, replicate(16, sample(LETTERS, 1, TRUE), FALSE))
temp_dir<-tempfile(temp_dir)
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
setup_fun("run_1","TxtInOut1.zip")
setup_fun("run_2","TxtInOut276.zip")
setup_fun("run_3","TxtInOut286.zip", pause=T)

#2.3 Drain restored wetlands----------------------------------------------------
# #Create change matrix [this will be an input for the function later]
# df<-df %>% 
#   #Select relevant collumns
#   select(BasinID, volume_actual, depth_actual, area_actual) %>%
#   #rename so they match input files
#   rename(BASINID = BasinID, 
#          MAXVOLM3 = volume_actual, 
#          MAXDEPTHMM = depth_actual, 
#          MAXAREAM2 = area_actual)
# 
# #Creatte list of .GIW files in temp folder
# files<-tibble(files = list.files(temp_dir, recursive = T)) %>%
#   filter(str_detect(files, ".giw"))
# 
# #Create function to modify .giw files
# fun<-function(n){
# 
#   #read .giw file in question 
#   giw<-read_table(paste0(temp_dir,"\\",files$files[n])) 
#     
#   #subset df to values in giw tibble
#   temp<-df[df$BASINID %in% giw$BASINID,] 
#   
#   #Conver to long format 
#   giw<-giw %>% gather("var", "value", -GLOBALHRUNUM, -BASINID)
#   temp<- temp %>% gather("var","new_value",-BASINID)
#   
#   #insert collumsn into giw and convert back to wide format
#   giw<-left_join(giw,temp) %>%
#     #Insert new value where appropriate
#     mutate(value = if_else(is.na(new_value), 
#                            value, 
#                            new_value)) %>%
#     #remove new value collumn
#     select(-new_value) %>%
#     #convert to wide format
#     spread(var, value) %>% 
#     #reorder collumns
#     select(GLOBALHRUNUM, LOCALHRUNUM, DRAINTONUM, GIWFLAG, BASINID,
#            MAXVOLM3, MAXDEPTHMM, MAXAREAM2, Volume0.1:Volume2.0, Area0.1:Area2.0)
#  
#   #format giw [there has to be a better way to do this...]
#   giw<-as.data.frame(giw)
#   giw$GLOBALHRUNUM<-str_pad(giw$GLOBALHRUNUM,24, "left")
#   giw$LOCALHRUNUM<-str_pad(giw$LOCALHRUNUM,24, "left")
#   giw$DRAINTONUM<-str_pad(giw$DRAINTONUM,24, "left")
#   giw$GIWFLAG<-str_pad(giw$GIWFLAG,24, "left")
#   giw$BASINID<-str_pad(giw$BASINID,24, "left")
#   for(i in 6:ncol(giw)){
#     giw[,i]<-
#       formatC(giw[,i], format ="e", digits=18) %>% 
#       stringr::str_replace(.,"e","E")
#     }
#   colnames(giw)<-str_pad(colnames(giw),24, "left")
#   
#   #Write table
#   write.table(giw, paste0(temp_dir,"\\",files$files[n]), sep = " ", quote=F, row.names = F)
# }
# 
# #Apply functin
# lapply(seq(1,nrow(files)), fun)
# remove(files)
# 
#2.4 Identify wetland HRUs [exclude drained wetland HRU's for now]--------------
#Creatte list of .GIW files in temp folder
files<-tibble(files = list.files(paste0(temp_dir,"\\run_1"), recursive = T)) %>%
  filter(str_detect(files, ".giw")) %>%
  filter(!str_detect(files, "output"))

#Create function to identify undrained wetlands in each file
fun<-function(n){

  #read .giw file in question
  giw<-read_table(paste0(temp_dir,"\\run_1\\",files$files[n]))

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

#Apply functin
giw<-lapply(seq(1,nrow(files)), fun) %>% bind_rows(.)
remove(files)

#2.5 Execute SWAT simulations---------------------------------------------------
t0<-Sys.time()
setwd(paste0(temp_dir,"\\run_1\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\run_2\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = F)
setwd(paste0(temp_dir,"\\run_3\\TxtInOut\\"))
system2("./swat_dsf.exe", wait = T)

#2.5 Read Output----------------------------------------------------------------
#Read giw outputs
output1<-fread(paste0(temp_dir, "\\run_1\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="1")
output2<-fread(paste0(temp_dir, "\\run_2\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="2")
output3<-fread(paste0(temp_dir, "\\run_3\\TxtInOut\\output.giw")) %>% 
  as_tibble(.) %>%
  mutate(sim="3")
output<-bind_rows(output1,output2,output3) %>% rename(GLOBALHRUNUM = GlobalHRUNumber)
remove(output1, output2, output3)  

#Limit to wetlands [use data.table for merge b/c of size]
output<-as.data.table(output)
setkey(output, GLOBALHRUNUM)
giw<-as.data.table(giw)
setkey(giw, GLOBALHRUNUM)
output<-merge(output, giw, all.x=F)
output<-as_tibble(output)

#Determine mean duration accross the catchments
duration<-output %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWStageMM) %>%
  filter(GIWStageMM>0) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  count(GIWStageMM) %>% select(sim, BASINID, n) %>%
  group_by(BASINID) %>%
  summarise(duration=mean(n, na.rm=T)/8)
  
#Determine area accross each catchment
area<-output %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWSurfaceAreaM2) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  summarise(area=mean(GIWSurfaceAreaM2, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(area=mean(area, na.rm=T))

#Determine surface water export from each wetland
export<-output %>% 
  #organize tibble
  mutate(date = paste0(DOY,"-", Year)) %>%
  select(sim, BASINID, date, GIWSpillageM3) %>%
  #count days of inundation/year for each wetland HRU
  group_by(sim, BASINID) %>%
  summarise(export=sum(GIWSpillageM3, na.rm=T))  %>%
  group_by(BASINID) %>%
  summarise(export=mean(export, na.rm=T)/8)
  
#Now combine!

#2.6 Disconnect temp file-------------------------------------------------------
unlink(temp_dir, recursive = T)
