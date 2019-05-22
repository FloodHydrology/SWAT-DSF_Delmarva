#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Demo SWAT-DSF Workflow
#Coder: C. Nathan Jones (cnjones7@ua.edu)
#Date: 5/22/2019
#Purpose: Develop worflow for SWAT-DSF simulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#0.0 Psuedo Code----------------------------------------------------------------
# -create temporary directory
# -Unzip TxtInOut files associated with each model
# -"Restore" wetlands by changing max area and max volume of individual wetlands in .GIW files
# -Run behavioral model 1
# -Run behavioral model 2
# -Run behavioral model 3
# -summarize results
# -archive model output
# -export results summary


#1.0 Setup workspace------------------------------------------------------------
#Clear memory
remove(list=ls())
scratch_dir<-"C:\\ScratchWorkspace/"
swat_dir<-"C:\\ScratchWorkspace/SWAT_Files/"

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

#2.3 Change Input files---------------------------------------------------------
#This is still TBD. Need to develop method to systemeatically change 
#files.  Some thoughts...maybe a long walk required. 

#2.4 Execute SWAT simulations---------------------------------------------------
t0<-Sys.time()
setwd(paste0(temp_dir,"\\run_1\\TxtInOut\\"))
system("swat_dsf.exe", wait = T)
setwd(paste0(temp_dir,"\\run_1\\TxtInOut\\"))
system("swat_dsf.exe", wait = T)
setwd(paste0(temp_dir,"\\run_1\\TxtInOut\\"))
system("swat_dsf.exe", wait = T)
tf<-Sys.time()
tf-t0

#2.5 Read Output----------------------------------------------------------------
#This is still TBD. Need to develop method to systemeatically change 
#files.  Some thoughts...maybe a long walk required. 

#2.6 Disconnect temp file-------------------------------------------------------
unlink(temp_dir, recursive = T)