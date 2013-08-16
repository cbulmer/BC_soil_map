# This is a manual launcher for the controller script

# Load libraries ----
library(foreign)
library(RSAGA)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(randomForest)
#********************

# Create and save manual objects ----

dir_Proj <- paste(strsplit(getwd(),"/BC_soil_map/BC_script",fixed=TRUE,perl=FALSE,useBytes=FALSE),"\\BC_soil_map",sep="") # Change project drive
dir_script <- paste(dir_Proj,"\\BC_script",sep="") # Change script drive
dir_data <- paste("Y:\\BC_soil_map\\BC_data",sep="") # Change data drive
ed <- readOGR(paste(dir_data,"\\BC_shapes",sep=""),layer="EcoDistricts_BC")
ed_all <- ed$ECODISTRIC
ed_list <- ed_all[1] # NULL for debugging or bypassing loop
run_pureUpdt <- FALSE
run_mlayer <- FALSE
run_td <- TRUE
run_rf <- TRUE
run_map <- TRUE

save(list=ls(all.names=TRUE, pos=parent.frame()), envir=parent.frame(), file="obs.RData")
#************************************

# Source controller script ----
print("Running BCSM controller...")
source(paste(dir_script,"\\BCSM_controller.R",sep=""),echo=TRUE)