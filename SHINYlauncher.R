# If you want to run this app without the shiny UI, run 'Rlauncher.R'
# To start the application:
# # Load the 'BC_script.Rproj' project file into RStudio by selecting
#   the project in the top right corner of the RStudio interface
# # Check to be sure the project is loaded from the correct drive

# Run this block to initiate the shiny launcher ----
library(shiny)
dir_script <- paste(strsplit(getwd(),":/BC_soil_map/BC_script",fixed=TRUE),":\\BC_soil_map\\BC_script",sep="")
runApp(dir_script)