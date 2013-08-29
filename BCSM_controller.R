# This controller will run sourced scripts that depend on
# objects created in 'SHINYlauncher.R' or 'Rlauncher.R'

# Setup the environment ----
load("obs.RData", envir=.GlobalEnv)
#*****************************

# Non-loop controller ----
if (run_pureUpdt==TRUE) {
  print("Updating pure soil datasets...")
  source(paste(dir_script,"\\pure_updt.R",sep=""),echo=TRUE)
} else {return}

# Loop Controller ----
if (length(ed_list)==0) {return} else {
  for(i in 1:length(ed_list)){
       
  # Create looped objects
  dir_name <- paste(dir_Proj,"\\BC_ecodistricts\\ED_",ed_list[i],sep="")
  #***********************
       
  # Source selected scripts
  if (run_mlayer==TRUE) {
    print(paste("Running map layer script for ED_",ed_list[i],sep=""))
    source(paste(dir_script,"\\BCSM_map_layers.R",sep=""),echo=TRUE)
  } else {return}
       
  if (run_td==TRUE) {
    print(paste("Running training data script for ED_",ed_list[i],sep=""))
    source(paste(dir_script,"\\BCSM_training_data.R",sep=""),echo=TRUE)
  } else {return}
  
  if (run_rf==TRUE) {
    print(paste("Running randomForest script for ED_",ed_list[i],sep=""))
    source(paste(dir_script,"\\BCSM_rf.R",sep=""),echo=TRUE)
  } else {return}
  
  if (run_map==TRUE) {
    print(paste("Running map output script for ED_",ed_list[i],sep=""))
    source(paste(dir_script,"\\BCSM_maps.R",sep=""),echo=TRUE)
  }   
  
  sessionInfo()
}}