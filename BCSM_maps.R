#This (BCSM_maps) program: 
#... retrieves gridded maps of predicted materials for a selected EcoDistrict ... 
#... calculates the mode for each cell
#... prepares a new map with the amalgamated prediction based on all of the maps

print(paste("Preparing provincial map: mosaic ED_",ed_list[i], sep=""))

#... Set up output directories
if (file.exists(paste(dir_Proj,"\\BC_maps\\MATL",sep=""))){return} else{
  dir.create(paste(dir_Proj,"\\BC_maps\\MATL",sep=""))
}

if (file.exists(paste(dir_Proj,"\\BC_maps\\MATL\\Ecodistrict_maps",sep=""))){return} else{
  dir.create(paste(dir_Proj,"\\BC_maps\\MATL\\Ecodistrict_maps",sep=""))
}

# etc
# etc

if (file.exists(".\\ED_maps\\MATL\\MATRF_all_bal.asc")){
  r <- raster(paste(dir_name,"\\ED_data\\grids\\one_habc.asc",sep=""))
  q <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_bal.asc",sep=""))
  qc <- crop(q,r)
  qm <- mask(qc,r)
  writeRaster(qm,filename=paste(dir_Proj,"\\BC_maps\\MATL\\Ecodistrict_maps\\MATRF_all_bal_ED_",ed_list[i],".asc",sep=""),format="ascii",overwrite=TRUE)
  image(qm)
} else {
  sink(paste(dir_Proj,"\\BC_maps\\MATL\\EcoDistrict_maps\\Could_not_find_MATRF_all_bal_ED_",ed_list[i],".txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a map file with training data for MATRF_all_bal")
  sink() 
  return
}

if (file.exists(".\\ED_maps\\MATL\\MATRF_all_con.asc")){
  r <- raster(paste(dir_name,"\\ED_data\\grids\\one_habc.asc",sep=""))
  p <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_con.asc",sep=""))
  pc <- crop(p,r)
  pm <- mask(pc,r)
  writeRaster(pm,filename=paste(dir_Proj,"\\BC_maps\\MATL\\Ecodistrict_maps\\MATRF_all_con_ED_",ed_list[i],".asc",sep=""),format="ascii",overwrite=TRUE)
  image(pm)
} else {
  sink(paste(dir_Proj,"\\BC_maps\\MATL\\EcoDistrict_maps\\Could_not_find_MATRF_all_con_ED_",ed_list[i],".txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a map file with training data for MATRF_all_con")
  sink() 
  return
}

rm(r,q,qc,qm,p,pc,pm)
setwd(dir_script)