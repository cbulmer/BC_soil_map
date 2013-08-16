#This (BCSM_training_data)program: 
#... prepares training datasets for input to randomForest as shape (points) files andas  csv

print(paste("Preparing training datasets: ED_",ed_list[i], sep=""))

# Prepare training datasets -----------------------------------------------
            ##create a stack of all (cropped) input grids
            ##create a point shapefile based on every pixel of the habc grid
            ##attach polygon attribute for material type for pure (single component) polygons
            ##add grid values for each point
            ##save the (points) shapefile datasets in the folder "ED_xyzz\\ED_training_data\\MATL"
            ##save a csv of each dataset in "ED_xyzz\\ED_training_data\\MATL"

#Create raster stack of all grids
p <- stack(paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_A_1HA.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_N_B_L.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_SLOPE.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_PL.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_PR.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_US.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\ELEV.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HD_2_CH.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HT_NORM.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HT_STD.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HTNRM_K.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HTSTD_K.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MB_IND.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MDSLP_K.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MID_SLP.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRRTFHA.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRRTFKM.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRVBFHA.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRVBFKM.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\OPENNEG.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\OPENPOS.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\RHSP_HA.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\RHSP_KM.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SL_HT_K.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPE.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPEHT.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPEUS.asc",sep=""), 
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\V_D_C_N.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\VALLY_D.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\VY_DP_K.asc",sep=""),
           paste(dir_name,"\\ED_data\\grids\\topography_1ha\\WETSAGA.asc",sep=""))

names(p)

nlayers(p)
#*********************


#Create training point data set: Soil_Polygons_CanSIS_matl
if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_CanSIS_matl.shp",sep=""))){
  rsaga.geoprocessor("shapes_grid", "Clip Grid with Polygon", list(INPUT=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep=""),POLYGONS=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_CanSIS_matl.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_CanSIS_matl.sgrd",sep="")))
  
  rsaga.grid.to.points(in.grid=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_CanSIS_matl.sgrd",sep=""),out.shapefile=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_CanSIS_matl.shp",sep=""))
    
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_CanSIS_matl.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_CanSIS_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_CanSIS_matl_atts.shp",sep=""),FIELD="MATERIAL"))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_CanSIS_matl_atts.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_CanSIS_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_CanSIS_matl_atts.shp",sep=""),FIELD="MAT_CODE"))
  
  t <- shapefile(paste(dir_name,"\\ED_training_data\\MATL\\points_CanSIS_matl_atts.shp",sep=""))
  
  v <- extract(p,t,sp=TRUE)
  
  #writeOGR(v,paste(dir_name,"\\ED_training_data\\MATL",sep=""), layer = "points_CanSIS_matl_atts","ESRI Shapefile",overwrite=TRUE)
  
  w <- as.data.frame(v)
  
  w <- subset(w, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))
  
  write.csv(w,file=paste(dir_name,"\\ED_training_data\\MATL\\Training_CanSIS_matl.csv",sep=""))
  rm(t,v,w)
} else {
  return
}
#*********************


#Create training point data set: Soil_Polygons_Seamless_matl
if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_Seamless_matl.shp",sep=""))){
  rsaga.geoprocessor("shapes_grid", "Clip Grid with Polygon", list(INPUT=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep=""),POLYGONS=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_Seamless_matl.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_Seamless_matl.sgrd",sep="")))
  
  rsaga.grid.to.points(in.grid=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_Seamless_matl.sgrd",sep=""),out.shapefile=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_Seamless_matl.shp",sep=""))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_Seamless_matl.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_Seamless_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_Seamless_matl_atts.shp",sep=""),FIELD="MATERIAL"))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_Seamless_matl_atts.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_Seamless_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_Seamless_matl_atts.shp",sep=""),FIELD="MAT_CODE"))
  
  t <- shapefile(paste(dir_name,"\\ED_training_data\\MATL\\points_Seamless_matl_atts.shp",sep=""))
  
  v <- extract(p,t,sp=TRUE)
  
  #writeOGR(v,paste(dir_name,"\\ED_training_data\\MATL",sep=""), layer = "points_Seamless_matl_atts","ESRI Shapefile",overwrite=TRUE)
  
  w <- as.data.frame(v)
  
  w <- subset(w, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))
  
  write.csv(w,file=paste(dir_name,"\\ED_training_data\\MATL\\Training_Seamless_matl.csv",sep=""))
  rm(t,v,w)
} else {
  return
}
#*********************


#Create training point data set: Soil_Polygons_SoBC_matl
if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_SoBC_matl.shp",sep=""))){
  rsaga.geoprocessor("shapes_grid", "Clip Grid with Polygon", list(INPUT=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep=""),POLYGONS=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_SoBC_matl.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_SoBC_matl.sgrd",sep="")))
  
  rsaga.grid.to.points(in.grid=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_SoBC_matl.sgrd",sep=""),out.shapefile=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_SoBC_matl.shp",sep=""))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_SoBC_matl.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_SoBC_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_SoBC_matl_atts.shp",sep=""),FIELD="MATERIAL"))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_SoBC_matl_atts.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_SoBC_pure_pgm.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_SoBC_matl_atts.shp",sep=""),FIELD="MAT_CODE"))
  
  t <- shapefile(paste(dir_name,"\\ED_training_data\\MATL\\points_SoBC_matl_atts.shp",sep=""))
  
  v <- extract(p,t,sp=TRUE)
  
  #writeOGR(v,paste(dir_name,"\\ED_training_data\\MATL",sep=""), layer = "points_SoBC_matl_atts","ESRI Shapefile",overwrite=TRUE)
  
  w <- as.data.frame(v)
  
  w <- subset(w, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))
  
  write.csv(w,file=paste(dir_name,"\\ED_training_data\\MATL\\Training_SoBC_matl.csv",sep=""))
  rm(t,v,w)
} else {
  return
}
#*********************


#Create training point data set: ST_20K_matl
if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\ST_20K_matl.shp",sep=""))){
  rsaga.geoprocessor("shapes_grid", "Clip Grid with Polygon", list(INPUT=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep=""),POLYGONS=paste(dir_name,"\\ED_training_data\\MATL\\ST_20K_matl.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_20K_matl.sgrd",sep="")))
  
  rsaga.grid.to.points(in.grid=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_20K_matl.sgrd",sep=""),out.shapefile=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_20K_matl.shp",sep=""))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_20K_matl.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_20K.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_20K_matl_atts.shp",sep=""),FIELD="MATERIAL"))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_20K_matl_atts.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_20K.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_20K_matl_atts.shp",sep=""),FIELD="MAT_CODE"))
  
  t <- shapefile(paste(dir_name,"\\ED_training_data\\MATL\\points_ST_20K_matl_atts.shp",sep=""))
  
  v <- extract(p,t,sp=TRUE)
  
  #writeOGR(v,paste(dir_name,"\\ED_training_data\\MATL",sep=""), layer = "points_ST_20K_matl_atts","ESRI Shapefile",overwrite=TRUE)
  
  w <- as.data.frame(v)
  
  w <- subset(w, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))
  
  write.csv(w,file=paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_20K_matl.csv",sep=""))
  rm(t,v,w)
} else {
  return
}
#*********************


#Create training point data set: ST_GT20K_matl
if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\ST_GT20K_matl.shp",sep=""))){
  rsaga.geoprocessor("shapes_grid", "Clip Grid with Polygon", list(INPUT=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep=""),POLYGONS=paste(dir_name,"\\ED_training_data\\MATL\\ST_GT20K_matl.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_GT20K_matl.sgrd",sep="")))
  
  rsaga.grid.to.points(in.grid=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_GT20K_matl.sgrd",sep=""),out.shapefile=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_GT20K_matl.shp",sep=""))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\xTemp\\points_ST_GT20K_matl.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_GT20K.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_GT20K_matl_atts.shp",sep=""),FIELD="MATERIAL"))
  
  rsaga.geoprocessor("shapes_points","Add Polygon Attributes to Points",list(INPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_GT20K_matl_atts.shp",sep=""),POLYGONS=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_GT20K.shp",sep=""),OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\points_ST_GT20K_matl_atts.shp",sep=""),FIELD="MAT_CODE"))
  
  t <- shapefile(paste(dir_name,"\\ED_training_data\\MATL\\points_ST_GT20K_matl_atts.shp",sep=""))
  
  v <- extract(p,t,sp=TRUE)
  
  #writeOGR(v,paste(dir_name,"\\ED_training_data\\MATL",sep=""), layer = "points_ST_GT20K_matl_atts","ESRI Shapefile",overwrite=TRUE)
  
  w <- as.data.frame(v)
  
  w <- subset(w, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))
  
  write.csv(w,file=paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_GT20K_matl.csv",sep=""))
  rm(t,v,w)
} else {
  return
}
#*********************

do.call(file.remove,list(list.files(paste(dir_name,"\\ED_training_data\\MATL\\xTemp",sep=""),full.names=TRUE)))

gc()


#proc.time() - ptm