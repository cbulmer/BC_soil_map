#This (BCSM_map_layers)program: 
#... creates a new directory / subdirector tree for a single EcoDistrict 
#... creates a new shapefile with the buffered ecodistrict boundary 
#... clips data layers for a series of map layers to the buffered ecodistrict boundary 
#... clips a series of raster datasets for attributes
 
# Create the directories ----
if (file.exists(dir_name)){return} else{
  dir.create(dir_name)
}

if (file.exists(paste(dir_name,"\\_Resources",sep=""))){return} else{
  dir.create(paste(dir_name,"\\_Resources",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\grids",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\grids",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\grids\\climate_1ha",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\grids\\climate_1ha",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\grids\\topography_1ha",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\grids\\topography_1ha",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\grids\\vegetation_1ha",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\grids\\vegetation_1ha",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\grids\\x_other_grids_ha",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\grids\\x_other_grids_ha",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\boundaries",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\boundaries",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\geology",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\geology",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\grids&tiles",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\grids&tiles",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\infrastructure",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\infrastructure",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\soils",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\soils",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\land_cover",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\land_cover",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\vegetation",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\vegetation",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_data\\shapes\\water",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_data\\shapes\\water",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_maps",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_maps",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_maps\\MATL",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_maps\\MATL",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_models",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_models",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_models\\MATL",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_models\\MATL",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_script",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_script",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_training_data",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_training_data",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_training_data\\MATL",sep=""))
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\xTemp",sep=""))){return} else{
  dir.create(paste(dir_name,"\\ED_training_data\\MATL\\xTemp",sep=""))
}
#*********************

print(paste("Extracting geographic data: ED_",ed_list[i], sep=""))

# Extract and clip shapes ----
 
sub_ed <- ed[ed$ECODISTRIC==ed_list[i],]
plot(sub_ed)
writeOGR(sub_ed,paste(dir_name,"\\ED_data\\shapes",sep=""), layer = "ecodistrict_bdy","ESRI Shapefile",overwrite=TRUE)

#Create a buffered EcoDistrict Boundary file
sub_edb <- gBuffer(sub_ed,width=5000,quadsegs=5,byid=TRUE)
d<-as.data.frame("edb")
rownames(d)<-row.names(sub_edb)
sub_edb <- SpatialPolygonsDataFrame(sub_edb,d)
plot(sub_edb)
writeOGR(sub_edb,paste(dir_name,"\\ED_data\\shapes",sep=""), layer = "ecodistrict_bdy_b","ESRI Shapefile",overwrite=TRUE)
#*********************


#Clip (Polygon) Shape files to buffered EcoDistrict boundary: map layers
rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_water\\BC_lakes_fwa.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\water\\BC_lakes_fwa.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_water\\BC_rivers_fwa.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\water\\BC_rivers_fwa.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_water\\BC_wetlands_fwa.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\water\\BC_wetlands_fwa.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_water\\BC_glaciers_fwa.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\water\\BC_glaciers_fwa.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_vegetation\\BEC_zones.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\vegetation\\BEC_zones.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_vegetation\\Grasslands_May2009.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\vegetation\\BEC_zonesGrasslands_May2009.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_geology\\Mathews_phys_areas_BC.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\geology\\Mathews_phys_areas_BC.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_geology\\BC_geol_quaternary.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\geology\\BC_geol_quaternary.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_geology\\BC_geol_quaternaryGeologyEcoGeoClasses.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\geology\\GeologyEcoGeoClasses.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))
#**************************


#Clip (Polygon) Shape files to EcoDistrict boundary: soil data
rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_Seamless_2012_11.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\soils\\Soil_Polygons_CanSIS_2012_11.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_CanSIS_2012_11.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\soils\\Soil_Polygons_Seamless_2012_11.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_SLC_2012_11.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\soils\\Soil_Polygons_SLC_2012_11.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_SoBC_2012_11.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\soils\\Soil_Polygons_SoBC_2012_11.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Short_Table.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_data\\shapes\\soils\\Soil_Short_Table.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))
#*******************************


#Clip (Polygon) Shape files to buffered EcoDistrict boundary: Training data
rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_CanSIS_pure_pgm.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_CanSIS_matl.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_Seamless_pure_pgm.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_Seamless_matl.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\Soil_Polygons_SoBC_pure_pgm.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\Soil_Polygons_SoBC_matl.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_20K.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\ST_20K_matl.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))

rsaga.geoprocessor("shapes_polygons","Polygon Clipping",list(S_INPUT=paste(dir_data,"\\BC_shapes\\BC_soils\\ST_pure_GT20K.shp",sep=""),CLIP=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),S_OUTPUT=paste(dir_name,"\\ED_training_data\\MATL\\ST_GT20K_matl.shp",sep=""),M_INPUT=NULL,MULTIPLE=FALSE))
#*******************************


#Clip (Line) Shape files to buffered EcoDistrict boundary
rsaga.geoprocessor("shapes_lines","Line-Polygon Intersection",list(LINES=paste(dir_data,"\\BC_shapes\\BC_infrastructure\\BC_main_roads_nrn.shp",sep=""),POLYGONS=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),INTERSECT=paste(dir_name,"\\ED_data\\shapes\\infrastructure\\BC_main_roads_nrn.shp",sep=""),METHOD=1))

rsaga.geoprocessor("shapes_lines","Line-Polygon Intersection",list(LINES=paste(dir_data,"\\BC_shapes\\BC_infrastructure\\Forest_Service_Roads.shp",sep=""),POLYGONS=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),INTERSECT=paste(dir_name,"\\ED_data\\shapes\\infrastructure\\Forest_Service_Roads.shp",sep=""),METHOD=1))

rsaga.geoprocessor("shapes_lines","Line-Polygon Intersection",list(LINES=paste(dir_data,"\\BC_shapes\\BC_infrastructure\\Active_Forest_Road_Sections.shp",sep=""),POLYGONS=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),INTERSECT=paste(dir_name,"\\ED_data\\shapes\\infrastructure\\Active_Forest_Road_Sections.shp",sep=""),METHOD=1))

rsaga.geoprocessor("shapes_lines","Line-Polygon Intersection",list(LINES=paste(dir_data,"\\BC_shapes\\BC_water\\Stream_network_3_10.shp",sep=""),POLYGONS=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),INTERSECT=paste(dir_name,"\\ED_data\\shapes\\water\\Stream_network_3_10.shp",sep=""),METHOD=1))
#*************************


#Clip (point) Shape files to buffered EcoDistrict map boundary
rsaga.geoprocessor("shapes_points","Clip Points with Polygons",list(POINTS=paste(dir_data,"\\BC_shapes\\BC_infrastructure\\BC_Major_cities.shp",sep=""),POLYGONS=paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""),CLIPS=paste(dir_name,"\\ED_data\\shapes\\infrastructure\\BC_Major_cities.shp",sep=""),METHOD=1))
#**************************


##Clip grid files to EcoDistrict map boundary -----------------------------
##load constant raster of '1' (one_habc) to get provincial extent of habc grid
##load provincial raster for each topopgraphic, climatic or other covariate
##load buffered ecodistrict shapefile
#crop grid with (buffered) ecodistrict shapefile
##multiply result with one_habc_b (buffered) 
##write the result to file in the folder "ED_xyzz\\grids\\"

one_habc_prov <- raster(paste(dir_data,"\\BC_grids\\one_habc.sdat",sep=""))
image(one_habc_prov,main=paste("EcoDistrict_",ed_list[i],sep=""))
s <- shapefile(paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy.shp",sep=""))
s_b <- shapefile(paste(dir_name,"\\ED_data\\shapes\\ecodistrict_bdy_b.shp",sep=""))
plot(s,add=TRUE)
plot(s_b,add=TRUE)
one_habc_b <- crop(one_habc_prov,s_b)
one_habc_b <- mask(one_habc_b,s_b)
one_habc <- crop(one_habc_b,s)
one_habc <- mask(one_habc,s)
writeRaster(one_habc_b,filename=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.asc",sep=""),format="ascii",overwrite=TRUE)
rsaga.esri.to.sgrd(list(in.grid=paste(dir_name,"\\ED_data\\grids\\one_HABC_b",sep=""),out.sgrd=paste(dir_name,"\\ED_data\\grids\\one_HABC_b.sgrd",sep="")))
writeRaster(one_habc,filename=paste(dir_name,"\\ED_data\\grids\\one_HABC.asc",sep=""),format="ascii",overwrite=TRUE)
rsaga.esri.to.sgrd(list(in.grid=paste(dir_name,"\\ED_data\\grids\\one_HABC",sep=""),out.sgrd=paste(dir_name,"\\ED_data\\grids\\one_HABC.sgrd",sep="")))
rm(s,s_b)
image(one_habc_b)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\C_A_1HA.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_A_1HA.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\C_N_B_L.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_N_B_L.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\C_SLOPE.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\C_SLOPE.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\CURV.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\CURV_PL.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_PL.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\CURV_PR.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_PR.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\CURV_US.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\CURV_US.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\ELEV.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\ELEV.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HD_2_CH.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HD_2_CH.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HILL_SH.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HILL_SH.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HT_NORM.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HT_NORM.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HT_STD.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HT_STD.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HTNRM_K.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HTNRM_K.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\HTSTD_K.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\HTSTD_K.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MB_IND.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MB_IND.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MDSLP_K.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MDSLP_K.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MID_SLP.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MID_SLP.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MRRTFHA.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRRTFHA.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MRRTFKM.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRRTFKM.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MRVBFHA.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRVBFHA.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\MRVBFKM.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\MRVBFKM.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\OPENNEG.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\OPENNEG.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\OPENPOS.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\OPENPOS.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\RHSP_HA.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\RHSP_HA.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\RHSP_KM.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\RHSP_KM.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\SL_HT_K.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SL_HT_K.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\SLOPE.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPE.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\SLOPEHT.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPEHT.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\SLOPEUS.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\SLOPEUS.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\V_D_C_N.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\V_D_C_N.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\VALLY_D.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\VALLY_D.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\VY_DP_K.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\VY_DP_K.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)

r <- raster(paste(dir_data,"\\BC_grids\\BC_topography_1ha\\WETSAGA.sdat",sep=""))
q <- crop(r,one_habc_b)
q1 <- q*one_habc_b
writeRaster(q1,filename=paste(dir_name,"\\ED_data\\grids\\topography_1ha\\WETSAGA.asc",sep=""),format="ascii",overwrite=TRUE)
rm(r,q,q1)
#*********************


#add grid files for climate, bec
#*********************

setwd(dir_script)
gc()


#proc.time() - ptm