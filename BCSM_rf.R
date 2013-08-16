#This program: 
#... retrieves point attribute data from a csv created (previously) from the dbf of a shapefile ... 
#... prepares training datasets that are 1- balanced by class or 2- constrained by geomorph principles 
#... runs randomForest on (a subset of) the training data 
#... predicts values for a map area with available ascii grids. 
#... writes the predicted output to an ascii grid file. 

print(paste("Running randomForest prediction: ED_",ed_list[i], sep=""))

# Load rF Training data ----

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Training_CanSIS_matl.csv",sep=""))){
MAT_raw_CanSIS <- read.csv(paste(dir_name,"\\ED_training_data\\MATL\\Training_CanSIS_matl.csv",sep=""),header=TRUE)
table(MAT_raw_CanSIS$MATERIAL)
} else {
  print("Could not find a csv file with training data for CanSIS")
  sink(paste(dir_name,"\\ED_models\\MATL\\_CanSIS_no training data found.txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a csv file with training data for CanSIS")
  sink() 
  return
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Training_Seamless_matl.csv",sep=""))){
  MAT_raw_Seamless <- read.csv(paste(dir_name,"\\ED_training_data\\MATL\\Training_Seamless_matl.csv",sep=""),header=TRUE)
table(MAT_raw_Seamless$MATERIAL)
} else {
  print("Could not find a csv file with training data for Seamless")
  sink(paste(dir_name,"\\ED_models\\MATL\\_Seamless_no training data found.txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a csv file with training data for Seamless")
  sink() 
  return
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Training_SoBC_matl.csv",sep=""))){
  MAT_raw_SoBC <- read.csv(paste(dir_name,"\\ED_training_data\\MATL\\Training_SoBC_matl.csv",sep=""),header=TRUE)
table(MAT_raw_SoBC$MATERIAL)
} else {
  print("Could not find a csv file with training data for SoBC")
  sink(paste(dir_name,"\\ED_models\\MATL\\_SoBC_no training data found.txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a csv file with training data for SoBC")
  sink() 
  return
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_20K_matl.csv",sep=""))){
  MAT_raw_ST_20K <- read.csv(paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_20K_matl.csv",sep=""),header=TRUE)
table(MAT_raw_ST_20K$MATERIAL)
} else {
  print("Could not find a csv file with training data for ST_20K")
  sink(paste(dir_name,"\\ED_models\\MATL\\_ST_20K_no training data found.txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a csv file with training data for ST_20K")
  sink() 
  return
}

if (file.exists(paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_GT20K_matl.csv",sep=""))){
  MAT_raw_ST_GT20K <- read.csv(paste(dir_name,"\\ED_training_data\\MATL\\Training_ST_GT20K_matl.csv",sep=""),header=TRUE)
table(MAT_raw_ST_GT20K$MATERIAL)
} else {
  print("Could not find a csv file with training data for ST_GT20K")
  sink(paste(dir_name,"\\ED_models\\MATL\\_ST_GT20K_no training data found.txt",sep=""), append=FALSE, split=FALSE)
  print("Could not find a csv file with training data for ST_GT20K")
  sink() 
  return
}

objects()

v<-ls(pattern = glob2rx("MAT_raw*"))
if (length(v) < 1){next} else {return}

MAT_raw_all <- data.frame(X=as.integer(character()),ID=as.numeric(character()),MATERIAL=as.factor(character()),MAT_CODE=as.factor(character()),C_A_1HA=as.numeric(character()),C_N_B_L=as.numeric(character()),C_SLOPE=as.numeric(character()),CURV=as.numeric(character()),CURV_PL=as.numeric(character()),CURV_PR=as.numeric(character()),CURV_US=as.numeric(character()),ELEV=as.numeric(character()),HD_2_CH=as.numeric(character()),HT_NORM=as.numeric(character()),HT_STD=as.numeric(character()),HTNRM_K=as.numeric(character()),HTSTD_K=as.numeric(character()),MB_IND=as.numeric(character()),MDSLP_K=as.numeric(character()),MID_SLP=as.numeric(character()),MRRTFHA=as.numeric(character()),MRRTFKM=as.numeric(character()),MRVBFHA=as.numeric(character()),MRVBFKM=as.numeric(character()),OPENNEG=as.numeric(character()),OPENPOS=as.numeric(character()),RHSP_HA=as.numeric(character()),RHSP_KM=as.numeric(character()),SL_HT_K=as.numeric(character()),SLOPE=as.numeric(character()),SLOPEHT=as.numeric(character()),SLOPEUS=as.numeric(character()),V_D_C_N=as.numeric(character()),VALLY_D=as.numeric(character()),VY_DP_K=as.numeric(character()),WETSAGA=as.numeric(character()))

#Build the combined dataset
for(j in 1:length(v)){
  MAT_raw_all<-rbind(MAT_raw_all,get(v[j]))  
}

MAT_raw_all <- subset(MAT_raw_all, select = c("ID","MATERIAL","MAT_CODE","C_A_1HA","C_N_B_L","C_SLOPE","CURV","CURV_PL","CURV_PR","CURV_US","ELEV","HD_2_CH","HT_NORM","HT_STD","HTNRM_K","HTSTD_K","MB_IND","MDSLP_K","MID_SLP","MRRTFHA","MRRTFKM","MRVBFHA","MRVBFKM","OPENNEG","OPENPOS","RHSP_HA","RHSP_KM","SL_HT_K","SLOPE","SLOPEHT","SLOPEUS","V_D_C_N","VALLY_D","VY_DP_K","WETSAGA"))

# 1 - create balanced dataset and run RF 
MAT_02_COLL <- subset(MAT_raw_all, MATERIAL == "02_COLL")
MAT_03_RKWE <- subset(MAT_raw_all, MATERIAL == "03_RKWE")
MAT_04_EOLI <- subset(MAT_raw_all, MATERIAL == "04_EOLI")  
MAT_05_FLUV <- subset(MAT_raw_all, MATERIAL == "05_FLUV")
MAT_06_GLFL <- subset(MAT_raw_all, MATERIAL == "06_GLFL")
MAT_07__ICE <- subset(MAT_raw_all, MATERIAL == "07__ICE")
MAT_08_LACU <- subset(MAT_raw_all, MATERIAL == "08_LACU")
MAT_09_GLLC <- subset(MAT_raw_all, MATERIAL == "09_GLLC")
MAT_10_TILL <- subset(MAT_raw_all, MATERIAL == "10_TILL")
MAT_11_UNDO <- subset(MAT_raw_all, MATERIAL == "11_UNDO")
MAT_12_RKUD <- subset(MAT_raw_all, MATERIAL == "12_RKUD")
MAT_13_UDIF <- subset(MAT_raw_all, MATERIAL == "13_UDIF")
MAT_14_VOLC <- subset(MAT_raw_all, MATERIAL == "14_VOLC")
MAT_15_MARI <- subset(MAT_raw_all, MATERIAL == "15_MARI")
MAT_16_GLMA <- subset(MAT_raw_all, MATERIAL == "16_GLMA")

MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]

MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)

rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)

rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)

summary(MAT_bal$MATERIAL)

MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]

MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)

MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)

table(MAT_bal$MATERIAL)

names(MAT_bal)

#If dataset has less than three material types delete it and skip
m<-levels(MAT_bal$MATERIAL)
if(length(m) < 3){next} else {return}

head(MAT_bal)

tail(MAT_bal)

MAT_bal.roughfix<-na.roughfix(MAT_bal)

set.seed(101)

MATRF_all_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)

write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_all_bal_roughfix.csv", sep=""))

MATRF_all_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
print(MATRF_all_bal_cv)
sink()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
importance(MATRF_all_bal,type=1)
sink()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
importance(MATRF_all_bal,type=2)
sink()

save(MATRF_all_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal.Rdata", sep=""))

pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
varImpPlot(MATRF_all_bal)
dev.off()

margins.rf = margin(MATRF_all_bal,MAT_bal.roughfix$MATERIAL)
pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_all_bal by class")
dev.off()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal_.txt", sep=""), append=FALSE, split=FALSE)
print(MATRF_all_bal)
sink()

print("finished all_bal")

rm(MAT_bal,MAT_bal.roughfix,margins.rf)

# 2 - create constrained dataset and run RF 
MAT_02_COLL <- subset(MAT_raw_all, MATERIAL == "02_COLL" & SLOPEUS > 25)
MAT_03_RKWE <- subset(MAT_raw_all, MATERIAL == "03_RKWE")
MAT_04_EOLI <- subset(MAT_raw_all, MATERIAL == "04_EOLI")  
MAT_05_FLUV <- subset(MAT_raw_all, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
MAT_06_GLFL <- subset(MAT_raw_all, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
MAT_07__ICE <- subset(MAT_raw_all, MATERIAL == "07__ICE")
MAT_08_LACU <- subset(MAT_raw_all, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
MAT_09_GLLC <- subset(MAT_raw_all, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
MAT_10_TILL <- subset(MAT_raw_all, MATERIAL == "10_TILL" & SLOPEUS < 35)
MAT_11_UNDO <- subset(MAT_raw_all, MATERIAL == "11_UNDO")
MAT_12_RKUD <- subset(MAT_raw_all, MATERIAL == "12_RKUD")
MAT_13_UDIF <- subset(MAT_raw_all, MATERIAL == "13_UDIF")
MAT_14_VOLC <- subset(MAT_raw_all, MATERIAL == "14_VOLC")
MAT_15_MARI <- subset(MAT_raw_all, MATERIAL == "15_MARI")
MAT_16_GLMA <- subset(MAT_raw_all, MATERIAL == "16_GLMA")

MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]

MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)

rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)

rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)

summary(MAT_con$MATERIAL)

MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]

MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)

MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)

table(MAT_con$MATERIAL)

names(MAT_con)

#If dataset has less than three material types delete it and skip
m<-levels(MAT_con$MATERIAL)
if(length(m) < 3){next} else {return}

head(MAT_con)

tail(MAT_con)

MAT_con.roughfix<-na.roughfix(MAT_con)

set.seed(101)

MATRF_all_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)

write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_all_con_roughfix.csv", sep=""))

MATRF_all_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_cv.txt", sep=""), append=FALSE, split=FALSE)
print(MATRF_all_con_cv)
sink()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
importance(MATRF_all_con,type=1)
sink()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
importance(MATRF_all_con,type=2)
sink()

save(MATRF_all_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con.Rdata", sep=""))

pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
varImpPlot(MATRF_all_con)
dev.off()

margins.rf = margin(MATRF_all_con,MAT_con.roughfix$MATERIAL)
pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_all_con by class")
dev.off()

sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con_.txt", sep=""), append=FALSE, split=FALSE)
print(MATRF_all_con)
sink()

print("finished all_con")

rm(MAT_con,MAT_con.roughfix,margins.rf)

rm(MATRF_all_bal_cv,MATRF_all_con_cv)
#*********************


# rF on CanSIS training data (balanced by "MATERIAL")
# does the training data csv exist? (ie. check if there is training data for the area...)
if (file.exists(".\\ED_training_data\\MATL\\Training_CanSIS_matl.csv")){
  
  # 1 - create balanced dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_CanSIS, MATERIAL == "02_COLL")
  MAT_03_RKWE <- subset(MAT_raw_CanSIS, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_CanSIS, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_CanSIS, MATERIAL == "05_FLUV")
  MAT_06_GLFL <- subset(MAT_raw_CanSIS, MATERIAL == "06_GLFL")
  MAT_07__ICE <- subset(MAT_raw_CanSIS, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_CanSIS, MATERIAL == "08_LACU")
  MAT_09_GLLC <- subset(MAT_raw_CanSIS, MATERIAL == "09_GLLC")
  MAT_10_TILL <- subset(MAT_raw_CanSIS, MATERIAL == "10_TILL")
  MAT_11_UNDO <- subset(MAT_raw_CanSIS, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_CanSIS, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_CanSIS, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_CanSIS, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_CanSIS, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_CanSIS, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_bal$MATERIAL)
  
  MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]
  
  MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)
  
  MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)
  
  table(MAT_bal$MATERIAL)
  
  names(MAT_bal)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_bal$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_bal)
  
  tail(MAT_bal)
  
  MAT_bal.roughfix<-na.roughfix(MAT_bal)
  
  set.seed(101)
  
  MATRF_CanSIS_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_CanSIS_bal_roughfix.csv", sep=""))
  
  MATRF_CanSIS_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_CanSIS_bal_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_CanSIS_bal,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_CanSIS_bal,type=2)
  sink()
  
  save(MATRF_CanSIS_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_CanSIS_bal)
  dev.off()
  
  margins.rf = margin(MATRF_CanSIS_bal,MAT_bal.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_CanSIS_bal by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_CanSIS_bal)
  sink()
  
  print("finished CanSIS_bal")
  
  rm(MAT_bal,MAT_bal.roughfix,margins.rf)
  
  # 2 - create constrained dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_CanSIS, MATERIAL == "02_COLL" & SLOPEUS > 25)
  MAT_03_RKWE <- subset(MAT_raw_CanSIS, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_CanSIS, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_CanSIS, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
  MAT_06_GLFL <- subset(MAT_raw_CanSIS, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_07__ICE <- subset(MAT_raw_CanSIS, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_CanSIS, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
  MAT_09_GLLC <- subset(MAT_raw_CanSIS, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_10_TILL <- subset(MAT_raw_CanSIS, MATERIAL == "10_TILL" & SLOPEUS < 35)
  MAT_11_UNDO <- subset(MAT_raw_CanSIS, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_CanSIS, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_CanSIS, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_CanSIS, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_CanSIS, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_CanSIS, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_con$MATERIAL)
  
  MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]
  
  MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)
  
  MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)
  
  table(MAT_con$MATERIAL)
  
  names(MAT_con)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_con$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_con)
  
  tail(MAT_con)
  
  MAT_con.roughfix<-na.roughfix(MAT_con)
  
  set.seed(101)
  
  MATRF_CanSIS_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_CanSIS_con_roughfix.csv", sep=""))
  
  MATRF_CanSIS_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_CanSIS_con_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_CanSIS_con,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_CanSIS_con,type=2)
  sink()
  
  save(MATRF_CanSIS_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_CanSIS_con)
  dev.off()
  
  margins.rf = margin(MATRF_CanSIS_con,MAT_con.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_CanSIS_con by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_CanSIS_con)
  sink()
  
  print("finished CanSIS_con")
  
  rm(MAT_con,MAT_con.roughfix,margins.rf)
  
  rm(MATRF_CanSIS_bal_cv,MATRF_CanSIS_con_cv)
  #*********************
  

# rF on Seamless training data (1- balanced by "MATERIAL" and 2- constrained by 1st principles)
# does the training data csv exist? (ie. check if there is training data for the area...)
if (file.exists(".\\ED_training_data\\MATL\\Training_Seamless_matl.csv")){
  
  # 1 - create balanced dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_Seamless, MATERIAL == "02_COLL")
  MAT_03_RKWE <- subset(MAT_raw_Seamless, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_Seamless, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_Seamless, MATERIAL == "05_FLUV")
  MAT_06_GLFL <- subset(MAT_raw_Seamless, MATERIAL == "06_GLFL")
  MAT_07__ICE <- subset(MAT_raw_Seamless, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_Seamless, MATERIAL == "08_LACU")
  MAT_09_GLLC <- subset(MAT_raw_Seamless, MATERIAL == "09_GLLC")
  MAT_10_TILL <- subset(MAT_raw_Seamless, MATERIAL == "10_TILL")
  MAT_11_UNDO <- subset(MAT_raw_Seamless, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_Seamless, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_Seamless, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_Seamless, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_Seamless, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_Seamless, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_bal$MATERIAL)
  
  MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]
  
  MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)
  
  MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)
  
  table(MAT_bal$MATERIAL)
  
  names(MAT_bal)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_bal$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_bal)
  
  tail(MAT_bal)
  
  MAT_bal.roughfix<-na.roughfix(MAT_bal)
  
  set.seed(101)
  
  MATRF_Seamless_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_Seamless_bal_roughfix.csv", sep=""))
  
  MATRF_Seamless_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_Seamless_bal_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_Seamless_bal,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_Seamless_bal,type=2)
  sink()
  
  save(MATRF_Seamless_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_Seamless_bal)
  dev.off()
  
  margins.rf = margin(MATRF_Seamless_bal,MAT_bal.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_Seamless_bal by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_Seamless_bal)
  sink()
  
  print("finished Seamless_bal")
  
  rm(MAT_bal,MAT_bal.roughfix,margins.rf)
  
  # 2 - create constrained dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_Seamless, MATERIAL == "02_COLL" & SLOPEUS > 25)
  MAT_03_RKWE <- subset(MAT_raw_Seamless, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_Seamless, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_Seamless, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
  MAT_06_GLFL <- subset(MAT_raw_Seamless, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_07__ICE <- subset(MAT_raw_Seamless, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_Seamless, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
  MAT_09_GLLC <- subset(MAT_raw_Seamless, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_10_TILL <- subset(MAT_raw_Seamless, MATERIAL == "10_TILL" & SLOPEUS < 35)
  MAT_11_UNDO <- subset(MAT_raw_Seamless, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_Seamless, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_Seamless, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_Seamless, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_Seamless, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_Seamless, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_con$MATERIAL)
  
  MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]
  
  MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)
  
  MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)
  
  table(MAT_con$MATERIAL)
  
  names(MAT_con)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_con$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_con)
  
  tail(MAT_con)
  
  MAT_con.roughfix<-na.roughfix(MAT_con)
  
  set.seed(101)
  
  MATRF_Seamless_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_Seamless_con_roughfix.csv", sep=""))
  
  MATRF_Seamless_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_Seamless_con_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_Seamless_con,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_Seamless_con,type=2)
  sink()
  
  save(MATRF_Seamless_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_Seamless_con)
  dev.off()
  
  margins.rf = margin(MATRF_Seamless_con,MAT_con.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_Seamless_con by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_Seamless_con)
  sink()
  
  print("finished Seamless_con")
  
  rm(MAT_con,MAT_con.roughfix,margins.rf)
  
  rm(MATRF_Seamless_bal_cv,MATRF_Seamless_con_cv)
  #*********************


# rF on SoBC training data ((1- balanced by "MATERIAL" and 2- constrained by 1st principles))
# does the training data csv exist? (ie. check if there is training data for the area...)
if (file.exists(".\\ED_training_data\\MATL\\Training_SoBC_matl.csv")){

  # 1 - create balanced dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_SoBC, MATERIAL == "02_COLL")
  MAT_03_RKWE <- subset(MAT_raw_SoBC, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_SoBC, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_SoBC, MATERIAL == "05_FLUV")
  MAT_06_GLFL <- subset(MAT_raw_SoBC, MATERIAL == "06_GLFL")
  MAT_07__ICE <- subset(MAT_raw_SoBC, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_SoBC, MATERIAL == "08_LACU")
  MAT_09_GLLC <- subset(MAT_raw_SoBC, MATERIAL == "09_GLLC")
  MAT_10_TILL <- subset(MAT_raw_SoBC, MATERIAL == "10_TILL")
  MAT_11_UNDO <- subset(MAT_raw_SoBC, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_SoBC, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_SoBC, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_SoBC, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_SoBC, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_SoBC, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_bal$MATERIAL)
  
  MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]
  
  MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)
  
  MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)
  
  table(MAT_bal$MATERIAL)
  
  names(MAT_bal)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_bal$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_bal)
  
  tail(MAT_bal)
  
  MAT_bal.roughfix<-na.roughfix(MAT_bal)
  
  set.seed(101)
  
  MATRF_SoBC_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_SoBC_bal_roughfix.csv", sep=""))
  
  MATRF_SoBC_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_SoBC_bal_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_SoBC_bal,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_SoBC_bal,type=2)
  sink()
  
  save(MATRF_SoBC_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_SoBC_bal)
  dev.off()
  
  margins.rf = margin(MATRF_SoBC_bal,MAT_bal.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_SoBC_bal by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_SoBC_bal)
  sink()
  
  print("finished SoBC_bal")
  
  rm(MAT_bal,MAT_bal.roughfix,margins.rf)
  
  # 2 - create constrained dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_SoBC, MATERIAL == "02_COLL" & SLOPEUS > 25)
  MAT_03_RKWE <- subset(MAT_raw_SoBC, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_SoBC, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_SoBC, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
  MAT_06_GLFL <- subset(MAT_raw_SoBC, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_07__ICE <- subset(MAT_raw_SoBC, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_SoBC, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
  MAT_09_GLLC <- subset(MAT_raw_SoBC, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_10_TILL <- subset(MAT_raw_SoBC, MATERIAL == "10_TILL" & SLOPEUS < 35)
  MAT_11_UNDO <- subset(MAT_raw_SoBC, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_SoBC, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_SoBC, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_SoBC, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_SoBC, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_SoBC, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_con$MATERIAL)
  
  MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]
  
  MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)
  
  MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)
  
  table(MAT_con$MATERIAL)
  
  names(MAT_con)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_con$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_con)
  
  tail(MAT_con)
  
  MAT_con.roughfix<-na.roughfix(MAT_con)
  
  set.seed(101)
  
  MATRF_SoBC_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_SoBC_con_roughfix.csv", sep=""))
  
  MATRF_SoBC_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_SoBC_con_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_SoBC_con,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_SoBC_con,type=2)
  sink()
  
  save(MATRF_SoBC_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_SoBC_con)
  dev.off()
  
  margins.rf = margin(MATRF_SoBC_con,MAT_con.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_SoBC_con by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_SoBC_con)
  sink()
  
  print("finished SoBC_con")
  
  rm(MAT_con,MAT_con.roughfix,margins.rf)
  
  rm(MATRF_SoBC_bal_cv,MATRF_SoBC_con_cv)
  #*********************


#rF on ST_20K training data ((1- balanced by "MATERIAL" and 2- constrained by 1st principles))
# does the training data csv exist? (ie. check if there is training data for the area...)
if (file.exists(".\\ED_training_data\\MATL\\Training_ST_20K_matl.csv")){
  
  # 1 - create balanced dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_ST_20K, MATERIAL == "02_COLL")
  MAT_03_RKWE <- subset(MAT_raw_ST_20K, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_ST_20K, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_ST_20K, MATERIAL == "05_FLUV")
  MAT_06_GLFL <- subset(MAT_raw_ST_20K, MATERIAL == "06_GLFL")
  MAT_07__ICE <- subset(MAT_raw_ST_20K, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_ST_20K, MATERIAL == "08_LACU")
  MAT_09_GLLC <- subset(MAT_raw_ST_20K, MATERIAL == "09_GLLC")
  MAT_10_TILL <- subset(MAT_raw_ST_20K, MATERIAL == "10_TILL")
  MAT_11_UNDO <- subset(MAT_raw_ST_20K, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_ST_20K, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_ST_20K, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_ST_20K, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_ST_20K, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_ST_20K, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_bal$MATERIAL)
  
  MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]
  
  MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)
  
  MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)
  
  table(MAT_bal$MATERIAL)
  
  names(MAT_bal)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_bal$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_bal)
  
  tail(MAT_bal)
  
  MAT_bal.roughfix<-na.roughfix(MAT_bal)
  
  set.seed(101)
  
  MATRF_ST_20K_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_ST_20K_bal_roughfix.csv", sep=""))
  
  MATRF_ST_20K_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_20K_bal_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_20K_bal,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_20K_bal,type=2)
  sink()
  
  save(MATRF_ST_20K_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_ST_20K_bal)
  dev.off()
  
  margins.rf = margin(MATRF_ST_20K_bal,MAT_bal.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_ST_20K_bal by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_20K_bal)
  sink()
  
  print("finished ST_20K_bal")
  
  rm(MAT_bal,MAT_bal.roughfix,margins.rf)
  
  # 2 - create constrained dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_ST_20K, MATERIAL == "02_COLL" & SLOPEUS > 25)
  MAT_03_RKWE <- subset(MAT_raw_ST_20K, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_ST_20K, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_ST_20K, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
  MAT_06_GLFL <- subset(MAT_raw_ST_20K, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_07__ICE <- subset(MAT_raw_ST_20K, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_ST_20K, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
  MAT_09_GLLC <- subset(MAT_raw_ST_20K, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_10_TILL <- subset(MAT_raw_ST_20K, MATERIAL == "10_TILL" & SLOPEUS < 35)
  MAT_11_UNDO <- subset(MAT_raw_ST_20K, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_ST_20K, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_ST_20K, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_ST_20K, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_ST_20K, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_ST_20K, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_con$MATERIAL)
  
  MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]
  
  MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)
  
  MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)
  
  table(MAT_con$MATERIAL)
  
  names(MAT_con)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_con$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_con)
  
  tail(MAT_con)
  
  MAT_con.roughfix<-na.roughfix(MAT_con)
  
  set.seed(101)
  
  MATRF_ST_20K_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_ST_20K_con_roughfix.csv", sep=""))
  
  MATRF_ST_20K_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_20K_con_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_20K_con,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_20K_con,type=2)
  sink()
  
  save(MATRF_ST_20K_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_ST_20K_con)
  dev.off()
  
  margins.rf = margin(MATRF_ST_20K_con,MAT_con.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_ST_20K_con by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_20K_con)
  sink()
  
  print("finished ST_20K_con")
  
  rm(MAT_con,MAT_con.roughfix,margins.rf)
  
  rm(MATRF_ST_20K_bal_cv,MATRF_ST_20K_con_cv)
  #*********************


#rF on ST_GT20K training data ((1- balanced by "MATERIAL" and 2- constrained by 1st principles))
# does the training data csv exist? (ie. check if there is training data for the area...)
if (file.exists(".\\ED_training_data\\MATL\\Training_ST_GT20K_matl.csv")){
  
  # 1 - create balanced dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_ST_GT20K, MATERIAL == "02_COLL")
  MAT_03_RKWE <- subset(MAT_raw_ST_GT20K, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_ST_GT20K, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_ST_GT20K, MATERIAL == "05_FLUV")
  MAT_06_GLFL <- subset(MAT_raw_ST_GT20K, MATERIAL == "06_GLFL")
  MAT_07__ICE <- subset(MAT_raw_ST_GT20K, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_ST_GT20K, MATERIAL == "08_LACU")
  MAT_09_GLLC <- subset(MAT_raw_ST_GT20K, MATERIAL == "09_GLLC")
  MAT_10_TILL <- subset(MAT_raw_ST_GT20K, MATERIAL == "10_TILL")
  MAT_11_UNDO <- subset(MAT_raw_ST_GT20K, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_ST_GT20K, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_ST_GT20K, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_ST_GT20K, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_ST_GT20K, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_ST_GT20K, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_bal <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_bal$MATERIAL)
  
  MAT_bal <- MAT_bal[!is.na(MAT_bal$MATERIAL),]
  
  MAT_bal$MATERIAL <- factor(MAT_bal$MATERIAL)
  
  MAT_bal$MAT_CODE <- factor(MAT_bal$MAT_CODE)
  
  table(MAT_bal$MATERIAL)
  
  names(MAT_bal)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_bal$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_bal)
  
  tail(MAT_bal)
  
  MAT_bal.roughfix<-na.roughfix(MAT_bal)
  
  set.seed(101)
  
  MATRF_ST_GT20K_bal<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_bal.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_bal.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_ST_GT20K_bal_roughfix.csv", sep=""))
  
  MATRF_ST_GT20K_bal_cv <- rfcv(MAT_bal.roughfix, MAT_bal.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_GT20K_bal_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_GT20K_bal,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_GT20K_bal,type=2)
  sink()
  
  save(MATRF_ST_GT20K_bal, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_ST_GT20K_bal)
  dev.off()
  
  margins.rf = margin(MATRF_ST_GT20K_bal,MAT_bal.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_bal.roughfix$MATERIAL,main="RF Margins for MAT_ST_GT20K_bal by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_GT20K_bal)
  sink()
  
  print("finished ST_GT20K_bal")
  
  rm(MAT_bal,MAT_bal.roughfix,margins.rf)
  
  # 2 - create constrained dataset and run RF 
  MAT_02_COLL <- subset(MAT_raw_ST_GT20K, MATERIAL == "02_COLL" & SLOPEUS > 25)
  MAT_03_RKWE <- subset(MAT_raw_ST_GT20K, MATERIAL == "03_RKWE")
  MAT_04_EOLI <- subset(MAT_raw_ST_GT20K, MATERIAL == "04_EOLI")  
  MAT_05_FLUV <- subset(MAT_raw_ST_GT20K, (MATERIAL == "05_FLUV" & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL"  & SLOPEUS < 10 & MRVBFHA > 0.5 & V_D_C_N < (.0005*VY_DP_K)))
  MAT_06_GLFL <- subset(MAT_raw_ST_GT20K, (MATERIAL == "05_FLUV" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "06_GLFL" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_07__ICE <- subset(MAT_raw_ST_GT20K, MATERIAL == "07__ICE")
  MAT_08_LACU <- subset(MAT_raw_ST_GT20K, (MATERIAL == "08_LACU" & V_D_C_N < (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N < (.0005*VY_DP_K)))
  MAT_09_GLLC <- subset(MAT_raw_ST_GT20K, (MATERIAL == "08_LACU" & V_D_C_N > (.0005*VY_DP_K)) | (MATERIAL == "09_GLLC" & V_D_C_N > (.0005*VY_DP_K)))
  MAT_10_TILL <- subset(MAT_raw_ST_GT20K, MATERIAL == "10_TILL" & SLOPEUS < 35)
  MAT_11_UNDO <- subset(MAT_raw_ST_GT20K, MATERIAL == "11_UNDO")
  MAT_12_RKUD <- subset(MAT_raw_ST_GT20K, MATERIAL == "12_RKUD")
  MAT_13_UDIF <- subset(MAT_raw_ST_GT20K, MATERIAL == "13_UDIF")
  MAT_14_VOLC <- subset(MAT_raw_ST_GT20K, MATERIAL == "14_VOLC")
  MAT_15_MARI <- subset(MAT_raw_ST_GT20K, MATERIAL == "15_MARI")
  MAT_16_GLMA <- subset(MAT_raw_ST_GT20K, MATERIAL == "16_GLMA")
  
  MAT_02_COLLs <- MAT_02_COLL[sample(1:nrow(MAT_02_COLL), 1000, replace= TRUE),]
  MAT_03_RKWEs <- MAT_03_RKWE[sample(1:nrow(MAT_03_RKWE), 1000, replace= TRUE),]
  MAT_04_EOLIs <- MAT_04_EOLI[sample(1:nrow(MAT_04_EOLI), 1000, replace= TRUE),]
  MAT_05_FLUVs <- MAT_05_FLUV[sample(1:nrow(MAT_05_FLUV), 1000, replace= TRUE),]
  MAT_06_GLFLs <- MAT_06_GLFL[sample(1:nrow(MAT_06_GLFL), 1000, replace= TRUE),]
  MAT_07__ICEs <- MAT_07__ICE[sample(1:nrow(MAT_07__ICE), 1000, replace= TRUE),]
  MAT_08_LACUs <- MAT_08_LACU[sample(1:nrow(MAT_08_LACU), 1000, replace= TRUE),]
  MAT_09_GLLCs <- MAT_09_GLLC[sample(1:nrow(MAT_09_GLLC), 1000, replace= TRUE),]
  MAT_10_TILLs <- MAT_10_TILL[sample(1:nrow(MAT_10_TILL), 1000, replace= TRUE),]
  MAT_11_UNDOs <- MAT_11_UNDO[sample(1:nrow(MAT_11_UNDO), 1000, replace= TRUE),]
  MAT_12_RKUDs <- MAT_12_RKUD[sample(1:nrow(MAT_12_RKUD), 1000, replace= TRUE),]
  MAT_13_UDIFs <- MAT_13_UDIF[sample(1:nrow(MAT_13_UDIF), 1000, replace= TRUE),]
  MAT_14_VOLCs <- MAT_14_VOLC[sample(1:nrow(MAT_14_VOLC), 1000, replace= TRUE),]
  MAT_15_MARIs <- MAT_15_MARI[sample(1:nrow(MAT_15_MARI), 1000, replace= TRUE),]
  MAT_16_GLMAs <- MAT_16_GLMA[sample(1:nrow(MAT_16_GLMA), 1000, replace= TRUE),]
  
  MAT_con <- rbind(MAT_02_COLLs, MAT_05_FLUVs, MAT_06_GLFLs, MAT_08_LACUs, MAT_09_GLLCs, MAT_10_TILLs, MAT_11_UNDOs, MAT_12_RKUDs, MAT_15_MARIs, MAT_16_GLMAs)
  
  rm(MAT_02_COLL,MAT_03_RKWE,MAT_04_EOLI,MAT_05_FLUV,MAT_06_GLFL,MAT_07__ICE,MAT_08_LACU,MAT_09_GLLC,MAT_10_TILL,MAT_11_UNDO,MAT_12_RKUD,MAT_13_UDIF,MAT_14_VOLC,MAT_15_MARI,MAT_16_GLMA)
  
  rm(MAT_02_COLLs,MAT_03_RKWEs,MAT_04_EOLIs,MAT_05_FLUVs,MAT_06_GLFLs,MAT_07__ICEs,MAT_08_LACUs,MAT_09_GLLCs,MAT_10_TILLs,MAT_11_UNDOs,MAT_12_RKUDs,MAT_13_UDIFs,MAT_14_VOLCs,MAT_15_MARIs,MAT_16_GLMAs)
  
  summary(MAT_con$MATERIAL)
  
  MAT_con <- MAT_con[!is.na(MAT_con$MATERIAL),]
  
  MAT_con$MATERIAL <- factor(MAT_con$MATERIAL)
  
  MAT_con$MAT_CODE <- factor(MAT_con$MAT_CODE)
  
  table(MAT_con$MATERIAL)
  
  names(MAT_con)
  
  #If dataset has less than three material types delete it and skip
  m<-levels(MAT_con$MATERIAL)
  if(length(m) < 3){next} else {return}
  
  head(MAT_con)
  
  tail(MAT_con)
  
  MAT_con.roughfix<-na.roughfix(MAT_con)
  
  set.seed(101)
  
  MATRF_ST_GT20K_con<- randomForest(MAT_CODE ~ C_A_1HA+C_N_B_L+C_SLOPE+CURV+CURV_PL+CURV_PR+CURV_US+ELEV+HD_2_CH+HT_NORM+HT_STD+HTNRM_K+HTSTD_K+MB_IND+MDSLP_K+MID_SLP+MRRTFHA+MRRTFKM+MRVBFHA+MRVBFKM+OPENNEG+OPENPOS+RHSP_HA+RHSP_KM+SL_HT_K+SLOPE+SLOPEHT+SLOPEUS+V_D_C_N+VALLY_D+VY_DP_K+WETSAGA,data=MAT_con.roughfix, importance=TRUE, proximity=TRUE, TYPE=classification)
  
  write.csv(MAT_con.roughfix, file=paste(dir_name,"\\ED_models\\MATL\\MAT_ST_GT20K_con_roughfix.csv", sep=""))
  
  MATRF_ST_GT20K_con_cv <- rfcv(MAT_con.roughfix, MAT_con.roughfix$MAT_CODE, cv.fold=5, scale="log", step=0.8)
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_cv.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_GT20K_con_cv)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_MDA.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_GT20K_con,type=1)
  sink()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_MDG.txt", sep=""), append=FALSE, split=FALSE)
  importance(MATRF_ST_GT20K_con,type=2)
  sink()
  
  save(MATRF_ST_GT20K_con, file = paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con.Rdata", sep=""))
  
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_Importance.pdf", sep=""),width = 7, height = 10, pointsize = 12,)
  varImpPlot(MATRF_ST_GT20K_con)
  dev.off()
  
  margins.rf = margin(MATRF_ST_GT20K_con,MAT_con.roughfix$MATERIAL)
  pdf(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_Margins.pdf", sep=""),width = 10, height = 7, pointsize = 12,)
  boxplot(margins.rf~MAT_con.roughfix$MATERIAL,main="RF Margins for MAT_ST_GT20K_con by class")
  dev.off()
  
  sink(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con_.txt", sep=""), append=FALSE, split=FALSE)
  print(MATRF_ST_GT20K_con)
  sink()
  
  print("finished ST_GT20K_con")
  
  rm(MAT_con,MAT_con.roughfix,margins.rf)
  
  rm(MATRF_ST_GT20K_bal_cv,MATRF_ST_GT20K_con_cv)
  #*********************
  

rm(MAT_raw_all,MAT_raw_CanSIS,MAT_raw_Seamless,MAT_raw_SoBC,MAT_raw_ST_20K,MAT_raw_ST_GT20K)

gc()

# Predict Map outputs for all models --------------------------------------
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

  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_bal.Rdata", sep=""))){
    predict(p, MATRF_all_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_all_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_bal.asc", sep=""))
    image(MATRF_all_bal.result, main = paste("MATRF_all_bal ED_",ed_list[i], sep=""))
    rm(MATRF_all_bal,MATRF_all_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_all_con.Rdata", sep=""))){
    predict(p, MATRF_all_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_all_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_all_con.asc", sep=""))
    image(MATRF_all_con.result, main = paste("MATRF_all_con ED_",ed_list[i], sep=""))
    rm(MATRF_all_con,MATRF_all_con.result)
  }  else  {
    return
  }
    
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_bal.Rdata", sep=""))){
    predict(p, MATRF_CanSIS_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_CanSIS_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_CanSIS_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_CanSIS_bal.asc", sep=""))
    image(MATRF_CanSIS_bal.result, main = paste("MATRF_CanSIS_bal ED_",ed_list[i], sep=""))
    rm(MATRF_CanSIS_bal,MATRF_CanSIS_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_CanSIS_con.Rdata", sep=""))){
    predict(p, MATRF_CanSIS_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_CanSIS_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_CanSIS_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_CanSIS_con.asc", sep=""))
    image(MATRF_CanSIS_con.result, main = paste("MATRF_CanSIS_con ED_",ed_list[i], sep=""))
    rm(MATRF_CanSIS_con,MATRF_CanSIS_con.result)
  }  else  {
    return
  }

  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_bal.Rdata", sep=""))){
    predict(p, MATRF_Seamless_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_Seamless_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_Seamless_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_Seamless_bal.asc", sep=""))
    image(MATRF_Seamless_bal.result, main = paste("MATRF_Seamless_bal ED_",ed_list[i], sep=""))
    rm(MATRF_Seamless_bal,MATRF_Seamless_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_Seamless_con.Rdata", sep=""))){
    predict(p, MATRF_Seamless_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_Seamless_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_Seamless_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_Seamless_con.asc", sep=""))
    image(MATRF_Seamless_con.result, main = paste("MATRF_Seamless_con ED_",ed_list[i], sep=""))
    rm(MATRF_Seamless_con,MATRF_Seamless_con.result)
  }  else  {
    return
  }

  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_bal.Rdata", sep=""))){
    predict(p, MATRF_SoBC_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_SoBC_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_SoBC_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_SoBC_bal.asc", sep=""))
    image(MATRF_SoBC_bal.result, main = paste("MATRF_SoBC_bal ED_",ed_list[i], sep=""))
    rm(MATRF_SoBC_bal,MATRF_SoBC_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_SoBC_con.Rdata", sep=""))){
    predict(p, MATRF_SoBC_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_SoBC_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_SoBC_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_SoBC_con.asc", sep=""))
    image(MATRF_SoBC_con.result, main = paste("MATRF_SoBC_con ED_",ed_list[i], sep=""))
    rm(MATRF_SoBC_con,MATRF_SoBC_con.result)
  }  else  {
    return
  }

  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_bal.Rdata", sep=""))){
    predict(p, MATRF_ST_20K_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_20K_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_ST_20K_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_20K_bal.asc", sep=""))
    image(MATRF_ST_20K_bal.result, main = paste("MATRF_ST_20K_bal ED_",ed_list[i], sep=""))
    rm(MATRF_ST_20K_bal,MATRF_ST_20K_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_20K_con.Rdata", sep=""))){
    predict(p, MATRF_ST_20K_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_20K_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_ST_20K_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_20K_con.asc", sep=""))
    image(MATRF_ST_20K_con.result, main = paste("MATRF_ST_20K_con ED_",ed_list[i], sep=""))
    rm(MATRF_ST_20K_con,MATRF_ST_20K_con.result)
  }  else  {
    return
  }

  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_bal.Rdata", sep=""))){
    predict(p, MATRF_ST_GT20K_bal, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_GT20K_bal.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_ST_GT20K_bal.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_GT20K_bal.asc", sep=""))
    image(MATRF_ST_GT20K_bal.result, main = paste("MATRF_ST_GT20K_bal ED_",ed_list[i], sep=""))
    rm(MATRF_ST_GT20K_bal,MATRF_ST_GT20K_bal.result)
  }  else  {
    return
  }
  
  if (file.exists(paste(dir_name,"\\ED_models\\MATL\\MATRF_ST_GT20K_con.Rdata", sep=""))){
    predict(p, MATRF_ST_GT20K_con, filename = paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_GT20K_con.asc", sep=""), fun = predict, se.fit=TRUE, overwrite=TRUE)
    MATRF_ST_GT20K_con.result <- raster(paste(dir_name,"\\ED_maps\\MATL\\MATRF_ST_GT20K_con.asc", sep=""))
    image(MATRF_ST_GT20K_con.result, main = paste("MATRF_ST_GT20K_con ED_",ed_list[i], sep=""))
    rm(MATRF_ST_GT20K_con,MATRF_ST_GT20K_con.result)
  }  else  {
    return
  }

rm(p)

gc()