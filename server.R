# Load libraries ----
library(shiny)
library(foreign)
library(RSAGA)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(randomForest)
#********************

# Setup the environment ----
ed_all <- c(244, 245, 247, 248, 249, 252, 581, 582, 583, 585, 591, 610, 
            618, 889, 905, 910, 914, 915, 916, 917, 918, 919, 920, 921, 922, 
            923, 925, 929, 931, 933, 934, 935, 937, 938, 939, 940, 941, 942, 
            943, 944, 945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 
            956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 
            969, 970, 971, 972, 973, 974, 975, 976, 977, 978, 979, 980, 981, 
            982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 
            995, 996, 997, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 
            1009, 1010, 1011, 1012, 1013, 1014, 1015, 1017, 1019)
#***************************

# Shiny server inputs/outputs ----
shinyServer(function(input, output, session) {

  iso_obs <- function() {
    Sys.sleep(1)
    shiny::isolate({
        tmp.env <- new.env()
        dir_Proj <- paste(input$drive_Proj,":\\BC_soil_map",sep="")
        dir_script <- paste(dir_Proj,"\\BC_script",sep="")
        dir_data <- paste(input$drive_data,":\\BC_soil_map\\BC_data",sep="")
        ed <- readOGR(paste(dir_data,"\\BC_shapes\\BC_soils",sep=""),layer="EcoDistricts_BC")
        ed_all <- ed$ECODISTRIC
        ed_list <- input$ed_seld
        run_pureUpdt <- input$run_pureUpdt
        run_ptchUpdt <- input$run_ptchUpdt
        run_mlayer <- input$run_mlayer
        run_td <- input$run_td
        run_rf <- input$run_rf
        run_map <- input$run_map
        
        assign("dir_Proj",dir_Proj,envir=tmp.env)
        assign("dir_script",dir_script,envir=tmp.env)
        assign("dir_data",dir_data,envir=tmp.env)
        assign("ed",ed,envir=tmp.env)
        assign("ed_all",ed_all,envir=tmp.env)
        assign("ed_list",ed_list,envir=tmp.env)
        assign("run_pureUpdt",run_pureUpdt,envir=tmp.env)
        assign("run_ptchUpdt",run_ptchUpdt,envir=tmp.env)
        assign("run_mlayer",run_mlayer,envir=tmp.env)
        assign("run_td",run_td,envir=tmp.env)
        assign("run_rf",run_rf,envir=tmp.env)
        assign("run_map",run_map,envir=tmp.env)
       
        save(list=ls(all.names=TRUE,pos=tmp.env),envir=tmp.env,file="obs.RData")
        rm(tmp.env)
        })
  }
  
  observe({
    if (input$run_scripts==0) {return} else {
      iso_obs()
      source(paste(dir_script,"\\BCSM_controller.R",sep=""),
             local=.GlobalEnv,
             echo=TRUE)
    }
  })

  autoInvalidate <- reactiveTimer(1000, session)
  
  output$date_time <- renderText({
    autoInvalidate()
    date()
  })
  
  observe({
    if (input$sel_multi=='dsel_all') {
      updateCheckboxGroupInput(session,
                               "ed_seld",
                               label = "No EcoDistricts have been selected",
                               choices = ed_all,
                               selected = NULL)
    }
    
    if (input$sel_multi=='sel_all') {
      updateCheckboxGroupInput(session,
                               "ed_seld",
                               label = paste("All ",length(ed_all)," EcoDistricts have been selected",sep=""),
                               choices = ed_all,
                               selected = ed_all)
    }
  })
  
  observe({
    if (length(input$ed_seld)==0) {
      updateCheckboxGroupInput(session,
                               "ed_seld",
                               label = "No EcoDistricts have been selected",
                               choices = ed_all,
                               selected = NULL)
    }
  })
  
  observe({
    if (length(input$ed_seld)>0 & length(input$ed_seld)<length(ed_all)) {
      updateCheckboxGroupInput(session,
                               "ed_seld",
                               label = paste(length(input$ed_seld)," EcoDistricts have been selected",sep=""),
                               choices = ed_all,
                               selected = input$ed_seld)
    }
    })
  
})