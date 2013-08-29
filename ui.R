# Setup the environment ----
dLetters <- c("A"="A","B"="B","C"="C","D"="D","E"="E","F"="F","G"="G",
              "H"="H","I"="I","J"="J","K"="K","L"="L","M"="M","N"="N",
              "O"="O","P"="P","Q"="Q","R"="R","S"="S","T"="T","U"="U",
              "V"="V","W"="W","X"="X","Y"="Y","Z"="Z")

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

# Shiny UI ----
shinyUI(pageWithSidebar(
  
  headerPanel("BC Soil Map Data-Quilt Application"), 
  
  sidebarPanel(h4("Script Settings"),
               
               selectInput("drive_Proj", 
                           label = "Select a drive for the project directory", 
                           choices = dLetters,
                           selected = "D",
                           multiple = FALSE),
               
               selectInput("drive_data", 
                           label = "Select a drive for the data directory", 
                           choices = dLetters, 
                           selected = "D",
                           multiple = FALSE),
               
               br(),
               
               checkboxInput("run_pureUpdt",
                             label = "Update pure datasets",
                             value = FALSE),
               
               checkboxInput("run_mlayer",
                             label = "Run map layers",
                             value = TRUE),
               
               checkboxInput("run_td",
                             label = "Run training data",
                             value = TRUE), 
               
               checkboxInput("run_rf",
                             label = "Run randomForest",
                             value = TRUE), 

               checkboxInput("run_map",
                             label = "Render map(s)",
                             value = TRUE),
               
               br(),
               
               actionButton("run_scripts", "Run Scripts"),
               
               verbatimTextOutput("date_time")
               ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Select EcoDistricts",
               
               radioButtons("sel_multi", 
                            label = "", 
                            choices = c("Deselect ALL EcoDistricts"="dsel_all",
                                        "Select ALL EcoDistricts"="sel_all"), 
                            selected = NULL),
               
               checkboxGroupInput("ed_seld", 
                                  label = "Select Individual EcoDistricts", 
                                  choices = ed_all,
                                  selected = NULL)
               ),
      
      
      
      id = "tabs",                      
      selected = NULL)
    )
  )
)