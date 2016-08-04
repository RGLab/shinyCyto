library(shinyjs)
library(shinydashboard)
library(DT)
library(shinyFiles)
library(networkD3)
library(shinysky)
library(shinyBS)

sidebar <- dashboardSidebar(
   useShinyjs()
  , sidebarMenu(
    
    menuItem("Import workspaces", tabName = "Import", icon = icon("arrow-right"))
    , menuItem("Load GatingSets", tabName = "load_menu", icon = icon("folder-open"))
    , menuItemOutput("gs_menu_obj")

    
#     , menuItem("Compensate & Transform", tabName = "Compensate", icon = icon("exchange"))
    , menuItem("Build a Gating Template",tabName = "GatingTemplate", icon = icon("table"), selected = TRUE)
    
  )
)



  
datadirectory = system.file("extdata",package = "flowWorkspaceData")#"~/rglab/workspace/analysis/sony1"

body <- dashboardBody(
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "helptip.css")
  ),
  tabItems(
    # tabItem("Data", title = "Data", h1("Please choose submenu: 'Import workspaces' or 'Load GatingSet'"))
    # tabItem("Import", parseWorkspaceUI("tab1", datadirectory))
     tabItem("load_menu"
              , box(shinyDirButton("gs_dir_btn",label = "choose GatingSet ...", title = "Please select a folder", buttonType = "primary")
                    , textInput("path_gs", label = "", value = "/loc/no-backup/mike/test_gs"#file.path(datadirectory, "gs_manual")
                                )
                    , bsButton(inputId = "load",label = "Load Data")
                    , div(verbatimTextOutput("message"),style='width:90%')
                    , title = "Load GatingSets", solidHeader = TRUE, status = "primary", width = NULL
                    )
              )
    
    # , tabItem("gs_menu", gsMenuUI("tab3") )
    
    , tabItem("Compensate", 
            box(title = "Compensate & Transform", solidHeader = TRUE, status = "primary")
            )
    ,tabItem("GatingTemplate", gatingTemplateUI("tab5"))
            )
    )
  

dashboardPage(
  
  dashboardHeader(title = "OpenCyto")
  ,sidebar
  , body          
  
)
