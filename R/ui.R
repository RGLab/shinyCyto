library(shinyjs)
library(shinydashboard)
library(DT)
library(shinyFiles)
library(networkD3)

sidebar <- dashboardSidebar(
  useShinyjs(),
  extendShinyjs(script = "www/actions.js"),
  sidebarMenu(
    menuItem("Data", tabName = "Data",icon = icon("database"),
            menuSubItem("Import workspaces", tabName = "Import", icon = icon("arrow-right"), selected = TRUE),
            menuSubItem("Load GatingSets", tabName = "Load", icon = icon("folder-open"))
            ),
    menuItem("Compensate & Transform", tabName = "Compensate", icon = icon("exchange")),
    menuItem("Build a Gating Template",tabName = "GatingTemplate", icon = icon("table")),
    menuItem("View Gating Tree", tabName = "Tree", icon = icon("bar-chart"))
  )
)


myActionButton <- function (inputId, label, icon = NULL, ...) 
{
  tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", 
              list(icon, label), ...)
}
  
datadirectory = system.file("extdata",package = "flowWorkspaceData")#"~/rglab/workspace/analysis/sony1"

body <- dashboardBody(
  tabItems(
    tabItem("Data", title = "Data", h1("Please choose submenu: 'Import workspaces' or 'Load GatingSet'")),
    tabItem("Import", title = "Import"
                       
                  , div(
                      div(textInput("path_import",label = "workspaces path", value = datadirectory), style="display:inline-block")
                      ,div(shinyDirButton("ws_dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary"),style="display:inline-block")
                        
                      , myActionButton(inputId = "refresh_import",label = "Scan")
                        
                        , div(h6(DT::dataTableOutput(outputId = "file_table")))
                        ,div(verbatimTextOutput("message1"),style='width:90%')           
                        , disabled(myActionButton("parse_chosen",label="next-->"))
                        , id = "ws_select_tab" 
                        )
      
                    
                       
                    
                        
                  , hidden(div(selectInput("grp_selected", choices = "", label = "Groups:")
                              ,id = "grp_select_tab"
                             ))
                  
                  , hidden(div(div(myActionButton("parseGroup",label = "Parse Workspace")
                                   ,style="display:inline-block;")
                               ,div(uiOutput("workspaceGroups"),style="display:inline-block")
                               ,div(textOutput("nsamples"),style="display:inline-block")
                               ,style='width:90%',id="parseUI"
                  ))
                  
            
            ),
    tabItem("Load",
            box(
              shinyDirButton("gs_dir_btn",label = "choose GatingSet ...", title = "Please select a folder", buttonType = "primary"),
              textInput("path_gs", label = "", value = datadirectory),
              myActionButton(inputId = "load",label = "Load Data"),
              div(verbatimTextOutput("message"),style='width:90%'),
              title = "Load GatingSets", solidHeader = TRUE, status = "primary", width = NULL
            )
          ),
    
    

    tabItem("Compensate", 
            box(title = "Compensate & Transform", solidHeader = TRUE, status = "primary")
            ),
    tabItem("GatingTemplate",
            box(title = "Build a Gating Template", solidHeader = TRUE, status = "primary")
            ),
    tabItem("Tree",
            # box(
              div(div(diagonalNetworkOutput("tree",width="400px",height="300px"),
                 style="display:inline-block;float:left;")
                 ,div(imageOutput("gateplot",width = "300px",height="300px")
                      ,style="margin-left:400px;", id = "tabSet")
                 ,style="width:100%;height:100%;"
                 )
              # title = "View Gating Tree", solidHeader = TRUE, status = "primary", width = NULL
              # )
            
            )
  )
)

dashboardPage(
  dashboardHeader(title = "OpenCyto"),
  sidebar,
  body          
  
)
