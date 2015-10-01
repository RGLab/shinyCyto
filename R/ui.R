library(shinyjs)
library(shinydashboard)
library(DT)
library(shinyFiles)
library(networkD3)

sidebar <- dashboardSidebar(
  useShinyjs(),
  extendShinyjs(script = "www/actions.js"),
  sidebarMenu(
    
    menuItem("Import workspaces", tabName = "Import", icon = icon("arrow-right"), selected = TRUE)
    , menuItem("Load GatingSets", tabName = "load_menu", icon = icon("folder-open"))
    , menuItemOutput("gs_menu_obj")

    
    , menuItem("Compensate & Transform", tabName = "Compensate", icon = icon("exchange"))
    , menuItem("Build a Gating Template",tabName = "GatingTemplate", icon = icon("table"))
    
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
    # tabItem("Data", title = "Data", h1("Please choose submenu: 'Import workspaces' or 'Load GatingSet'"))
    tabItem("Import", title = "Import"
                       
                  , div(
                      div(textInput("path_import",label = "workspaces path", value = datadirectory), style="display:inline-block")
                      ,div(shinyDirButton("ws_dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary"),style="display:inline-block")
                        
                      # , myActionButton(inputId = "refresh_import",label = "Scan")
                        
                        , div(h6(DT::dataTableOutput(outputId = "file_table")))
                         ,verbatimTextOutput("message1")
                        , myActionButton("open_ws",label="next-->")
                        , id = "ws_select_tab" 
                        )
      
                    
                       
                    
                        
                  , hidden(div(selectInput("grp_selected", choices = '', label = "Group")
                               
                              , hidden(div(id = "kw_block"
                                        , selectInput("kw_selected", choices = '', label = "keyword", multiple = TRUE)
                                        , radioButtons("kw_src", choices = c("XML", "FCS"), inline = TRUE, label = "keyword source")
                                            )
                                        )
                              , checkboxInput("isLeafBool", label = "Parse leaf boolean gates", value = FALSE)
                               ,hidden(verbatimTextOutput("message2"))
                               , div(myActionButton("back_to_ws",label="<--back")
                                     ,style="display:inline-block;"
                                     )
                               ,disabled(div(myActionButton("parse_ws",label = "Parse Workspace")
                                             ,style="display:inline-block;"
                                             )
                                         )
                               ,id = "grp_select_tab"
                              )
                           )
                  

            )
    , tabItem("load_menu"
              , box(shinyDirButton("gs_dir_btn",label = "choose GatingSet ...", title = "Please select a folder", buttonType = "primary")
                    , textInput("path_gs", label = "", value = file.path(datadirectory, "gs_manual"))
                    , myActionButton(inputId = "load",label = "Load Data")
                    , div(verbatimTextOutput("message"),style='width:90%')
                    , title = "Load GatingSets", solidHeader = TRUE, status = "primary", width = NULL
                    )
              )
    
    , tabItem("gs_menu" 
               
              , tabBox(id = "gs_tab", width = NULL
                     ,tabPanel(title = "Pheno Data", value = "pd_tab", icon = icon("th")
                               ,DT::dataTableOutput("pd_tbl")
                               )
                     ,tabPanel(title = "Gating Tree", value = "tree_tab", icon = icon("sitemap")
                               ,selectInput("sn_select", choices = c("select one sample ---" = ""), label = NULL)
                               # ,checkboxInput("isBool", "Show boolean gates")
                               ,div(div(diagonalNetworkOutput("tree",width="400px",height="300px"),
                                       style="display:inline-block;float:left;")
                                   ,div(imageOutput("gateplot",width = "300px",height="300px")
                                        ,style="margin-left:400px;", id = "tabSet")
                                   ,style="width:100%;height:100%;"
                               )
                               )
                     ,tabPanel(title = "Gating Layout", value = "gate_layout_tab", icon = icon("picture-o")
                               , plotOutput("gate_layout"))
                     ,tabPanel(title = "Pop Stats", value = "stats_tab", icon = icon("bar-chart")
                               ,DT::dataTableOutput("pop_stats_tbl")
                               )
                     )
              )
    
    , tabItem("Compensate", 
            box(title = "Compensate & Transform", solidHeader = TRUE, status = "primary")
            ),
    tabItem("GatingTemplate",
            box(title = "Build a Gating Template", solidHeader = TRUE, status = "primary")
            )
    
  )
)

dashboardPage(
  dashboardHeader(title = "OpenCyto"),
  sidebar,
  body          
  
)
