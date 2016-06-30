library(shinyjs)
library(shinydashboard)
library(DT)
library(shinyFiles)
library(networkD3)
library(shinysky)
library(shinyBS)
source("utils.R")
source("import_tab.R")
source("gs_menu_tab.R")

sidebar <- dashboardSidebar(
   useShinyjs()
  , extendShinyjs(script = "www/actions.js")
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
    
    import_tab(datadirectory)
    , tabItem("load_menu"
              , box(shinyDirButton("gs_dir_btn",label = "choose GatingSet ...", title = "Please select a folder", buttonType = "primary")
                    , textInput("path_gs", label = "", value = "/loc/no-backup/mike/test_gs"#file.path(datadirectory, "gs_manual")
                                )
                    , myActionButton(inputId = "load",label = "Load Data")
                    , div(verbatimTextOutput("message"),style='width:90%')
                    , title = "Load GatingSets", solidHeader = TRUE, status = "primary", width = NULL
                    )
              )
    
    , gs_menu_tab()
    
    , tabItem("Compensate", 
            box(title = "Compensate & Transform", solidHeader = TRUE, status = "primary")
            )
    ,tabItem("GatingTemplate"
            ,DT::dataTableOutput("gt_tbl")
            ,div(
                  div(id = "plot controls"
                        , selectInput("parent", choices="", label='Select Parent Population')
                        
                        , selectizeInput("dims", choices="", label='Channels', multiple =TRUE
                                         , options = list(maxItems = 2)
                                          )
                        , checkboxInput("collapseData", label = "collapse data for gating")
                          
                        , myActionButton("bt_plot_data",label="plot data")
                        , style="display:inline-block;float:left;"
                      )
                    
                  #plot  
                  , div(plotOutput("gt_data_plot", width = 400, height = 300)
                        , style="display:inline-block")
                    
                 ,tags$table(
                      tags$tr(
                              tags$td(
                                div(mytextInput.typeahead("alias"
                                               , label ="Population name"
                                               , local = data.frame(name = common_pop_names)
                                               , valueKey = "name"
                                               , tokens = seq_along(common_pop_names)
                                               , template = "{{name}}"
                                                )
                                    , style="display:inline-block;float:left;")
                            
                              
                  
                     , div(selectInput("pop", choices=c("A+","A-","B+","B-"), label='Population pattern', multiple =TRUE)
                        , style="display:inline-block;float:left;") 
                      , div(selectInput("groupBy", label = "Group By", choices = "", multiple = TRUE)
                            , style="display:inline-block;float:left;")
                     
                          )
                      )
                    , tags$tr(
                            tags$td(
                              div(selectInput("gating_method",
                                        choices = as.list(gating_methods),
                                        label="Gating Method",
                                        selected = "mindensity"
                                    )
                                  , style="display:inline-block;float:left;")    
                        
                              , div(textInput("gating_args", label="Gating Parameters", value='')
                                            , style="display:inline-block;float:left;")
                              , bsButton("open_gate_control", "...")
                              , bsModal("gating_control", "gating_args", "open_gate_control", uiOutput("gate_args_inputs"))
                              )
                            )
                  
                  , tags$tr(
                            tags$td(
                              div(selectInput("pp_method",
                                    choices = c("---" = "", as.list(pp_methods)),
                                    label = "Preprocessing Method"
                                    , selected = ""
                                  )
                                  , style="display:inline-block;float:left;")
                            ,div(textInput("pp_args", label="Preprocessing parameters", value='')
                                 , style="display:inline-block;float:left;")
                              )
                            )
    
                  , tags$tr(
                        tags$td(
                          div(myActionButton("bt_plot_tree",label="plot tree")
                              , busyIndicator(text = "conputing the gates..")
                              , myActionButton("bt_apply_gate",label="add gate")
                              , myActionButton("bt_undo",label="undo")
                              , style="display:inline-block;float:left;"
                              )
                        )
                     )
                  , tags$tr(tags$td(verbatimTextOutput("gt_message"),style='width:90%'))
                
              )
            )
    )
  )
)

dashboardPage(
  
  dashboardHeader(title = "OpenCyto")
  ,sidebar
  , body          
  
)
