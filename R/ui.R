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
                    , bsButton(inputId = "load",label = "Load Data")
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
            
                ,fluidRow(
                  column(2,
                      selectInput("parent", choices="", label='Select Parent Population')
                        , bsTooltip("parent"
                                    , title = "Parent population specifies the data where the gates will be applied on"
                                    , "right"
                                    )
                        , br()
                      
                        , selectizeInput("dims", choices="", label='Channels', multiple =TRUE
                                         , options = list(maxItems = 2)
                                          )
                        , bsTooltip("dims", title = "specifying the dimensions(1d or 2d) used for gating", "right")
                        , br()
                      
                        , checkboxInput("collapseData", label = "collapse data for gating")
                        , bsTooltip("collapseData", title = "When checked, data is collapsed (within the group if groupBy specified) before gating and the gate is replicated across collapsed samples. If unchecked,then groupBy argument is only used by preprocessing and ignored by gating.", "right")
                        , br()  
                      
                        , bsButton("bt_plot_data",label="plot data", style = "primary")
                        
                      )
                  , column(10
                        #plot  
                          , plotOutput("gt_data_plot", width = 400, height = 300)
                        
                        )
                  )
            ,fluidRow(
              column(2,
                      mytextInput.typeahead("alias"
                                           , label ="Population name"
                                           , local = data.frame(name = common_pop_names)
                                           , valueKey = "name"
                                           , tokens = seq_along(common_pop_names)
                                           , template = "{{name}}"
                                            )
                    , bsTooltip("alias"
                                , title = "a name used label the cell population, the path composed by the alias and its precedent nodes (e.g. /root/A/B/NAME) has to be uniquely identifiable."
                                , "right"
                                , options = list(container = "body")
                                )
                  )
                              
              , column(2
                     , selectInput("pop"
                                 , choices=c("A+","A-","B+","B-")
                                 , label='Population pattern'
                                 , multiple =TRUE)
                     , bsTooltip("pop"
                                 , title = "population patterns tells the algorithm which side (postive or negative) of 1d gate or which quadrant of 2d gate to be kept when it is in the form of 'A+/-B+/-', 'A' and 'B' should be the full name (or a substring as long as it is unqiuely matched) of either channel or marker of the flow data"
                                 , "right"
                                 , options = list(container = "body")
                                 )
                      ) 
              , column(2
                      , selectInput("groupBy", label = "Group By", choices = "", multiple = TRUE)
                      , bsTooltip("groupBy", title = "samples are split into groups by the unique combinations of study variable (i.e. column names of pData,e.g.“PTID:VISITNO”). when split is numeric, then samples are grouped by every N samples", "right")
                      )
              )
            , fluidRow(
               column(2
                       , selectInput("gating_method",
                                        choices = as.list(gating_methods),
                                        label="Gating Method",
                                        selected = "mindensity"
                                    )
                      )
#                               , textInput("gating_args", label="Gating Parameters", value='')
              , column(2
                       , br()
                       ,bsButton("open_gate_control", "Gating Parameters...", style = "primary")
                      , bsModal("gating_control"
                                # , "gating_args"
                                , trigger = "open_gate_control"
                                , uiOutput("gate_args_inputs")
                                )
                      )
               )    
            , fluidRow(
                column(2
                     ,selectInput("pp_method",
                                    choices = c("---" = "", as.list(pp_methods)),
                                    label = "Preprocessing Method"
                                    , selected = ""
                                  )
                      )
                , column(2
                     , br()
                    , bsButton("open_pp_control", "Preprocessing Parameters...", style = "primary")
                        )
                )
    
                 
          , fluidRow(
              column(2
                   ,bsButton("bt_plot_tree",label="plot tree", style = "primary")
                  , busyIndicator(text = "conputing the gates..")
                  , bsButton("bt_apply_gate",label="add gate", style = "primary")
                  , bsButton("bt_undo",label="undo", style = "primary")
                              
                  )
               )
        , fluidRow(
              column(12
                    , verbatimTextOutput("gt_message")
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
