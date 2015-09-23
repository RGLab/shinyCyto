library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  useShinyjs(),
  sidebarMenu(
    menuItem("Data", tabName = "Data",
            menuSubItem("Import workspaces", tabName = "Import", icon = icon("arrow-right"), selected = TRUE),
            menuSubItem("Load GatingSets", tabName = "Load", icon = icon("folder-open"))
            , icon = icon("database")
            ),
    menuItem("Compensate & Transform", tabName = "Compensate", icon = icon("exchange")),
    menuItem("Build a Gating Template",tabName = "GatingTemplate", icon = icon("table")),
    menuItem("View Gating Tree", tabName = "Tree", icon = icon("bar-chart"))
  )
)



body <- dashboardBody(
  tabItems(
    tabItem("Data", title = "Data", h1("Please choose submenu: 'Import workspaces' or 'Load GatingSet'")),
    tabItem("Import",
            box(
                shinyDirButton("dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary"),
                textInput("path_import",label = "workspaces path"),
                actionButton(inputId = "refresh_import",label = "Scan"),
                div(h6(DT::dataTableOutput(outputId = "file_table",))),
                actionButton("parse_chosen",label="Import Files"),
                hidden(div(div(actionButton("parseGroup",label = "Parse Workspace"),style="display:inline-block;"),div(uiOutput("workspaceGroups"),style="display:inline-block"),div(textOutput("nsamples"),style="display:inline-block"),style='width:90%',id="parseUI")),
                div(verbatimTextOutput("message1"),style='width:90%'),
                title = "Import workspaces", solidHeader = TRUE, status = "primary"
              )
            ),
    tabItem("Load",
            box(
             div(div(h5("Existing Data"),style="display:inline-block"),
                 div(div(textInput("path",label = "path",value = "../inst/extdata"),style="display:inline-block"),actionButton(inputId = "refresh",label = "Scan"),style="display:inline-block"),style="display:inline-block"),
             div(h6(DT::dataTableOutput(outputId = "existing_data")), style = 'width:90%;'),
             actionButton(inputId = "load",label = "Load Data"),
             div(verbatimTextOutput("message"),style='width:90%'),
            title = "Load GatingSets", solidHeader = TRUE, status = "primary"
            )
          ),
    # ,extendShinyjs(script = "www/actions.js")
    

    tabItem("Compensate", 
            box(title = "Compensate & Transform", solidHeader = TRUE, status = "primary")
            ),
    tabItem("GatingTemplate",
            box(title = "Build a Gating Template", solidHeader = TRUE, status = "primary")
            ),
    tabItem("Tree",
            box(
              div(div(diagonalNetworkOutput("tree",width="400px",height="300px"),
                 style="display:inline-block;float:left;"),div(imageOutput("gateplot",width = "300px",height="300px"),style="margin-left:400px;"),style="width:100%;height:100%;"
                 ),
              title = "View Gating Tree", solidHeader = TRUE, status = "primary"
              )
            )
  )
)

dashboardPage(
  dashboardHeader(title = "OpenCyto"),
  sidebar,
  body          
  
)
