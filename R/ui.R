library(shiny)
library(DT)
navbarPage("OpenCyto",
               tabsetPanel(tabPanel("Data",{
               navlistPanel(
                 tabPanel("Import",
                          tags$br(),
                          div(div(shinyDirButton("dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary")
                                  ,style="display:inline-block;")
                              , div(textInput("path_import",label = "workspaces path"), style="display:inline-block;")
                              ,div(actionButton(inputId = "refresh_import",label = "Scan")
                                   ,style="display:inline-block;")
                              ,style="display:inline-block;"),
                          div(h6(DT::dataTableOutput(outputId = "file_table",))),
                          actionButton("parse_chosen",label="Import Files"),
                          hidden(div(div(actionButton("parseGroup",label = "Parse Workspace"),style="display:inline-block;"),div(uiOutput("workspaceGroups"),style="display:inline-block"),div(textOutput("nsamples"),style="display:inline-block"),style='width:90%',id="parseUI")),
                          div(verbatimTextOutput("message1"),style='width:90%')
                          ),
                 
                 tabPanel("Load",
                   div(div(h5("Existing Data"),style="display:inline-block"),
                   div(div(textInput("path",label = "path",value = "../inst/extdata"),style="display:inline-block"),actionButton(inputId = "refresh",label = "Scan"),style="display:inline-block"),style="display:inline-block"),
                   div(h6(DT::dataTableOutput(outputId = "existing_data")), style = 'width:90%;'),
                   actionButton(inputId = "load",label = "Load Data"),
                   div(verbatimTextOutput("message"),style='width:90%')
                   
                 ),useShinyjs()
                 # ,extendShinyjs(script = "www/actions.js")
,id="datanavlist"
               )
               
             }),
             
             tabPanel("Compensate & Transform","Compensate & Transform"),
             tabPanel("Gating Template","Build a Gating Template"),
             tabPanel("Tree","View Gating Tree",
                      div(div(diagonalNetworkOutput("tree",width="400px",height="300px"),
                      style="display:inline-block;float:left;"),div(imageOutput("gateplot",width = "300px",height="300px"),style="margin-left:400px;"),style="width:100%;height:100%;")
                      ),id = "tabset"
             )
)
