library(shiny)
library(flowWorkspace)
library(openCyto)
library(data.table)
library(DT)
#Test if a directory contains a GatingSet
#TODO support for GatingSetList
.isGatingSet = function(x){
  x = lapply(x,function(x)list.files(as.character(x)))
  unlist(lapply(x,function(x)all(grepl("*.pb",x)|grepl("*.dat",x)|grepl("*.nc",x)|grepl("*.rds",x))))
}
datadirectory = "../inst/extdata"
ui <-   navbarPage("OpenCyto",
             tabPanel("Data",{
               
               navlistPanel(
                 tabPanel("Upload Files",
                          tags$br(),
                          div(fileInput("filechooser",h5("Upload FCS Files and/or XML workspaces"), multiple = TRUE),
                          h6(DT::dataTableOutput(outputId = "chooser")), style = 'width:90%;'),
                          actionButton("parse_chosen",label="Parse Files"),
                          div(verbatimTextOutput("message1"),style='width:90%')
                          
                          ),
                 
                 tabPanel("Existing Data",
                   div(div(h5("Existing Data"),style="display:inline-block"),
                   div(actionButton(inputId = "refresh",label = "Refresh List"),style="display:inline-block"),style="display:inline-block"),
                   div(h6(DT::dataTableOutput(outputId = "existing_data")), style = 'width:90%;'),
                   actionButton(inputId = "load",label = "Load Data"),
                   div(verbatimTextOutput("message"),style='width:90%')
                   
                 ),id="datanavlist"
               )
               
             }),
             
             tabPanel("Compensate & Transform","Compensate & Transform"),
             tabPanel("Gating Template","Build a Gating Template"),
             tabPanel("Tree","View Gating Tree"),
             id = "tabs"
             
             )
server <- function(input, output){
  MAX_MB_UPLOAD = 1024 # one Gb limit.
  options(shiny.maxRequestSize=MAX_MB_UPLOAD*1024^2)
  
  .getFilePaths = reactive({
  s = input$chooser_rows_selected
  if(length(s)){
    df = input$filechooser
    setDT(df)
    df[name%in%s,datapath]
  }
  })
  
  .getFileName = reactive({
    s = input$chooser_rows_selected
    if(length(s)){
      df = input$filechooser
      setDT(df)
      df[name%in%s,name]
    }
  })
  readyToParse<<-FALSE
  observeEvent(input$chooser_rows_selected,{
    output$message1 = renderPrint({
      fn = .getFileName()
      if (length(fn)) {
        if (grepl("*\\.xml",fn)) {
          readyToParse<<-TRUE
          cat("Ready to parse xml file ",fn,": ",readyToParse)
        }else{
          readyToParse<<-FALSE
          cat("Please choose an xml file.","(",readyToParse,")")
        }
      }
    })
  })
  observeEvent(input$parse_chosen,{
    output$message1 = renderPrint({
      if (readyToParse&length(.getFileName())) {
        cat("Ready to parse xml file ",readyToParse)
      }else{
        readyToParse <<- FALSE
        cat("Ready to parse xml file ",readyToParse)
      }
    })
  })
  
  # Refresh / filter existing data -----------------------------------------
  observeEvent(input$refresh,{
    output$existing_data = DT::renderDataTable(setDT(subset(
      data.frame(data = list.files(
        datadirectory, full.names = TRUE, recursive = FALSE
      ))[,("data"),drop = FALSE],!(data %like% "fcs" |
                                     data %like% "xml")))
      [.isGatingSet(data)],rownames = FALSE,selection = "single")
  })
  
  output$chooser = DT::renderDataTable(input$filechooser[,c("name","size")],rownames=FALSE, selection='multiple')
  
  
  observeEvent(input$load,{
    output$message = renderPrint(cat("Choose a dataset"))
    s = input$existing_data_rows_selected
    if(length(s)){
      gs = try(load_gs(path = s))
      if(inherits(gs,"try-error")){
        output$message = renderPrint(cat("Not a gating set"))
      }else{
        output$message = renderPrint(cat("Loaded ",s))
      }
    }
  })
  }
shinyApp(ui = ui, server = server)


