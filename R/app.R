library(shiny)
library(flowWorkspace)
library(openCyto)
library(shinyjs)
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
                          actionButton("parse_chosen",label="Import Files"),
                          div(verbatimTextOutput("message1"),style='width:90%'),
                          hidden(div(div(uiOutput("workspaceGroups"),style="display:inline-block"),div(textOutput("nsamples"),style="display:inline-block"),style='width:90%')),
                          hidden(actionButton("parseGroup",label = "Parse Workspace"))
                          ),
                 
                 tabPanel("Existing Data",
                   div(div(h5("Existing Data"),style="display:inline-block"),
                   div(actionButton(inputId = "refresh",label = "Refresh List"),style="display:inline-block"),style="display:inline-block"),
                   div(h6(DT::dataTableOutput(outputId = "existing_data")), style = 'width:90%;'),
                   actionButton(inputId = "load",label = "Load Data"),
                   div(verbatimTextOutput("message"),style='width:90%')
                   
                 ),useShinyjs(),id="datanavlist"
               )
               
             }),
             
             tabPanel("Compensate & Transform","Compensate & Transform"),
             tabPanel("Gating Template","Build a Gating Template"),
             tabPanel("Tree","View Gating Tree"),
             id = "tabs"
             
             )
server <- function(input, output,session){
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
  .getChosenFileTable = reactive({
    s = input$chooser_rows_selected
    if(length(s)){
      df = input$filechooser
      setDT(df)
      df
    }
  })
  readyForXMLParsing<<-FALSE
  observe({
    if(length(.getFileName())==0){
      shinyjs::hide("workspaceGroups")
      shinyjs::hide("parseGroup")
    }else if(grepl("\\.xml",.getFileName())){
      shinyjs::show("workspaceGroups")
      shinyjs::show("parseGroup")
      shinyjs::disable("parseGroup")
    }
  })
  
  observeEvent(input$chooser_rows_selected,{
    output$message1 = renderPrint({
      fn = .getFileName()
      if (length(fn)) {
        if (all(grepl("*\\.xml",fn))&length(fn)==1) {
          readyForXMLParsing<<-TRUE
          
          cat("Ready to import xml file ",fn)
        }else if(all(grepl("*\\.fcs",fn))){
          readyForXMLParsing<<-FALSE
          cat("Ready to convert FCS files to GatingSet.")
        }
      }else{
        readyForXMLParsing<<-FALSE
        cat("Please choose an xml file or some FCS files and click 'parse' to create a gating set.")
      }
    })
  })
  observeEvent(input$parse_chosen,{
      if (readyForXMLParsing&length(.getFileName())==1) {
        output$message1 = renderPrint(cat("Copying data into place and parsing xml.\n"))
        tbl = .getChosenFileTable()
        tmp = tempdir()
        copy_success = file.copy(from = tbl[,datapath],to = file.path(tmp,tbl[,name]), overwrite = TRUE)
        if(all(copy_success)){
          output$message1 = renderPrint({
            cat("Successfully copied all files.\n")
            cat("Parsing\n")
            })
          ws = openWorkspace(file.path(tmp,tbl[,name]))
          groups = getSampleGroups(ws)
          samples = getSamples(ws)
          setDT(groups)
          setDT(samples)
          samplegroups = merge(groups,samples,by="sampleID")
          output$message1 = renderPrint({
            if(class(ws)=="flowJoWorkspace"){
              enable("parseGroup")
              cat("Successfully read workspace! Ready to parse FCS files.")
            }
          })
          output$workspaceGroups = renderUI(selectInput("workspaceGroups",label = "Workspace Groups", choices = as.character(groups[,unique(groupName)])))
          observeEvent(input$workspaceGroups,{
            output$nsamples = renderText(paste0(groups[,.N,.(groupName)][groupName==input$workspaceGroups,N]," samples"))
          })
        }else{
          output$message1 = renderPrint(cat("Failed to copy :",tbl[!copy_success,name],"\n"))
        }
      }else if (!readyForXMLParsing&length(.getFileName())){
        readyForXMLParsing <<- FALSE
        output$message1 = renderPrint(cat("Copying data into place and creating a gating set from selected fcs files.\n"))
        tbl = .getChosenFileTable()
        tmp = tempdir()
        copy_success = file.copy(from = tbl[,datapath],to = file.path(tmp,tbl[,name]), overwrite = TRUE)
        if(all(copy_success)){
          output$message1 = renderPrint({
            cat("Successfully copied all files.\n")
            cat("Creating gating set\n")
          })
          fs = try(read.ncdfFlowSet(file.path(tmp,tbl[,name])))
          if(!inherits(fs,"try-error")){
            output$message1 = renderPrint(cat("Created ncdfFlowSet."))
            gs = try(GatingSet(fs))
            if(!inherits(gs,"try-error")){
              output$message1 = renderPrint({cat("GatingSet successfully created!")})
            }else{
              output$message1 = renderPrint(cat("Failed to create a GatingSet"))
            }
          }else{
            output$message1 = renderPrint(cat("Failed to create a FlowSet"))
          }
        }else{
          output$message1 = renderPrint(cat("Failed to copy :",tbl[!copy_success,name],"\n"))
        }
      }else{
        readyForXMLParsing <<- FALSE
        output$message1 = renderPrint(cat("Please choose an xml file or some FCS files and click 'parse' to create a gating set.\n"))
      }
    # })
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


