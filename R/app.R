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
                          hidden(div(div(actionButton("parseGroup",label = "Parse Workspace"),style="display:inline-block"),div(uiOutput("workspaceGroups"),style="display:inline-block"),div(textOutput("nsamples"),style="display:inline-block"),style='width:90%',id="parseUI")),
                          div(verbatimTextOutput("message1"),style='width:90%')
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
      shinyjs::hide("nsamples")
      shinyjs::hide("parseUI")
    }else if(grepl("\\.xml",.getFileName())){
      shinyjs::show("workspaceGroups")
      shinyjs::show("parseGroup")
      shinyjs::show("nsamples")
      shinyjs::disable("parseGroup")
      shinyjs::show("parseUI")
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
          ws <<- openWorkspace(file.path(tmp,tbl[name%in%input$chooser_rows_selected,name]))
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
          groups = unique(groups[,.(groupName,groupID)])
          groups = groups[,.(ID = .I),.(groupName,groupID)]
          output$workspaceGroups = renderUI(selectInput("workspaceGroups",label = "Workspace Groups", choices = plyr::dlply(groups,.variables="groupName",.fun=function(x)x$ID)))
          observeEvent(input$workspaceGroups,{
            output$nsamples = renderText(paste0(groups[,.N,.(groupID)][groupID==input$workspaceGroups,N]," samples"))
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
            gs <<- try(GatingSet(fs))
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
  
  observeEvent(input$parseGroup,{
    gs <<-try(parseWorkspace(ws,name = as.numeric(input$workspaceGroups),path=ws@path))
    if(inherits(gs,"try-error")){
      output$message1 = renderText(paste(geterrmessage(),"\nMaybe you need to upload FCS files as well?"))
    }else if(class(gs)=="GatingSet"){
      output$message1 = renderText(paste0("Success!\n",capture.output(print(gs))))
    }else{
      output$message1 = renderText("gs is a ",class(gs))
    }
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
      gs <<- try(load_gs(path = s))
      if(inherits(gs,"try-error")){
        output$message = renderPrint(cat("Not a gating set"))
      }else{
        output$message = renderPrint(cat("Loaded ",s))
      }
    }
  })
  }
shinyApp(ui = ui, server = server)


