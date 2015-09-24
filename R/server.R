library(shiny)
library(flowWorkspace)
library(openCyto)
library(shinyjs)
library(data.table)
library(networkD3)
library(ggcyto)
library(DT)
library(hash)
library(shinyFiles)
H = hash()

maketreelist <- function(df, root=df[1,1]) {
  if(is.factor(root)) root<-as.character(root)
  r <- list(name=root)
  children = df[df[,1]==root,2]
  if(is.factor(children)) children<-as.character(children)
  if(length(children)>0) {
    r$children <- lapply(children, maketreelist, df=df)
  }
  r
}
#Test if a directory contains a GatingSet
#TODO support for GatingSetList
.isGatingSet = function(x){
  x = lapply(x,function(x)list.files(as.character(x),recursive=FALSE))
  if(!all(lengths(x)>0)){
    return(FALSE)
  }
  unlist(lapply(x,function(x)all(grepl("*.pb",x)|grepl("*.dat",x)|grepl("*.txt",x)|grepl("*.nc",x)|grepl("*.rds",x))))
}
.isGatingSetList = function(x){
  x = lapply(x,function(x)list.files(as.character(x),recursive=FALSE))
  if(any(grepl("\\.rds",x))){
    x = x[!grepl("\\.rds",x)]
    all(unlist(lapply(x,function(x).isGatingSet(x))))  
  }else{
    FALSE
  }
  
}


roots <- c(home = system.file("extdata",package = "flowWorkspaceData")) #"~/rglab/workspace/analysis/sony"
function(input, output,session){
  MAX_MB_UPLOAD = 1024 # one Gb limit.
  options(shiny.maxRequestSize=MAX_MB_UPLOAD*1024^2)
 
#   .getFilePaths = reactive({
#   s = input$chooser_rows_selected
#   if(length(s)){
#     df = input$filechooser
#     setDT(df)
#     df[name%in%s,datapath]
#   }
#   })
  # select ws folder
  shinyDirChoose(input, "ws_dir_btn", session = session, roots = roots)
  
  observeEvent(input$ws_dir_btn, {
    path_selected <- parseDirPath(roots, input$ws_dir_btn)
    updateTextInput(session,'path_import', value = path_selected)
  })
  
  rv = reactiveValues()
  observeEvent(rv$gs,{
    output$tree = renderDiagonalNetwork({
      tree = getPopStats(rv$gs[1],format="long",showHidden=TRUE)[,.(Parent,Population)]
      diagonalNetwork(maketreelist(data.frame(tree),root="root"),fontSize=8,margin = c(100,100))
    })
  })
  
  observeEvent(input$tabset,{
    js$fire()
  })

  
  observeEvent(input$refresh_import,{
    datadirectory=input$path_import
    
    output$file_table = DT::renderDataTable(
      data.table(data=list.files(
        datadirectory, full.names = TRUE, recursive = FALSE
      ))[(data %like% "fcs" | data %like% "xml"| data %like% "wsp")]
      ,rownames = FALSE,selection = "multiple")
    output$message1 = renderPrint(cat("Select an xml file, or a set of FCS files."))
    shinyjs::disable("parse_chosen")
  })
  

  observeEvent(input$parse_chosen,{
      if (length(input$file_table_rows_selected)) {
        tbl = input$file_table_rows_selected
        if(grepl("\\.xml$",tbl)){
          output$message1 = renderPrint(cat("parsing xml."))
        }else if(all(grepl("\\.fcs$",tbl))){
          output$message1 = renderPrint(cat("parsing fcs."))
        }
      }
  })

  observeEvent(input$file_table_rows_selected,{
    if (length(input$file_table_rows_selected)) {
      tbl = input$file_table_rows_selected
      if(sum(grepl("\\.xml$",tbl))==1&length(input$file_table_rows_selected)==1){
        shinyjs::enable("parse_chosen")
      }else if(all(grepl("\\.fcs$",tbl))){
        shinyjs::enable("parse_chosen")
      }else {
        output$message1 = renderPrint(cat("Select an xml file, or a set of FCS files."))
        shinyjs::disable("parse_chosen")
      }
    }
  })
  
  observeEvent(input$parseGroup,{
    rv$gs <<-try(parseWorkspace(ws,name = as.numeric(input$workspaceGroups),path=ws@path))
    if(inherits(gs,"try-error")){
      output$message1 = renderText(paste(geterrmessage(),"\nMaybe you need to upload FCS files as well?"))
    }else if(class(gs)=="GatingSet"){
      output$message1 = renderText(paste0("Success!\n",capture.output(print(gs))))
    }else{
      output$message1 = renderText("gs is a ",class(gs))
    }
  })
  
  # select gs folder
  shinyDirChoose(input, "gs_dir_btn", session = session, roots = roots)
  observeEvent(input$gs_dir_btn, {
    
    path_selected <- parseDirPath(roots, input$gs_dir_btn)
    updateTextInput(session,'path_gs', value = path_selected)
  })
  
  
  observeEvent(input$load,{
    output$message = renderPrint(cat("Choose a dataset"))
    
    s = input$path_gs
    if(is.null(s)){
      output$message = renderPrint(cat("Choose a some files above"))
      return(NA)
    }
    if(length(s)&.isGatingSet(s)){
      output$message = renderPrint(cat("Loading GatingSet"))
      rv$gs <<- try(load_gs(path = s))
      if(inherits(rv$gs,"try-error")){
        output$message = renderPrint(cat("Error Loading ",s,":\n",geterrmessage()))
      }else{
        output$message = renderPrint(cat("Loaded ",s))
      }
    }else if (length(s)&.isGatingSetList(s)){
      output$message = renderPrint(cat("Loading GatingSetList"))
      rv$gs <<- try(load_gslist(path = s))
      if(inherits(rv$gs,"try-error")){
        output$message = renderPrint(cat("Error Loading ",s,":\n",geterrmessage()))
      }else{
        output$message = renderPrint(cat("Loaded ",s))
      }
    }else{
      output$message = renderPrint(cat("Not a stored GatingSet or GatingSetList"))
    }
  })
  plt = reactive({
        h = digest::digest(input$selnode)
        if(has.key(h,H)){
          return(list(src = H[[h]], alt = input$selnode))
        }else{
          tf = tempfile(fileext = ".png")
          png(file=tf,width = 6*75,height=6*75,units = "px",res=75)
          print(plotGate(rv$gs,input$selnode))
          dev.off()
          H[[h]] = tf 
          return(list(src = H[[h]], alt = input$selnode))
        }
    })
  
  observeEvent(input$selnode,{
    output$gateplot = renderImage(plt(),deleteFile = FALSE)
  })
  }



