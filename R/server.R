library(shiny)
library(flowWorkspace)
library(openCyto)
library(shinyjs)
library(data.table)
library(networkD3)
library(ggcyto)
if (packageVersion('DT') < '0.1.3') devtools::install_github('rstudio/DT')
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
 
  #---init gs menu ---
  output$gs_menu_obj <- renderMenu(menuItem("GatingSets", tabName = "gs_menu", icon = icon("database"), badgeLabel = "0", badgeColor = "orange"))
  #   .getFilePaths = reactive({
  #   s = input$chooser_rows_selected
  #   if(length(s)){
  #     df = input$filechooser
  #     setDT(df)
  #     df[name%in%s,datapath]
  #   }
  #   })
  
  rv = reactiveValues()
  

  
  #---- select gs folder ----
  shinyDirChoose(input, "gs_dir_btn", session = session, roots = roots)
  observeEvent(input$gs_dir_btn, {
    
    path_selected <- parseDirPath(roots, input$gs_dir_btn)
    updateTextInput(session,'path_gs', value = path_selected)
  })
  
  rv$preload <- 1 ##preload for testing
  observeEvent(input$load+rv$preload,{
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



