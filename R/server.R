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
  # select ws folder
  shinyDirChoose(input, "ws_dir_btn", session = session, roots = roots)
  
  observeEvent(input$ws_dir_btn, {
    path_selected <- parseDirPath(roots, input$ws_dir_btn)
    updateTextInput(session,'path_import', value = path_selected)
  })
  
  rv = reactiveValues()
  
  observeEvent(input$tabset,{
    js$fire()
  })

  
  #-------- read file info selected folder--------
  
  # observeEvent(input$refresh_import,{
  ws_list <- reactive(data.table(data=list.files(input$path_import
                                                 , full.names = TRUE
                                                 , recursive = TRUE
                                                )
                                 )[(data %like% "xml"| data %like% "wsp")]
                          )
  
  ws_count <- reactive(nrow(ws_list()))
  
  #-------- fill the DT with file info --------
  output$file_table = DT::renderDataTable(ws_list(),rownames = FALSE
                                          , selection = list(mode = "multiple", selected = 2)
                                          )
  
  #-------- update the message and button based on the file info--------  
  observeEvent(ws_count(), {
          if(ws_count() >= 1){
            output$message1 = renderPrint(cat("Select an xml file."))
            shinyjs::show("message1")
          }else{
            shinyjs::hide("message1")
          }
          
    })
    
  # })
  
  #-------- update message and button based on the selected rows--------
  ws_selected <- reactive(input$file_table_rows_selected)
  ws_selected_count <- reactive(length(ws_selected()))
  
  observeEvent(ws_selected_count(),{
    if (ws_selected_count() >= 1) {
        shinyjs::enable("open_ws")
        shinyjs::hide("message1")
      }else {
        shinyjs::disable("open_ws")
        shinyjs::show("message1")
      }
    
    
  })

  #-------- populate the group selectInput with the selected ws--------
  observeEvent(input$open_ws,{
      if (ws_selected_count() == 1) {
        
          rv$ws <<- openWorkspace(ws_list()[ws_selected(), data])
          #sample info  
          allSamples <- getSamples(rv$ws)
          # group Info
          g <- getSampleGroups(rv$ws)
          # merge two
          sg <- merge(allSamples, g, by="sampleID");
          
          sg <- sg[sg$pop.counts>0,]
          
          # filter by group name
          sg$groupName<-factor(sg$groupName)
          groups<-levels(sg$groupName)
          updateSelectInput(session, "grp_selected"
                            , choices = c(`select a group ---` = '', groups)
                            , label = NULL
                            )
          shinyjs::disable("parse_ws")
          shinyjs::show("grp_select_tab")
          shinyjs::hide("ws_select_tab")
          
          
          
      }
  })
  
  observeEvent(input$back_to_ws, {
    shinyjs::disable("parse_ws")
    shinyjs::hide("grp_select_tab")
    shinyjs::show("ws_select_tab")
    
  })
  
  observeEvent(input$grp_selected, {
    
    shinyjs::enable("parse_ws")
  })
  
  observeEvent(input$parse_ws,{
    shinyjs::show("message2")
    gs <- try(parseWorkspace(rv$ws,name = input$grp_selected))
    
    if(inherits(gs,"try-error")){
      output$message2 = renderText(paste(geterrmessage(),"\nMaybe you need to upload FCS files as well?"))
    }else if(class(gs)=="GatingSet"){
      output$message2 = renderText(paste0("Success!\n",capture.output(print(gs))))
      rv$gs <<- gs
      shinyjs::disable("parse_ws")
    }else{
      output$message2 = renderText("gs is a ",class(gs))
    }
  })
  
  #---- update gs panels ----
  observeEvent(rv$gs,{
      #update badge of gs menu
      output$gs_menu_obj <- renderMenu(menuItem("GatingSets"
                                                , tabName = "gs_menu"
                                                , icon = icon("database")
                                                , badgeLabel = "1"
                                                , badgeColor = "green")
                                      )
      #update the pData tbl
      output$pd_tbl <- DT::renderDataTable(pData(rv$gs))  
      
      #---- Populate sn selector ----    
      updateSelectInput(session, "sn_select"
                    , choices = sampleNames(rv$gs)
                    , label = NULL)
      
  })
  
  #---update gh related panels---
  observeEvent(input$sn_select,{
    sn <- input$sn_select
    
    if(nchar(sn) > 0){
      gh <- rv$gs[[sn]]
      # gate
      output$gate_layout <- renderPlot(plotGate(gh))
      
      #pop stats
      stats <- getPopStats(gh)
      output$pop_stats_tbl <- DT::renderDataTable(stats)
      # tree
      
      output$tree = renderDiagonalNetwork({
        tree = getPopStats(rv$gs[sn],format="long",showHidden=TRUE)[,.(Parent,Population)]
        diagonalNetwork(maketreelist(data.frame(tree),root="root"),fontSize=8,margin = c(100,100))
      })  
    }
    
  })
  #---- select gs folder ----
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



