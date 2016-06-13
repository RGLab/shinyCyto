library(shiny)
if (packageVersion('flowWorkspace') < '3.15.19') devtools::install_github('RGLab/flowWorkspace')
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
                                          , selection = list(mode = "single"#"multiple"
                                                             , selected = 2)
                                          
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
  # observeEvent(input$open_ws, {
  observe({ #skip action button open_ws for debugging purpose
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
                            , selected = "T-cell"# preselect for debugging purpose
                            )
          
          
          shinyjs::disable("parse_ws")
          shinyjs::show("grp_select_tab")
          shinyjs::hide("ws_select_tab")
          
          
          
      }
  })
  
  
  
  observeEvent(input$grp_selected, {
    
    if(input$grp_selected!=''){
      shinyjs::show("kw_block")
      #populate the keyword selector
      # if(input$kw_src == "XML"){ #TODO: support querying FCS header 
        g <- getSampleGroups(rv$ws)
        samples <- getSamples(rv$ws)
        sg <- merge(samples, g, by="sampleID")
        sg <- sg[sg$pop.counts>0,]
        rv$sg <<- subset(sg, groupName == input$grp_selected)

        sid <- rv$sg[["sampleID"]]
        kw <- getKeywords(rv$ws, sid[1])
        kw_name <- names(kw)
        #filter out the useless kw
        kw_name <- kw_name[!grepl('(\\$)|(LASER)|(^P[1-9]{1,2})|(^FJ_)|(FCS)|FSC ASF|(CYTOMETER)|COMPENSATION|WINDOW|THRESHOLD|(CST )|SPILL|EXPORT |CREATOR|AUTOBS', kw_name)]
        pre_selected <- kw_name[grepl('EXPERIMENT NAME|Stim|Sample Order|PATIENT|ASSAY_ID|PTID|VISITNO|rx_code', kw_name, ignore.case = TRUE)]
      # }
      updateSelectInput(session, "kw_selected"
                        , choices = kw_name
                        , selected = pre_selected
      )

      shinyjs::enable("parse_ws")  
    }
    
  })
  
  observeEvent(input$kw_selected, {
    if(input$kw_selected!=""){
      # browser()
      pd <- suppressWarnings(suppressMessages(
                                          flowWorkspace:::.parse.pData(rv$ws, input$kw_selected
                                                                       , rv$sg, input$kw_src
                                                                       , execute = TRUE
                                                                       , additional.keys = "$TOT"
                                                                       , path = rv$ws@path
                                                                       , keyword.ignore.case = FALSE
                                                                       , subset = NULL)
                                  )
                            )
      
      pd[["guid"]] <- NULL
      #remove temporary columns
      pd[["sampleID"]] <- NULL
      pd[["nFound"]] <- NULL
      pd[["file"]] <- NULL
      class(pd) <- "data.frame"
      rv$pd_filtered <<- pd
      output$sub_pd <- DT::renderDataTable(datatable(pd, filter = "bottom"
                                                     , options = list(
                                                                      search = list(regex = TRUE)
                                                                      , autoWidth = TRUE
                                                                      )
                                                     , rownames = FALSE
                                                     , selection = "none"
                                                      )
                                            )
                              
      
    }
    
  })
  
  onclick("toggleAdvanced", {
    toggle(id = "advanced", anim = TRUE) 
  })
  
  observeEvent(input$back_to_ws, {
    shinyjs::disable("parse_ws")
    shinyjs::hide("grp_select_tab")
    shinyjs::show("ws_select_tab")
    
  })
  
  
  
  observeEvent(input$parse_ws,{
    shinyjs::show("message2")
    
    sub_Ind <- input$sub_pd_rows_all
    sn <- rv$pd_filtered[sub_Ind, "name"]
    thisCall <- substitute(parseWorkspace(rv$ws
                                          ,name = input$grp_selected
                                          , keywords =  input$kw_selected
                                          , keywords.source = input$kw_src
                                          , leaf.bool = input$isLeafBool
                                          , subset = sn
                                        )
                          )
    
    gs <- try(eval(thisCall))
      
      
      
    if(inherits(gs,"try-error")){
      output$message2 = renderText(paste(geterrmessage()))#,"\nMaybe you need to upload FCS files as well?"))
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
      output$pd_tbl <- DT::renderDataTable(datatable(pData(rv$gs)
                                                     , rownames = FALSE
                                                     ,selection = list(mode = "single", selected = 1)
                                                     , filter = "bottom"
                                                     , options = list(search = list(regex = TRUE)
                                                                       , autoWidth = TRUE
#                                                                       , dom = 'T<"clear">lfrtip'
#                                                                       , tableTools = list(sSwfPath = copySWF())
                                                                     )
                                                     # , extensions = list("ColVis", "TableTools")#they are retired by Buttons extension, which is yet to be supported by DT package
                                                     )
                                           , server = FALSE
                                           )  
      
      #---- Populate sn selector ----    
#       updateSelectInput(session, "sn_select"
#                     , choices = sampleNames(rv$gs)
#                     , label = NULL)
      
  })
  
  #---update gh related panels---
  observeEvent(input$pd_tbl_rows_selected,{
    sn <- rownames(pData(rv$gs)[input$pd_tbl_rows_selected, ,drop = FALSE])
    
    if(nchar(sn) > 0){
      gh <- rv$gs[[sn]]
      nPop <- length(getNodes(gh))
      if(nPop > 1){
        output$gate_layout <- renderPlot(autoplot(gh))
        
        #pop stats
        stats <- getPopStats(gh)
        output$pop_stats_tbl <- DT::renderDataTable(stats)
        # tree
        
        output$tree = renderDiagonalNetwork({
          tree = getPopStats(rv$gs[sn],format="long",showHidden=TRUE)[,.(Parent,Population)]
          treeList <- maketreelist(data.frame(tree),root="root")
          diagonalNetwork(treeList,fontSize=8)
        })
      }
          
    }
    
  })
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
  
  ###init template tbl
  rv$gt.tbl <- data.frame(alias = character(0)
                      , pop = character(0)
                      , parent = character(0)
                      , dims = character(0)
                      , gating_method = character(0)
                      , gating_args = character(0)
                      , collapseDataForGating = logical(0)
                      , groupBy = character(0)
                      , preprocessing_method = character(0)
                      , preprocessing_args = character(0)
                    )
  #update template table view
  observeEvent(rv$gt.tbl,{
                output$gt_tbl <- DT::renderDataTable(datatable(rv$gt.tbl
                                                               , rownames = FALSE
                                                               , selection = list(mode = "single", selected = 1)
                                                               , extensions = "Buttons"
                                                               , options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel", "pdf", "print"))
                )
                
                , server = FALSE
                )  
  })
  
  ##init dims and nodes
  observeEvent(rv$gs,
               {
                  rv$nodes <- getNodes(rv$gs)
                 fr_pd <- pData(parameters(getData(rv$gs[[1]], use.exprs = FALSE)))
                 marker <- fr_pd[["desc"]]
                 marker[is.na(marker)] <- ""
                 chnl <- fr_pd[["name"]] 
                 
                 names(chnl) <- paste(chnl, marker, sep = " ")
                 updateSelectInput(session, "dims", choices = chnl)
               }
  )

  ##update parent input 
  observeEvent(rv$nodes,
               {
                 updateSelectInput(session, "parent", choices = rv$nodes)
               }
  )
  ##update data based on selected parent
  observeEvent(input$parent,
               {
                 if(input$parent!="")
                  rv$fs <- getData(rv$gs, input$parent)
               })
  
  #plot to inspect the data before adding gating method
  observeEvent(input$bt_plot_data, {
      
      
      output$gt_data_plot <- renderPlot({
                                  
                                    chnl <- input$dims
                                    
                                    mylimit <- ggcyto_par_set(limits = "instrument")
                                    if(length(chnl) == 1)
                                      autoplot(rv$fs[1], x = chnl) + mylimit
                                    else if(length(chnl) == 2)
                                      autoplot(rv$fs[1], x = chnl[1], y = chnl[2]) + mylimit
                                  })
  })
  
  #plot tree
  observeEvent(input$bt_plot_tree, {
    if(length(getNodes(rv$gs))>1)
      output$gt_data_plot <- renderPlot(plot(rv$gs))
  })
  
  #add and plot the gate
  observeEvent(input$bt_apply_gate, {
    nodes <- getNodes(rv$gs)
    #convert some field to be compaitle with template-parser 
    groupBy <- input$groupBy
    groupBy <- paste(groupBy, collapse = ":")
    dims <- input$dims
    if(length(dims)==0){
      output$gt_message <- renderPrint("channels not set yet!")
    }else{
      
      dims <- paste(dims, collapse = ",")
      #parse pop patterns
      pop <- input$pop
      pop.parsed <- ""
      ind <- grepl("A", pop)
      if(sum(ind) == 1)
        pop.parsed <- pop[ind]
      else if(sum(ind) == 2)
        pop.parsed <- "A+/-"
      
      ind <- grepl("B", pop)
      if(sum(ind) == 1)
        pop.parsed <- paste0(pop.parsed, pop[ind])
      else if(sum(ind) == 2)
        pop.parsed <- paste0(pop.parsed, "B+/-")
      #add gates to gs
      new_row <- try(add_pop(rv$gs
                             , alias = input$alias
                             , pop = pop.parsed
                             , parent = input$parent
                             , dims = dims
                             , gating_method = input$gating_method
                             , gating_args = input$gating_args
                             , groupBy = groupBy
                             , collapseDataForGating = as.logical(input$collapseData)
                             , preprocessing_method = input$pp_method
                             , preprocessing_args = input$pp_args)
      )
      if(class(new_row)=="try-error")
        output$gt_message <- renderPrint(new_row)
      else{
        # add the row to template
        rv$gt.tbl <- rbind(rv$gt.tbl, new_row)
        
        #plot the new gates
        rv$nodes <- getNodes(rv$gs)
        rv$new.nodes <- setdiff(rv$nodes, nodes)
        output$gt_data_plot <- renderPlot({
          autoplot(rv$gs[1], rv$new.nodes)
        })  
      }
      
    }
})
  
  #undo gate
  observeEvent(input$bt_undo, {
    #restore gs
    for(toRm in rv$new.nodes)
      Rm(toRm, rv$gs)
    #restore template
    rv$gt.tbl <- rv$gt.tbl[-nrow(rv$gt.tbl), ]
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



