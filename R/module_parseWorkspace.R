#' @importFrom shinyjs hidden
parseWorkspaceUI <- function(id, datadirectory){
  ns <- NS(id)
  tagList(
      div(
            div(textInput("path_import",label = "workspaces path", value = datadirectory), style="display:inline-block")
            ,div(shinyDirButton("ws_dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary"),style="display:inline-block")
            
            # , bsButton(inputId = "refresh_import",label = "Scan")
            
            , div(h6(DT::dataTableOutput(outputId = "file_table")))
            ,verbatimTextOutput("message1")
            , bsButton("open_ws",label="next-->")
            , id = ns("ws_select_tab")
          )
          
          
          
          
          
          , hidden(div(selectInput("grp_selected", choices = '', label = "Group")
                       
                       , hidden(div(id = "kw_block"
                                    , div(selectInput("kw_selected", choices = '', label = "keyword", multiple = TRUE)
                                          ,style="display:inline-block;"
                                    )
                                    , helpPopup(title = "keywords can be extracted from either XML workspaces or FCS files and attached to the phenoData of resulting GatingSets"
                                                , content = ""
                                                , trigger = "hover"
                                    )
                                    
                                    , radioButtons("kw_src", choices = c("XML", "FCS"), inline = TRUE, label = "keyword source")
                       )
                       )
                       
                       
                       
                       , div(a(id = "toggleAdvanced"
                               , "show/hide advanced settings"
                               , style = "cursor:pointer")
                       ),
                       # hidden(
                       div(id = "advanced"
                           , style = "display:inline-block"
                           #leaf node option
                           , div(
                             div(style="display:inline-block;"
                                 , checkboxInput("isLeafBool", label = "Leaf boolean gates", value = FALSE)
                             )
                             , helpPopup(title = "Skipping the leaf/terminal boolean nodes can speed up the parsing significantly (especially for ICS gating scheme that typically contains lots of polyfunctional boolean gates, 
                                                                      which can be computed through COMPASS package.
                                                                      Also if user does want them back later, simply call 'recompute()' method to calculate them without re-parsing the entire workspace."
                                         , content = ""
                                         , trigger = "hover"
                             )
                           )
                           #subset option
                           , div(
                             span("Filter samples by pData:")
                             
                             , helpPopup(title = "select samples to parse by typing the keyword in each filtering boxes under each field"
                                         , content = ""
                                         , trigger = "hover"
                             )
                             , dataTableOutput("sub_pd")    
                           )
                           
                           # )
                       )
                       ,hidden(verbatimTextOutput("message2"))
                       , div(
                         div(bsButton("back_to_ws",label="<--back")
                             ,style="display:inline-block;"
                         )
                         ,disabled(div(bsButton("parse_ws",label = "Parse Workspace")
                                       ,style="display:inline-block;"
                         )
                         )
                       )
                       ,id = "grp_select_tab"
          )
          )
          
          
  )
}

parseWorkspaceSever <- function(input, output, session){
  # select ws folder
  shinyDirChoose(input, "ws_dir_btn", session = session, roots = roots)
  
  observeEvent(input$ws_dir_btn, {
    path_selected <- parseDirPath(roots, input$ws_dir_btn)
    updateTextInput(session,'path_import', value = path_selected)
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
  
}