#' @importFrom shinysky busyIndicator
#' @include utils.R module_gatingMethod.R
gatingTemplateUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(DT::dataTableOutput(ns("gt_tbl")))
    ,fluidRow(
      column(2,
             selectInput(ns("parent"), choices="", label='Select Parent Population')
             , bsTooltip(ns("parent")
                         , title = "Parent population specifies the data where the gates will be applied on"
                         , "right"
             )
             , br()
             
             , selectizeInput(ns("dims"), choices="", label='Channels', multiple =TRUE
                              , options = list(maxItems = 2)
             )
             , bsTooltip(ns("dims"), title = "specifying the dimensions(1d or 2d) used for gating", "right")
             , br()
             
             , checkboxInput(ns("collapseData"), label = "collapse data for gating")
             , bsTooltip(ns("collapseData"), title = "When checked, data is collapsed (within the group if groupBy specified) before gating and the gate is replicated across collapsed samples. If unchecked,then groupBy argument is only used by preprocessing and ignored by gating.", "right")
             , br()  
             
             , bsButton(ns("bt_plot_data"),label="plot data", style = "primary")
             
      )
      , column(10
               #plot  
               , plotOutput(ns("gt_data_plot"), width = 400, height = 300)
               
      )
    )
    ,fluidRow(
      column(2,
             mytextInput.typeahead(ns("alias")
                                   , label ="Population name"
                                   , local = data.frame(name = common_pop_names)
                                   , valueKey = "name"
                                   , tokens = seq_along(common_pop_names)
                                   , template = "{{name}}"
             )
             , bsTooltip(ns("alias")
                         , title = "a name used label the cell population, the path composed by the alias and its precedent nodes (e.g. /root/A/B/NAME) has to be uniquely identifiable."
                         , "right"
                         , options = list(container = "body")
             )
      )
      
      , column(2
               , selectInput(ns("pop")
                             , choices=c("A+","A-","B+","B-")
                             , label='Population pattern'
                             , multiple =TRUE)
               , bsTooltip(ns("pop")
                           , title = "population patterns tells the algorithm which side (postive or negative) of 1d gate or which quadrant of 2d gate to be kept when it is in the form of 'A+/-B+/-', 'A' and 'B' should be the full name (or a substring as long as it is unqiuely matched) of either channel or marker of the flow data"
                           , "right"
                           , options = list(container = "body")
               )
      ) 
      , column(2
               , selectInput(ns("groupBy"), label = "Group By", choices = "", multiple = TRUE)
               , bsTooltip(ns("groupBy"), title = "samples are split into groups by the unique combinations of study variable (i.e. column names of pData,e.g.“PTID:VISITNO”). when split is numeric, then samples are grouped by every N samples", "right")
      )
    )
    , fluidRow(
      column(2
             , selectInput(ns("gating_method"),
                           choices = as.list(gating_methods),
                           label="Gating Method",
                           selected = "mindensity"
             )
      )
      
      , column(2
               , br()
               ,bsButton(ns("open_gate_control"), "Gating Parameters...", style = "primary")
               , bsModal(ns("gating_control")
                         # , "gating_args"
                         , trigger = ns("open_gate_control")
                         , uiOutput(ns("gate_args_inputs"))
                         # , bsButton("gating_args_apply", "apply", style = "primary")
               )
      )
    )    
    , fluidRow(style="visibility: hidden",
      column(2
             , selectInput(ns("pp_method"),
                          choices = c("---" = ""
                                      # , as.list(pp_methods)
                                      )
                          ,label = "Preprocessing Method"
                          , selected = ""
                          )
             
              )
      , column(2
               , br()
               , bsButton(ns("open_pp_control"), "Preprocessing Parameters...", style = "primary", disabled = TRUE)
               , bsModal(ns("pp_control")
                         , trigger = ns("open_pp_control")
                         , uiOutput(ns("pp_args_inputs"))
               )
      )
    )
    
    
    , fluidRow(
      column(2
             ,bsButton(ns("bt_plot_tree"),label="plot tree", style = "primary")
             , busyIndicator(text = "conputing the gates..")
             , bsButton(ns("bt_apply_gate"),label="add gate", style = "primary")
             , bsButton(ns("bt_undo"),label="undo", style = "primary")
             
      )
    )
    , fluidRow(
      column(12
             , verbatimTextOutput(ns("gt_message"))
      )
    )     
  )
}

#' @import openCyto
#' @import flowWorkspace
#' @import flowCore
#' @importFrom ggcyto ggcyto_par_set
#' @importFrom ggplot2 autoplot
#' @importFrom DT datatable
gatingTemplateServer <- function(input, output, session, gs){
  rv = reactiveValues()
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
  
  ##init dims 
  fr_pd <- pData(parameters(getData(gs[[1]], use.exprs = FALSE)))
  marker <- fr_pd[["desc"]]
  marker[is.na(marker)] <- ""
  chnl <- fr_pd[["name"]] 
  names(chnl) <- paste(chnl, marker, sep = " ")
  updateSelectInput(session, "dims", choices = chnl)
  
  #init nodes
  rv$nodes <- getNodes(gs)  
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
                   rv$fs <- getData(gs, input$parent)
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
    if(length(getNodes(gs))>1)
      output$gt_data_plot <- renderPlot(plot(gs))
  })
  
  ns <- session$ns
  #render gating method specific controls
  gating_control_id <- ns("gating_control_id1")
  observeEvent({input$open_gate_control}, {
    fs <- rv$fs
    chnl <- input$dims
    if(!is.null(fs)&&!is.null(chnl)){
      data.range <- range(fs[[1, use.exprs = FALSE]])[, chnl]  
      # browser()
      output$gate_args_inputs <- renderUI({
        
        if(input$gating_method == "mindensity"){
          if(length(chnl)>1)
            data.range <- data.range[,1] #hack:pick the first channel
          rv$gating_module <- mindensityArgs
          mindensityArgsUI(gating_control_id, data.range)
          
        }else{
          
          rv$gating_module <- genericArgs
          genericArgsUI(gating_control_id)
        }
      })
      
    }
  })
  
  #collect gating args
  observeEvent(input$gating_control, {
    if(!is.null(rv$gating_module)){
      #call gating-method-specific module 
      gating_args_list <- callModule(rv$gating_module, gating_control_id)       
      if(is.list(gating_args_list)){
        rv$gating_args <- list2string(gating_args_list)
      }else
        rv$gating_args <- gating_args_list #generic args control already returns a string 
      
      
    }
    
  })
  
  
  pp_control_id <- ns("pp_control_id1")
  observeEvent({input$open_pp_control}, {
    output$pp_args_inputs <- renderUI({
      rv$pp_module <- genericArgs
      genericArgsUI(pp_control_id)
    })
  })
  
  #collect gating args
  observeEvent(input$pp_control, {
    if(!is.null(rv$pp_module)){
      #call gating-method-specific module 
      pp_args_list <- callModule(rv$pp_module, pp_control_id)       
      if(is.list(pp_args_list)){
        rv$pp_args <- list2string(pp_args_list)
      }else
        rv$pp_args <- pp_args_list #generic args control already returns a string 
      
      # browser()
    }
    
  })
  
  #add and plot the gate
  observeEvent(input$bt_apply_gate, {
    nodes <- getNodes(gs)
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
      
      if(is.null(rv$pp_args))
        rv$pp_args <- ""
      if(is.null(rv$gating_args))
        rv$gating_args <- ""
      # browser()
      #add gates to gs
      new_row <- try(add_pop(gs
                             , alias = input$alias
                             , pop = pop.parsed
                             , parent = input$parent
                             , dims = dims
                             , gating_method = input$gating_method
                             , gating_args = rv$gating_args
                             , groupBy = groupBy
                             , collapseDataForGating = as.logical(input$collapseData)
                             , preprocessing_method = input$pp_method
                             , preprocessing_args = rv$pp_args)
      )
      if(class(new_row)=="try-error")
        output$gt_message <- renderPrint(new_row)
      else{
        # add the row to template
        rv$gt.tbl <- rbind(rv$gt.tbl, new_row)
        
        #plot the new gates
        rv$nodes <- getNodes(gs)
        rv$new.nodes <- setdiff(rv$nodes, nodes)
        output$gt_data_plot <- renderPlot({
          autoplot(gs[1], rv$new.nodes)
        })  
      }
      
    }
  })
  
  #undo gate
  observeEvent(input$bt_undo, {
    #restore gs
    for(toRm in rv$new.nodes)
      Rm(toRm, gs)
    #restore template
    rv$gt.tbl <- rv$gt.tbl[-nrow(rv$gt.tbl), ]
    rv$nodes <- getNodes(gs)
  })
  
}