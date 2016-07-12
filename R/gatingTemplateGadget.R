#' gatingTemplate gadget
#'
#' This gadget allows you to interactively build gating template for the given GatingSet.
#'
#' @param gs A GatingSet
#' @return A template as a data.table 
#' @export
#' @examples
#' if (interactive()) {
#'   gt_tbl <- gatingTemplateGadget(gs)
#' }
#' @import shiny
#' @import miniUI
gatingTemplateGadget <- function(gs){
  if(!is(gs, "GatingSet"))
    stop("You did not provide a GatingSet!", call. = FALSE)
  ui <- miniPage(
        gadgetTitleBar("Build a gating template")
        
        , DT::dataTableOutput("gt_tbl")
        
        ,fluidRow(
          column(2,
                 selectInput("parent", choices="", label='Select Parent Population')
                 , bsTooltip("parent"
                             , title = "Parent population specifies the data where the gates will be applied on"
                             , "right"
                 )
                 , br()
                 
                 , selectizeInput("dims", choices="", label='Channels', multiple =TRUE
                                  , options = list(maxItems = 2)
                 )
                 , bsTooltip("dims", title = "specifying the dimensions(1d or 2d) used for gating", "right")
                 , br()
                 
                 , checkboxInput("collapseData", label = "collapse data for gating")
                 , bsTooltip("collapseData", title = "When checked, data is collapsed (within the group if groupBy specified) before gating and the gate is replicated across collapsed samples. If unchecked,then groupBy argument is only used by preprocessing and ignored by gating.", "right")
                 , br()  
                 
                 , bsButton("bt_plot_data",label="plot data", style = "primary")
                 
          )
          , column(10
                   #plot  
                   , plotOutput("gt_data_plot", width = 400, height = 300)
                   
          )
        )
        ,fluidRow(
          column(2,
                 mytextInput.typeahead("alias"
                                       , label ="Population name"
                                       , local = data.frame(name = common_pop_names)
                                       , valueKey = "name"
                                       , tokens = seq_along(common_pop_names)
                                       , template = "{{name}}"
                 )
                 , bsTooltip("alias"
                             , title = "a name used label the cell population, the path composed by the alias and its precedent nodes (e.g. /root/A/B/NAME) has to be uniquely identifiable."
                             , "right"
                             , options = list(container = "body")
                 )
          )
          
          , column(2
                   , selectInput("pop"
                                 , choices=c("A+","A-","B+","B-")
                                 , label='Population pattern'
                                 , multiple =TRUE)
                   , bsTooltip("pop"
                               , title = "population patterns tells the algorithm which side (postive or negative) of 1d gate or which quadrant of 2d gate to be kept when it is in the form of 'A+/-B+/-', 'A' and 'B' should be the full name (or a substring as long as it is unqiuely matched) of either channel or marker of the flow data"
                               , "right"
                               , options = list(container = "body")
                   )
          ) 
          , column(2
                   , selectInput("groupBy", label = "Group By", choices = "", multiple = TRUE)
                   , bsTooltip("groupBy", title = "samples are split into groups by the unique combinations of study variable (i.e. column names of pData,e.g.“PTID:VISITNO”). when split is numeric, then samples are grouped by every N samples", "right")
          )
        )
        , fluidRow(
          column(2
                 , selectInput("gating_method",
                               choices = as.list(gating_methods),
                               label="Gating Method",
                               selected = "mindensity"
                 )
          )
          #                               , textInput("gating_args", label="Gating Parameters", value='')
          , column(2
                   , br()
                   ,bsButton("open_gate_control", "Gating Parameters...", style = "primary")
                   , bsModal("gating_control"
                             # , "gating_args"
                             , trigger = "open_gate_control"
                             , uiOutput("gate_args_inputs")
                   )
          )
        )    
        , fluidRow(
          column(2
                 ,selectInput("pp_method",
                              choices = c("---" = "", as.list(pp_methods)),
                              label = "Preprocessing Method"
                              , selected = ""
                 )
          )
          , column(2
                   , br()
                   , bsButton("open_pp_control", "Preprocessing Parameters...", style = "primary")
          )
        )
        
        
        , fluidRow(
          column(2
                 ,bsButton("bt_plot_tree",label="plot tree", style = "primary")
                 , busyIndicator(text = "conputing the gates..")
                 , bsButton("bt_apply_gate",label="add gate", style = "primary")
                 , bsButton("bt_undo",label="undo", style = "primary")
                 
          )
        )
        , fluidRow(
          column(12
                 , verbatimTextOutput("gt_message")
          )
        )     
  )
  server <- function(input, output, session) {
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
    
    ##init dims and nodes
    observeEvent(gs,
                 {
                   rv$nodes <- getNodes(gs)
                   fr_pd <- pData(parameters(getData(gs[[1]], use.exprs = FALSE)))
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
    
    observeEvent(input$gating_method, {
      output$gate_args_inputs <- renderUI({
        if(input$gating_method == "mindensity"){
          list(sliderInput("gate_range", label = "gate_range", min = 0, max = 6, value = c(1,3))
               , numericInput("adjust", "adjust", value = 2, min = 1, max = 4, step = 0.5)
               , numericInput("num_peaks", "num_peaks", value = 2, min = 1, max = 4, step = 1)
               , numericInput("min", "min", value = 0)
               , numericInput("max", "max", value = 0)
          )
        }
      })
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
        #add gates to gs
        new_row <- try(add_pop(gs
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
    })
    plt = reactive({
      h = digest::digest(input$selnode)
      if(has.key(h,H)){
        return(list(src = H[[h]], alt = input$selnode))
      }else{
        tf = tempfile(fileext = ".png")
        png(file=tf,width = 6*75,height=6*75,units = "px",res=75)
        print(plotGate(gs,input$selnode))
        dev.off()
        H[[h]] = tf 
        return(list(src = H[[h]], alt = input$selnode))
      }
    })
    
    observeEvent(input$selnode,{
      output$gateplot = renderImage(plt(),deleteFile = FALSE)
    })
  }
  runGadget(ui, server)
}