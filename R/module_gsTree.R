#' @importFrom networkD3 diagonalNetworkOutput
gatingTreeUI <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
        radioButtons(ns("type"), "plot type",  choices = c("2d scatter", "1d density"), selected = "1d density", inline = TRUE)
    
        )
    ,fluidRow(# ,div(
        column(6,
            diagonalNetworkOutput(ns("tree")
                                    # ,width="400px",height="300px"
                                    )
              # ,style="display:inline-block;float:left;"
              # )
            )
        ,column(6,
        # ,div(
            imageOutput(ns("gateplot")
                        # ,width = "600px",height="400px"
                        )
            ,dataTableOutput(ns("metricstbl"))
        )
        # ,column(2,
        # ,div(
           
          # ,style="margin-left:802px;"
          # ,style="display:inline-block;float:left;"
          
        # )
        # ,style="width:100%;height:100%;"
        
            # )
        # )
    )
  )
}

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
#' @importFrom networkD3 renderDiagonalNetwork diagonalNetwork
#' @import openCyto
#' @import flowWorkspace
#' @import flowCore
#' @importFrom ggcyto ggcyto_par_set
#' @importFrom ggplot2 autoplot
#' @importFrom hash hash has.key
#' @importFrom DT datatable
#' @importFrom digest digest
#' @import data.table
#' @importFrom cytoEx getMetrics plot.flowClust
gatingTreeServer <- function(input, output, session, gs){
  rv = reactiveValues()
  H = hash()
  output$tree = renderDiagonalNetwork({
        stats = getPopStats(gs[1] ,showHidden=TRUE)
        tree <- stats[,.(Parent,Population)]
        
        treelist <- maketreelist(data.frame(tree),root="root")
        diagonalNetwork(treelist, linkColour = "black")
      })
  
  shinyjs::js$fire()
  
  ns <- session$ns
  metricsID <- ns("metricstbl")
  
  # gsEnv <- getOption("openCyto")[["exhaustive"]][[gs@guid]]
  
  rv$key_plot <- reactive(paste0(input$selnode, input$type))
  
  
  observeEvent(rv$key_plot(),{
    
    key_plot <- rv$key_plot()
    node <- input$selnode
    
    if(is.null(node))
      return(NULL)
    
    tf = tempfile(fileext = ".png")
    key_metrics <- paste0(node, "metrics")
    #generate the plot and metrics if not run before
    if(!has.key(key_plot, H))
    {
      png(file=tf,width = 500,height=500,units = "px")
      
      if(input$type == "2d scatter"){
          p <- plotGate(gs, node)
          print(p)
      }else{
        
        metrics <- getMetrics(gs, node)
        if(is.null(metrics)){ # terminal node : simply plot density
          fs <- getData(gs, node)
          #for now we use the first sample
          fr <- fs[[1, use.exprs = FALSE]]
          #exclude the non-stained channels
          pd <- pData(parameters(fr))
          pd <- pd[!is.na(pd[["desc"]]),]
          channels <- pd[["name"]]
          
          fr <- fs[[1,channels]]  
          p <- autoplot(fr) 
          p@arrange.main <- ""
          print(p)
        }else{
          
          plot.flowClust(gs, node)
          H[[key_metrics]] = metrics
        
          
        }
      }
      # browser()
      #update hash with new plot
      dev.off()
      H[[key_plot]] = tf
      
      children <- getChildren(gs, node)
      if(length(children)!=0)
        rv$key_metrics <- key_metrics #potentially trigger the table render
        
      
      
    }
    
    
    output$gateplot = renderImage(list(src = H[[key_plot]], alt = node),deleteFile = FALSE)
  })

  observeEvent(rv$key_metrics, {
    output$metricstbl <- renderDataTable(H[[rv$key_metrics]])      
  })
  }
    
  