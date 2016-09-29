#' @importFrom networkD3 diagonalNetworkOutput
gatingTreeUI <- function(id){
  
  ns <- NS(id)
  div(
    radioButtons(ns("type"), "plot type",  choices = c("2d scatter", "1d density"), selected = "1d density", inline = TRUE)
    ,div(
        diagonalNetworkOutput(ns("tree")
                                ,width="800px",height="300px"
                                )
          ,style="display:inline-block;float:left;"
          )
    ,div(
        imageOutput(ns("gateplot"),width = "800px",height="400px")
        # plotOutput(ns("gateplot") ,width = "300px",height="300px")
          ,style="margin-left:400px;")
    ,style="width:100%;height:100%;"
    ,id = ns("tabset")
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
#' @importFrom openCyto best.separation mindensity
gatingTreeServer <- function(input, output, session, gs){
  # rv = reactiveValues()
  H = hash()
  output$tree = renderDiagonalNetwork({
        stats = getPopStats(gs[1] ,showHidden=TRUE)
        tree <- stats[,.(Parent,Population)]
        
        treelist <- maketreelist(data.frame(tree),root="root")
        diagonalNetwork(treelist, linkColour = "black")
      })
  
  shinyjs::js$fire()
  

  
  plt = reactive({
        node <- input$selnode
        h = digest(node)
        if(has.key(h,H)){
          return(list(src = H[[h]], alt = node))
        }else{
          tf = tempfile(fileext = ".png")
          png(file=tf,width = 500,height=500,units = "px"
              # ,res=75
              )
          
          
          if(input$type == "2d scatter"){
            p <- plotGate(gs, node)
            
            print(p)
          }else{
            
            fs <- getData(gs, node)
            #for now we use the first sample
            fr <- fs[[1, use.exprs = FALSE]]
            #exclude the non-stained channels
            pd <- pData(parameters(fr))
            pd <- pd[!is.na(pd[["desc"]]),]
            channels <- pd[["name"]]
            fr <- fs[[1,channels]]
            
            children <- getChildren(gs, node)
            if(length(children)==0){ # terminal node : simply plot density
              p <- autoplot(fr) 
              p@arrange.main <- ""
              print(p)
            }else{
              gating.function <- mindensity
              #otherwise plot marker selection process
              flist <- sapply(channels, function(channel){
                g <- gating.function(fr, channel)
                g
              })
              flist <- flowWorkspace:::compact(flist)
              
              p <- best.separation(flist, fr, debug.mode = T
                                   # , min.percent = input$min.percent
              )
              plot(p)
            }
            
            
          }
          
          dev.off()
          H[[h]] = tf
          return(list(src = H[[h]], alt = node))
        }
      })

  observeEvent(input$selnode,{
         
          # browser()
        output$gateplot = renderImage(plt(),deleteFile = FALSE)
    })


  }
    
  