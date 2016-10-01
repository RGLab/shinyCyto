#' gatingTree shiny app
#'
#' This app allows you to interactively plot gated data by clicking the tree note

#' @param gs A GatingSet
#' @return A shiny app object
#' @export
#' @examples
#' if (interactive()) {
#'   app <- gatingTreeApp(gs)
#'   runApp(app)
#' }
#' @import shiny
#' @import miniUI
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @include module_gsTree.R
gatingTreeApp <- function(gs){
  if(!is(gs, "GatingSet"))
    stop("You did not provide a GatingSet!", call. = FALSE)
  # gs <- clone(gs)
  ui <- miniPage(
    miniTitleBar("shinyCyto gadget: gating tree")
      
      , miniContentPanel(gatingTreeUI("tree1"))
      ,useShinyjs()
      ,extendShinyjs(
        # script = "inst/www/actions.js"
        script =   system.file("www/actions.js", package = "shinyCyto")
                     )
  )
  server <- function(input, output, session) {
    callModule(gatingTreeServer, "tree1", gs)
  }
  shinyApp(ui = ui, server = server)
  
}

#' gatingTree gadget
#'
#' This gadget allows you to interactively plot gated data 
#'
#' @param gs A GatingSet
#' @export
#' @examples
#' if (interactive()) {
#'   gatingTreeGadget(gs)
#' }
gatingTreeGadget <- function(gs){
  app <- gatingTreeApp(gs)
  runGadget(app
            , viewer = browserViewer()
  )
}               
               
                   
                 
             



