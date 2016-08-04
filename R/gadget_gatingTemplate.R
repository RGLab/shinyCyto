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
#' @include module_gatingTemplate.R
gatingTemplateGadget <- function(gs){
  if(!is(gs, "GatingSet"))
    stop("You did not provide a GatingSet!", call. = FALSE)
  gs <- clone(gs)
  ui <- miniPage(
        gadgetTitleBar("Build a gating template")
        
        , miniContentPanel(gatingTemplateUI("gt1"))
  )
  server <- function(input, output, session) {
    callModule(gatingTemplateServer, "gt1", gs)
  }
  runGadget(ui, server, viewer = browserViewer())
}