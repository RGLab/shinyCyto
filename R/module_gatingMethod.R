#' Generic UI module for the gating method that does not have dedicated controls defined.
#' 
genericArgsUI <- function(id){
  ns <- NS(id)
  tagList(textInput(ns("args_textInput"), label="Parameters", value=''))
}
genericArgs <- function(input, output, session){
  input$args_textInput
}

#' UI for mindensity method
mindensityArgsUI <- function(id, data_range = c(0, 6)){
  ns <- NS(id)
  data_range <- round(data_range, digits = 1)
  rn <- round(0.2 * abs(diff(data_range)))
  preselcted <- data_range + c(rn, -rn)
  preselcted <- round(preselcted, digits = 1)
  tagList(sliderInput(ns("gate_range"), label = "gate_range"
                      , min = data_range[1], max = data_range[2]
                      , value = preselcted
                      , sep = ""
                      )
       , numericInput(ns("adjust"), "adjust", value = 2, min = 1, max = 4, step = 0.5)
       , numericInput(ns("num_peaks"), "num_peaks", value = 2, min = 1, max = 4, step = 1)
       , numericInput(ns("min"), "min", value = data_range[1])
       , numericInput(ns("max"), "max", value = data_range[2])
  )
}

mindensityArgs <- function(input, output, session){
  return(list(gate_range = input$gate_range
              , adjust = input$adjust
              , num_peaks = input$num_peaks
              , min = input$min
              , max = input$max
              ))
}

list2string <- function(args_list){
  # args <- NULL
  # for(arg in names(args_list)){
  #   argv <- args_list[[arg]]
  #   args <- c(args, paste(arg, argv, sep = "="))     
  # }
  # 
  # args <- paste(args, collapse = ",")   
  args <- deparse(args_list)
  args <- paste0(args, collapse = "")
  args <- sub("^structure\\(list\\(", "", args)
  
  args <- sub("\\), \\.Names \\= .*$", "", args)
  return(args)
}