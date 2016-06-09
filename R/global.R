library(shiny)
plugIns <- openCyto:::.getPluginMethods()
gating_methods <- c(plugIns[["gating"]], "refGate", "boolGate")
pp_methods <- plugIns[["preprocessing"]]

