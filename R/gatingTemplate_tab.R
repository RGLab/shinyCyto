
  tabItem("GatingTemplate"
          ,DT::dataTableOutput("gt_tbl")
          
          
          ,div(
            div(
              div(selectInput("parent", choices="", label='Select Parent Population')
                  
                  , selectizeInput("dims", choices="", label='Channels', multiple =TRUE
                                   , options = list(maxItems = 2)
                  )
                  , myActionButton("bt_plot_data",label="plot data")
                  , style="display:inline-block;float:left;"
              )
              
              
              , div(plotOutput("gt_data_plot", width = 400, height = 300)
                    , style="display:inline-block;float:left;"
              )
            )
            ,div(mytextInput.typeahead("alias"
                                       , placeholder="Population name"
                                       , local = data.frame(name = c("boundary", "bcell"
                                                                     ,"lymph", "live"
                                                                     ,"tcell", "transitional"
                                                                     , "nonDebris"
                                                                     , "cd3", "cd4", "cd8")
                                       )
                                       , valueKey = "name"
                                       , tokens = 1:10
                                       , template = "{{name}}"
            )
            # ,style="display:inline-block;"
            ) 
#             , div(selectInput("pop", choices=c("A+","A-","B+","B-"), label='Population pattern', multiple =TRUE)
#                   ,style="display:inline-block;float:left;") 
#             , div(selectInput("gating_method",
#                               choices = as.list(gating_methods),
#                               label="Gating Method",
#                               selected = "mindensity"
#             )
#             ,style="display:inline-block;float:left;") 
#             , div(textInput("gating_args", label="Gating Parameters", value='')
#                   ,style="display:inline-block;float:left;") 
#             , div(checkboxInput("collapseData", label = "collapse data for gating")
#                   ,style="display:inline-block;float:left;") 
#             , div(selectInput("groupBy", label = "Group By", choices = "", multiple = TRUE)
#                   ,style="display:inline-block;float:left;") 
#             , div(selectInput("pp_method",
#                               choices = c("---" = "", as.list(pp_methods)),
#                               label = "Preprocessing Method"
#                               , selected = ""
#                             )
#                 ,style="display:inline-block;float:left;"
#                 ) 
#             , div(textInput("pp_args", label="Preprocessing parameters", value='')
#                   ,style="display:inline-block;float:left;")
          ,style="display:inline-block")
          
          ,div(
            
            
            myActionButton("bt_plot_tree",label="plot tree")
            , myActionButton("bt_apply_gate",label="add gate")
            , myActionButton("bt_undo",label="undo")
            
            
          )
          , div(verbatimTextOutput("gt_message"),style='width:90%')     
  )
  
  
  
  
  
  
}
