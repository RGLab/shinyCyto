import_tab <- function(datadirectory){
  tabItem("Import" 
          
          , div(
            div(textInput("path_import",label = "workspaces path", value = datadirectory), style="display:inline-block")
            ,div(shinyDirButton("ws_dir_btn",label = "choose...", title = "Please select a folder", buttonType = "primary"),style="display:inline-block")
            
            # , bsButton(inputId = "refresh_import",label = "Scan")
            
            , div(h6(DT::dataTableOutput(outputId = "file_table")))
            ,verbatimTextOutput("message1")
            , bsButton("open_ws",label="next-->")
            , id = "ws_select_tab" 
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
