gs_menu_tab <- function(){
  tabItem("gs_menu" 
          
          #               ,  tabBox(id = "gs_tab", width = NULL
          #                      ,tabPanel(title = "Pheno Data", value = "pd_tab", icon = icon("th")
          ,DT::dataTableOutput("pd_tbl")
          
          , tabBox(id = "gh_tab", width = NULL
                   ,tabPanel(title = "Gating Tree", value = "tree_tab", icon = icon("sitemap")
                             # ,selectInput("sn_select", choices = c("select one sample ---" = ""), label = NULL)
                             # ,checkboxInput("isBool", "Show boolean gates")
                             ,div(div(
                               diagonalNetworkOutput("tree",width="400px",height="300px")
                               ,style="display:inline-block;float:left;")
                               ,div(imageOutput("gateplot",width = "300px",height="300px")
                                    ,style="margin-left:400px;", id = "tabSet")
                               ,style="width:100%;height:100%;"
                             )
                   )
                   ,tabPanel(title = "Gating Layout", value = "gate_layout_tab", icon = icon("picture-o")
                             , plotOutput("gate_layout"))
                   ,tabPanel(title = "Pop Stats", value = "stats_tab", icon = icon("bar-chart")
                             ,DT::dataTableOutput("pop_stats_tbl")
                   )
          )
          #                           )
          #                      )
  )
}
