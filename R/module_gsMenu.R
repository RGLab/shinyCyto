gsMenuUI <- function(id){
  ns <- NS(id)
  tagList( 
          
          #               ,  tabBox(id = "gs_tab", width = NULL
          #                      ,tabPanel(title = "Pheno Data", value = "pd_tab", icon = icon("th")
          DT::dataTableOutput(ns("pd_tbl"))
          
          , tabBox(id = ns("gh_tab"), width = NULL
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

gsMenuSever <- function(input, output, session){
  #---- update gs panels ----
  observeEvent(rv$gs,{
    #update badge of gs menu
    output$gs_menu_obj <- renderMenu(menuItem("GatingSets"
                                              , tabName = "gs_menu"
                                              , icon = icon("database")
                                              , badgeLabel = "1"
                                              , badgeColor = "green")
    )
    
    #update the pData tbl
    output$pd_tbl <- DT::renderDataTable(datatable(pData(rv$gs)
                                                   , rownames = FALSE
                                                   ,selection = list(mode = "single", selected = 1)
                                                   , filter = "bottom"
                                                   , options = list(search = list(regex = TRUE)
                                                                    , autoWidth = TRUE
                                                                    #                                                                       , dom = 'T<"clear">lfrtip'
                                                                    #                                                                       , tableTools = list(sSwfPath = copySWF())
                                                   )
                                                   # , extensions = list("ColVis", "TableTools")#they are retired by Buttons extension, which is yet to be supported by DT package
    )
    , server = FALSE
    )  
    
    #---- Populate sn selector ----    
    #       updateSelectInput(session, "sn_select"
    #                     , choices = sampleNames(rv$gs)
    #                     , label = NULL)
    
  })
  
  #---update gh related panels---
  observeEvent(input$pd_tbl_rows_selected,{
    sn <- rownames(pData(rv$gs)[input$pd_tbl_rows_selected, ,drop = FALSE])
    
    if(nchar(sn) > 0){
      gh <- rv$gs[[sn]]
      nPop <- length(getNodes(gh))
      if(nPop > 1){
        output$gate_layout <- renderPlot(autoplot(gh))
        
        #pop stats
        stats <- getPopStats(gh)
        output$pop_stats_tbl <- DT::renderDataTable(stats)
        # tree
        
        output$tree = renderDiagonalNetwork({
          tree = getPopStats(rv$gs[sn],format="long",showHidden=TRUE)[,.(Parent,Population)]
          treeList <- maketreelist(data.frame(tree),root="root")
          diagonalNetwork(treeList,fontSize=8)
        })
      }
      
    }
    
  })
  
}