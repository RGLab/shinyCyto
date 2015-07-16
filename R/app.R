library(shiny)
library(flowWorkspace)
library(openCyto)
library(DT)
datadirectory = "../inst/extdata"
ui <-   navbarPage("OpenCyto",
             tabPanel("Data",{
               
               navlistPanel(
                 tabPanel("Upload Files",
                          tags$br(),
                          div(fileInput("filechooser",h5("Upload FCS Files"), multiple = TRUE),
                          h6(DT::dataTableOutput(outputId = "chooser")), style = 'width:90%;')),
                 
                 tabPanel("Existing Data",
                   div(h5("Existing Data"),
                   h6(DT::dataTableOutput(outputId = "existing_data")), style = 'width:90%;'),
                   actionButton(inputId = "load",label = "Load Data"),
                   div(verbatimTextOutput("message"),style='width:75%')
                   
                 ),id="datanavlist"
               )
               
             }),
             
             tabPanel("Compensate & Transform","Compensate & Transform"),
             tabPanel("Gating Template","Build a Gating Template"),
             id = "tabs"
             
             )
server <- function(input, output){
  output$existing_data = DT::renderDataTable(data.frame(data = list.files(datadirectory,full.names = TRUE,recursive = FALSE))[,("data"),drop=FALSE],rownames=FALSE,selection="single")
  output$chooser = DT::renderDataTable(input$filechooser[,c("name","size")],selection='single')
  observeEvent(input$load,{
    output$message = renderPrint(cat("Choose a dataset"))
    s = input$existing_data_rows_selected
    if(length(s)){
      gs = load_gs(path = s)
      output$message = renderPrint(cat("Loaded ",s))
    }
  })
  }
shinyApp(ui = ui, server = server)
