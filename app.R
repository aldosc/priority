#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
require(shiny)
require(igraph)
require(dplyr)
library(DT)
library(readxl)
library(shinydashboard)
require(visNetwork)
require(shinycssloaders)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Target Prioritization"),
        dashboardSidebar(
            fileInput('file1', 'Choose xlsx file',
                      accept = c(".xlsx")
            )
        ),

        # Show a plot of the generated distribution
        dashboardBody(
            
            fluidRow(
                box(withSpinner(DTOutput('tbl'), type = 6), width = 20),
                box(textInput('x2', 'Selected target'), width = 15),
                box(withSpinner(visNetworkOutput('net'), type = 6), width = 100),
                box(withSpinner(DTOutput('tbl2'), type = 6), width = 20)
            )    
            
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    options(shiny.maxRequestSize=40*1024^2)
    
    tar_1 <- eventReactive(input$file1, {  ### This table contains the results for targets
        req(input$file1)
        inFile <- input$file1
        df <- readxl::read_excel(inFile$datapath, 1)
    })
    tool_1 <- eventReactive(input$file1, {  ### This table contains the results for targets
        req(input$file1)
        inFile <- input$file1
        df <- readxl::read_excel(inFile$datapath, 2)
    })
    nodes_1 <- eventReactive(input$file1, {  ### This table contains the results for targets
        req(input$file1)
        inFile <- input$file1
        df <- readxl::read_excel(inFile$datapath, 3)
    })
    
    edges_1 <- eventReactive(input$file1, {  ### This table contains the results for targets
        req(input$file1)
        inFile <- input$file1
        df <- readxl::read_excel(inFile$datapath, 4)
    })
    
    output$tbl = DT::renderDT({
        #req(input$file1)
        DT::datatable(tar_1(), rownames = F, extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'FixedColumns' = NULL), 
                      options = list(escape = F, pageLength = 10, lengthMenu = c(10, 50, 150, 200,400),
                                     autoWidth = T, scrollX = T, dom = 'lBfrtip',
                                     buttons = list('csv', 'excel', 'pdf', list(extend = 'colvis', columns = c(1:39))), fixedColumns = list(leftColumns = 1)
                                ) 
        )
    })
    
    observeEvent(input$tbl_cell_clicked, {
        info = input$tbl_cell_clicked
        # do nothing if not clicked yet, or the clicked cell is not in the 1st column
        if (is.null(info$value)) return()
        updateTextInput(session, 'x2', value = info$value)
    })
  
    
    nodes_filt <- eventReactive(input$tbl_cell_clicked, {
        my_tar <- input$tbl_cell_clicked
        if (is.null(my_tar$value)) return()
        cellValue <- paste(my_tar$value)
        edgess <- as.data.frame(edges_1())
        df_indi <- dplyr::filter(edgess, from %in% cellValue)
        list_of_nodes <- unique(c(df_indi$from, df_indi$to))
        nodess <- as.data.frame(nodes_1())
        df_indi <- dplyr::filter(nodess, id %in% list_of_nodes)
    })
    
    edges_filt <- eventReactive(input$tbl_cell_clicked, {
        my_tar <- input$tbl_cell_clicked
        if (is.null(my_tar$value)) return()
        cellValue <- paste(my_tar$value)
        edgess <- as.data.frame(edges_1())
        df_indi <- dplyr::filter(edgess, from %in% cellValue)
    })
    
    output$net <- renderVisNetwork({
        if (is.null(nodes_filt())) return()
        visNetwork(nodes_filt(), edges_filt(), height = "100%",width = "100%") %>%
            visIgraphLayout() %>%
            visOptions(highlightNearest = list(enabled =TRUE, degree = 2), selectedBy = list(variable = "chembl_id", multiple = T)) %>%
            visGroups(groupname = "Drug", color = "red") %>%
            visGroups(groupname = "Target", color = "orange") %>%
            visNodes(shadow = list(enabled = TRUE, size = 10)) %>%
            visEdges(shadow = F,
                     color = list(color = "lightblue", highlight = "orange")) %>%
            visLegend()
            #visPhysics(stabilization = FALSE) %>%
            #visIgraphLayout(physics = FALSE)
    })
    
    output$tbl2 = DT::renderDT({
      if (is.null(nodes_filt())) return()
        nodess <- as.data.frame(nodes_filt())
        tool_comp <- as.data.frame(tool_1())
        tool_comp_filt <- tool_comp %>% dplyr::filter(chembl_id %in% nodess$id) %>% as.data.frame()
        DT::datatable(tool_comp_filt,  rownames = F, caption = "Tool compounds", extensions = list('Buttons' = NULL, 'ColReorder' = NULL, 'FixedColumns' = NULL), 
                      options = list(escape = F, pageLength = 10, lengthMenu = c(10, 50, 100, 200), 
                                     autoWidth = T, scrollX = T, dom = 'lBfrtip',
                                     buttons = list('csv', 'excel', 'pdf', list(extend = 'colvis', columns = c(2:19))), fixedColumns = list(leftColumns = 1)
                      ) 
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
