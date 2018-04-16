library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(shiny)
library(highcharter)
library(DT)
library(htmltools)
library(RMySQL)

# Use the `config` package to get the credentials

dbDisconnectAll <- function(){
  ile <- length(dbListConnections(MySQL())  )
  lapply( dbListConnections(MySQL()), function(x) dbDisconnect(x) )
  cat(sprintf("%s connection(s) closed.\n", ile))
}                  
dbDisconnectAll()

dw <- config::get("GTmysql")

con = dbConnect(MySQL(), user=dw$user,  password=dw$password, dbname=dw$dbname, host=dw$host)

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value


# airline_list <- tbl(con, "airlines") %>%
#   collect()  %>%
#   split(.$name) %>%
#   map(~.$carrier)

table_list = dbListTables(con)



ui <- dashboardPage(
  dashboardHeader(title = "Tables Dashboard",
                  titleWidth = 200),
  dashboardSidebar(
     selectInput(
        inputId = "Table",
        label = "Tables:", 
        choices = table_list, 
        selectize = FALSE),
     sidebarMenu(
       selectInput(
         "type",
         "Test Type:", 
         list(
           "All",
           "F1",
           "UCSF500"
         ) , 
         selected =  "All Year", 
         selectize = FALSE),
       actionLink("remove", "Remove detail tabs")
    )
  ),
  dashboardBody(      
    tabsetPanel(id = "tabs",
                  tabPanel(
                    title = "Main Dashboard",
                    value = "page1",
                    fluidRow(
                      column(width = 5,
                         DT::dataTableOutput("summaryCountTable")
                      ),
                      column(width = 5,
                         highchartOutput("summaryCountPie")
                      )
                    ),
                    fluidRow(
                      column(width = 7,
                             p(textOutput("type")),
                             highchartOutput("group_totals")),
                      column(width = 5,
                             p("Click on a month in the plot to see the details"),
                             highchartOutput("top_patients"))
                    )
                  )
      )
  )
)
                      
                    

server <- function(input, output, session) { 
  
  tab_list <- NULL
  
  
  # Preparing the data by pre-joining patients to other
  # tables and doing some name clean-up
  db_patients <- tbl(con, "FILES")  %>% filter(type != "dump") %>% collect()
  

  output$type <- renderText({
    if(input$type == "99")"Click on a type in the plot to see the daily counts"
  })
  
  
  summaryCount <-  db_patients %>%  count(type) %>% collect() %>% arrange(desc(n))

  output$summaryCountTable <-  DT::renderDataTable({ summaryCount }, options=list(bPaginate=FALSE, bInfo=FALSE, bLengthChange=FALSE, bFilter=FALSE), rownames=FALSE)

  output$summaryCountPie <-  renderHighchart({
      # summaryCount %>% hchart("pie")
      highchart() %>%
      hc_add_series_labels_values(summaryCount$type, summaryCount$n, name = "Genomic Test",colorByPoint = TRUE, type = "pie")

  })
    
  
  
  observeEvent(input$bar_clicked,
               {
                 
               })
  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  
  output$group_totals <- renderHighchart({
    
    
  })
}


 options(launch.browser=FALSE)
 options(shiny.port=8111)

shinyApp(ui, server)
