library(shiny)
library(DT)
data <- mtcars

shinyApp(
  ui = fluidPage(
    selectInput("select1", "select cyl:", choices = unique(data$cyl)),
    uiOutput("checkbox"),
    dataTableOutput("table")
  ),
  server = function(input, output) {
    
    output$checkbox <- renderUI({
      choice <-  unique(data[data$cyl %in% input$select1, "gear"])
      checkboxGroupInput("checkbox","Select gear", choices = choice)
      
    })
    
    output$table <- renderDataTable({
      data <-  data %>% stats::filter(cyl %in% input$select1) %>% stats::filter(gear %in% input$checkbox)
      datatable(data)
    })
    
  }
)