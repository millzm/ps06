library(shiny)
library(tidyverse)
ds <- read_delim("UAH-lower-troposphere-long.csv.bz2")


ui <- fluidPage(
  titlePanel("Global Temperature Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year","Year:",min = 1978 ,max = 2023,value = 2000),
      selectInput("reg","Select a Region", choices = ds$region)),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("main"),
                  tabPanel("graph",
                           plotOutput("plot"),
                           textOutput("plotText")),
                  tabPanel("table",
                           dataTableOutput("ltData")))
)))

server <- function(input, output) {
  
    output$plot <- renderPlot({
      years <- input$year
      ds %>%
        filter(temp <= 3 | temp >= -3) %>% 
        filter(year == years) %>% 
        ggplot() +
        geom_point(aes(x = month, y = temp, col=region)) +
        xlim(1,12) +
        ylim(-3.5,3.5)+
        labs(title = "temp by month",
             y = "Tempurture",
             x = 'month')
    })
    output$plotText <- renderText("The average temp this year was")
    output$ltData <- renderDataTable({
      ds
    })
}
shinyApp(ui = ui, server = server)
