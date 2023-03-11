library(shiny)
library(tidyverse)
ds <- read_delim("UAH-lower-troposphere-long.csv.bz2")


ui <- fluidPage(
  titlePanel("Global Temperature Data"),
  tabsetPanel(type = "tabs",
              tabPanel("main",
                       h2("Explanatory Text"),
                       p("This dataset is a collection of",
                         tags$b("Global Temperatures"),
                         "in different regions in the lower troposphere from 1978 to 2023"),
                       p("In all regions in 1979 the average temperature was -0.431 C",
                         em("While"), "in 2022 the average was 0.2 C"),
                       img(src = "https://www.nsstc.uah.edu/climate/2023/february/202302_Map.png",
                           width = "700px", height = "500px")),
              tabPanel("graph",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(
                             "year",
                             "Year:",
                             min = 1978 ,
                             max = 2023,value = 2000),
                           radioButtons("color",
                                        "Color is",
                                        choices= c("Region","Red"),
                                        selected = "Red"
                           )),
                         mainPanel(
                           h2("Plot"),
                           plotOutput("plot"),
                           h2("Description"),
                           verbatimTextOutput("plotText")))),
              tabPanel("table",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             "im_reg",
                             "Select a Region",
                             choices = append(ds$region, "All Regions"),
                             selected = "All Regions")),
                         mainPanel(
                           h2("Table"),
                           dataTableOutput("ltData"),
                           h2("Summary"),
                           verbatimTextOutput("tableText"))))
  ))


server <- function(input, output) {
  
  output$plot <- renderPlot({
    color_ds <-  input$color
    years <- input$year
    if (color_ds == "Region"){
      ds %>%
        filter(temp <= 3 | temp >= -3) %>% 
        filter(year == years) %>% 
        ggplot() +
        geom_point(aes(x = month, y = temp, col=region)) +
        xlim(1,12) +
        ylim(-3.5,3.5)+
        labs(title = "Temperature by month",
             y = "Temperature",
             x = 'Month')
    } else{
      ds %>%
        filter(temp <= 3 | temp >= -3) %>% 
        filter(year == years) %>% 
        ggplot() +
        geom_point(aes(x = month, y = temp, col="red")) +
        xlim(1,12) +
        ylim(-3.5,3.5)+
        labs(title = "Temperature by month",
             y = "Temperature",
             x = 'Month')
    }
  })
  
  output$plotText <- renderPrint({
    im_year <- input$year
    avg_temp <- ds %>% filter(year == im_year) %>% summarise(avg = mean(temp))
    
    cat("In", im_year, "the average temperature was", avg_temp[[1]], "C")
  })
  
  output$ltData <- renderDataTable({
    im_reg <- input$im_reg
    if(im_reg == "All Regions"){
      ds
    }else{
      ds %>% filter(region == im_reg)
    }
  })
  output$tableText <- renderPrint({
    im_region <- input$im_reg
    if(im_region == "All Regions"){
      avg_temp_79 <- ds %>% filter(year == 1979) %>% summarise(avg = mean(temp))
      avg_temp_22 <- ds %>% filter(year == 2022) %>% summarise(avg = mean(temp))
    } else {
      avg_temp_79 <- ds %>% filter(year == 1979) %>%
        filter(region == im_region) %>% summarise(avg = mean(temp))
      avg_temp_22 <- ds %>% filter(year == 2022) %>%
        filter(region == im_region) %>% summarise(avg = mean(temp))
    }
    change <- avg_temp_22[[1]]- avg_temp_79[[1]]
    cat("In", im_region, "in 1979 the average temperature was", avg_temp_79[[1]], "C ")
    cat("and in 2022 was", avg_temp_22[[1]], "C which is an change of", change, "C")
  })
}
#took me about 8 hours
shinyApp(ui = ui, server = server)
