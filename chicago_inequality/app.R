
library(tidyverse)
library(stringr)
library(ggplot2)
library(kableExtra)
library(lubridate)
library(knitr)
library(readr)
library(dplyr)
library(haven)
library(base)
library(shinydashboard)

# Define UI for application.

clean_data <- read_rds("chicago_inequality.rds")

ui <- fluidPage(
  
  # Creating a Title
  titlePanel("Chicago: A SocioEconomic Diaspra"),
  
  # Formatting the layout of the page 
  sidebarLayout(
    sidebarPanel(
      h4("Plot Parameters"),
      selectInput("x", "X-axis:",
                  choices = c("geo_name", "adult_smoking_2015", "adult_smoking2016", "homicide_rate_2015", "homicide_rate_2016"),
                  selected = "homicide_rate_2016"),
      selectInput("y", "Y-axis:",
                  c("income_2015", "income_2016"),
                  selected = "income_2016"),
      selectInput("z", "Color By:",
                  c("income_2015", "income_2016", "geo_name"),
                  selected = "geo_name"),
      hr(),
      helpText("Data from DATA USA.")
    ),
 
       
    
    mainPanel(
      plotOutput("barplot", width = 800, height = 450, hover = "plot_hover", hoverDelay = 0),
      uiOutput("dynamic")
    )
  ) 
)


server <- function(input, output) {
  
  output$barplot <- renderPlot({
    
    
    ##Read in the results data from UPSHOT
    clean_data %>% 
      ggplot(aes_string(x = input$x, y = input$y, fill = input$z)) +
      geom_bar(stat="identity", color = "white", width=0.5, position = position_dodge(width=0.9))+
      labs(title="The Effect of Income on Homicide and Smoking rates in Chicago Counties")+
      theme_minimal()+
      theme(legend.position="bottom")
    
    
  })
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    # print(str(hover)) # list
    y <- nearPoints(geo_name, input$plot_hover)[input$y]
    req(nrow(y) != 0)
    y
  })
}


# Run the application below. 

shinyApp(ui = ui, server = server)

