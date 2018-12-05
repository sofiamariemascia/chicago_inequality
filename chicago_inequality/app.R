
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
      selectInput("y", "Color By:",
                  c("income_2015", "income_2016"),
                  selected = "geo_name"),
      hr(),
      helpText("Data from DATA USA.")
    ),
 
       
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)


server <- function(input, output) {
  
  output$barplot <- renderPlot({
    
    
    ##Read in the results data from UPSHOT
    clean_data %>% 
      ggplot(aes_string(x = input$x, y = input$y, fill = input$y)) +
      geom_bar(position = "dodge", stat="identity")

  
    
    
  })
}


# Run the application below. 

shinyApp(ui = ui, server = server)

