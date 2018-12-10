#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
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
library(leaflet)
library(ggmap)


clean_data <- read_rds("chicago_inequality.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    #HEADER
   dashboardHeader(title = "Illinois: A SocioEconomic Diaspra"),
   
   #SIDEBAR
   dashboardSidebar(
     sidebarMenu(
       menuItem("Figure 1", tabName = "figure1", icon = icon("dashboard")),
       menuItem("Maps", tabName = "maps", icon = icon("th"))
     )
   ),
   
   #BODY
   dashboardBody(
    tabItems(
      tabItem(tabName = "figure1",
               fluidRow(
                 box(plotOutput("barplot")),
                 
                 box( h4("Plot Parameters"),
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
                      helpText("Data from DATA USA.")))
              
              ),
      
      tabItem(tabName = "maps",
              h2("Widgets tab content"),
              leafletOutput("mymap",height = 1000)
              
              )
             )
            )
)



# SERVER LOGIC
server <- function(input, output) {
  set.seed(122)
   #output plot stuff
  output$barplot <- renderPlot({
    
    
    ##Read in the results data from UPSHOT
    clean_data %>% 
      ggplot(aes_string(x = input$x, y = input$y, fill = input$z)) +
      geom_bar(stat="identity", color = "white", width=0.2, position = position_dodge(width=0.9))+
      labs(title="The Effect of Income on Homicide and Smoking rates in Chicago Counties")+
      theme_minimal()+
      theme(legend.position="bottom")
    
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet(options = 
              leafletOptions(dragging = FALSE,
                             minZoom = 14, 
                             maxZoom = 18)) %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite") %>% 
      setView(lng = 36.7, 
            lat = 40.3333, 
            zoom = 18) 
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

