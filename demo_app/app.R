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
library(dplyr)
library(base)
library(devtools)
library(urbnmapr)



clean_data <- read_rds("chicago_inequality.rds")
income_white <- read_rds("income_white.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    #HEADER
   dashboardHeader(title = "Illinois"),
   
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
              h2("Median Household Income of Illinois by County"),
              plotOutput("mymap",height = 500)
              
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
  
  
  output$mymap <- renderPlot({
    
    countydata %>% 
      left_join(counties, by = "county_fips") %>% 
      filter(state_name =="Illinois") %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = medhhincome)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_gradient(labels = scales::number_format(),
                           guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "darkblue") +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Median Household Income")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

