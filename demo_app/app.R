#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(ggplot2)
library(dplyr)
library(base)
library(devtools)
library(urbnmapr)
library(plotly)
library(RColorBrewer)


clean_data <- read_rds("chicago_inequality.rds")
income_white <- read_rds("income_white.rds")
race_mapping_data <- read_rds("race_mapping_data.rds")
poverty_race <- read_rds("poverty_race.rds")


# Define UI for application that draws a histogram
ui <- dashboardPage( skin = "purple",
  
    #HEADER
   dashboardHeader(title = "Mind the Gap!"
                   ),
   
   #SIDEBAR
   dashboardSidebar(
                  
     sidebarMenu(
       menuItem("About the Study", tabName = "about", icon = icon("home")),
       menuItem("Homicide x Drug", tabName = "figure1", icon = icon("dashboard")),
       menuItem("Income X Maps", tabName = "maps", icon = icon("th"))
     )
   ),
   
   #BODY
   dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              box(
                h3("Please Mind the Gap! Illinois, a Racial and Socioeconomic Diaspra"),
                h4("By Sofia Marie Mascia"),
                div(),
                p("This app aims to prove how socioeconomic gaps in Illinois 
                   indicate negative effects of inequality elsewhere."),
                br(),
                
                p("Chicago has been dubed America's deadliest city by Axios and Reuters. While its homicide rate is not 
                 the highest in the U.S., Chicago has consistently had more total killings than any other U.S. city —
                 with 27 people killed in November 2018 alone."),
             
                h5("WHY IT MATTERS"),
                p("Racial segregation, wealth inequality, gangs and the inability of law enforcement to solve 
                 crimes have fueled a crime epidemic. The presense of drugs and violence in areas of all different income 
                 levels has only made mattes worse. Minority and impoverished neighborhoods have received the brunt of the impact."),
                br(),
                
                p("The median income in the majority-African-American neighborhood is $20,000 less than the median income for 
                 Chicago, and almost a third of Illinois's poorest neighborhood's residents live below the poverty line."),
                br(),
                
                p("The Chicago Police Department recovered 7,000 guns per year that had been illegally owned or associated with
                 a crime between 2013 and 2016, which is three times more than a resident in New York."),
                br(),
                
                h5("MY QUESTION"),
                p("Are these factors really related? Is the prevelence of drugs in an Illinois county correlated to increased
                 homicide? Do all low income areas have increased drug use or homicide levels? I hope to challege some of these assumptions.")
                
                
                  ),
                
                fluidPage(
                  # A static infoBox
                  infoBox("Population", "12.8 Mil.", icon = icon("users")),
      
                  infoBox("Poverty Rt.", "13 % ", icon = icon("angle-double-down"), color = "light-blue", fill = TRUE),
                  
                  infoBox("Med.Income", "$60.9 K", icon = icon("money")),
                  
                  infoBox("Med. Age", "37.9 yrs.", icon = icon("birthday-cake"), color = "light-blue", fill = TRUE)
              
                )
                
              
      ),
    
      
      tabItem(tabName = "figure1",
               fluidPage(
                 box(plotOutput("barplot")
                     ), #box for barplot
                 
                 box(plotOutput("barplot2")
                 ), #box for scatter
                 
                 box(h4("Plot A Parameters"),
                      selectInput("x", "X-axis:",
                                  choices = c("homicide_rate_2015", "homicide_rate_2016"),
                                  selected = "homicide_rate_2016"),
                      selectInput("y", "Y-axis:",
                                  c("income_2015", "income_2016"),
                                  selected = "income_2016"),
                      selectInput("z", "Color By:",
                                  c("income_2015", "income_2016", "geo_name"),
                                  selected = "geo_name"),
                      hr(),
                      helpText("Data from DATA USA.")
                      ), #box for plot A param. 
                
              
                 box(h4("Plot B Parameters"),
                   selectInput("a", "X-axis:",
                               choices = c("adult_smoking_2015", "adult_smoking2016"),
                               selected = "adult_smoking_2016"),
                   selectInput("b", "Y-axis:",
                               c("income_2015", "income_2016"),
                               selected = "income_2016"),
                   hr(),
                   helpText("Data from DATA USA.")) #box for plot B param.
                 
                  ) #fluid row
                  ), #tab item tag
              
              
      #MAP TAB CODE
      tabItem(tabName = "maps",
              fluidRow(
               box( 
                 h4("Median Household Income of Illinois by County"),
                 h5("Compare this map to those of income separated by race to the right."),
                 plotOutput("mymap")
               ), #box for map1
               
               tabBox(
                 title = "Income by Race",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1", height = "250px",
                 tabPanel("WHT", 
                          "Household Income: White",
                          plotOutput("mymap2")
                          ),
                 tabPanel("BLK", 
                          "Household Income: Black",
                          plotOutput("mymap3")
                 ),
                 tabPanel("HISP", 
                          "Household Income: Hispanic",
                          plotOutput("mymap4")
                 )
                 
                 #plotOutput("mymap3")
               )
               
          
              ) #fluid row
              
              ) #tab item (maps)
             ) #tab items tag
            ) #dashboard body tag
)




# SERVER LOGIC
server <- function(input, output) {
  
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
  set.seed(122)
  
  
  
  
   #output plot stuff
  output$barplot <- renderPlot({
    
    
    ##Read in the results data from UPSHOT
    clean_data %>% 
      ggplot(aes_string(x = input$x, y = input$y, fill = input$z)) +
      geom_bar(stat="identity", color = "white", width=0.2, position = position_dodge(width=0.9))+
      labs(title="The Effect of Income on Homicides in Chicago Counties")+
      theme(legend.position="bottom")
    
  })
  
  output$barplot2 <- renderPlot({
    
    ##Read in the results data from UPSHOT
    poverty_race %>% 
      ggplot(aes_string(x = "total_race", y= "share", fill = "race")) +
      geom_bar(stat="identity", color = "white", width = 0.05 ) + scale_x_log10()+
      labs(title="Share of Population in Poverty Per Race")+
      theme_minimal()+
      theme(legend.position="bottom")+
      scale_fill_brewer(palette = "Purples")
    
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
      labs(fill = "White: Med. HH Income")
  
     
  })
  
  output$mymap2 <- renderPlot({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_white)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#FD1FDF"
                          ) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Black: Med. HH Income")
    
    
  })
  
  output$mymap3 <- renderPlot({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_black)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#C9FF02"
      ) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Hisp: Med. HH Income")
    
    
  })
  
  output$mymap4 <- renderPlot({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_hispanic)) +
      geom_polygon(color = "#ffffff", size = .25) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#61C8FF"
      ) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Hisp: Med. HH Income")
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

