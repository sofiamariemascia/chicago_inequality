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
library(shinyWidgets)


clean_data <- read_rds("chicago_inequality.rds")
income_white <- read_rds("income_white.rds")
race_mapping_data <- read_rds("race_mapping_data.rds")
poverty_race <- read_rds("poverty_race.rds")


# Define UI for application that draws a histogram
ui <- dashboardPage( skin = "purple",
  
    #HEADER
   dashboardHeader(title = "Mind the Gap!"),
   
   #SIDEBAR
   dashboardSidebar(
                  
     sidebarMenu(
       menuItem("About the Study", tabName = "about", icon = icon("home")),
       menuItem("Homicide x Income", tabName = "figure1", icon = icon("dashboard")),
       menuItem("Poverty x Race", tabName = "figure2", icon = icon("dashboard")),
       menuItem("Interactive Income Maps", tabName = "maps", icon = icon("th"))
     )
   ),
   
   #BODY
   dashboardBody(
    tabItems(
      tabItem(tabName = "about",
              img(src = "skyline.png"),
              box(
                h3("Please Mind the Gap! Illinois, a Racial and Socioeconomic Spectrum"),
                h4("By Sofia Marie Mascia"),
                div(),
                p("This app aims to prove how socioeconomic gaps in Illinois 
                   indicate negative effects of inequality."),
                p("Chicago has been dubed America's deadliest city by Axios and Reuters. While its homicide rate is not 
                 the highest in the U.S., Chicago has consistently had more total killings than any other U.S. city —
                 with 27 people killed in November 2018 alone."),
                p("The median income in the majority-African-American neighborhood is $20,000 less than the median income for 
                 Chicago, and almost a third of Illinois's poorest neighborhood's residents live below the poverty line."),
                p("The Chicago Police Department recovered 7,000 guns per year that had been illegally owned or associated with
                 a crime between 2013 and 2016, which is three times more than a resident in New York."),
                h5("MY QUESTION"),
                p("Are these factors really related? Are income gaps in Illinois counties correlated to increased
                 homicide? Who makes up the composition of people in poverty? Does your race affect your income? I hope to challege some of these assumptions.")
              
                  ),
              
              
                fluidPage(
                  # A static infoBox
                  infoBox("Population", "12.8 Mil.", icon = icon("users")),
      
                  infoBox("Poverty Rt.", "13 % ", icon = icon("angle-double-down"), color = "light-blue", fill = TRUE),
                  
                  infoBox("Med.Income", "$60.9 K", icon = icon("money")),
                  
                  infoBox("Med. Age", "37.9 yrs.", icon = icon("birthday-cake"), color = "light-blue", fill = TRUE)
              
                )
                
              
      ),
    
      #CODE FOR FIRST BAR PLOT
      tabItem(tabName = "figure1",
               fluidPage(
                 
       
                 
                 box(plotlyOutput("barplot"),
                     width = 750, 
                     height = 900,
                     
                     box(
                       title = "Homicide Death's per 10,000 People in Illinois Counties.", width = 12, solidHeader = TRUE,
                       "Explore this page to view how income affects homicide rates in Illinois Counties. 
                       Counties of importance include Cook County where Chicago is located, and St. Clair County 
                       that lies on the outskirts of St. Louis Missouri. Both counties host the state's largest economic hubs
                       and thus see higher incomes, but they are racked by inequalities in inner city neighboorhoods which could
                       explain the high homicide rates across 2015 and 2016."
                     ),
                     
                     h4("Plot A Parameters"),
                     selectInput("x", "X-axis:",
                                 choices = c("homicide_rate_2015", "homicide_rate_2016"),
                                 selected = "homicide_rate_2016"),
                     selectInput("y", "Y-axis:",
                                 c("income_2015", "income_2016"),
                                 selected = "income_2016"),
                     selectInput("z", "Color By:",
                                 c("income_2015", "income_2016", "County"),
                                 selected = "County"),
                     
                    
                     
                     hr(),
                     helpText("Data from DATA USA.")
                 ) #box for barplot
                
                 
                 
                 ) #fluid row
      ), #tab item tag
                
      
      #CODE FOR SECOND FIGURE
             tabItem(tabName = "figure2",
              fluidPage(
                
                box(plotlyOutput("barplot2"),
                    width = 10000, 
                    height = 700,
                    
                    box(
                      title = "Percentage of Pop. in Poverty Annualy by Race", width = 12, solidHeader = TRUE,
                      "Explore this page to view what perctage of the population lived under the povertyline in Illinois, separated by race.
                      It is striking that around 47 per cent of the population living in poverty was white.  "
                    )
                ) #box for scatter
                
                  ) #fluid row
                ), #tab item tag
              
      #MAP TAB CODE
      tabItem(tabName = "maps",
              fluidRow(
               box(
                 h4("Median Household Income of Illinois by County"),
                 h5("Compare this map to those of income separated by race to the right."),
                 plotlyOutput("mymap")
               ), #box for map1
               
               tabBox(
                 title = "Income by Race",
                 # The id lets us use input$tabset1 on the server to find the current tab
                 id = "tabset1",
                 tabPanel("WHT", 
                          "Median HouseHousehold Income: White",
                          plotlyOutput("mymap2")
                          ),
                 tabPanel("BLK", 
                          "Median Household Income: Black",
                          plotlyOutput("mymap3")
                 ),
                 tabPanel("HISP", 
                          "Median Household Income: Hispanic",
                          plotlyOutput("mymap4")
                 )
                 
                 #plotOutput("mymap3")
               ),
               
               box(
                 title = "Interactive Maps", width = 12, solidHeader = TRUE,
                 "Explore this page to compare how median house-hold income separated by race, compares to median household income in Illinois in.  "
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
  output$barplot <- renderPlotly({
    
    ##Read in the results data from UPSHOT
    clean_data %>% 
      ggplot(aes_string(x = input$x, y = input$y, fill = input$z)) +
      geom_bar(stat="identity", color = "white", width=0.2)+
      xlim(3, 17)+
      labs(title="The Effect of Income on Homicides")+
      theme(legend.position="bottom", title = element_text( size = 13, lineheight = .9, family = "Times", face = "bold.italic", colour = "black"))
    
  })
  
  output$barplot2 <- renderPlotly({
    
    ##Read in the results data from UPSHOT
    poverty_race %>% 
      ggplot(aes_string(x = "total_race", y= "share", fill = "race")) +
      geom_point(stat="identity", size = 4) + 
      xlab("Percentage of Race in Poverty")+
      ylab("Total People in Poverty")+
      ylim(0, 50)+
      geom_line()+
      facet_wrap("year")+
      labs(title="Share of Population in Poverty Per Race")+
      theme_minimal()+
      theme(legend.position="bottom", title = element_text(size = 14, lineheight = .9, family = "Times", 
                                                            face = "bold.italic", colour = "black"), 
            strip.text = element_text(size = 12, face = "bold"))+
      
      scale_fill_brewer(palette = "Purples")
    
  })
  
  
  output$mymap <- renderPlotly({
    
      countydata %>% 
      left_join(counties, by = "county_fips") %>% 
      filter(state_name =="Illinois") %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = medhhincome, county_name=county_name))+
      geom_polygon(color = "#ffffff", size = .25) +
      xlab(NULL) +
      ylab(NULL) +
      scale_fill_gradient(labels = scales::number_format(),
                           guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "darkblue") +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")
            ) +
      labs(fill = "Med. HH Income")
  
     
  })
  
  output$mymap2 <- renderPlotly({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_white, county_name=county_name)) +
      geom_polygon(color = "#ffffff", size = .25) +
      xlab(NULL) +
      ylab(NULL) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#FD1FDF"
                          ) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Income: White")
    
    
  })
  
  output$mymap3 <- renderPlotly({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_black,county_name=county_name)) +
      geom_polygon(color = "#ffffff", size = .25) +
      xlab(NULL) +
      ylab(NULL) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#C9FF02"
      ) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Income: Black")
    
    
  })
  
  output$mymap4 <- renderPlotly({
    
    race_mapping_data %>% 
      ggplot(mapping = aes(long, lat, group = group, fill = income_hispanic, county_name=county_name)) +
      geom_polygon(color = "#ffffff", size = .25) +
      xlab(NULL) +
      ylab(NULL) +
      scale_fill_gradient(labels = scales::number_format(),
                          guide = guide_colorbar(title.position = "top"),
                          low = "white", high = "#61C8FF"
      ) +
      
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      theme(legend.title = element_text(),
            legend.key.width = unit(.5, "in")) +
      labs(fill = "Income: Hisp")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

