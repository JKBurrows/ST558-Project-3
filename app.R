# Author: Joshua Burrows
# Purpose: 
# Date Created: 18 November 2020

library(shiny) 
library(shinydashboard)
library(tidyverse)
library(DT)

source("build.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Star Wars Viewership", 
    titleWidth = 450
    ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("journal-whills")), 
      menuItem("Explore Data", tabName = "EDA", icon = icon("galactic-senate")), 
      menuItem("Cluster Data", tabName = "clust", icon = icon("old-republic"))
      )
    ), 
  
  dashboardBody(
    tabItems(
      
      # Home page 
      tabItem(
        tabName = "home", 
        fluidRow(
          column(width = 6, 
                 h2("About This App"), 
                 box(
                   p("Info about this app. Describe data."),
                   width = NULL
                   )
            ), 
          column(width = 6, 
                 h2("How to Use This App"), 
                 box(
                   p("Info about how to use this app"), 
                   width = NULL
                   )
            )
          )
        ), 
      
      # EDA page 
      tabItem(
        tabName = "EDA",
        fluidRow(
          # Column: select a plot 
          column(width = 3,
                 # Box: which info
                 box(
                   radioButtons("info", 
                                "Which Info", 
                                choices = c("Views", 
                                            "Ratings")),
                   width = NULL
                   ),
                 conditionalPanel(
                   condition = "input.info == 'Views'",
                   box(radioButtons("plots",
                                  "Which Plot", 
                                  choices = c("Overall", 
                                              "Gender",
                                              "Trek Fan")), 
                     width = NULL)), 
                 conditionalPanel(
                   condition = "input.info == 'Ratings'", 
                   box(radioButtons("rankInfo", 
                                    "Which Plot", 
                                    choices = c("Overall", 
                                                "SW Fans")), 
                       width = NULL))
                   ),
          # Display the Plot
          column(width = 9, 
                 conditionalPanel(condition = "input.info == 'Views'", 
                                  plotlyOutput("plotEDA")), 
                 conditionalPanel(condition = "input.info == 'Ratings'", 
                                  tableOutput("numericEDA"))
                 )
        )
      ), 
      
      # Clustering tab
      tabItem(
        tabName = "clust", 
        fluidRow(
          column(
            width = 3, 
            box(
              radioButtons("dendSubset", 
                           "Subsets", 
                           choices = c("Overall", 
                                       "Male", 
                                       "Female")
              ), 
              width = NULL
            )
            
          ),
          column(
            width = 9,
            plotOutput("dend")
          )
        )
      )
    )
  )
)

server <- function(input, output){
  # EDA tab
  # Get plot to output 
  output$plotEDA <- renderPlotly(
    if(input$plots == "Overall"){
      pctSeenOverall
    } else if(input$plots == "Gender"){
      pctSeenGender
    } else if(input$plots == "Trek Fan"){
      pctSeenTrek
    } else{
      stop("Error")
    }
  ) 
  
  # EDA tab
  # Get numeric summary to output 
  output$numericEDA <- renderTable(
    if(input$rankInfo == "Overall"){
      ranks
    } else if(input$rankInfo == "SW Fans"){
      fanRanks
    } else{
      stop("Error")
    }
  )
  
  # Clustering tab
  output$dend <- renderPlot(
    if(input$dendSubset == "Overall"){
      hierClust %>% as.dendrogram() %>% plot(main = "", xlab = "")
    } else if(input$dendSubset == "Male"){
      hierClustM %>% as.dendrogram() %>% plot(main = "", xlab = "")
    } else if(input$dendSubset == "Female"){
      hierClustF %>% as.dendrogram() %>% plot(main = "", xlab = "")
    } else{
      stop("Error")
    }
  )
}


shinyApp(ui, server)

























