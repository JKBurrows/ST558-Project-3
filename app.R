# Author: Joshua Burrows
# Purpose: 
# Date Created: 18 November 2020

library(shiny) 
library(shinydashboard)
library(tidyverse)
library(DT)
library(dendextend)

source("build.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Star Wars Viewership", 
    titleWidth = 450
    ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "home", icon = icon("journal-whills")), 
      menuItem("Explore", tabName = "EDA", icon = icon("galactic-senate")), 
      menuItem("Cluster", tabName = "clust", icon = icon("old-republic")), 
      menuItem("Model", tabName = "model", icon = icon("galactic-republic")),
      menuItem("Download", tabName = "down", icon = icon("jedi"))
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
                                            "Movie Ratings",
                                            "Character Ratings"
                                            )),
                   width = NULL
                   ),
                 conditionalPanel(
                   condition = "input.info == 'Views'",
                   box(radioButtons("plots",
                                  "Breakdown", 
                                  choices = c("Overall", 
                                              "Gender",
                                              "Trek Fan")), 
                     width = NULL)), 
                 conditionalPanel(
                   condition = "input.info == 'Movie Ratings'", 
                   box(
                     radioButtons("rankInfo", 
                                  "Breakdown", 
                                  choices = c("Overall", 
                                              "SW Fans")), 
                       width = NULL)), 
                 conditionalPanel(
                   condition = "input.info == 'Character Ratings'", 
                   box(
                     radioButtons("charRankInfo", 
                                  "Breakdown", 
                                  choiceNames = c("All", 
                                                  "Rebel Scum", 
                                                  "Imperial"), 
                                  choiceValues = c("all", 
                                                   "rebel", 
                                                   "imperial")), 
                     width = NULL
                   )
                 )
                   ),
          # Display the EDA plot or summary
          column(width = 9, 
                 conditionalPanel(condition = "input.info == 'Views'", 
                                  plotlyOutput("plotEDA")), 
                 conditionalPanel(condition = "input.info == 'Movie Ratings'", 
                                  h4(textOutput("numericEDATitle")), 
                                  DT::dataTableOutput("numericEDA")), 
                 conditionalPanel(condition = "input.info == 'Character Ratings'", 
                                  DT::dataTableOutput("charNumericEDA"))
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
              radioButtons(
                "dendSubset", 
                "Subsets", 
                choices = c("Overall", 
                            "Male", 
                            "Female")
              ), 
              width = NULL
            ),
            box(
              selectInput(
                "numClust", 
                "Number of Clusters", 
                choices = c(2, 3, 4, 5, 6, 7, 8, 9, 10), 
                selected = 2
              ), 
              width = NULL
            ), 
            box(
              downloadButton(
                "downloadClust", 
                "Download", 
              ), 
              helpText("Download data with selected clustering"),
              width = NULL
            )
          ),
          column(
            width = 9,
            plotOutput("dend")
          )
        )
      ), 
      
      # Modeling tab
      tabItem(
        tabName = "model", 
        fluidRow(
          column(
            width = 3, 
            box(
              radioButtons(
                "whichModel", 
                "Pick a Model", 
                choices = c("Boosted Tree", 
                            "Logistic Regression"), 
                selected = "Boosted Tree"
              ), 
              width = NULL
            ), 
            conditionalPanel(
              condition = "input.whichModel == 'Boosted Tree'", 
              box(
                checkboxGroupInput(
                  "predsBoost", 
                  "Select Predictors", 
                  choiceNames = c("Gender", 
                                  "Trek Fan"), 
                  choiceValues = c("gender", 
                                   "trekFan"),
                  selected = c("gender", 
                               "trekFan")
                ), 
                actionButton(
                  "train", 
                  "Train Model"
                ),
                width = NULL
              )
            )
          ), 
          column(
            width = 9, 
            box(
              textOutput("textBoost"),
              width = NULL
            )
          )
        )
      ),
      
      # Subset and download tab
      tabItem(
        tabName = "down", 
        fluidRow(
          column(
            width = 3,
            box(
              checkboxGroupInput(
                "genderSubset", 
                "Filter by Gender?", 
                choices = c("Male", "Female", "NA"), 
                selected = c("Male", "Female", "NA")
              ), 
              width = NULL
            ), 
            box(
              downloadButton("download", 
                             "Download"), 
              width = NULL
            )
          ), 
          column(
            width = 9, 
            DT::dataTableOutput("downloadTable")
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
  # Get movie rankings summary 
  output$numericEDA <- DT::renderDataTable(
    {
      if(input$rankInfo == "Overall"){
        ranks
      } else if(input$rankInfo == "SW Fans"){
        fanRanks
      } else{
        stop("Error")
      }
    }, 
    options = list(scrollX = TRUE)
  )
  
  output$numericEDATitle <- renderText(
    if(input$rankInfo == "Overall"){
      "Movie Rankings as Percentages"
    } else if(input$rankInfo == "SW Fans"){
      "Movie Rankings as Percentages, Star Wars Fans Only"
    } else{
      stop("Error")
    }
    )
  
  # EDA tab
  # Get character ratings summary 
  output$charNumericEDA <- DT::renderDataTable(
    {
      if(input$charRankInfo == "all"){
        favor
      }
    }, 
    options = list(scrollX = TRUE)
    )
  
  # Clustering tab
  # Get dendrogram 
  output$dend <- renderPlot(
    if(input$dendSubset == "Overall"){
      hierClust %>% 
        as.dendrogram() %>% 
        color_branches(k = as.numeric(input$numClust)) %>% 
        plot(main = "Clustering", xlab = "")
    } else if(input$dendSubset == "Male"){
      hierClustM %>% 
        as.dendrogram() %>% 
        color_branches(k = as.numeric(input$numClust)) %>% 
        plot(main = "Clustering", xlab = "")
    } else if(input$dendSubset == "Female"){
      hierClustF %>% 
        as.dendrogram() %>% 
        color_branches(k = as.numeric(input$numClust)) %>% 
        plot(main = "Clustering", xlab = "")
    } else{
      stop("Error")
    }
  )
  
  # Clustering tab
  # Get clustered data 
  clustered <- reactive({
    if(input$dendSubset == "Overall"){
      clustData <- clustSub 
      clustData$cluster <- hierClust %>% stats::cutree(k = as.numeric(input$numClust))
    } else if(input$dendSubset == "Male"){
      clustData <- clustSubM
      clustData$cluster <- hierClustM %>% stats::cutree(k = as.numeric(input$numClust))
    } else if(input$dendSubset == "Female"){
      clustData <- clustSubF 
      clustData$cluster <- hierClustF %>% stats::cutree(k = as.numeric(input$numClust))
    } else{
      stop("Error")
    }
    
    clustData
  })
  
  # Clustering tab
  # Download clustered data
  output$downloadClust <- downloadHandler(
    filename = "ClusteredStarWarsData.csv",
    content = function(tempFile){
      write.csv(clustered(), tempFile, row.names = FALSE)
    }
  )
  
  # Modeling tab
  output$textBoost <- renderText({
    input$train
    
    boostMod <- isolate(input$predsBoost) %>% getBoost()
    
    boostMod$results$Accuracy
  })
  
  # Subset and Download Tab
  # Get filtered data 
  SWSub <- reactive({
    sub <- SW
    
    if(!("Male" %in% input$genderSubset)){
      sub <- sub %>% filter((gender == "Female" | is.na(gender)))
    }
    
    if(!("Female" %in% input$genderSubset)){
      sub <- sub %>% filter((gender == "Male" | is.na(gender)))
    }
    
    if(!("NA" %in% input$genderSubset)){
      sub <- sub %>% filter((gender == "Male" | gender == "Female"))
    }
    
    sub
  })
  
  # Subset and download tab
  # Display filtered data
  output$downloadTable <- DT::renderDataTable(
    {SWSub()},
    options = list(scrollX = TRUE)
  )
  
  # Subset and download tab 
  # Download filtered data
  output$download <- downloadHandler(
    filename = "StarWarsData.csv",
    content = function(tempFile){
      write.csv(SWSub(), tempFile, row.names = FALSE)
      }
  )

}

shinyApp(ui, server)

























