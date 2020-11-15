# Author: Joshua Burrows
# Purpose: 
# Date Created: 18 November 2020

library(shiny) 
library(shinydashboard)
library(tidyverse)
library(DT)
library(dendextend)
library(gbm)
library(formula.tools)

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
                "downloadDend", 
                "Download Plot"
              ), 
              helpText("Download dendrogram with selected clustering"), 
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
              checkboxGroupInput(
                "preds", 
                "Select Predictors", 
                choiceNames = c("Gender", 
                                "Trek Fan",
                                "Age", 
                                "Household Income", 
                                "Education Level", 
                                "Location"
                                ), 
                choiceValues = c("gender", 
                                 "trekFan", 
                                 "age", 
                                 "HHIncome", 
                                 "education", 
                                 "location"),
                selected = NULL
              ),
              conditionalPanel(
                condition = "input.whichModel == 'Boosted Tree'", 
                selectInput(
                  "tune", 
                  "Tuning Grid Size", 
                  choices = c(1, 2, 3, 4, 5)
                )
              ),
              #uiOutput("varImpInput"),
              conditionalPanel(
                condition = "output.showHide == 'show'", 
                radioButtons(
                        "showVarImp",
                        "Show Variable Importance?",
                        choiceNames = c("Yes", "No"),
                        choiceValues = c(TRUE, FALSE)
                      )
              ),
              actionButton(
                "train", 
                "Train Model"
              ),
              width = NULL
            )
          ), 
          column(
            width = 9, 
            tabBox(
              tabPanel(
                title = "Model Accuracy",
                tableOutput("accuracy")
              ),
              tabPanel(
                title = "Variable Importance", 
                plotOutput("varImportance")
              ),
              tabPanel(
                title = "Formula",
                h5("Your model used the formula:"),
                uiOutput("formulaMathFormat")
              ),
              tabPanel(
                title = "Prediction", 
                fluidRow(
                  column(
                    width = 3,
                    box(
                      uiOutput("genderPredInput"), 
                      uiOutput("trekFanPredInput"),
                      uiOutput("agePredInput"),
                      uiOutput("HHIncomePredInput"),
                      uiOutput("educationPredInput"),
                      uiOutput("locationPredInput"),
                      width = NULL
                    )
                  ), 
                  column(
                    width = 6, 
                    box(
                      actionButton(
                        "getPrediction", 
                        "Make Prediction"
                      ),
                      textOutput("prediction"),
                      width = NULL
                    )
                  )
                )
              ),
              width = NULL, 
              id = "modTabs"
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

server <- function(input, output, session){
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
  dend <- reactive({
    if(input$dendSubset == "Overall"){
      hierClust %>%
        as.dendrogram() %>%
        color_branches(k = as.numeric(input$numClust))
    } else if(input$dendSubset == "Male"){
      hierClustM %>%
        as.dendrogram() %>%
        color_branches(k = as.numeric(input$numClust))
    } else if(input$dendSubset == "Female"){
      hierClustF %>%
        as.dendrogram() %>%
        color_branches(k = as.numeric(input$numClust))
    } else{
      stop("Error")
    }
  }
  )
  
  # Clustering tab
  # Output dendrogram
  output$dend <- renderPlot(
    plot(dend(), main = "Clustering", xlab = "")
  )
  
  # Clustering tab
  # Download dendrogram
  output$downloadDend <- downloadHandler(
    filename = "StarWarsDendrogram.png", 
    content = function(file){
      png(file, 
          width = 1000, 
          height = 600)
      
      plot(dend(), main = "Clustering", xlab = "")
      
      dev.off()
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
  # Create model
  mod <- reactive({
    withProgress(
      {
        req(input$train)
    
        if(isolate(input$whichModel) == "Boosted Tree"){
          boostMod <- getBoost(preds = isolate(input$preds), 
                         tnSize = isolate(input$tune))
          boostMod
        } else if(isolate(input$whichModel) == "Logistic Regression"){
          logReg <- getLogReg(preds = isolate(input$preds))
          logReg
        } else{
          stop("Error")
        }
      }, 
      message = "Training model"
    )
  }
  )
  
  observeEvent(
    input$train, 
    {
      updateTabsetPanel(
        session, 
        inputId = "modTabs", 
        selected = "Model Accuracy"
      )
    }
  )
  
  # Modeling tab
  # Variable importance
  output$varImportance <- renderPlot({
    req(input$train)
    
    mod() %>% varImp() %>% plot()
  })
  
  output$showHide <- renderText({
    if(length(input$preds) > 1){
      "show"
    } else{
      "hide"
    }
  })
  
  outputOptions(output, "showHide", suspendWhenHidden = FALSE)
  
  # output$varImpInput <- renderUI({
  #   if(length(input$preds) > 1){
  #     radioButtons(
  #       "showVarImp", 
  #       "Show Variable Importance?", 
  #       choiceNames = c("Yes", "No"), 
  #       choiceValues = c(TRUE, FALSE)
  #     )
  #   }
  # })
  
  hideTab(inputId = "modTabs",
          target = "Variable Importance")
  
  observeEvent(
    input$train,
    {
      if(is.null(input$showVarImp)){
      hideTab(inputId = "modTabs",
              target = "Variable Importance")
      } else if((input$showVarImp == TRUE)){
        showTab(inputId = "modTabs",
                target = "Variable Importance")
      } else if(input$showVarImp == FALSE){
        hideTab(inputId = "modTabs",
               target = "Variable Importance")
      } else{
        stop("Error") 
      }
      
      if(length(input$preds) <= 1){
        hideTab(inputId = "modTabs",
                target = "Variable Importance")
      }
    }
  )
  
  # Modeling tab
  # Formula math format 
  output$formulaMathFormat <- renderUI({
    req(input$train) 
    
    charForm <- isolate(input$preds) %>% getFormula() %>% as.character() %>% str_split(pattern = "~")
    
    charForm <- charForm[[1]][2]
    
    withMathJax(h5(paste0("\\(fan\\)", "\\(\\sim\\)", "\\(", charForm, "\\)")))
  })
  
  # Modeling tab
  # Accuracy
  output$accuracy <- renderTable({
    req(input$train)
    
    trainAcc <- mod()$results$Accuracy %>% max()
    
    predictions <- predict(mod(), test)
      
    testAcc <- postResample(predictions, test$fan)[1]
    
    tibble("Train Accuracy" = trainAcc, 
           "Test Accuracy" = testAcc)
  })
  
  # Modeling tab
  # Prediction 
  currentPreds <- reactive({
    req(input$train)
    
    isolate(input$preds)
  })
  
  output$genderPredInput <- renderUI({
    if("gender" %in% currentPreds()){
      selectInput(
        "genderPred",
        "Gender",
        choices = c("Male", "Female")
      )
    }
  })
  
  output$trekFanPredInput <- renderUI({
    if("trekFan" %in% currentPreds()){
      selectInput(
        "trekFanPred",
        "Trek Fan",
        choices = c("Yes", "No")
      )
    }
  })
  
  output$agePredInput <- renderUI({
    if("age" %in% currentPreds()){
      selectInput(
        "agePred",
        "Age",
        choices = c("18-29", "30-44", "45-60", "> 60")
      )
    }
  })
  
  output$HHIncomePredInput <- renderUI({
    if("HHIncome" %in% currentPreds()){
      selectInput(
        "HHIncomePred",
        "Household Income",
        choices = c("$0 - $24,999",
                    "$25,000 - $49,999", 
                    "$50,000 - $99,999", 
                    "$100,000 - $149,999", 
                    "$150,000+")
      )
    }
  })
  
  output$educationPredInput <- renderUI({
    if("education" %in% currentPreds()){
      selectInput(
        "educationPred",
        "Education Level",
        choices = c("Less than high school degree", 
                    "High school degree", 
                    "Some college or Associate degree", 
                    "Bachelor degree", 
                    "Graduate degree")
      )
    }
  })
  
  output$locationPredInput <- renderUI({
    if("location" %in% currentPreds()){
      selectInput(
        "locationPred",
        "Location",
        choices = c("New England", 
                    "Middle Atlantic", 
                    "South Atlantic", 
                    "East North Central", 
                    "East South Central", 
                    "West North Central", 
                    "West South Central", 
                    "Mountain", 
                    "Pacific")
      )
    }
  })
  
  predSelections <- reactive({
    df <- tibble(gender = "", 
                 trekFan = "", 
                 age = "", 
                 HHIncome = "", 
                 education = "", 
                 location = "")
    
    if("gender" %in% currentPreds()){
      df$gender <- input$genderPred
    }
    
    if("trekFan" %in% currentPreds()){
      df$trekFan <- input$trekFanPred
    }
    
    if("age" %in% currentPreds()){
      df$age <- input$agePred
    }
    
    if("HHIncome" %in% currentPreds()){
      df$HHIncome <- input$HHIncomePred
    }
    
    if("education" %in% currentPreds()){
      df$education <- input$educationPred
    }
    
    if("location" %in% currentPreds()){
      df$location <- input$locationPred
    }
    
    df
  })
  
  vals <- reactiveValues(pred = NULL)
  
  observeEvent(input$train, {vals$pred <- NULL})
  
  observeEvent(input$getPrediction, {vals$pred <- predict(mod(), newdata = isolate(predSelections())) %>% as.character()})
  
  output$prediction <- renderText({
    vals$pred
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

























