# Author: Joshua Burrows
# Purpose: Explore Star Wars viewership data
# Date Created: 18 November 2020

library(shiny) 
library(shinydashboard)
library(tidyverse)
library(DT)
library(dendextend)
library(gbm)
library(formula.tools)

#source("https://raw.githubusercontent.com/JKBurrows/ST558-Project-3/main/build.R")
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
                   h4("The purpose of this app is to explore data about Star Wars viewership. I used data that has been made available", HTML("<a href='https://github.com/fivethirtyeight/data/tree/master/star-wars-survey'>here</a>"), "by FiveThrityEight."),
                   h4("Use the tabs along the left hand side to navigate."), 
                   width = NULL
                   )
            ), 
          column(width = 6, 
                 h2("Tabs"), 
                 box(
                   h3("Explore"), 
                   h4("Subset the data and create common summaries and plots"),
                   h3("Cluster"),
                   h4("Group data using hierarchical clustering and download the results"),
                   h3("Model"),
                   h4("Create models to predict who is a Star Wars fan and who isn't"),
                   h3("Download"),
                   h4("Subset and download the data"),
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
                                            "Movie Rankings",
                                            "Demographic Info"
                                            )),
                   width = NULL
                   ),
                 conditionalPanel(
                   condition = "input.info == 'Views'",
                   box(radioButtons("plots",
                                  "Breakdown", 
                                  choices = c("Overall",
                                              "Gender",
                                              "Trek Fan", 
                                              "Household Income")), 
                     width = NULL)), 
                 conditionalPanel(
                   condition = "input.info == 'Movie Rankings'",
                   box(
                     radioButtons(
                       "movieToRank", 
                       "Movie", 
                       choices = c("Ep. I", 
                                   "Ep. II", 
                                   "Ep. III", 
                                   "Ep. IV", 
                                   "Ep. V", 
                                   "Ep. VI")
                     ), 
                     width = NULL
                   ),
                 ), 
                 conditionalPanel(
                   condition = "input.info == 'Demographic Info'",
                   box(
                     radioButtons(
                       "demoInfo", 
                       "Breakdown", 
                       choiceNames = c("Gender by Age", 
                                       "Gender by Household Income", 
                                       "Age by Household Income"), 
                       choiceValues = c("GA", 
                                        "GI", 
                                        "AI")
                     ), 
                     width = NULL
                   )
                 )
                   ),
          # Display the EDA plot or summary
          column(
            width = 9, 
            conditionalPanel(
              condition = "input.info == 'Views'", 
              plotlyOutput("plotEDA")
            ), 
            conditionalPanel(
              condition = "input.info == 'Demographic Info'",
              h3(textOutput("demoText")),
              tableOutput("demoEDA")
            ),
            conditionalPanel(
              condition = "input.info == 'Movie Rankings'",
              h3(textOutput("rankingsAgeText")), 
              h4("Respondents were asked to rank the Star Wars movies"),
              h4("from favorite (1) to least favorite (6)"),
              tableOutput("rankingsAgeEDA")
            )
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
                "Download Data", 
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
                            "Logistic Regression", 
                            "Boosted Logistic Regression"), 
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
                condition = "input.whichModel == 'Boosted Tree' || input.whichModel == 'Boosted Logistic Regression'", 
                selectInput(
                  "tune", 
                  "Tuning Grid Size", 
                  choices = list("1" = 1, 
                                 "2" = 2, 
                                 "3 Long Wait" = 3, 
                                 "4 Really Long Wait" = 4, 
                                 "5 Pit of Sarlacc" = 5)
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
              conditionalPanel(
                condition = "output.showHideTrain == 'show'", 
                actionButton(
                  "train",
                  "Train Model"
                )
              ),
              conditionalPanel(
                condition = "output.showHideTrain == 'hide'", 
                h4("Pick Some Predictors")
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
                title = "Predict", 
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
                      h3("Predict whether someone is a Star Wars fan"), 
                      actionButton(
                        "getPrediction", 
                        "Make Prediction"
                      ),
                      width = NULL
                    ), 
                    box(
                      h3("Prediction:"),
                      h4(uiOutput("prediction")),
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
    } else if(input$plots == "Household Income"){
      pctSeenHHI
    } else{
      stop("Error")
    }
  ) 
  
  # EDA tab
  # Movie rankings by age 
  output$rankingsAgeEDA <- renderTable(
    if(input$movieToRank == "Ep. I"){
      AI
    } else if(input$movieToRank == "Ep. II"){
      AII
    } else if(input$movieToRank == "Ep. III"){
      AIII
    } else if(input$movieToRank == "Ep. IV"){
      AIV
    } else if(input$movieToRank == "Ep. V"){
      AV
    } else if(input$movieToRank == "Ep. VI"){
      AVI
    } else{
      stop("Error") 
    }
  )
  
  output$rankingsAgeText <- renderText(
    paste0("Ranking of Star Wars ", input$movieToRank, " by Respondent Age")
  )
  
  # EDA tab
  # Demo info table 
  output$demoEDA <- renderTable(
    {
      if(input$demoInfo == "GA"){
        demoGA
      } else if(input$demoInfo == "GI"){
        demoGI
      } else if(input$demoInfo == "AI"){
        demoAI
      } else{
        stop("Error")
      }
    }
  )
  
  output$demoText <- renderText(
    {
      if(input$demoInfo == "GA"){
        "Breakdown of Respondents: Gender by Age"
      } else if(input$demoInfo == "GI"){
        "Breakdown of Respondents: Gender by Household Income"
      } else if(input$demoInfo == "AI"){
        "Breakdown of Respondents: Age by Household Income"
      } else{
        stop("Error")
      }
    }
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
  
  output$showHideTrain <- renderText({
    if(length(input$preds) > 0){
      "show"
    } else{
      "hide"
    }
  })
  
  outputOptions(output, "showHideTrain", suspendWhenHidden = FALSE)
  
  
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
        } else if(isolate(input$whichModel) == "Boosted Logistic Regression"){
          boostLogReg <- getBoostLogReg(preds = isolate(input$preds), tnSize = isolate(input$tune))
        }
        else{
          stop("Error")
        }
      }, 
      message = "Training model"
    )
    }
  )
  
  # Modeling tab
  # Bring accuracy tab to front when you train
  # Prevents progress bar from being covered up by tabs
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
  
  # Modeling tab
  # Whether the user should have the option to show var importance
  output$showHide <- renderText({
    if(length(input$preds) > 1){
      "show"
    } else{
      "hide"
    }
  })
  
  # So that showHide can be used on the UI side
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
  
  # Modeling tab
  # Variable importance
  # Show or hide var importance tab at appropriate times
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
  # Get predictors in current model
  currentPreds <- reactive({
    req(input$train)
    
    isolate(input$preds)
  })
  
  # Modeling tab
  # Prediction 
  # Let user select values for predictors in current model
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
  
  # Modeling tab
  # Prediction 
  # Put user selected values of predictors into one object 
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
  
  # Modeling tab
  # Prediction 
  # Get prediction
  vals <- reactiveValues(pred = NULL)
  
  observeEvent(input$train, {vals$pred <- NULL})
  
  observeEvent(input$getPrediction, {vals$pred <- predict(mod(), newdata = isolate(predSelections())) %>% as.character()})
  
  # Modeling tab
  # Prediction 
  # Output prediction
  output$prediction <- renderUI({
    req(vals$pred)
    
    if(vals$pred == "Yes"){
      HTML(paste0("The model predicts that this person ", strong("IS"), " a Star Wars Fan."))
    } else if(vals$pred == "No"){
      HTML(paste0("The model predicts that this person ", strong("IS NOT"), " a Star Wars Fan."))
    } else if(is.null(vals$pred)){
      HTML("")
    } else{
      stop("Error")
    }
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

shinyApp(ui, server) #%>% runApp()

#options(shiny.reactlog=TRUE) 























