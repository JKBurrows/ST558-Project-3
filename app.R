# Author: Joshua Burrows
# Purpose: 
# Date Created: 18 November 2020

library(shiny) 
library(shinydashboard)
library(tidyverse)

# Read in and clean data 
# This data has been made available by FiveThrityEight
# here https://github.com/fivethirtyeight/data/tree/master/star-wars-survey

SWCols <- c("id", "seenAny", "fan", "seenI", "seenII", "seenIII", "seenIV", "seenV", "seenVI", "rankI", "rankII", "rankIII", "rankIV", "rankV", "rankVI", "han", "luke", "leia", "anakin", "obiWan", "palpatine", "vader", "lando", "boba", "C3P0", "R2D2", "jarJar", "padme", "yoda", "shotFirst", "familiarExpanded", "fanExpanded", "trekFan", "gender", "age", "HHIncome", "education", "location")

SW <- read_csv("StarWars.csv", skip = 2, col_names = SWCols, col_types = cols(Id = col_double(), .default = col_factor(NULL))) %>% select(-id)

replaceName <- function(vec){
  newVec <- SW[[vec]] %>% is.na() %>% ifelse("No", "Yes") %>% as.factor()
  
  return(newVec)
}

SW$seenI <- replaceName("seenI")
SW$seenII <- replaceName("seenII")
SW$seenIII <- replaceName("seenIII")
SW$seenIV <- replaceName("seenIV")
SW$seenV <- replaceName("seenV")
SW$seenVI <- replaceName("seenVI")

ui <- dashboardPage(
  dashboardHeader(
    title = "Star Wars Viewership" 
    ), 
  
  dashboardSidebar(
    menuItem("Home", tabName = "home", icon = icon("journal-whills")), 
    menuItem("Explore Data", tabName = "EDA", icon = icon("galactic-senate"))
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
          )
        )
      )
    )
  )

server <- function(input, output){
  
  
  }


shinyApp(ui, server)

























