# Author: Joshua Burrows
# Purpose: Create shiny app to explore Star Wars viewership data
# Date Created: 18 November 2020

library(tidyverse)
library(cluster)
#library(knitr)
library(caret)
library(plotly)

set.seed(234)

# Star Wars Viewership data has been made available 
#by FiveThrityEight here: 
#https://github.com/fivethirtyeight/data/tree/master/star-wars-survey

# Read and clean
# Col names 
SWCols <- c("Id", "Any", "fan", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI", "rankI", "rankII", "rankIII", "rankIV", "rankV", "rankVI", "han", "luke", "leia", "anakin", "obiWan", "palpatine", "vader", "lando", "boba", "C3P0", "R2D2", "jarJar", "padme", "yoda", "shotFirst", "familiarExpanded", "fanExpanded", "trekFan", "gender", "age", "HHIncome", "education", "location")

# Read and clean data 
# Read from the GitHub repo for this app
SW <-
  read_csv(
    "https://raw.githubusercontent.com/JKBurrows/ST558-Project-3/main/StarWars.csv",
    skip = 2,
    col_names = SWCols,
    col_types = cols(Id = col_double(),
                .default = col_factor(NULL))
  ) %>%
  select(-Id)

# SW <- 
#   read_csv(
#     "StarWars.csv", 
#     skip = 2, 
#     col_names = SWCols, 
#     col_types = cols(Id = col_double(), 
#                      .default = col_factor(NULL))
#   ) %>% 
#   select(-Id)

# Read and clean 
# Replace movie names with 'Yes' or 'No' to indicate
# whether the respondent has seen the movie 
replaceName <- function(vec){
  newVec <- 
    SW[[vec]] %>% 
    is.na() %>% 
    ifelse("No", "Yes") %>% 
    as.factor()
  
  return(newVec)
}

SW[["Ep. I"]] <- replaceName("Ep. I")
SW[["Ep. II"]] <- replaceName("Ep. II")
SW[["Ep. III"]] <- replaceName("Ep. III")
SW[["Ep. IV"]] <- replaceName("Ep. IV")
SW[["Ep. V"]] <- replaceName("Ep. V")
SW[["Ep. VI"]] <- replaceName("Ep. VI")

# Create EDA plots 
# Function takes vector of 'Yes' and 'No' and returns pct 'Yes' 
pctYes <- function(vec){
  sum <- vec %>% summary()
  
  pct <- sum[["Yes"]] / length(vec)
  
  return(pct)
}

# Create EDA plots 
# Plot percent who have seen each movie 
seen <- 
  SW %>% 
  select(Any, starts_with("Ep.")) %>% 
  sapply(FUN = pctYes) %>% 
  as_tibble()

colnames(seen) <- c("pctSeen")

seen$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

pctSeenOverall <- 
  plot_ly(
    data = seen,
    x = ~movieName, 
    y = ~pctSeen, 
    type = "bar"
  )

pctSeenOverall <- 
  pctSeenOverall %>% 
  layout(
    title = "Star Wars Viewership", 
    xaxis = list(title = "Movie Name"), 
    yaxis = list(title = "Viewership Proportion"), 
    margin = list(t = 50)
  )

# Create EDA plots 
# Function takes factor vector and a level of that factor
# returns pct of that level who have seen each movie 
getPctSeen <- function(var, lev){
  seen <- 
    SW %>% 
    filter(SW[[var]] == lev) %>% 
    select(Any, starts_with("Ep.")) %>% 
    sapply(FUN = pctYes) %>% 
    as.data.frame()
  
  seen <- cbind(seen, lev)
  
  colnames(seen) <- c("pctSeen", var)
  
  seen$movieName <- seen %>% row.names()
  
  return(seen)
}

# Create EDA plots 
# Plot pct seen by gender 
seenM <- getPctSeen(var = "gender", lev = "Male")

seenF <- getPctSeen(var = "gender", lev = "Female")

seenMF <- rbind(seenM, seenF)

pctSeenGender <- 
  plot_ly(
    data = seenMF, 
    x = ~movieName, 
    y = ~pctSeen, 
    color = ~gender, 
    type = "bar"
 )

pctSeenGender <- 
  pctSeenGender %>% 
  layout(
    title = "Star Wars Viewership by Gender", 
    xaxis = list(title = "Movie Name"), 
    yaxis = list(title = "Viewership Proportion"), 
    margin = list(t = 50)
  )

# Create EDA plots 
# Plot pct seen by Trek fan
seenTrekFan <- 
  rbind(
    getPctSeen(var = "trekFan", lev = "Yes"), 
    getPctSeen(var = "trekFan", lev = "No")
  )

pctSeenTrek <- 
  plot_ly(
    data = seenTrekFan, 
    x = ~movieName, 
    y = ~pctSeen, 
    color = ~trekFan, 
    type = "bar"
  )

pctSeenTrek <- 
  pctSeenTrek %>% 
  layout(
    title = "Star Wars Viewership by Trek Fan", 
    xaxis = list(title = "Movie Name"), 
    yaxis = list(title = "Viewership Proportion"),
    legend = list(title = list(text = "Trek fan?")),
    margin = list(t = 50)
  )

# Create EDA plots 
# Pct seen by household income 
pctSeenHHI <- 
  rbind(
    getPctSeen(var = "HHIncome", lev = "$0 - $24,999"), 
    getPctSeen(var = "HHIncome", lev = "$25,000 - $49,999"), 
    getPctSeen(var = "HHIncome", lev = "$50,000 - $99,999"), 
    getPctSeen(var = "HHIncome", lev = "$100,000 - $149,999"), 
    getPctSeen(var = "HHIncome", lev = "$150,000+") 
  )

pctSeenHHI$HHIncome <- 
  factor(
    pctSeenHHI$HHIncome, 
    levels = c("$0 - $24,999", 
               "$25,000 - $49,999", 
               "$50,000 - $99,999", 
               "$100,000 - $149,999", 
               "$150,000+")
  ) 

pctSeenHHI <- 
  plot_ly(
    data = pctSeenHHI, 
    x = ~movieName, 
    y = ~pctSeen, 
    color = ~HHIncome
  )

pctSeenHHI <- 
  pctSeenHHI %>% 
  layout(
    title = "Star Wars Viewership by Household Income", 
    xaxis = list(title = "Movie Name"), 
    yaxis = list(title = "Viewership Proportion"),
    legend = list(title = list(text = "Household Income (Thousands)")),
    margin = list(t = 50)
  )

# EDA demographic tables
# Get tables
demoGA <- 
  SW %>% 
  select(gender, age) %>% 
  table() %>% 
  as_tibble()

demoGI <- 
  SW %>% 
  select(gender, HHIncome) %>% 
  table() %>% 
  as_tibble()

demoAI <- 
  SW %>% 
  select(age, HHIncome) %>% 
  table() %>% 
  as_tibble()

# EDA demographic tables 
# Re-level factors before spreading
demoGA$age <- 
  factor(
    demoGA$age, 
    levels = c("18-29", 
               "30-44", 
               "45-60", 
               "> 60")
  )

demoGI$HHIncome <- 
  factor(
    demoGI$HHIncome, 
    levels = c("$0 - $24,999", 
               "$25,000 - $49,999", 
               "$50,000 - $99,999", 
               "$100,000 - $149,999", 
               "$150,000+")
  )

demoAI$age <- 
  factor(
    demoAI$age, 
    levels = c("18-29", 
               "30-44", 
               "45-60", 
               "> 60")
  )

demoAI$HHIncome <- 
  factor(
    demoAI$HHIncome, 
    levels = c("$0 - $24,999", 
               "$25,000 - $49,999", 
               "$50,000 - $99,999", 
               "$100,000 - $149,999", 
               "$150,000+")
  )

# EDA demographic tables 
# Spread tables for easier viewing 
demoGA <- 
  demoGA %>% 
  spread(key = age, value = n)

demoGI <- 
  demoGI %>% 
  spread(key = HHIncome, value = n)

demoAI <- 
  demoAI %>% 
  spread(key = HHIncome, value = n)

# EDA tables movie rankings by age 
# Re-level factors 
SW$age <- factor(SW$age, levels = c("18-29", "30-44", "45-60", "> 60"))
SW$rankI <- factor(SW$rankI, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankII <- factor(SW$rankII, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankIII <- factor(SW$rankIII, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankIV <- factor(SW$rankIV, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankV <- factor(SW$rankV, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankVI <- factor(SW$rankVI, levels = c("1", "2", "3", "4", "5", "6"))

# EDA tables movie rankings by age 
# Get tables 
AI <- 
  SW %>% 
  select(age, rankI) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankI, value = Freq)

AII <- 
  SW %>% 
  select(age, rankII) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankII, value = Freq)

AIII <- 
  SW %>% 
  select(age, rankIII) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankIII, value = Freq)

AIV <- 
  SW %>% 
  select(age, rankIV) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankIV, value = Freq)

AV <- 
  SW %>% 
  select(age, rankV) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankV, value = Freq)

AVI <- 
  SW %>% 
  select(age, rankVI) %>% 
  table() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  spread(key = rankVI, value = Freq)

# Hierarchical clustering
# Only use a subset of data so that table is readable 
clustSub <- SW[sample(1:nrow(SW), 150, replace = FALSE),]

# Hierarchical clustering
# Get clustering overall
hierClust <- 
  clustSub %>% 
  daisy(metric = "gower") %>% 
  as.matrix() %>% 
  diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Hierarchical clustering
# Get clustering with just males
clustSubM <- 
  clustSub %>% 
  filter(gender == "Male")

hierClustM <- 
  clustSubM %>% 
  daisy(metric = "gower") %>% 
  as.matrix() %>% 
  diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Hierarchical clustering
# Get clustering with just females 
clustSubF <- 
  clustSub %>% 
  filter(gender == "Female")

hierClustF <- 
  clustSubF %>% 
  daisy(metric = "gower") %>% 
  as.matrix() %>% 
  diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Models
# Get training and test sets
subSW <- 
  SW %>% 
  select(fan,
         rankI,
         jarJar,
         gender, 
         trekFan, 
         age, 
         HHIncome, 
         education, 
         location
  ) %>% 
  na.omit()

trainIndex <- createDataPartition(subSW$fan, p = .7, list = FALSE)

train <- subSW[trainIndex,] 
test <- subSW[-trainIndex,]

# Models 
# Boosted tree tuning grid
getBTTuneGr <- function(size){
  seq1 <- seq(from = 50, to = 300, by = 50)
  seq2 <- 1:6
  seq3 <- seq(from = .025, to = .15, by = .025)
  seq4 <- 8:13
  
  seq1 <- seq1[1:size]
  seq2 <- seq2[1:size]
  seq3 <- seq3[1:size]
  seq4 <- seq4[1:size]
  
  grid <- expand.grid(n.trees = seq1, 
                      interaction.depth = seq2, 
                      shrinkage = seq3, 
                      n.minobsinnode = seq4)
  
  return(grid)
}

# Models 
# Function takes predictors returns formula 
getFormula <- function(preds){
  form <- "fan ~ "
  
  for(i in 1:length(preds)){
    if(i == 1){
      form <- paste0(form, preds[i])
    } else{
      form <- paste0(form, " + ", preds[i])
    }
  }
  
  form <- formula(form)
  
  return(form) 
}

# Models 
# Function takes predictors and tuning grid size
# returns boosted tree model 
getBoost <- function(preds, tnSize){
  
  form <- getFormula(preds)
  
  boostTree <- 
    train(
      form, 
      data = train, 
      method = "gbm", 
      trControl = trainControl(method = "cv", number = 10),
      tuneGrid = getBTTuneGr(tnSize),
      verbose = FALSE
    )
  
  return(boostTree)
}

# Models 
# Function takes predictors returns logistic regression model
getLogReg <- function(preds){
  
  form <- getFormula(preds)
  
  logReg <- 
    train(
      form,
      data = train, 
      method = "glm", 
      family = "binomial"
    )
  
  return(logReg)
}

# Models 
# Method 'LogitBoost' prefers a data frame 
train1 <- train %>% as.data.frame()

# Models 
# Function takes predictors and tuning grid size
# returns boosted logistic regression model 
getBoostLogReg <- function(preds, tnSize){
  form <- getFormula(preds)
  
  boostLogReg <- 
    train(
      form, 
      data = train1, 
      method = "LogitBoost", 
      trControl = trainControl(method = "cv", number = 10), 
      tuneLength = tnSize
    )
}

















