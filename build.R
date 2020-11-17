# Author: Joshua Burrows
# Purpose: Explore Star Wars viewership data
# Date Created: 18 November 2020

library(tidyverse)
library(cluster)
library(knitr)
library(caret)
library(plotly)

set.seed(234)

# Read in and clean Star Wars data 
# This data has been made available by FiveThrityEight
# here https://github.com/fivethirtyeight/data/tree/master/star-wars-survey
SWCols <- c("Id", "Any", "fan", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI", "rankI", "rankII", "rankIII", "rankIV", "rankV", "rankVI", "han", "luke", "leia", "anakin", "obiWan", "palpatine", "vader", "lando", "boba", "C3P0", "R2D2", "jarJar", "padme", "yoda", "shotFirst", "familiarExpanded", "fanExpanded", "trekFan", "gender", "age", "HHIncome", "education", "location")

#SW <- read_csv("https://raw.githubusercontent.com/JKBurrows/ST558-Project-3/main/StarWars.csv", skip = 2, col_names = SWCols, col_types = cols(Id = col_double(), .default = col_factor(NULL))) %>% select(-Id)
SW <- read_csv("StarWars.csv", skip = 2, col_names = SWCols, col_types = cols(Id = col_double(), .default = col_factor(NULL))) %>% select(-Id)

replaceName <- function(vec){
  newVec <- SW[[vec]] %>% is.na() %>% ifelse("No", "Yes") %>% as.factor()
  
  return(newVec)
}

SW[["Ep. I"]] <- replaceName("Ep. I")
SW[["Ep. II"]] <- replaceName("Ep. II")
SW[["Ep. III"]] <- replaceName("Ep. III")
SW[["Ep. IV"]] <- replaceName("Ep. IV")
SW[["Ep. V"]] <- replaceName("Ep. V")
SW[["Ep. VI"]] <- replaceName("Ep. VI")

# Get percent who said yes 
pctYes <- function(vec){
  sum <- vec %>% summary()
  
  pct <- sum[["Yes"]] / length(vec)
  
  return(pct)
}

# Plot percent who have seen each movie 
seen <- SW %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as_tibble()

colnames(seen) <- c("pctSeen")

seen$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

pctSeenOverall <- plot_ly(
  data = seen,
  x = ~movieName, 
  y = ~pctSeen, 
  type = "bar"
)

pctSeenOverall <- pctSeenOverall %>% layout(
  title = "Star Wars Viewership", 
  xaxis = list(title = "Movie Name"), 
  yaxis = list(title = "Viewership Proportion"), 
  margin = list(t = 50))

# Pct seen by gender 
getPctSeen <- function(var, lev){
  seen <- SW %>% filter(SW[[var]] == lev) %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as.data.frame()
  
  seen <- cbind(seen, lev)
  
  colnames(seen) <- c("pctSeen", var)
  
  seen$movieName <- seen %>% row.names()
  
  return(seen)
}

seenM <- getPctSeen(var = "gender", lev = "Male")

seenF <- getPctSeen(var = "gender", lev = "Female")

seenMF <- rbind(seenM, seenF)

pctSeenGender <- plot_ly(data = seenMF, 
                         x = ~movieName, 
                         y = ~pctSeen, 
                         color = ~gender, 
                         type = "bar")

pctSeenGender <- pctSeenGender %>% layout(
  title = "Star Wars Viewership by Gender", 
  xaxis = list(title = "Movie Name"), 
  yaxis = list(title = "Viewership Proportion"), 
  margin = list(t = 50))

# Pct seen by Trek fan
seenTrekFan <- rbind(getPctSeen(var = "trekFan", lev = "Yes"), 
                     getPctSeen(var = "trekFan", lev = "No"))

pctSeenTrek <- plot_ly(data = seenTrekFan, 
                       x = ~movieName, 
                       y = ~pctSeen, 
                       color = ~trekFan, 
                       type = "bar")

pctSeenTrek <- pctSeenTrek %>% layout(
  title = "Star Wars Viewership by Trek Fan", 
  xaxis = list(title = "Movie Name"), 
  yaxis = list(title = "Viewership Proportion"),
  legend = list(title = list(text = "Trek fan?")),
  margin = list(t = 50))

# Pct seen by household income 
pctSeenHHI <- rbind(
  getPctSeen(var = "HHIncome", lev = "$0 - $24,999"), 
  getPctSeen(var = "HHIncome", lev = "$25,000 - $49,999"), 
  getPctSeen(var = "HHIncome", lev = "$50,000 - $99,999"), 
  getPctSeen(var = "HHIncome", lev = "$100,000 - $149,999"), 
  getPctSeen(var = "HHIncome", lev = "$150,000+") 
)

pctSeenHHI <- plot_ly(data = pctSeenHHI, 
                      x = ~movieName, 
                      y = ~pctSeen, 
                      color = ~HHIncome)

pctSeenHHI <- pctSeenHHI %>% layout(
  title = "Star Wars Viewership by Household Income", 
  xaxis = list(title = "Movie Name"), 
  yaxis = list(title = "Viewership Proportion"),
  legend = list(title = list(text = "Household Income")),
  margin = list(t = 50))

# Demographic tables
# Get tables
demoGA <- SW %>% select(gender, age) %>% table() %>% as_tibble()
demoGI <- SW %>% select(gender, HHIncome) %>% table() %>% as_tibble()
demoAI <- SW %>% select(age, HHIncome) %>% table() %>% as_tibble()

# Relevel factors before spreading
demoGA$age <- factor(demoGA$age, levels = c("18-29", "30-44", "45-60", "> 60"))

demoGI$HHIncome <- factor(demoGI$HHIncome, levels = c("$0 - $24,999", "$25,000 - $49,999", "$50,000 - $99,999", "$100,000 - $149,999", "$150,000+"))

demoAI$age <- factor(demoAI$age, levels = c("18-29", "30-44", "45-60", "> 60"))

demoAI$HHIncome <- factor(demoAI$HHIncome, levels = c("$0 - $24,999", "$25,000 - $49,999", "$50,000 - $99,999", "$100,000 - $149,999", "$150,000+"))

# Spread tables for easier viewing 
demoGA <- demoGA %>% spread(key = age, value = n)

demoGI <- demoGI %>% spread(key = HHIncome, value = n)

demoAI <- demoAI %>% spread(key = HHIncome, value = n)

# Movie ranking by age tables 
SW$age <- factor(SW$age, levels = c("18-29", "30-44", "45-60", "> 60"))
SW$rankI <- factor(SW$rankI, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankII <- factor(SW$rankII, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankIII <- factor(SW$rankIII, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankIV <- factor(SW$rankIV, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankV <- factor(SW$rankV, levels = c("1", "2", "3", "4", "5", "6"))
SW$rankVI <- factor(SW$rankVI, levels = c("1", "2", "3", "4", "5", "6"))

AI <- SW %>% select(age, rankI) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankI, value = Freq)

AII <- SW %>% select(age, rankII) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankII, value = Freq)

AIII <- SW %>% select(age, rankIII) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankIII, value = Freq)

AIV <- SW %>% select(age, rankIV) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankIV, value = Freq)

AV <- SW %>% select(age, rankV) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankV, value = Freq)

AVI <- SW %>% select(age, rankVI) %>% table() %>% as.data.frame() %>% as_tibble() %>% spread(key = rankVI, value = Freq)

# Hierarchical clustering
set.seed(234)
clustSub <- SW[sample(1:nrow(SW), 150, replace = FALSE),]

# Overall
hierClust <- clustSub %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Males
clustSubM <- clustSub %>% filter(gender == "Male")

hierClustM <- clustSubM %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Females 
clustSubF <- clustSub %>% filter(gender == "Female")

hierClustF <- clustSubF %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Models
# Get training and test sets
set.seed(234) 

subSW <- SW %>% select(fan, gender, trekFan, age, HHIncome, education, location) %>% na.omit()

trainIndex <- createDataPartition(subSW$fan, p = .7, list = FALSE)

train <- subSW[trainIndex,] 
test <- subSW[-trainIndex,]

# Boosted tree tuning grid
getBTTuneGr <- function(size){
  seq1 <- seq(from = 50, to = 250, by = 50)
  seq2 <- 1:5
  seq3 <- seq(from = .025, to = .125, by = .025)
  seq4 <- 8:12
  
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

# Create formula 
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


# Boosted tree 
getBoost <- function(preds, tnSize){
  
  form <- getFormula(preds)
  
  boostTree <- train(form, 
                     data = train, 
                     method = "gbm", 
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = getBTTuneGr(tnSize),
                     verbose = FALSE)
  
  return(boostTree)
}

# Logistic regression 
getLogReg <- function(preds){
  
  form <- getFormula(preds)
  
  logReg <- train(form,
                  data = train, 
                  method = "glm", 
                  family = "binomial")
  
  return(logReg)
}

# Boosted logistic regression 
train1 <- train %>% as.data.frame()

getBoostLogReg <- function(preds, tnSize){
  form <- getFormula(preds)
  
  boostLogReg <- train(form, 
                       data = train1, 
                       method = "LogitBoost", 
                       trControl = trainControl(method = "cv", number = 10), 
                       tuneLength = tnSize
  )
}

















