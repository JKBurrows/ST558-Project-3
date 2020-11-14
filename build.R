# Author: Joshua Burrows
# Purpose: 
# Date Created: 18 November 2020

library(tidyverse)
library(cluster)
library(knitr)
library(caret)
library(plotly)

# Read in and clean Star Wars data 
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

# Get percent who said yes 
pctYes <- function(vec){
  sum <- vec %>% summary()
  
  pct <- sum[["Yes"]] / length(vec)
  
  return(pct)
}

# Plot percent who have seen each movie 
seen <- SW %>% select(starts_with("seen")) %>% sapply(FUN = pctYes) %>% as.data.frame()

colnames(seen) <- c("pctSeen")

seen$movieName <- seen %>% row.names()

pctSeenOverall <- plot_ly(
  x = seen$movieName, 
  y = seen$pctSeen, 
  type = "bar"
)

# Male pct seen
seenM <- SW %>% filter(gender == "Male") %>% select(starts_with("seen")) %>% sapply(FUN = pctYes) %>% as.data.frame()

colnames(seenM) <- c("pctSeen")

seenM <- cbind(seenM, data.frame(Gender = c("Male")))

seenM$movieName <- seenM %>% row.names()

# Female pct seen
seenF <- SW %>% filter(gender == "Female") %>% select(starts_with("seen")) %>% sapply(FUN = pctYes) %>% as.data.frame()

colnames(seenF) <- c("pctSeen")

seenF <- cbind(seenF, data.frame(Gender = c("Female")))

seenF$movieName <- seenF %>% row.names()

seenMF <- rbind(seenM, seenF)

seenMF

# Plot 
pctSeenGender <- plot_ly(data = seenMF, x = ~movieName, y = ~pctSeen, color = ~Gender, type = "bar")


# trekFan
seenY <- SW %>% filter(trekFan == "Yes") %>% select(starts_with("seen")) %>% sapply(FUN = pctYes) %>% as.data.frame()

colnames(seenY) <- c("pctSeen")

seenY <- cbind(seenY, data.frame(trekFan = c("Yes")))

seenY$movieName <- seenY %>% row.names()

# Not trekFan
seenN <- SW %>% filter(trekFan == "No") %>% select(starts_with("seen")) %>% sapply(FUN = pctYes) %>% as.data.frame()

colnames(seenN) <- c("pctSeen")

seenN <- cbind(seenN, data.frame(trekFan = c("No")))

seenN$movieName <- seenN %>% row.names()

seenYN <- rbind(seenY, seenN)

# Plot
pctSeenTrek <- plot_ly(data = seenYN, x = ~movieName, y = ~pctSeen, color = ~trekFan, type = "bar")

# Summarize rankings 
getRanks <- function(vec){
  sum <- vec %>% na.omit() %>% summary()
  
  df <- data.frame(rankedfirst = sum[["1"]], 
                   rankedSecond = sum[["2"]], 
                   rankedThird = sum[["3"]], 
                   rankedFourth = sum[["4"]], 
                   rankedFifth = sum[["5"]], 
                   rankedSixth = sum[["6"]])
  
  return(df)
}

ranks <- rbind(getRanks(SW[["rankI"]]), 
               getRanks(SW[["rankII"]]),
               getRanks(SW[["rankIII"]]), 
               getRanks(SW[["rankIV"]]), 
               getRanks(SW[["rankV"]]), 
               getRanks(SW[["rankVI"]]))

ranks <- cbind(data.frame(movieName = c("I", "II", "III", "IV", "V", "VI")), ranks)

# Summarize fan rankings 
fans <- SW %>% filter(fan == "Yes")

fanRanks <- rbind(getRanks(fans[["rankI"]]),  
                  getRanks(fans[["rankII"]]),
                  getRanks(fans[["rankIII"]]), 
                  getRanks(fans[["rankIV"]]), 
                  getRanks(fans[["rankV"]]), 
                  getRanks(fans[["rankVI"]]))

fanRanks <- cbind(data.frame(movieName = c("I", "II", "III", "IV", "V", "VI")), fanRanks)

# Hierarchical clustering
# Overall
hierClust <- SW %>% na.omit() %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Males
hierClustM <- SW %>% filter(gender == "Male") %>% na.omit() %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Females 
hierClustF <- SW %>% filter(gender == "Female") %>% na.omit() %>% daisy(metric = "gower") %>% as.matrix() %>% diana(diss = TRUE, keep.diss = FALSE, keep.data = FALSE)

# Models
# Boosted tree

getBoost <- function(preds){
  set.seed(234)
  
  form <- "fan ~ "
  
  for(i in 1:length(preds)){
    if(i == 1){
      form <- paste0(form, preds[i])
    } else{
      form <- paste0(form, " + ", preds[i])
    }
  }
  
  form <- formula(form)
  
  boostTree <- train(form, 
                     data = train, 
                     method = "gbm", 
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 3,
                     verbose = FALSE)
  
  return(boostTree)
}



















