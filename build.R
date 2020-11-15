# Author: Joshua Burrows
# Purpose: 
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

SW <- read_csv("https://raw.githubusercontent.com/JKBurrows/ST558-Project-3/main/StarWars.csv", skip = 2, col_names = SWCols, col_types = cols(Id = col_double(), .default = col_factor(NULL))) %>% select(-Id)

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

# Male pct seen
seenM <- SW %>% filter(gender == "Male") %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as_tibble()

colnames(seenM) <- c("pctSeen")

seenM <- cbind(seenM, data.frame(Gender = c("Male")))

seenM$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

# Female pct seen
seenF <- SW %>% filter(gender == "Female") %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as_tibble()

colnames(seenF) <- c("pctSeen")

seenF <- cbind(seenF, data.frame(Gender = c("Female")))

seenF$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

seenMF <- rbind(seenM, seenF)

# Plot 
pctSeenGender <- plot_ly(data = seenMF, 
                         x = ~movieName, 
                         y = ~pctSeen, 
                         color = ~Gender, 
                         type = "bar")

pctSeenGender <- pctSeenGender %>% layout(
  title = "Star Wars Viewership by Gender", 
  xaxis = list(title = "Movie Name"), 
  yaxis = list(title = "Viewership Proportion"), 
  margin = list(t = 50))

# trekFan
seenY <- SW %>% filter(trekFan == "Yes") %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as_tibble()

colnames(seenY) <- c("pctSeen")

seenY <- cbind(seenY, data.frame(trekFan = c("Yes")))

seenY$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

# Not trekFan
seenN <- SW %>% filter(trekFan == "No") %>% select(Any, starts_with("Ep.")) %>% sapply(FUN = pctYes) %>% as_tibble()

colnames(seenN) <- c("pctSeen")

seenN <- cbind(seenN, data.frame(trekFan = c("No")))

seenN$movieName <- c("Any", "Ep. I", "Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")

seenYN <- rbind(seenY, seenN)

# Plot
pctSeenTrek <- plot_ly(data = seenYN, 
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

# Summarize rankings 
getRanks <- function(vec){
  subVec <- vec %>% na.omit()
  
  sum <- subVec %>% summary()
  
  df <- data.frame(First = sum[["1"]], 
                   Second = sum[["2"]], 
                   Third = sum[["3"]], 
                   Fourth = sum[["4"]], 
                   Fifth = sum[["5"]], 
                   Sixth = sum[["6"]])
  
  df <- ((df / length(subVec)) * 100) %>% round(digits = 1)
  
  return(df)
}

ranks <- rbind(getRanks(SW[["rankI"]]), 
               getRanks(SW[["rankII"]]),
               getRanks(SW[["rankIII"]]), 
               getRanks(SW[["rankIV"]]), 
               getRanks(SW[["rankV"]]), 
               getRanks(SW[["rankVI"]]))

ranks <- cbind(data.frame(movieName = c(" Ep. I", " Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")), ranks)

# Summarize fan rankings 
fans <- SW %>% filter(fan == "Yes")

fanRanks <- rbind(getRanks(fans[["rankI"]]),  
                  getRanks(fans[["rankII"]]),
                  getRanks(fans[["rankIII"]]), 
                  getRanks(fans[["rankIV"]]), 
                  getRanks(fans[["rankV"]]), 
                  getRanks(fans[["rankVI"]]))

fanRanks <- cbind(data.frame(movieName = c(" Ep. I", " Ep. II", "Ep. III", "Ep. IV", "Ep. V", "Ep. VI")), fanRanks)

# Character favorability 
getFavor <- function(vec){
  subVec <- vec %>% na.omit()
  
  sum <- subVec %>% summary()
  
  df <- data.frame(vFavor = sum[["Very favorably"]], 
                   sFavor = sum[["Somewhat favorably"]], 
                   neutral = sum[["Neither favorably nor unfavorably (neutral)"]], 
                   sUn = sum[["Somewhat unfavorably"]],
                   vUn = sum[["Very unfavorably"]], 
                   un = sum[["Unfamiliar (N/A)"]])
  
  df <- ((df / length(subVec)) * 100) %>% round(digits = 1)
  
  return(df)
}

favor <- rbind(getFavor(SW$han), 
               getFavor(SW$luke), 
               getFavor(SW$leia), 
               getFavor(SW$anakin), 
               getFavor(SW$obiWan), 
               getFavor(SW$palpatine), 
               getFavor(SW$vader), 
               getFavor(SW$lando), 
               getFavor(SW$boba), 
               getFavor(SW$C3P0), 
               getFavor(SW$R2D2), 
               getFavor(SW$jarJar), 
               getFavor(SW$padme), 
               getFavor(SW$yoda))


charNames <- c("han", "luke", "leia", "anakin", "obiWan", "palpatine", "vader", "lando", "boba", "C3P0", "R2D2", "jarJar", "padme", "yoda") 

favor <- cbind(charNames, favor)

colnames(favor) <- c("Character Names",
                     "Very Favorable", 
                     "Somewhat Favorable", 
                     "Neutral", 
                     "Somewhat Unfavorable", 
                     "Very Unfavorable", 
                     "Unfamiliar")

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

















