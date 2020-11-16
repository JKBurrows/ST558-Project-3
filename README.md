Joshua Burrows Project 3
================
18 December 2020

# Introduction

The purpose of this project is to create a Shiny app to explore data
about Star Wars fans. I used data that has been made available
[here](https://github.com/fivethirtyeight/data/tree/master/star-wars-survey)
by FiveThrityEight.

I made use of the following packages: *shiny*, *shinydashboard*,
*tidyverse*, *DT*, *dendextend*, *caret*, *gbm*, *formula.tools*,
*cluster*, *knitr*, *devtools*, *rmarkdown*, and *plotly*.

# Run App

To run the app, run the following code in R on your computer:

``` r
install.packages(c("shiny", "shinydashboard", "tidyverse", "DT", "dendextend", "caret", "gbm", "formula.tools", "cluster", "knitr", "devtools", "rmarkdown", "plotly"))

library(devtools)

source_url("https://raw.githubusercontent.com/JKBurrows/ST558-Project-3/main/app.R")
```
