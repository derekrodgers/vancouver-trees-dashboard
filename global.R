library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(plotly)

# To run locally, start an R console in the repo root and run:
#     shiny::runApp("app.R")
# To deploy:
#     rsconnect::deployApp(appDir = ".", appName = "vancouver-street-trees")
# Deploy location:
#     https://derekrodgers.shinyapps.io/vancouver-street-trees

street_trees <- read_csv2("data/raw/street-trees.csv")

# Concatenate Genus + Species for Binomial Name
street_trees <- street_trees |>
  mutate(
    Binomial_Name = paste(GENUS_NAME, SPECIES_NAME),
    HEIGHT_RANGE = factor(HEIGHT_RANGE, levels = c(
      "0' - 10'", "10' - 20'", "20' - 30'", "30' - 40'", 
      "40' - 50'", "50' - 60'", "60' - 70'", "70' - 80'", 
      "80' - 90'", "90' - 100'", "> 100'"
    ), ordered = TRUE)
  )