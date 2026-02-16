library(dplyr)
library(ggplot2)
library(fst)
library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(later)
library(leaflet)

# Source all modules and utilities
source("R/utils_data.R")
source("R/utils_app_config.R")
source("R/mod_filters.R")
source("R/mod_map.R")
source("R/mod_heatmap.R")
source("R/mod_species_table.R")
source("R/app_ui.R")
source("R/app_server.R")

# Load data once at startup
street_trees <- load_street_trees()
google_api_key <- load_google_api_key()

shinyApp(
  ui = app_ui(google_api_key, street_trees),
  server = app_server(street_trees),
  options = get_app_options()
)
