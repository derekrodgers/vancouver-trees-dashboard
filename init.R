# This file is only used by Heroku

my_packages = c("tidyverse", "fst", "shiny", "shinyWidgets", "ggplot2", "DT", "plotly", "later", "raster", "leaflet", "leaflet.extras")

install_if_missing = function(p) {
  if (!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))