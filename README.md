# Vancouver Street Trees Dashboard

## Motivation

**Target Audience:** Arborists at the Vancouver Board of Parks and Recreation.

Street trees provide essential environmental and social benefits, including air quality improvement, urban cooling, and aesthetic enhancement. However, some neighbourhoods may have less tree coverage than others, impacting the overall urban canopy. This dashboard allows arborists to explore Vancouver's public street tree dataset to identify areas with lower tree coverage, assess species diversity, and make informed decisions about where new trees should be planted.

## App Description

The **Vancouver Street Trees Dashboard** is an interactive Shiny app that enables users to:

- Filter street trees by **neighbourhood, height category, binomial species name, and common name**.
- Visualize tree distributions with a **heatmap** and **bar chart**.
- View detailed **data tables** for both species and individual trees, with interactive search and sorting.
- Access **Google Maps links** for individual tree locations.
- Access **Wikipedia Articles** for tree species.

The app is built using R and Shiny, with visualization support from `plotly` and `ggplot2`.

## Demo

https://github.ubc.ca/mds-2024-25/DSCI_532_individual-assignment_rodgersd/assets/3428/abf78235-a4a3-40fa-a43e-94555d46acc1

## Dataset

The data used in this dashboard comes from the **City of Vancouver's Public Trees Dataset**, available at:

[City of Vancouver Open Data - Public Trees](https://opendata.vancouver.ca/explore/dataset/public-trees/information/?disjunctive.neighbourhood_name&disjunctive.on_street&disjunctive.species_name&disjunctive.common_name)

## Live App

You can try the live version of the app here:

🔗 **[Vancouver Street Trees Dashboard](https://derekrodgers.shinyapps.io/vancouver-street-trees/)**

## Local Installation

If you'd like to run this app locally, follow these steps:

### 1. Clone the Repository

```sh
git clone https://github.ubc.ca/mds-2024-25/DSCI_532_individual-assignment_rodgersd.git
cd DSCI_532_individual-assignment_rodgersd
```
### 2. Install Dependencies

Ensure you have R (>= 4.4.1) installed, then start R in the terminal:

```sh
R
```

At the R console, install the `renv` package and restore from the project's `renv.lock` file to install required packages:

```r
install.packages("renv")
renv::restore()
```

### 3. Run the App

At the R console, run:

```r
shiny::runApp("src/app.R")
```

© 2025 Derek Rodgers. Licensed under MIT.
