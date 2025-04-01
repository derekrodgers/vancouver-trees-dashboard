# Vancouver Trees Dashboard

The **Vancouver Trees Dashboard** is an interactive R Shiny app that has:

- Filters for public trees by neighbourhood, height category, binomial species name, and common name.
- A live map that displays all tree locations, reactive to all filters.
- Reactive Google Street View, allowing a visual perspective on each tree.
- A table displaying aggreate tree counts by species.
- An "All Trees" table.
- Height distribution charts: (i) heights across all neighbourhoods and (ii) neighbourhoods x height category.

## Dataset

The data used in this dashboard comes from the **City of Vancouver's Public Trees Dataset**, available at:

[City of Vancouver Open Data Portal - Public Trees](https://opendata.vancouver.ca/explore/dataset/public-trees/information/?disjunctive.neighbourhood_name&disjunctive.on_street&disjunctive.species_name&disjunctive.common_name)

## Live App

You can try the live version of the app here:

🔗 **[Vancouver Trees Dashboard](https://vancouvertrees.databyderek.com/)**

## Local Installation

If you'd like to run this app locally, follow these steps:

### 1. Clone the Repository

```sh
git clone https://github.com/derekrodgers/vancouver-trees-dahboard.git
cd vancouver-trees-dahboard
```
### 2. Install Dependencies

Ensure you have R (>= 4.4.1) installed, then start R in the terminal:

```sh
R
```

At the R console, install the `renv` package, then restore from the project's `renv.lock` file to install required packages:

```r
install.packages("renv")
renv::restore()
```

### 3. Run the App

At the R console, run:

```r
shiny::runApp("src/app.R")
```

© 2025 [Derek Rodgers](https://databyderek.com/). Licensed under MIT.
