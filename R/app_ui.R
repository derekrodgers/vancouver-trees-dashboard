app_ui <- function(google_api_key, street_trees) {
  fluidPage(
    # Browser page title
    title = "Vancouver Trees Dashboard",

    tags$head(
      # Favicon
      tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png"),

      # Custom CSS
      tags$link(rel = "stylesheet", href = "styles.css"),

      # Leaflet fullscreen plugin
      tags$link(rel = "stylesheet", href = "https://api.mapbox.com/mapbox.js/plugins/leaflet-fullscreen/v1.0.1/leaflet.fullscreen.css"),
      tags$script(src = "https://api.mapbox.com/mapbox.js/plugins/leaflet-fullscreen/v1.0.1/Leaflet.fullscreen.min.js"),

      # Google Maps Street View API
      tags$script(src = paste0("https://maps.googleapis.com/maps/api/js?key=", google_api_key, "&libraries=geometry")),

      # Custom JS handlers
      tags$script(src = "handlers.js")
    ),

    # Title and filters card
    mod_filters_ui("filters", street_trees),

    # Map & Street View Row
    fluidRow(
      # Map Column
      column(8, mod_map_ui("map")),

      # Street View Column (kept outside module to avoid JS namespacing)
      column(4,
        div(class = "panel panel-default",
            style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); margin-top: 0px;",
            h3("Street View", style = "margin-top: 1px; margin-bottom: 15px;"),
            tags$div(id = "street_view_container", style = "width: 100%; height: 521px;")
        )
      )
    ),

    # Heatmap & Tree Species table
    fluidRow(
      # Heatmap column
      column(8, mod_heatmap_ui("heatmap")),

      # Tree Species table
      column(4, mod_species_table_ui("species"))
    ),

    # Footer row
    fluidRow(
      column(12,
        div(
          style = "padding: 3px; text-align: left",
          tags$p(
            "Dataset: ",
            tags$a(href = "https://opendata.vancouver.ca/explore/dataset/public-trees/information/?disjunctive.neighbourhood_name&disjunctive.on_street&disjunctive.species_name&disjunctive.common_name", target = "_blank", "Vancouver Open Data Portal - Public Trees")
          ),
          tags$p(
            tags$a(href = "https://github.com/derekrodgers/vancouver-trees-dashboard", target = "_blank", "GitHub Repository"),
            " \u2014 Licensed under MIT."
          ),
          tags$p(
            "\u00A9 2025 ",
            tags$a(href = "https://databyderek.com/", target = "_blank", "Derek Rodgers")
          )
        )
      )
    )
  )
}
