library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(plotly)

# Map
library(raster)  # Load first (so it doesn't mask later)
library(leaflet)
library(spatstat)  # For spatial point patterns
library(gstat)      # For kriging interpolation
library(sp)         # For spatial data structures
library(raster)     # For rasterizing interpolation results
library(leaflet.extras)  # For heatmap layers in Leaflet


# To run locally, start an R console in the repo root and run:
#     shiny::runApp("app.R")
# To deploy:
#     rsconnect::deployApp(appDir = ".", appName = "vancouver-trees-dashboard")
# Deploy location:
#     https://derekrodgers.shinyapps.io/vancouver-trees-dashboard/
###

street_trees <- read_csv2("data/raw/street-trees.csv")

# Concatenate Genus + Species for Binomial Name
street_trees <- street_trees |>
  mutate(
    Binomial_Name = paste(GENUS_NAME, SPECIES_NAME),
    HEIGHT_RANGE = factor(
      str_replace_all(HEIGHT_RANGE, " ", ""),  # Remove spaces
      levels = c(
        "0'-10'", "10'-20'", "20'-30'", "30'-40'", 
        "40'-50'", "50'-60'", "60'-70'", "70'-80'", 
        "80'-90'", "90'-100'", ">100'"
      ), 
      ordered = TRUE
    )
  )

ui <- fluidPage(
  title = "Vancouver Street Trees Dashboard",
  # Filters row
  fluidRow(
    column(12, 
          div(class = "panel panel-default", 
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 10px;",
              
              # Title Row
              fluidRow(
                column(12, h2("Vancouver Street Trees Dashboard", 
                              style = "margin-top: 0px; margin-bottom: 15px; text-align: left;"))
              ),

              # Filters & Reset Button Row
              fluidRow(
                style = "margin-bottom: 0px;",  # Remove extra margin below row
                column(10, 
                      fluidRow(
                        column(3, pickerInput("neighbourhood", "Neighbourhood",
                                              choices = unique(street_trees$NEIGHBOURHOOD_NAME),
                                              multiple = TRUE,
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                              width = "100%")),
                        column(3, pickerInput("height_range", "Height Range",
                                              choices = levels(street_trees$HEIGHT_RANGE),
                                              multiple = TRUE,
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                              width = "100%")),
                        column(3, pickerInput("binomial_name", "Binomial Name",
                                              choices = unique(street_trees$Binomial_Name),
                                              multiple = TRUE,
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                              width = "100%")),
                        column(3, pickerInput("common_name", "Common Name",
                                              choices = unique(street_trees$COMMON_NAME),
                                              multiple = TRUE,
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                              width = "100%"))
                      )
                ),
                column(2, div(style = "text-align: right; margin-top: 25px;",  
                            actionButton("reset_filters", "Reset Filters", class = "btn-danger"))
                )
              )
          )
    )
  ),

  # First chart row
  fluidRow(
    column(5, 
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 10px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Height Distribution", style = "margin-top: 1px; margin-bottom: 1px;"),
               plotlyOutput("height_distribution", height = "450px")
           )
    ),
    column(7, 
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 10px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Height by Neighbourhood", style = "margin-top: 1px; margin-bottom: 1px;"),
               plotlyOutput("heatmap", height = "450px")
           )
    )
  ),

  # Second chart row
  fluidRow(
    column(5,  
      div(class = "panel panel-default", 
          style = "background-color: #ffffff; padding: 15px; border-radius: 8px; 
                  box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); height: 550px; 
                  display: flex; flex-direction: column;",  
          h3("Tree Count by Species", style = "margin-top: 5px; margin-bottom: 10px;"),  
          fluidRow(
            column(12, div(style = "display: flex; align-items: center;",
                          actionButton("reset_species", "Reset Selection", class = "btn btn-info btn-sm"),
                          span(style = "padding-left: 15px; font-size: 14px;", textOutput("species_count_text"))
            ))
          ),
          br(),  
          div(style = "flex-grow: 1; height: 100%; max-height: 100%; overflow-y: auto;",  
              DTOutput("tree_table", width = "100%")
          )
      )
    ),
    column(7,  
      div(class = "panel panel-default", 
          style = "background-color: #ffffff; padding: 15px; border-radius: 8px; 
                  box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); height: 550px; 
                  display: flex; flex-direction: column;",  
          h3("All Street Trees", style = "margin-top: 1px; margin-bottom: 10px;"),  
          fluidRow(
            column(12, div(style = "display: flex; align-items: center;",
                          actionButton("reset_tree", "Reset Selection", class = "btn btn-info btn-sm"),
                          span(style = "padding-left: 15px; font-size: 14px;", textOutput("tree_count_text"))
            ))
          ),
          br(),  
          div(style = "flex-grow: 1; height: 100%; max-height: 100%; overflow-y: auto;",  
              DTOutput("all_trees_table", width = "100%")
          )
      )
    )
  ),

  # Map Row (Hidden Outputs, Only Input Dependent)
  fluidRow(
    column(12, 
          div(class = "panel panel-default", 
              style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
              h3("Tree Map", style = "margin-top: 1px; margin-bottom: 10px;"),  
              leafletOutput("tree_map", height = "600px")
          )
    )
  )
)

server <- function(input, output, session) {
  selected_species <- reactiveVal(NULL)
  selected_tree <- reactiveVal(NULL)

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "neighbourhood", selected = character(0))  # Reset to no selection
    updatePickerInput(session, "height_range", selected = character(0))
    updatePickerInput(session, "binomial_name", selected = character(0))
    updatePickerInput(session, "common_name", selected = character(0))
  
    selected_species(NULL)  # Reset selected species in table
    selected_tree(NULL)  # Reset selected tree in table
  })

  # Reset species selection
  observeEvent(input$reset_species, {
    selected_species(NULL)
  })

  # Reset tree selection
  observeEvent(input$reset_tree, {
    selected_tree(NULL)
  })

  # Reactive Data Filtering
  filtered_data <- reactive({
    data <- street_trees
  
    if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
      data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
    }
    if (!is.null(input$height_range) && length(input$height_range) > 0) {
      data <- data |> filter(HEIGHT_RANGE %in% input$height_range)
    }
    if (!is.null(input$binomial_name) && length(input$binomial_name) > 0) {
      data <- data |> filter(Binomial_Name %in% input$binomial_name)
    }
    if (!is.null(input$common_name) && length(input$common_name) > 0) {
      data <- data |> filter(COMMON_NAME %in% input$common_name)
    }
    
    # Apply species selection from table click
    if (!is.null(selected_species())) {
      data <- data |> filter(Binomial_Name == selected_species())
    }
  
    # Apply tree selection from table click
    if (!is.null(selected_tree())) {
      data <- data |> filter(TREE_ID == selected_tree())
    }
  
    return(data)
  })

  # Render Map (Initially Empty, Will Update via Proxy)
  output$tree_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = -123.1216, lat = 49.2827, zoom = 12)  # Default to Vancouver
  })

  # Dynamic filter updates
  observe({
    data <- filtered_data()

    observeEvent(input$tree_map_marker_click, {
        click <- input$tree_map_marker_click
        if (is.null(click)) return()  # No click detected

        # Find the tree closest to the clicked location
        clicked_tree <- filtered_data() |>
          mutate(
            lng = as.numeric(sapply(strsplit(geo_point_2d, ","), function(x) x[2])),
            lat = as.numeric(sapply(strsplit(geo_point_2d, ","), function(x) x[1]))
          ) |>
          arrange(abs(lng - click$lng) + abs(lat - click$lat)) |>
          slice(1)  # Closest tree

        if (nrow(clicked_tree) == 1) {
          updatePickerInput(session, "neighbourhood", selected = clicked_tree$NEIGHBOURHOOD_NAME)
          updatePickerInput(session, "height_range", selected = clicked_tree$HEIGHT_RANGE)
          updatePickerInput(session, "binomial_name", selected = clicked_tree$Binomial_Name)
          updatePickerInput(session, "common_name", selected = clicked_tree$COMMON_NAME)

          # Update table selections
          selected_species(clicked_tree$Binomial_Name)
          selected_tree(clicked_tree$TREE_ID)
        }
    })

    if (nrow(data) > 0) {
      # Extract coordinates
      coords <- as.data.frame(do.call(rbind, strsplit(data$geo_point_2d, ", ")))
      names(coords) <- c("lat", "lng")
      coords <- mutate(coords, lat = as.numeric(lat), lng = as.numeric(lng)) |> drop_na(lat, lng)

      if (nrow(coords) == 0) return()  # Exit if no valid coordinates

      # Convert to spatial points
      coordinates(coords) <- ~lng+lat
      proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

      if (is.null(coords) || nrow(coords) == 0 || !("lng" %in% names(coords)) || !("lat" %in% names(coords)) ||
          anyNA(coords$lng) || anyNA(coords$lat)) {
        print("No valid tree coordinates available. Skipping kriging.")
        return()  # Exit observer safely
      }

      grid_size <- ifelse(nrow(coords) > 10000, 50, 100)  # Ensure it's always numeric

      grid <- expand.grid(
        lng = seq(min(coords$lng, na.rm = TRUE), max(coords$lng, na.rm = TRUE), length.out = grid_size),  
        lat = seq(min(coords$lat, na.rm = TRUE), max(coords$lat, na.rm = TRUE), length.out = grid_size)
      )
      coordinates(grid) <- ~lng+lat
      proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

      # Compute Tree Density (Number of Trees per Grid Cell)
      density_counts <- as.data.frame(table(cut(coords$lng, 50), cut(coords$lat, 50)))
      names(density_counts) <- c("lng_bin", "lat_bin", "tree_density")

      # Merge Density with Grid
      grid_df <- as.data.frame(grid)
      density_counts <- merge(grid_df, density_counts, by.x = c("lng", "lat"), by.y = c("lng_bin", "lat_bin"), all.x = TRUE)
      density_counts$tree_density[is.na(density_counts$tree_density)] <- 0  # Fill NAs with 0

      # Convert to Spatial Data
      coordinates(density_counts) <- ~lng+lat
      proj4string(density_counts) <- CRS("+proj=longlat +datum=WGS84")

      # Kriging Interpolation for Tree Density
      print("Starting kriging...")  # Debugging statement

      kriging_model <- gstat::gstat(
        formula = tree_density ~ 1, locations = density_counts, nmax = 10, set = list(idp = 2)
      )

      print("Kriging model created. Running prediction...")  # Debugging statement
      flush.console()  # Forces print output

      interpolated <- predict(kriging_model, grid)

      print("Prediction complete!")  # Debugging statement
      flush.console()

      # Convert to raster for heatmap rendering
      if (is.na(crs(rast))) {
        crs(rast) <- CRS("+proj=longlat +datum=WGS84")  # Set projection explicitly
      } else if (crs(rast)@projargs != "+proj=longlat +datum=WGS84") {
        rast <- projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))  # Reproject if incorrect CRS
      }

      max_val <- max(interpolated$var1.pred, na.rm = TRUE)
      min_val <- min(interpolated$var1.pred, na.rm = TRUE)

      if (max_val == min_val) {  
        interpolated$var1.pred <- 0  # Set to zero if no variation  
      } else {
        interpolated$var1.pred <- (interpolated$var1.pred - min_val) / (max_val - min_val)
      }

      pal <- colorNumeric("YlOrRd", domain = c(0, 1), na.color = "transparent")  # Normalize domain

      # Update Leaflet map with Heatmap
      leafletProxy("tree_map") |>
        clearHeatmap() |>  
        addRasterImage(rast, colors = pal, opacity = 0.7) |>
        addLegend(pal = pal, values = interpolated$var1.pred, title = "Tree Density")
    }
  })

  # Heatmap of Tree Count x Neighbourhood
  output$heatmap <- renderPlotly({
    data <- filtered_data()
  
    heatmap_data <- data |>
      count(NEIGHBOURHOOD_NAME, HEIGHT_RANGE)
  
    # heatmap with formatted tooltips
    plot <- ggplot(heatmap_data, aes(x = HEIGHT_RANGE, y = NEIGHBOURHOOD_NAME, fill = n, 
                                     text = paste0("<b>Neighbourhood</b>: ", NEIGHBOURHOOD_NAME, "<br>",
                                                   "<b>Height Range</b>: ", HEIGHT_RANGE, "<br>",
                                                   "<b>Tree Count</b>: ", format(n, big.mark = ",")))) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Height Range", y = "Neighbourhood", fill = "Tree Count") +
      theme_minimal()
  
    ggplotly(plot, tooltip = "text")  # tooltips
  })

  # Tree Height Distribution (All Neighbourhoods)
  output$height_distribution <- renderPlotly({
    data <- filtered_data() |>
      count(HEIGHT_RANGE)  # Compute counts beforehand
  
    plot <- ggplot(data, aes(x = HEIGHT_RANGE, y = n, 
                             text = paste0("<b>Height Range</b>: ", HEIGHT_RANGE, "<br>",
                                           "<b>Tree Count</b>: ", format(n, big.mark = ",")))) +
      geom_bar(stat = "identity", fill = "seagreen") +  # Use precomputed counts
      labs(x = "Height Range", y = "Tree Count") +
      theme_minimal()
  
    ggplotly(plot, tooltip = "text")  # tooltips
  })

  # Tree species count table
  output$tree_table <- renderDT({
    data <- filtered_data() |>
      group_by(Binomial_Name, COMMON_NAME) |>
      summarise(Count_Common_Name = n(), .groups = "drop") |>
      arrange(Binomial_Name, desc(Count_Common_Name)) |>
      group_by(Binomial_Name) |>
      summarise(
        `Common Names` = paste(unique(COMMON_NAME[order(-Count_Common_Name)]), collapse = ", "),
        Count = sum(Count_Common_Name)
      ) |>
      mutate(
        # Capitalize first word, lowercase second word (otherwise link doesn't work)
        `Binomial_Link` = paste0(
          "<a href='https://en.wikipedia.org/wiki/",
          gsub(" ", "_", 
               paste0(
                 toupper(substr(Binomial_Name, 1, 1)),  # Uppercase first letter
                 tolower(substr(Binomial_Name, 2, nchar(Binomial_Name)))  # Lowercase rest
               )),
          "' target='_blank'>", Binomial_Name, "</a>"
        ),
        `Common Names` = ifelse(nchar(`Common Names`) > 65, 
                                paste0(substr(`Common Names`, 1, 65), "..."), 
                                `Common Names`),
        Count = format(Count, big.mark = ",")
      ) |>
      arrange(desc(Count))
  
    datatable(data |> dplyr::select(Count, `Binomial_Link`, `Common Names`),  
          escape = FALSE,
          colnames = c("Count", "Binomial Name", "Common Names"),
          options = list(
            pageLength = 100,
            lengthMenu = list(c(10, 25, 50, 100, 250, 500, 750), 
                              c("10", "25", "50", "100", "250", "500", "750")),
            autoWidth = TRUE,
            searchHighlight = TRUE
          ))
  })
  
  # Handle species selection from table clicks
  observeEvent(input$tree_table_rows_selected, {
    selected_row <- input$tree_table_rows_selected
    displayed_data <- filtered_data() |>
      group_by(Binomial_Name) |>
      summarise(
        `Common Names` = paste(unique(COMMON_NAME), collapse = ", "),  
        Count = sum(n())
      ) |>
      arrange(desc(Count))  # Sort for display
  
    if (!is.null(selected_row) && length(selected_row) > 0 && selected_row <= nrow(displayed_data)) {
      species <- displayed_data$Binomial_Name[selected_row]  
      selected_species(species)
      selected_tree(NULL)  
    }
  })
  
  output$all_trees_table <- renderDT({
    data <- filtered_data() |>
      dplyr::select(TREE_ID, Binomial_Name, COMMON_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, geo_point_2d) |>
      mutate(
        `Google Maps Link` = paste0(
          "<a href='https://www.google.com/maps/search/?api=1&query=",
          geo_point_2d, "' target='_blank'>View</a>"
        ),
        # Capitalize first word, lowercase second word (otherwise link doesn't work)
        `Binomial_Link` = paste0(
          "<a href='https://en.wikipedia.org/wiki/",
          gsub(" ", "_", 
               paste0(
                 toupper(substr(Binomial_Name, 1, 1)),  # Uppercase first letter
                 tolower(substr(Binomial_Name, 2, nchar(Binomial_Name)))  # Lowercase rest
               )),
          "' target='_blank'>", Binomial_Name, "</a>"
        )
      ) |>
      dplyr::select(-geo_point_2d)  
  
    datatable(data |> dplyr::select(TREE_ID, `Binomial_Link`, COMMON_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, `Google Maps Link`),  
              escape = FALSE,
              colnames = c("Tree ID", "Binomial Name", "Common Name", "Neighbourhood", "Height Range", "Google Maps Link"),
              options = list(
                pageLength = 100,
                lengthMenu = list(c(10, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000), 
                                  c("10", "50", "100", "250", "500", "1K", "2.5K", "5K", "10K", "25K")),
                autoWidth = TRUE,
                searchHighlight = TRUE
              ))
  })
  
  # Handle tree selection from table clicks
  observeEvent(input$all_trees_table_rows_selected, {
    selected_row <- input$all_trees_table_rows_selected
    if (!is.null(selected_row)) {
      tree_id <- filtered_data() |> distinct(TREE_ID) |> slice(selected_row) |> pull(TREE_ID)
      selected_tree(tree_id)
      selected_species(NULL)  # Clear species selection if a tree is chosen
    }
  })
  
  # Add Unique Species Count Text
  output$species_count_text <- renderText({
    num_species <- filtered_data() |>
      distinct(Binomial_Name) |>
      nrow()
    
    paste("Unique Species:", format(num_species, big.mark = ","))
  })
  
  output$tree_count_text <- renderText({
    num_trees <- filtered_data() |>
      nrow()
  
    paste("Total Trees:", format(num_trees, big.mark = ","))
  })

  observe({
    data <- filtered_data()
    print(paste("Filtered Data Rows:", nrow(data)))  # Debugging output

    data <- data |> mutate(
      lng = as.numeric(sapply(strsplit(geo_point_2d, ","), function(x) x[2])),
      lat = as.numeric(sapply(strsplit(geo_point_2d, ","), function(x) x[1]))
    )

    leafletProxy("tree_map", data = data) |>
      clearMarkers() |>
      clearMarkerClusters() |>
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        clusterOptions = markerClusterOptions()
      )
  })

}

options(shiny.autoreload = TRUE)
shinyApp(ui, server, options = list(port = 3838))