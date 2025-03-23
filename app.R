library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(plotly)
library(later)

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

google_api_key <- trimws(readLines("google_api_key.txt", warn = FALSE))

# Load only the cols we need
street_trees <- read_csv2(
  "data/raw/street-trees.csv",
  col_select = c(
    TREE_ID,
    CIVIC_NUMBER,
    STD_STREET,
    GENUS_NAME,
    SPECIES_NAME,
    COMMON_NAME,
    NEIGHBOURHOOD_NAME,
    HEIGHT_RANGE,
    DATE_PLANTED,
    geo_point_2d
  )
)

#source("src/preprocessing.R")

# Preprocessing
street_trees <- street_trees |> 
  drop_na(geo_point_2d) |>
  mutate(
    # Construct Binomial_Name: capitalize genus and make species lowercase, then remove any trailing " x"
    Binomial_Name = paste0(
      toupper(substr(GENUS_NAME, 1, 1)),
      tolower(substr(GENUS_NAME, 2, nchar(GENUS_NAME))),
      " ",
      tolower(SPECIES_NAME)
    ),
    Binomial_Name = gsub(" x$", "", Binomial_Name),
    Binomial_Name = gsub(" xx$", "", Binomial_Name),

    # Convert to title case
    COMMON_NAME = str_to_title(COMMON_NAME),
    NEIGHBOURHOOD_NAME = str_to_title(NEIGHBOURHOOD_NAME),
    CIVIC_ADDRESS = paste0(CIVIC_NUMBER, " ", str_to_title(STD_STREET)),

    HEIGHT_RANGE = factor(
      str_replace_all(HEIGHT_RANGE, " ", ""),
      levels = c(
        "0'-10'", "10'-20'", "20'-30'", "30'-40'", 
        "40'-50'", "50'-60'", "60'-70'", "70'-80'", 
        "80'-90'", "90'-100'", ">100'"
      ),
      ordered = TRUE
    ),

    # Parse lat/lon directly from geo_point_2d
    LATITUDE = as.numeric(str_split_fixed(geo_point_2d, ",\\s*", 2)[, 1]),
    LONGITUDE = as.numeric(str_split_fixed(geo_point_2d, ",\\s*", 2)[, 2])
  ) |> 
  dplyr::select(-geo_point_2d) # Don't need this anymore

ui <- fluidPage(
  # Browser title
  title = "Vancouver Street Trees Dashboard",
  
  tags$head(
    tags$style(HTML("
      #tree_table table.dataTable tbody tr:hover,
      #all_trees_table table.dataTable tbody tr:hover {
        cursor: pointer;
      }
    "))
  ),

  tags$head(
    tags$link(rel = "stylesheet", href = "https://api.mapbox.com/mapbox.js/plugins/leaflet-fullscreen/v1.0.1/leaflet.fullscreen.css"),
    tags$script(src = "https://api.mapbox.com/mapbox.js/plugins/leaflet-fullscreen/v1.0.1/Leaflet.fullscreen.min.js")
  ),

  tags$head(
    tags$style(HTML("
      .bootstrap-select .dropdown-menu {
        z-index: 2000 !important;
      }
    "))
  ),

  # Title and filters card
  fluidRow(
    column(12, 
          div(class = "panel panel-default", 
              style = "background-color: #f8f9fa; padding: 10px 20px; border-radius: 8px; margin-top: 5px;",
              
              # Title Row
              fluidRow(
                style = "margin-bottom: 15px;",
                column(12,
                  div(
                    style = "display: flex; align-items: center;",
 
                    h2(
                      "Vancouver Street Trees Dashboard", 
                      style = "margin: 0; text-align: left; line-height: 1.2; margin-right: 10px;"
                    ),
 
                    tags$img(
                      src = "favicon.png", 
                      height = "30px"
                    )
                  )
                )
              ),
              # Filters & Reset Button Row
              fluidRow(
                    column(2, pickerInput("neighbourhood", "Neighbourhood",
                                          choices = sort(unique(street_trees$NEIGHBOURHOOD_NAME)),
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                          width = "100%")),
                    column(2, pickerInput("binomial_name", "Binomial Name",
                                          choices = sort(unique(street_trees$Binomial_Name)),
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                          width = "100%")),
                    column(2, pickerInput("common_name", "Common Name",
                                          choices = sort(unique(street_trees$COMMON_NAME)),
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                          width = "100%")),
                    column(2, pickerInput("height_range", "Height Range",
                                          choices = levels(street_trees$HEIGHT_RANGE),
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                          width = "100%")),
                    column(2, pickerInput("interesting_trees", "🌴 Interesting Trees 🌴",
                                          choices = c("🌸 Cherry & Plum Trees", "🏞️ VanDusen Botanical Garden"),
                                          multiple = TRUE,
                                          options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                          width = "100%")),
                    column(2, div(style = "text-align: right; margin-top: 25px;",
                                  actionButton("reset_filters", "Reset Filters", 
                                              class = "btn-danger",
                                              style = "font-weight: bold; font-size: 14.5px; padding: 8px 14px;")))
              )
          )
    )
  ),

  # Map Row
  fluidRow(
    # Map Column
    column(8, 
      div(class = "panel panel-default", 
          style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); margin-top: 0px;",
          fluidRow(
            column(12,
              div(
                style = "display: flex; flex-wrap: wrap; align-items: center;",
                
                div(
                  style = "flex: 0 1 auto; margin-right: 15px; margin-bottom: 5px;",
                  h3("Tree Map", style = "margin-top: 1px; margin-bottom: 10px;")
                ),
                
                div(
                  style = "flex: 1 1 auto; text-align: center; font-size: 14px; margin-bottom: 5px;",
                  textOutput("map_tree_count_text")
                ),
                
                div(
                  style = "flex: 0 1 auto; text-align: right; margin-left: auto; margin-bottom: 5px;",
                  actionButton("reset_map", "Clear Selection", class = "btn btn-info btn-xs"),
                  actionButton("reset_zoom", "Reset Zoom", class = "btn btn-info btn-xs", style = "margin-left: 10px;")
                )
              )
            )
          ),
          div(leafletOutput("tree_map", height = "520px")
          )
      )
    ),
    # Street View Column
    column(4,
      div(class = "panel panel-default",
          style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1); margin-top: 0px;",
          h3("Street View", style = "margin-top: 1px; margin-bottom: 15px;"),
          # Again, fix the Street View container to 500px
          tags$div(id = "street_view_container", style = "width: 100%; height: 521px;")
      )
    )
  ),

  # Tables row
  fluidRow(
    column(8,  
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               fluidRow(
                 column(12,
                   div(
                     style = "display: flex; align-items: center; white-space: nowrap;",
 
                     div(
                       style = "flex: 0 1 auto; margin-right: 15px; margin-bottom: 5px;",
                       h3("All Trees", style = "margin-top: 1px; margin-bottom: 10px;")
                     ),
 
                     div(
                       style = "flex: 1 1 auto; text-align: center; font-size: 14px; margin-bottom: 5px;",
                       textOutput("tree_count_text")
                     ),
 
                     div(
                       style = "flex: 0 1 auto; text-align: right; margin-left: auto; margin-bottom: 5px;",
                       actionButton("reset_tree", "Clear Selection", class = "btn btn-info btn-xs")
                     )
                   )
                 )
               ),
               DTOutput("all_trees_table")
           )
    ),
    column(4,  
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               fluidRow(
                 column(12,
                   div(
                     style = "display: flex; flex-wrap: wrap; align-items: center;",
 
                     div(
                       style = "flex: 0 0 auto; margin-right: 15px;",
                       h3("Tree Species", style = "margin-top: 5px; margin-bottom: 10px;")
                     ),
 
                     div(
                       style = "flex: 1 1 auto; text-align: center; font-size: 14px;",
                       textOutput("species_count_text")
                     ),
 
                     div(
                       style = "flex: 0 0 auto; text-align: right;",
                       actionButton("reset_species", "Clear Selection", class = "btn btn-info btn-xs")
                     )
                   )
                 )
               ),
               DTOutput("tree_table")
           )
    )
  ),

  # Heatmap / bar chart row
  fluidRow(
    column(5, 
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Height Distribution", style = "margin-top: 1px; margin-bottom: 1px;"),
               plotlyOutput("height_distribution", height = "420px")
           )
    ),
    column(7, 
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Height by Neighbourhood", style = "margin-top: 1px; margin-bottom: 1px;"),
               plotlyOutput("heatmap", height = "420px")
           )
    )
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
          # tags$p(
          #   tags$a(href = "https://github.com/derekrodgers/vancouver-trees-dashboard", "GitHub Repository")
          # ),
        tags$p(paste("©", format(Sys.Date(), "%Y"), "Derek Rodgers. Licensed under MIT."))
      )
    )
  ),
  # Favicon
  tags$head(tags$link(rel = "shortcut icon", type = "image/png", href = "favicon.png")),

  # JS for street view
  tags$head(
    tags$script(src = paste0("https://maps.googleapis.com/maps/api/js?key=", google_api_key, "&libraries=geometry")),
    tags$script(HTML('
      Shiny.addCustomMessageHandler("updateStreetView", function(message) {
        var location = new google.maps.LatLng(message.lat, message.lon);
        var sv = new google.maps.StreetViewService();
        sv.getPanorama({location: location, radius: 50}, function(data, status) {
          if (status === google.maps.StreetViewStatus.OK) {
            var panoLatLng = data.location.latLng;
            var computedHeading = google.maps.geometry.spherical.computeHeading(panoLatLng, location);
            if (!window.streetPanorama) {
              window.streetPanorama = new google.maps.StreetViewPanorama(
                document.getElementById("street_view_container"),
                {
                  position: location,
                  pov: {heading: computedHeading, pitch: 0},
                  visible: true
                }
              );
            } else {
              window.streetPanorama.setPosition(location);
              window.streetPanorama.setPov({heading: computedHeading, pitch: 0});
              window.streetPanorama.setVisible(true);
            }
          } else {
            console.log("No panorama found for this location.");
          }
        });
      });

      Shiny.addCustomMessageHandler("clearStreetView", function(message) {
        // If a Street View panorama exists, hide it and remove references.
        if (window.streetPanorama) {
          window.streetPanorama.setVisible(false);
          window.streetPanorama = null;
        }
        
        // Get the container for Street View.
        var container = document.getElementById("street_view_container");
        
        // Remove all existing child elements.
        while (container.firstChild) {
          container.removeChild(container.firstChild);
        }
        
        // Reset the container\'s background to white.
        container.style.backgroundColor = "white";
        
        // Insert the placeholder message.
        container.innerHTML = "<div style=\'font-size:16px; padding:10px;\'>Select a single tree.</div>";
      });
    '))
  ),

  # Fix popup / zoom conflict
  tags$script(HTML('
    // Custom handler to open popup after zoom
    Shiny.addCustomMessageHandler("openPopupAfterZoom", function(message) {
      var map = window.treeMap;
      if (!map) return;
      // Capture current view if not already saved
      if (!window.prevView) {
        window.prevView = {
          center: map.getCenter(),
          zoom: map.getZoom()
        };
      }
      map.once("zoomend", function() {
        var markerFound = null;
        map.eachLayer(function(layer) {
          if (layer.options && layer.options.layerId == message.id) {
            markerFound = layer;
          }
        });
        if (markerFound) {
          markerFound.bindPopup(message.content).openPopup();
          // Attach listener on this marker for popup close
          markerFound.on("popupclose", function(e) {
            Shiny.setInputValue("popup_closed", new Date().getTime(), {priority: "event"});
          });
        }
      });
    });
  
  Shiny.addCustomMessageHandler("saveCurrentMapView", function(message) {
    var map = window.treeMap;
    if (map) {
      window.prevView = {
        center: map.getCenter(),
        zoom: map.getZoom()
      };
    }
  });
  
    // Custom handler to restore previous map view
    Shiny.addCustomMessageHandler("restorePrevMapView", function(message) {
      var map = window.treeMap;
      if (map && window.prevView) {
        map.setView(window.prevView.center, window.prevView.zoom);
        window.prevView = null;  // Clear stored view after restoration
      }
    });
  '))
)

server <- function(input, output, session) {
  # Reactive for the manual "Interesting Trees" filter
  base_data <- reactive({
    street_trees |> apply_interesting_tree_filters()
  })

  selected_species <- reactiveVal(NULL)
  selected_tree <- reactiveVal(NULL)
  restoring_view <- reactiveVal(FALSE)
  
  interesting_areas <- tibble::tibble(
    label = "🏞️ VanDusen Botanical Garden",
    min_lng = -123.138048600311,
    max_lng = -123.12791305292048,
    min_lat = 49.23785042226124,
    max_lat = 49.241214933282045
  )
  
  apply_interesting_tree_filters <- function(data) {
    if (!is.null(input$interesting_trees) && length(input$interesting_trees) > 0) {
      # Initialize a logical vector with FALSE for each row
      filter_condition <- rep(FALSE, nrow(data))
      
      # If 'Cherry & Plum Trees' is selected, mark rows matching the pattern
      if ("🌸 Cherry & Plum Trees" %in% input$interesting_trees) {
        filter_condition <- filter_condition | grepl("cherry|plum", data$COMMON_NAME, ignore.case = TRUE)
      }
      
      # If 'VanDusen Botanical Garden' is selected, mark rows within the geographic bounds
      if ("🏞️ VanDusen Botanical Garden" %in% input$interesting_trees) {
        filter_condition <- filter_condition | (
          data$LONGITUDE >= interesting_areas$min_lng &
          data$LONGITUDE <= interesting_areas$max_lng &
          data$LATITUDE >= interesting_areas$min_lat &
          data$LATITUDE <= interesting_areas$max_lat
        )
      }
      
      # Subset the data to rows that satisfy at least one condition
      data <- data[filter_condition, ]
    }
    return(data)
  }
  
  show_tree_popup <- function(tree_id, save_view = FALSE) {
    selected_tree(tree_id)
    selected_species(NULL)
  
    tree_info <- filtered_data() |> filter(TREE_ID == tree_id) |> slice(1)
  
    if (nrow(tree_info) > 0) {
      content <- paste0(
        "<div style='font-size: 14px; width: 400px;'>",
        "<b>Binomial Name:</b> ", tree_info$Binomial_Name, " (",
        "<a href='https://en.wikipedia.org/wiki/", gsub(' ', '_', tree_info$Binomial_Name), "' target='_blank'>wiki</a>)<br>",
        "<b>Common Name:</b> ", tree_info$COMMON_NAME, "<br>",
        "<b>Address:</b> ", tree_info$CIVIC_ADDRESS, "<br>",
        "<b>Neighbourhood:</b> ", tree_info$NEIGHBOURHOOD_NAME, "<br>",
        "<b>Height Range:</b> ", tree_info$HEIGHT_RANGE, "<br>",
        "<b>Google Maps:</b> <a href='https://www.google.com/maps/search/?api=1&query=", tree_info$LATITUDE, ",", tree_info$LONGITUDE, "' target='_blank'>View</a>",
        "</div>"
      )
    } else {
      content <- "No tree info found."
    }
  
    if (save_view) {
      session$sendCustomMessage("saveCurrentMapView", list())
    }
  
    session$sendCustomMessage("openPopupAfterZoom", list(id = tree_id, content = content))
  }

available_neighbourhoods <- reactive({
  data <- base_data()
  
  # Apply other filters (if any) that affect what neighbourhoods are available
  if (!is.null(input$height_range) && length(input$height_range) > 0) {
      data <- data |> filter(HEIGHT_RANGE %in% input$height_range)
    }
    if (!is.null(input$binomial_name) && length(input$binomial_name) > 0) {
      data <- data |> filter(Binomial_Name %in% input$binomial_name)
    }
    if (!is.null(input$common_name) && length(input$common_name) > 0) {
      data <- data |> filter(COMMON_NAME %in% input$common_name)
    }
    
    sort(unique(data$NEIGHBOURHOOD_NAME))
  })

  observe({
    updatePickerInput(session, "neighbourhood",
                      choices = available_neighbourhoods(),
                      selected = intersect(input$neighbourhood, available_neighbourhoods()))
  })

  # Compute available Height Range values based on other filters
  available_height_range <- reactive({
    data <- base_data()
    
    # Apply the other filters
    if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
      data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
    }
    if (!is.null(input$binomial_name) && length(input$binomial_name) > 0) {
      data <- data |> filter(Binomial_Name %in% input$binomial_name)
    }
    if (!is.null(input$common_name) && length(input$common_name) > 0) {
      data <- data |> filter(COMMON_NAME %in% input$common_name)
    }
    if (!is.null(input$interesting_trees) && "🌸 Cherry & Plum Trees" %in% input$interesting_trees) {
      data <- data |> filter(grepl("cherry|plum", COMMON_NAME, ignore.case = TRUE))
    }

    # Preserve the original factor order from street_trees$HEIGHT_RANGE
    hr_levels <- levels(street_trees$HEIGHT_RANGE)
    # Only keep levels actually present in the filtered data
    hr_levels[hr_levels %in% data$HEIGHT_RANGE]
  })

  # Update the Height Range picker
  observe({
    updatePickerInput(
      session,
      "height_range",
      choices = available_height_range(),
      selected = intersect(input$height_range, available_height_range())
    )
  })

  # Compute available Binomial Name values based on other filters
  available_binomial_name <- reactive({
    data <- base_data()
    if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
      data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
    }
    if (!is.null(input$height_range) && length(input$height_range) > 0) {
      data <- data |> filter(HEIGHT_RANGE %in% input$height_range)
    }
    if (!is.null(input$common_name) && length(input$common_name) > 0) {
      data <- data |> filter(COMMON_NAME %in% input$common_name)
    }
    sort(unique(data$Binomial_Name))
  })

  # Update the Binomial Name picker
  observe({
    updatePickerInput(session, "binomial_name",
                      choices = available_binomial_name(),
                      selected = intersect(input$binomial_name, available_binomial_name()))
  })

  # Compute available Common Name values based on other filters
  available_common_name <- reactive({
    data <- base_data()
    if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
      data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
    }
    if (!is.null(input$height_range) && length(input$height_range) > 0) {
      data <- data |> filter(HEIGHT_RANGE %in% input$height_range)
    }
    if (!is.null(input$binomial_name) && length(input$binomial_name) > 0) {
      data <- data |> filter(Binomial_Name %in% input$binomial_name)
    }
    sort(unique(data$COMMON_NAME))
  })

  # Update the Common Name picker
  observe({
    updatePickerInput(session, "common_name",
                      choices = available_common_name(),
                      selected = intersect(input$common_name, available_common_name()))
  })
  

  # change behaviour of map popup's "x"
  observeEvent(input$popup_closed, {
    selected_tree(NULL)
    later::later(function() {
      session$sendCustomMessage("restorePrevMapView", list())
    }, delay = 0.2)
  })

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "neighbourhood", selected = character(0))
    updatePickerInput(session, "height_range", selected = character(0))
    updatePickerInput(session, "binomial_name", selected = character(0))
    updatePickerInput(session, "common_name", selected = character(0))
    updatePickerInput(session, "interesting_trees", selected = character(0))
    
    selected_species(NULL)
    selected_tree(NULL)
    
    # Reset the map zoom/pan to show all current points:
    data <- filtered_data()
    if(nrow(data) > 0) {
      minLng <- min(data$LONGITUDE, na.rm = TRUE)
      maxLng <- max(data$LONGITUDE, na.rm = TRUE)
      minLat <- min(data$LATITUDE, na.rm = TRUE)
      maxLat <- max(data$LATITUDE, na.rm = TRUE)
      
      leafletProxy("tree_map", data = data) |>
        fitBounds(lng1 = minLng, lat1 = minLat, lng2 = maxLng, lat2 = maxLat)
    } else {
      leafletProxy("tree_map") |>
        setView(lng = -123.1216, lat = 49.2827, zoom = 12)
    }
  })

  observeEvent(input$reset_species, {
    selected_species(NULL)
    updatePickerInput(session, "binomial_name", selected = character(0))
    proxy <- dataTableProxy('tree_table')
    selectRows(proxy, integer(0))
  })

  # Reset tree selection
  observeEvent(input$reset_tree, {
    selected_tree(NULL)
  })

  # Reactive Data Filtering
  filtered_data <- reactive({
    data <- base_data()
  
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
    leaflet() |> # leaflet(options = leafletOptions(maxZoom = 22)) |>
      addTiles() |>
      setView(lng = -123.1216, lat = 49.2827, zoom = 12) |> 
      htmlwidgets::onRender("
    function(el, x) {
      window.treeMap = this;
      this.addControl(new L.Control.Fullscreen());
    }
  ")
  })

  # # Satellite imagery map
  # output$tree_map <- renderLeaflet({
  #   leaflet(options = leafletOptions(maxZoom = 21)) |>
  #     addProviderTiles(providers$Esri.WorldImagery, options = providerTileOptions(maxZoom = 21)) |>
  #     setView(lng = -123.1216, lat = 49.2827, zoom = 12) |> 
  #     htmlwidgets::onRender("
  #       function(el, x) {
  #         window.treeMap = this;
  #         this.addControl(new L.Control.Fullscreen());
  #       }
  #     ")
  # })

  # Street view
  observe({
    req(selected_tree())  # Only proceed when a tree is selected
    
    # Retrieve the selected tree's info
    tree_info <- street_trees %>% 
      filter(TREE_ID == selected_tree()) %>% 
      slice(1)
    
    lat <- tree_info$LATITUDE
    lon <- tree_info$LONGITUDE
    
    # Send a custom message to update Street View
    session$sendCustomMessage("updateStreetView", list(lat = lat, lon = lon))
  })

  observe({
    if (is.null(selected_tree())) {
      session$sendCustomMessage("clearStreetView", list())
    }
  })

# Dynamic filter updates
observe({
  data <- filtered_data()

  if (nrow(data) > 1) {
    coords <- data
    coordinates(coords) <- ~LONGITUDE+LATITUDE
    proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")


    grid_size <- ifelse(nrow(coords) > 10000, 50, 100)

    grid <- expand.grid(
      lng = seq(min(data$LONGITUDE, na.rm = TRUE), max(data$LONGITUDE, na.rm = TRUE), length.out = grid_size),
      lat = seq(min(data$LATITUDE, na.rm = TRUE), max(data$LATITUDE, na.rm = TRUE), length.out = grid_size)
    )
    coordinates(grid) <- ~lng+lat
    proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

    # Compute Tree Density
    density_counts <- as.data.frame(table(cut(data$LONGITUDE, 50), cut(data$LATITUDE, 50)))
    names(density_counts) <- c("lng", "lat", "tree_density")

    grid_df <- as.data.frame(grid)
    density_counts <- merge(grid_df, density_counts,
                            by = c("lng", "lat"), all.x = TRUE)
    density_counts$tree_density[is.na(density_counts$tree_density)] <- 0

    coordinates(density_counts) <- ~lng+lat
    proj4string(density_counts) <- CRS("+proj=longlat +datum=WGS84")

    print("Starting kriging...")
    kriging_model <- gstat::gstat(
      formula = tree_density ~ 1, locations = density_counts, nmax = 10, set = list(idp = 2)
    )
    print("Kriging model created. Running prediction...")
    flush.console()

    interpolated <- predict(kriging_model, grid)

    # Convert to raster
    rast <- rasterFromXYZ(as.data.frame(interpolated)[, c("lng", "lat", "var1.pred")])
    crs(rast) <- CRS("+proj=longlat +datum=WGS84")

    max_val <- max(interpolated$var1.pred, na.rm = TRUE)
    min_val <- min(interpolated$var1.pred, na.rm = TRUE)

    if (max_val != min_val) {
      interpolated$var1.pred <- (interpolated$var1.pred - min_val) / (max_val - min_val)
 
      pal <- colorNumeric("YlOrRd", domain = c(0, 1), na.color = "transparent")
 
      leafletProxy("tree_map") |>
        clearHeatmap() |>  
        addRasterImage(rast, colors = pal, opacity = 0.7) |>
        addLegend(pal = pal, values = interpolated$var1.pred, title = "Tree Density")
    } else {
      leafletProxy("tree_map") |> clearHeatmap()
    }
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
      theme_minimal() +
      scale_y_discrete(limits = sort(unique(data$NEIGHBOURHOOD_NAME), decreasing = TRUE))
  
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

  output$all_trees_table <- renderDT({
    data <- filtered_data() |>
      dplyr::select(TREE_ID, Binomial_Name, COMMON_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, LATITUDE, LONGITUDE) |>
      mutate(
        `Google Maps Link` = paste0(
          "<a href='https://www.google.com/maps/search/?api=1&query=",
          LATITUDE, ",", LONGITUDE, "' target='_blank'>View</a>"
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
      dplyr::select(-LATITUDE, -LONGITUDE)  
  
    datatable(data |> dplyr::select(TREE_ID, `Binomial_Link`, COMMON_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, `Google Maps Link`),  
              escape = FALSE,
              colnames = c("Tree ID", "Binomial Name", "Common Name", "Neighbourhood", "Height Range", "Google Maps Link"),
              options = list(
                pageLength = 100,
                lengthMenu = list(c(10, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000), 
                                  c("10", "50", "100", "250", "500", "1k", "2.5k", "5k", "10k", "25k")),
                autoWidth = TRUE,
                searchHighlight = TRUE,
                scrollY = "370px"
              ))
  })

  # Tree species count table
  common_name_trucation_chars <- 45
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
        `Binomial_Link` = paste0(
          "<a href='https://en.wikipedia.org/wiki/",
          gsub(" ", "_", Binomial_Name),
          "' target='_blank'>", Binomial_Name, "</a>"
        ),
        `Common Names` = ifelse(nchar(`Common Names`) > common_name_trucation_chars, 
                                paste0(substr(`Common Names`, 1, common_name_trucation_chars), "..."), 
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
                searchHighlight = TRUE,
                scrollY = "340px"
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
      updatePickerInput(session, "binomial_name", selected = species)
    }
  })
  
  # Handle tree selection from table clicks
  observeEvent(input$all_trees_table_rows_selected, {
    selected_row <- input$all_trees_table_rows_selected
    if (!is.null(selected_row)) {
      tree_id <- filtered_data() |> distinct(TREE_ID) |> slice(selected_row) |> pull(TREE_ID)
      show_tree_popup(tree_id, save_view = TRUE)
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

  # Is this redundant with the above?
  output$map_tree_count_text <- renderText({
    num_trees <- filtered_data() |>
      nrow()
    paste("Total Trees:", format(num_trees, big.mark = ","))
  })

observe({
  # Skip view updates if we're in the middle of restoring the view.
  if (restoring_view()) return()
  
  data <- filtered_data()
  print(paste("Filtered Data Rows:", nrow(data)))  # Debug output
  
  if(nrow(data) > 0) {
    # Calculate bounds from the filtered data
    minLng <- min(data$LONGITUDE, na.rm = TRUE)
    maxLng <- max(data$LONGITUDE, na.rm = TRUE)
    minLat <- min(data$LATITUDE, na.rm = TRUE)
    maxLat <- max(data$LATITUDE, na.rm = TRUE)

    icon_create_string <- "function(cluster) {
      var maxCount = 45000;
      var numBuckets = 8;
      var colors = [
        '#90EE90', // light green
        '#4cb04c', // green
        '#FFFF00', // yellow
        '#FFD700', // gold
        '#FFA500', // orange
        '#FF8C00', // dark orange
        '#FF4500', // orange red
        '#FF0000'  // red
      ];
      var count = cluster.getChildCount();
      // If count is less than 1000, leave it as is; otherwise, round to the nearest thousand.
      var countFormatted = (count < 1000) ? count : (Math.round(count / 1000)) + 'k';
      var bucket = Math.floor(Math.pow(count / maxCount, 0.5) * numBuckets);
      bucket = Math.max(0, Math.min(bucket, numBuckets - 1));
      return new L.DivIcon({
        html: '<div style=\"background-color:' + colors[bucket] + ';\"><span style=\"color: black; font-size: 14px;\">' + countFormatted + '</span></div>',
        className: 'marker-cluster',
        iconSize: new L.Point(50, 50)
      });
    }"

    if(nrow(data) == 1) {
      leafletProxy("tree_map", data = data) |>
        clearMarkers() |>
        clearMarkerClusters() |>
        addMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          layerId = ~TREE_ID,
          clusterOptions = markerClusterOptions(
            disableClusteringAtZoom = 18,
            iconCreateFunction = JS(icon_create_string)
          )
        ) |>
        setView(lng = data$LONGITUDE[[1]], lat = data$LATITUDE[[1]], zoom = 15)
    } else {
      leafletProxy("tree_map", data = data) |>
        clearMarkers() |>
        clearMarkerClusters() |>
        addMarkers(
          lng = ~LONGITUDE,
          lat = ~LATITUDE,
          layerId = ~TREE_ID,
          clusterOptions = markerClusterOptions(
            disableClusteringAtZoom = 18,
            iconCreateFunction = JS(icon_create_string)
          )
        ) |>
        fitBounds(lng1 = minLng, lat1 = minLat, lng2 = maxLng, lat2 = maxLat)
    }
  } else {
    leafletProxy("tree_map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      setView(lng = -123.1216, lat = 49.2827, zoom = 12)
  }
})
  
  observeEvent(input$tree_map_marker_click, {
    event <- input$tree_map_marker_click
    if (!is.null(event$id)) {
      show_tree_popup(event$id, save_view = TRUE)
    }
  })

  observeEvent(input$reset_map, {
    selected_tree(NULL)
    later::later(function() {
      session$sendCustomMessage("restorePrevMapView", list())
    }, delay = 0.2)
  })

  observeEvent(input$reset_zoom, {
    data <- filtered_data()
    if(nrow(data) > 0) {
      minLng <- min(data$LONGITUDE)
      maxLng <- max(data$LONGITUDE)
      minLat <- min(data$LATITUDE)
      maxLat <- max(data$LATITUDE)
      
      leafletProxy("tree_map", data = data) |>
        fitBounds(lng1 = minLng, lat1 = minLat, lng2 = maxLng, lat2 = maxLat)
    } else {
      leafletProxy("tree_map") |>
        setView(lng = -123.1216, lat = 49.2827, zoom = 12)
    }
  })

}

# Only enable autoreload if on Derek's computer
if (grepl("^/Users/derek", getwd())) {
  options(shiny.autoreload = TRUE)
  app_options <- list(port = 3838)
} else {
  app_options <- list()
}

shinyApp(ui, server, options = app_options)