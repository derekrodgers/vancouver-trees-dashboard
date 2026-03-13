mod_map_ui <- function(id) {
  ns <- NS(id)
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
              textOutput(ns("tree_count_text"))
            ),
            div(
              style = "flex: 0 1 auto; text-align: right; margin-left: auto; margin-bottom: 5px;",
              actionButton(ns("reset_zoom"), "Reset Zoom", class = "btn btn-info btn-xs")
            )
          )
        )
      ),
      div(leafletOutput(ns("tree_map"), height = "520px"))
  )
}

mod_map_server <- function(id, street_trees, filtered_data, selected_tree, selected_species, parent_session, freeze_map_bounds) {
  moduleServer(id, function(input, output, session) {

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
        parent_session$sendCustomMessage("saveCurrentMapView", list())
      }

      parent_session$sendCustomMessage("openPopupAfterZoom", list(id = tree_id, content = content))
    }

    fit_map_to_data <- function(data) {
      if (nrow(data) > 0) {
        leafletProxy("tree_map", data = data) |>
          fitBounds(
            lng1 = min(data$LONGITUDE, na.rm = TRUE),
            lat1 = min(data$LATITUDE, na.rm = TRUE),
            lng2 = max(data$LONGITUDE, na.rm = TRUE),
            lat2 = max(data$LATITUDE, na.rm = TRUE)
          )
      } else {
        leafletProxy("tree_map") |>
          setView(lng = -123.1216, lat = 49.2827, zoom = 12)
      }
    }

    # Render initial map
    output$tree_map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        setView(lng = -123.1216, lat = 49.2827, zoom = 12) |>
        htmlwidgets::onRender("
      function(el, x) {
        window.treeMap = this;
        this.addControl(new L.Control.Fullscreen());
      }
    ")
    })

    # Street view update
    observe({
      req(selected_tree())
      tree_info <- street_trees |>
        filter(TREE_ID == selected_tree()) |>
        slice(1)
      parent_session$sendCustomMessage("updateStreetView", list(lat = tree_info$LATITUDE, lon = tree_info$LONGITUDE))
    })

    # Street view clear
    observe({
      if (is.null(selected_tree())) {
        parent_session$sendCustomMessage("clearStreetView", list())
      }
    })

    # Tree count text
    output$tree_count_text <- renderText({
      num_trees <- filtered_data() |>
        nrow()
      paste("Total Trees:", format(num_trees, big.mark = ","))
    })

    # Main map update observer
    observe({

      data <- filtered_data()

      if (nrow(data) > 0) {
        minLng <- min(data$LONGITUDE, na.rm = TRUE)
        maxLng <- max(data$LONGITUDE, na.rm = TRUE)
        minLat <- min(data$LATITUDE, na.rm = TRUE)
        maxLat <- max(data$LATITUDE, na.rm = TRUE)

        icon_create_string <- "function(cluster) {
          var maxCount = 45000;
          var numBuckets = 8;
          var colors = [
            '#90EE90', '#4cb04c', '#FFFF00', '#FFD700',
            '#FFA500', '#FF8C00', '#FF4500', '#FF0000'
          ];
          var count = cluster.getChildCount();
          var countFormatted = (count < 1000) ? count : (Math.round(count / 1000)) + 'k';
          var bucket = Math.floor(Math.pow(count / maxCount, 0.5) * numBuckets);
          bucket = Math.max(0, Math.min(bucket, numBuckets - 1));
          return new L.DivIcon({
            html: '<div style=\"background-color:' + colors[bucket] + ';\"><span style=\"color: black; font-size: 14px;\">' + countFormatted + '</span></div>',
            className: 'marker-cluster',
            iconSize: new L.Point(50, 50)
          });
        }"

        add_tree_markers <- function(proxy, data, cluster_options = NULL) {
          proxy |>
            addMarkers(
              lng = ~LONGITUDE,
              lat = ~LATITUDE,
              layerId = ~TREE_ID,
              clusterOptions = cluster_options
            )
        }

        if (nrow(data) == 1) {
          leafletProxy("tree_map", data = data) |>
            clearMarkers() |>
            clearMarkerClusters()
          add_tree_markers(
            leafletProxy("tree_map", data = data),
            data,
            cluster_options = markerClusterOptions(
              disableClusteringAtZoom = 18,
              iconCreateFunction = JS(icon_create_string)
            )
          ) |>
            setView(lng = data$LONGITUDE, lat = data$LATITUDE, zoom = 15)
        } else {
          proxy <- leafletProxy("tree_map", data = data) |>
            clearMarkers() |>
            clearMarkerClusters()
            
          proxy <- add_tree_markers(
            proxy,
            data,
            cluster_options = markerClusterOptions(
              disableClusteringAtZoom = 18,
              iconCreateFunction = JS(icon_create_string)
            )
          )

          should_freeze <- isolate(freeze_map_bounds())
          if (!should_freeze) {
             proxy |> fitBounds(lng1 = minLng, lat1 = minLat, lng2 = maxLng, lat2 = maxLat)
          } else {
             freeze_map_bounds(FALSE) # Reset after one use
             parent_session$sendCustomMessage("restorePrevMapView", list())
          }
        }
      } else {
        leafletProxy("tree_map") |>
          clearMarkers() |>
          clearMarkerClusters() |>
          setView(lng = -123.1216, lat = 49.2827, zoom = 12)
      }
    })

    # Marker click handler
    observeEvent(input$tree_map_marker_click, {
      event <- input$tree_map_marker_click
      if (!is.null(event$id)) {
        show_tree_popup(event$id, save_view = TRUE)
      }
    })

    reset_map_view <- function() {
      freeze_map_bounds(TRUE)
      selected_tree(NULL)
    }

    # Popup closed event (from JS via parent session)
    observeEvent(parent_session$input$popup_closed, {
      reset_map_view()
    })

    # Reset Zoom button
    observeEvent(input$reset_zoom, {
      fit_map_to_data(filtered_data())
    })
  })
}
