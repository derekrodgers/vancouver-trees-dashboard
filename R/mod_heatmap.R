mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  div(class = "panel panel-default",
      style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
      h3("Tree Height by Neighbourhood", style = "margin-top: 1px; margin-bottom: 1px;"),
      plotlyOutput(ns("heatmap"), height = "502px")
  )
}

mod_heatmap_server <- function(id, filtered_data, set_filters) {
  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderPlotly({
      data <- filtered_data()

      # Build count matrix for native plotly heatmap
      heatmap_data <- data |>
        count(NEIGHBOURHOOD_NAME, HEIGHT_RANGE)

      # Ordered axes
      height_levels <- levels(data$HEIGHT_RANGE)
      height_levels <- height_levels[height_levels %in% heatmap_data$HEIGHT_RANGE]
      neighbourhood_levels <- sort(unique(heatmap_data$NEIGHBOURHOOD_NAME), decreasing = TRUE)

      # Pivot to matrix using tapply (rows = neighbourhoods, cols = height ranges)
      count_matrix <- tapply(
        heatmap_data$n,
        list(heatmap_data$NEIGHBOURHOOD_NAME, heatmap_data$HEIGHT_RANGE),
        FUN = sum,
        default = 0
      )
      count_matrix <- count_matrix[neighbourhood_levels, height_levels, drop = FALSE]

      plot_ly(
        x = height_levels,
        y = neighbourhood_levels,
        z = count_matrix,
        type = "heatmap",
        source = "heatmap",
        colorscale = list(c(0, 1), c("white", "blue")),
        hovertemplate = paste0(
          "<b>Neighbourhood</b>: %{y}<br>",
          "<b>Height Range</b>: %{x}<br>",
          "<b>Tree Count</b>: %{z:,}<extra></extra>"
        )
      ) |>
        layout(
          xaxis = list(title = "Height Range"),
          yaxis = list(title = "Neighbourhood"),
          dragmode = FALSE
        ) |>
        event_register("plotly_click") |>
        htmlwidgets::onRender("
          function(el) {
            function setPointer() {
              var drag = el.querySelector('.nsewdrag');
              if (drag) drag.style.cursor = 'pointer';
            }
            setPointer();
            el.on('plotly_afterplot', setPointer);
          }
        ")
    })

    # When a heatmap cell is clicked, apply filters
    observeEvent(event_data("plotly_click", source = "heatmap"), {
      click <- event_data("plotly_click", source = "heatmap")
      if (!is.null(click)) {
        set_filters(neighbourhood = click$y, height_range = click$x)
      }
    })
  })
}
