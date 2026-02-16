mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  div(class = "panel panel-default",
      style = "background-color: #ffffff; padding: 12px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
      h3("Tree Height by Neighbourhood", style = "margin-top: 1px; margin-bottom: 1px;"),
      plotlyOutput(ns("heatmap"), height = "502px")
  )
}

mod_heatmap_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    output$heatmap <- renderPlotly({
      data <- filtered_data()

      heatmap_data <- data |>
        count(NEIGHBOURHOOD_NAME, HEIGHT_RANGE)

      plot <- ggplot(heatmap_data, aes(x = HEIGHT_RANGE, y = NEIGHBOURHOOD_NAME, fill = n,
                                       text = paste0("<b>Neighbourhood</b>: ", NEIGHBOURHOOD_NAME, "<br>",
                                                     "<b>Height Range</b>: ", HEIGHT_RANGE, "<br>",
                                                     "<b>Tree Count</b>: ", format(n, big.mark = ",")))) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(x = "Height Range", y = "Neighbourhood", fill = "Tree Count") +
        theme_minimal() +
        scale_y_discrete(limits = sort(unique(data$NEIGHBOURHOOD_NAME), decreasing = TRUE))

      ggplotly(plot, tooltip = "text")
    })
  })
}
