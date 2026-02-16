mod_species_table_ui <- function(id) {
  ns <- NS(id)
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
              textOutput(ns("species_count_text"))
            ),
            div(
              style = "flex: 0 0 auto; text-align: right;",
              actionButton(ns("reset_species"), "Clear Selection", class = "btn btn-info btn-xs")
            )
          )
        )
      ),
      DTOutput(ns("tree_table"))
  )
}

mod_species_table_server <- function(id, filtered_data, selected_species, selected_tree) {
  moduleServer(id, function(input, output, session) {
    output$tree_table <- DT::renderDT({
      common_name_trucation_chars <- 45
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
                  lengthMenu = list(c(10, 25, 50, 100),
                                    c("10", "25", "50", "100")),
                  autoWidth = TRUE,
                  searchHighlight = TRUE,
                  scrollY = "340px"
                ))
    }, server = FALSE)

    # Handle species selection from table clicks
    observeEvent(input$tree_table_rows_selected, {
      selected_row <- input$tree_table_rows_selected
      displayed_data <- filtered_data() |>
        group_by(Binomial_Name) |>
        summarise(
          `Common Names` = paste(unique(COMMON_NAME), collapse = ", "),
          Count = sum(n())
        ) |>
        arrange(desc(Count))

      if (!is.null(selected_row) && length(selected_row) > 0 && selected_row <= nrow(displayed_data)) {
        species <- displayed_data$Binomial_Name[selected_row]
        selected_species(species)
        selected_tree(NULL)
      }
    })

    # Reset species selection
    observeEvent(input$reset_species, {
      selected_species(NULL)
      proxy <- dataTableProxy("tree_table")
      selectRows(proxy, integer(0))
    })

    # Species count text
    output$species_count_text <- renderText({
      num_species <- filtered_data() |>
        distinct(Binomial_Name) |>
        nrow()
      paste("Unique Species:", format(num_species, big.mark = ","))
    })
  })
}
