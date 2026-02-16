app_server <- function(street_trees) {
  function(input, output, session) {
    # Shared reactive values
    selected_species <- reactiveVal(NULL)
    selected_tree <- reactiveVal(NULL)

    # Filters module — returns base_filtered_data reactive
    filters <- mod_filters_server("filters", street_trees, selected_species, selected_tree)

    # Combine base filtered data with species/tree selection
    filtered_data <- reactive({
      data <- filters$base_filtered_data()

      if (!is.null(selected_species()))
        data <- data |> filter(Binomial_Name == selected_species())

      if (!is.null(selected_tree()))
        data <- data |> filter(TREE_ID == selected_tree())

      return(data)
    })

    # Map module (includes Street View, tree count, popups, map controls)
    mod_map_server("map", street_trees, filtered_data, selected_tree, selected_species, session)

    # Enable/disable Clear Selected Tree button based on tree selection
    observe({
      if (is.null(selected_tree())) {
        shinyjs::disable("clear_selected_tree")
      } else {
        shinyjs::enable("clear_selected_tree")
      }
    })

    # Clear Selected Tree button — clears selected tree and restores previous map view
    observeEvent(input$clear_selected_tree, {
      selected_tree(NULL)
      later::later(function() {
        session$sendCustomMessage("restorePrevMapView", list())
      }, delay = 2.8)
    })

    # Heatmap module
    mod_heatmap_server("heatmap", filtered_data)

    # Species table module
    mod_species_table_server("species", filtered_data, selected_species, selected_tree, filters$reset_species_pickers)
  }
}
