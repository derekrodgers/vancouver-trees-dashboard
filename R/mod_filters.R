mod_filters_ui <- function(id, street_trees) {
  ns <- NS(id)
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
                  "Vancouver Trees Dashboard",
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
            column(2, pickerInput(ns("neighbourhood"), "Neighbourhood",
                                  choices = sort(unique(street_trees$NEIGHBOURHOOD_NAME)),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                  width = "100%")),
            column(2, pickerInput(ns("binomial_name"), "Binomial Name",
                                  choices = sort(unique(street_trees$Binomial_Name)),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                  width = "100%")),
            column(2, pickerInput(ns("common_name"), "Common Names",
                                  choices = sort(unique(street_trees$COMMON_NAME)),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                  width = "100%")),
            column(2, pickerInput(ns("height_range"), "Height Range",
                                  choices = levels(street_trees$HEIGHT_RANGE),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                  width = "100%")),
            column(2, pickerInput(ns("interesting_trees"), "\u2B50 Interesting Trees \u2B50",
                                  choices = c("\U0001F338 Cherry & Plum Trees", "\U0001F334 Palm Trees", "\U0001F333 All Park Trees", "\U0001F3DE\uFE0F Stanley Park Trees", "\U0001F337 VanDusen Botanical Garden"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                  width = "100%")),
            column(2, div(style = "text-align: right; margin-top: 25px;",
                          actionButton(ns("reset_filters"), "Reset Filters",
                                      class = "btn-danger",
                                      style = "font-weight: bold; font-size: 14.5px; padding: 8px 14px;")))
          )
      )
    )
  )
}

mod_filters_server <- function(id, street_trees, selected_species, selected_tree) {
  moduleServer(id, function(input, output, session) {

    apply_interesting_tree_filters <- function(data) {
      if (!is.null(input$interesting_trees)) {
        if ("\U0001F338 Cherry & Plum Trees" %in% input$interesting_trees) {
          data <- data |> filter(grepl("cherry|plum", COMMON_NAME, ignore.case = TRUE))
        }
        if ("\U0001F334 Palm Trees" %in% input$interesting_trees) {
          data <- data |> filter(grepl("palm", COMMON_NAME, ignore.case = TRUE))
        }
        if ("\U0001F337 VanDusen Botanical Garden" %in% input$interesting_trees) {
          data <- data |> filter(vandusen_botanical_gardens)
        }
        if ("\U0001F333 All Park Trees" %in% input$interesting_trees) {
          data <- data |> filter(PARK_TREE)
        }
        if ("\U0001F3DE\uFE0F Stanley Park Trees" %in% input$interesting_trees) {
          data <- data |> filter(stanley_park)
        }
      }
      return(data)
    }

    # Cascading filters
    available_neighbourhoods <- reactive({
      data <- street_trees
      data <- apply_interesting_tree_filters(data)
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

    available_height_range <- reactive({
      data <- street_trees
      data <- apply_interesting_tree_filters(data)
      if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0) {
        data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
      }
      if (!is.null(input$binomial_name) && length(input$binomial_name) > 0) {
        data <- data |> filter(Binomial_Name %in% input$binomial_name)
      }
      if (!is.null(input$common_name) && length(input$common_name) > 0) {
        data <- data |> filter(COMMON_NAME %in% input$common_name)
      }
      hr_levels <- levels(street_trees$HEIGHT_RANGE)
      hr_levels[hr_levels %in% data$HEIGHT_RANGE]
    })

    observe({
      updatePickerInput(
        session,
        "height_range",
        choices = available_height_range(),
        selected = intersect(input$height_range, available_height_range())
      )
    })

    available_binomial_name <- reactive({
      data <- street_trees
      data <- apply_interesting_tree_filters(data)
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

    observe({
      updatePickerInput(session, "binomial_name",
                        choices = available_binomial_name(),
                        selected = intersect(input$binomial_name, available_binomial_name()))
    })

    available_common_name <- reactive({
      data <- street_trees
      data <- apply_interesting_tree_filters(data)
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

    observe({
      updatePickerInput(session, "common_name",
                        choices = available_common_name(),
                        selected = intersect(input$common_name, available_common_name()))
    })

    # When species is selected/deselected from table, update pickers
    observeEvent(selected_species(), {
      if (is.null(selected_species())) {
        updatePickerInput(session, "binomial_name", selected = character(0))
        updatePickerInput(session, "common_name", selected = character(0))
      } else {
        updatePickerInput(session, "binomial_name", selected = selected_species())
      }
    }, ignoreNULL = FALSE)

    # Reset all filters
    observeEvent(input$reset_filters, {
      updatePickerInput(session, "neighbourhood", selected = character(0))
      updatePickerInput(session, "height_range", selected = character(0))
      updatePickerInput(session, "binomial_name", selected = character(0))
      updatePickerInput(session, "common_name", selected = character(0))
      updatePickerInput(session, "interesting_trees", selected = character(0))

      selected_species(NULL)
      selected_tree(NULL)
    })

    # Base filtered data (before species/tree selection)
    base_filtered_data <- reactive({
      data <- street_trees
      data <- apply_interesting_tree_filters(data)

      if (!is.null(input$neighbourhood) && length(input$neighbourhood) > 0)
        data <- data |> filter(NEIGHBOURHOOD_NAME %in% input$neighbourhood)
      if (!is.null(input$height_range) && length(input$height_range) > 0)
        data <- data |> filter(HEIGHT_RANGE %in% input$height_range)
      if (!is.null(input$binomial_name) && length(input$binomial_name) > 0)
        data <- data |> filter(Binomial_Name %in% input$binomial_name)
      if (!is.null(input$common_name) && length(input$common_name) > 0)
        data <- data |> filter(COMMON_NAME %in% input$common_name)

      return(data)
    })

    # Clear species-related pickers (called by species table reset button)
    reset_species_pickers <- function() {
      updatePickerInput(session, "binomial_name", selected = character(0))
      updatePickerInput(session, "common_name", selected = character(0))
    }

    # Set neighbourhood + height_range filters from heatmap click
    set_heatmap_filter <- function(neighbourhood, height_range) {
      updatePickerInput(session, "neighbourhood", selected = neighbourhood)
      updatePickerInput(session, "height_range", selected = height_range)
    }

    # Clear neighbourhood + height_range filters (called by heatmap reset button)
    reset_heatmap_filters <- function() {
      updatePickerInput(session, "neighbourhood", selected = character(0))
      updatePickerInput(session, "height_range", selected = character(0))
    }

    # Return values for use by the orchestrator
    list(
      base_filtered_data = base_filtered_data,
      reset_species_pickers = reset_species_pickers,
      set_heatmap_filter = set_heatmap_filter,
      reset_heatmap_filters = reset_heatmap_filters
    )
  })
}
