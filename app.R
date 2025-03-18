library(shiny)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(plotly)

# To run locally, start an R console in the repo root and run:
#     shiny::runApp("app.R")
# To deploy:
#     rsconnect::deployApp(appDir = ".", appName = "vancouver-trees-dashboard")
# Deploy location:
#     https://derekrodgers.shinyapps.io/vancouver-trees-dashboard/

street_trees <- read_csv2("data/raw/street-trees.csv")

# Concatenate Genus + Species for Binomial Name
street_trees <- street_trees |>
  mutate(
    Binomial_Name = paste(GENUS_NAME, SPECIES_NAME),
    HEIGHT_RANGE = factor(HEIGHT_RANGE, levels = c(
      "0' - 10'", "10' - 20'", "20' - 30'", "30' - 40'", 
      "40' - 50'", "50' - 60'", "60' - 70'", "70' - 80'", 
      "80' - 90'", "90' - 100'", "> 100'"
    ), ordered = TRUE)
  )

ui <- fluidPage(
  title = "Vancouver Street Trees Dashboard",
  # Filters row
  fluidRow(
    column(12, 
           div(class = "panel panel-default", 
               style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-top: 10px;",
               h2("Vancouver Street Trees Dashboard", style = "margin-top: 0px; margin-bottom: 10px;"),
               
               # flexbox to arrange elements in one row
               div(style = "display: flex; align-items: center; justify-content: space-between; gap: 10px;",
                   pickerInput("neighbourhood", "Neighbourhood",
                               choices = unique(street_trees$NEIGHBOURHOOD_NAME),
                               multiple = TRUE,
                               options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                   pickerInput("height_range", "Height Range",
                               choices = levels(street_trees$HEIGHT_RANGE),
                               multiple = TRUE,
                               options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                   pickerInput("binomial_name", "Binomial Name",
                               choices = unique(street_trees$Binomial_Name),
                               multiple = TRUE,
                               options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                   pickerInput("common_name", "Common Name",
                               choices = unique(street_trees$COMMON_NAME),
                               multiple = TRUE,
                               options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                   actionButton("reset_filters", "Reset Filters", class = "btn btn-info btn-sm", 
                                style = "white-space: nowrap; padding: 6px 12px;")
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
               plotlyOutput("height_distribution", height = "500px")
           )
    ),
    column(7, 
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 10px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Height by Neighbourhood", style = "margin-top: 1px; margin-bottom: 1px;"),
               plotlyOutput("heatmap", height = "500px")
           )
    )
  ),

  # Second chart row
  fluidRow(
    column(5,  
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("Tree Count by Species", style = "margin-top: 5px; margin-bottom: 10px;"),  
               fluidRow(
                 column(12, div(style = "display: flex; align-items: center;",
                                actionButton("reset_species", "Reset Selection", class = "btn btn-info btn-sm"),
                                span(style = "padding-left: 15px; font-size: 14px;", textOutput("species_count_text"))
                 ))
               ),
               br(),  
               DTOutput("tree_table")
           )
    ),
    column(7,  
           div(class = "panel panel-default", 
               style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.1);",
               h3("All Street Trees", style = "margin-top: 1px; margin-bottom: 10px;"),  
               fluidRow(
                 column(12, div(style = "display: flex; align-items: center;",
                                actionButton("reset_tree", "Reset Selection", class = "btn btn-info btn-sm"),
                                span(style = "padding-left: 15px; font-size: 14px;", textOutput("tree_count_text"))
                 ))
               ),
               br(),  
               DTOutput("all_trees_table")
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

  # Dynamic filter updates
  observe({
    data <- filtered_data()

    # Preserve all neighbourhood choices, but disable unavailable ones
    updatePickerInput(session, "neighbourhood", 
                      choices = unique(street_trees$NEIGHBOURHOOD_NAME),  # Show all neighbourhoods
                      selected = input$neighbourhood,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        `selected-text-format` = "count > 1",
                        `max-options` = length(unique(street_trees$NEIGHBOURHOOD_NAME)),  # Ensures all are selectable
                        `virtual-scroll` = 10
                      ))

    updatePickerInput(session, "height_range", 
                      choices = levels(street_trees$HEIGHT_RANGE), 
                      selected = input$height_range)

    updatePickerInput(session, "binomial_name", 
                      choices = unique(street_trees$Binomial_Name), 
                      selected = input$binomial_name)

    updatePickerInput(session, "common_name", 
                      choices = unique(street_trees$COMMON_NAME), 
                      selected = input$common_name)
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
        `Common Names` = ifelse(nchar(`Common Names`) > 75, 
                                paste0(substr(`Common Names`, 1, 75), "..."), 
                                `Common Names`),
        Count = format(Count, big.mark = ",")
      ) |>
      arrange(desc(Count))
  
    datatable(data |> select(Count, `Binomial_Link`, `Common Names`),  
          escape = FALSE,
          colnames = c("Count", "Binomial Name", "Common Names"),
          options = list(
            pageLength = 100,
            lengthMenu = list(c(10, 25, 50, 100, 250, 500, 750), 
                              c("10", "25", "50", "100", "250", "500", "750")),
            autoWidth = TRUE,
            searchHighlight = TRUE,
            scrollY = "517px"
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
  
    if (!is.null(selected_row) && length(selected_row) > 0) {
      species <- displayed_data$Binomial_Name[selected_row]  # Extract species name
      selected_species(species)
      selected_tree(NULL)  # Clear tree selection if species is chosen
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
  
    datatable(data |> select(TREE_ID, `Binomial_Link`, COMMON_NAME, NEIGHBOURHOOD_NAME, HEIGHT_RANGE, `Google Maps Link`),  
              escape = FALSE,
              colnames = c("Tree ID", "Binomial Name", "Common Name", "Neighbourhood", "Height Range", "Google Maps Link"),
              options = list(
                pageLength = 100,
                lengthMenu = list(c(10, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000), 
                                  c("10", "50", "100", "250", "500", "1K", "2.5K", "5K", "10K", "25K")),
                autoWidth = TRUE,
                searchHighlight = TRUE,
                scrollY = "500px"
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
}

shinyApp(ui, server)