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
  )