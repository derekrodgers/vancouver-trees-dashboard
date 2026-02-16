load_street_trees <- function(path = "data/processed/street-trees.fst") {
  data <- fst::read_fst(path)
  data |>
    dplyr::mutate(
      TREE_ID = as.numeric(TREE_ID),
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE)
    )
}

load_google_api_key <- function() {
  key <- Sys.getenv("GOOGLE_API_KEY")
  if (key == "") {
    key <- trimws(readLines("google_api_key.txt", warn = FALSE))
  }
  key
}
