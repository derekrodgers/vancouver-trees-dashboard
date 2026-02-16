get_app_options <- function() {
  if (grepl("^/Users/derek", getwd())) {
    options(shiny.autoreload = TRUE)
    list(port = 3838)
  } else {
    list(
      host = "0.0.0.0",
      port = as.numeric(Sys.getenv("PORT", 8080))
    )
  }
}
