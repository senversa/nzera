#' Write a CSV input template for a given model
#'
#' @param model_id Model identifier
#' @param path File path to write to (defaults into data/input_templates/)
#' @export
write_input_template <- function(model_id, path = NULL) {
  schema <- get_model_schema(model_id)

  if (is.null(path)) {
    dir.create("data/input_templates", showWarnings = FALSE, recursive = TRUE)
    path <- file.path("data/input_templates", paste0(model_id, "_input_template.csv"))
  }

  # Only write header row (blank example)
  header <- as.list(rep(NA_character_, nrow(schema)))
  names(header) <- schema$field
  df <- as.data.frame(header, stringsAsFactors = FALSE)

  readr::write_csv(df, path, na = "")

  invisible(path)
}
