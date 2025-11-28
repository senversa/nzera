#' Write a CSV input template for a given model
#'
#' @param model_id Model identifier
#' @param path File path to write to (defaults into data/input_templates/)
#' @export
write_input_template <- function(model_id, path = NULL) {
  schema <- get_model_schema(model_id)

  if (is.null(path)) {
    path <- file.path(
      tempdir(),
      "nzera/input_templates",
      paste0(model_id, "_input_template.csv")
    )
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  }

  # Only write header row (blank example)
  header <- as.list(rep(NA_character_, nrow(schema)))
  names(header) <- schema$field
  df <- as.data.frame(header, stringsAsFactors = FALSE)

  readr::write_csv(df, path, na = "")

  invisible(path)
}
