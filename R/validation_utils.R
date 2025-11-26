#' Validate input data against a schema
#'
#' @param data Data frame to validate
#' @param schema Tibble with columns: field, type, units, mandatory
#' @param strict Logical, if TRUE then error on unexpected columns
#'
#' @return Invisibly returns TRUE if valid; otherwise throws an error
#' @export
validate_input <- function(data, schema, strict = FALSE) {
  # Check required columns present
  missing <- schema$field[schema$mandatory & !(schema$field %in% names(data))]
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Optionally warn/error on unexpected columns
  unexpected <- setdiff(names(data), schema$field)
  if (strict && length(unexpected) > 0) {
    stop("Unexpected columns present: ", paste(unexpected, collapse = ", "))
  }

  # Type checks (soft, to avoid being too rigid)
  for (i in seq_len(nrow(schema))) {
    fld <- schema$field[i]
    if (!fld %in% names(data)) {
      next
    }

    expected_type <- schema$type[i]
    actual <- data[[fld]]

    ok <- switch(
      expected_type,
      "numeric" = is.numeric(actual),
      "character" = is.character(actual) || is.factor(actual),
      "integer" = is.integer(actual),
      TRUE
    )

    if (!isTRUE(ok)) {
      warning(
        "Field '",
        fld,
        "' does not match expected type '",
        expected_type,
        "'. Class is: ",
        paste(class(actual), collapse = ", ")
      )
    }
  }

  invisible(TRUE)
}
