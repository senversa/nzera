#' Validate input data against a schema
#'
#' @param data Data frame to validate
#' @param schema Tibble with columns: field, type, units, mandatory
#' @param strict Logical, if TRUE then error on unexpected columns
#'
#' @return Invisibly returns TRUE if valid; otherwise throws an error
#' @export
validate_input <- function(data, schema, strict = FALSE, na_check = TRUE) {
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

    val <- data[[fld]]
    if (na_check && schema$mandatory[i] && any(is.na(val))) {
      stop("Field '", fld, "' has NA values but is mandatory")
    }

    ok <- switch(
      schema$type[i],
      numeric = is.numeric(val),
      character = is.character(val) || is.factor(val),
      integer = is.integer(val),
      TRUE
    )
    if (!ok) {
      stop(
        "Field '",
        fld,
        "' expected type '",
        schema$type[i],
        "' got: ",
        paste(class(val), collapse = ", ")
      )
    }
  }
  invisible(TRUE)
}
