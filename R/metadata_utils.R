#' Discover model IDs from exported metadata functions
#'
#' Looks for exported functions named `model_<id>_metadata`.
#'
#' @return Character vector of model IDs
#' @export
discover_model_ids <- function() {
  ns <- asNamespace("nzera")
  exports <- getNamespaceExports(ns)
  meta_fns <- grep("^model_.*_metadata$", exports, value = TRUE)
  ids <- sub("^model_(.*)_metadata$", "\\1", meta_fns)
  sort(unique(ids))
}

#' Get metadata for a specific model
#'
#' @param model_id Model identifier (e.g. "example_pesticide")
#'
#' @return Named list (metadata)
#' @export
get_model_metadata <- function(model_id) {
  fn_name <- paste0("model_", model_id, "_metadata")
  ns <- asNamespace("nzera")

  if (!exists(fn_name, envir = ns, mode = "function")) {
    stop(
      "No metadata function found for model_id '",
      model_id,
      "'. Expected: ",
      fn_name
    )
  }

  fn <- get(fn_name, envir = ns)
  meta <- fn()
  if (is.null(meta$id)) {
    meta$id <- model_id
  }
  meta
}

#' Get schema for a specific model
#'
#' @param model_id Model identifier
#'
#' @return Tibble describing input schema
#' @export
get_model_schema <- function(model_id) {
  fn_name <- paste0("model_", model_id, "_schema")
  ns <- asNamespace("nzera")

  if (!exists(fn_name, envir = ns, mode = "function")) {
    stop(
      "No schema function found for model_id '",
      model_id,
      "'. Expected: ",
      fn_name
    )
  }

  fn <- get(fn_name, envir = ns)
  fn()
}

#' Get metadata for all models
#'
#' @return Tibble with one row per model
#' @export
get_all_model_metadata <- function() {
  ids <- discover_model_ids()
  meta_list <- lapply(ids, get_model_metadata)

  tibble::tibble(
    id = vapply(meta_list, `[[`, character(1), "id"),
    title = vapply(
      meta_list,
      function(x) x$title %||% NA_character_,
      character(1)
    ),
    version = vapply(
      meta_list,
      function(x) x$version %||% NA_character_,
      character(1)
    ),
    status = vapply(meta_list, function(x) x$status %||% "draft", character(1)),
    owners = vapply(
      meta_list,
      function(x) paste(x$owners, collapse = "; "),
      character(1)
    ),
    doc_path = vapply(
      meta_list,
      function(x) x$doc_path %||% NA_character_,
      character(1)
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
