#' List available models
#'
#' @return Tibble with id, title, version, status, owners
#' @export
list_models <- function() {
  get_all_model_metadata()
}

#' Run a model by ID
#'
#' @param model_id Model identifier
#' @param data Input data frame
#' @param params Named list of model parameters
#' @param config Optional config list
#'
#' @return Model output tibble
#' @export
run_model <- function(model_id, data, params = list(), config = list()) {
  fn_name <- paste0("model_", model_id, "_run")
  ns <- asNamespace("nzera")

  if (!exists(fn_name, envir = ns, mode = "function")) {
    stop(
      "No run function found for model_id '",
      model_id,
      "'. Expected: ",
      fn_name
    )
  }

  fn <- get(fn_name, envir = ns)
  fn(data = data, params = params, config = config)
}
