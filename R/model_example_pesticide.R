#' Run example pesticide risk model (DEMONSTRATION ONLY)
#'
#' @param data Data frame with columns: species, concentration, toxicity_factor
#' @param params Named list of additional parameters (unused in this simple example)
#' @param config Optional list for configuration (e.g. debug = TRUE)
#' @details Calculates a simplified risk score based on concentration and a toxicity factor. This model demonstrates framework capabilities with fictitious calculations and should not be used for real assessments.
#'
#' @return A tibble with original columns plus risk_score
#' @export
model_example_pesticide_run <- function(
  data,
  params = list(),
  config = list()
) {
  schema <- model_example_pesticide_schema()
  validate_input(data, schema)

  data$risk_score <- data$concentration * data$toxicity_factor

  tibble::as_tibble(data)
}

#' Metadata for example pesticide risk model
#'
#' @return Named list describing the model
#' @export
model_example_pesticide_metadata <- function() {
  list(
    id = "example_pesticide",
    version = "0.0.1",
    title = "Example Pesticide Risk Model (DEMONSTRATION ONLY)",
    status = "draft", # draft | under_review | approved | deprecated
    owners = c("Example SME"),
    risk_domain = "aquatic",
    ecosystems = c("freshwater"),
    inputs = c("species", "concentration", "toxicity_factor"),
    outputs = c("species", "concentration", "toxicity_factor", "risk_score"),
    doc_path = "model_docs/example/example_pesticide.qmd",
    references = c("Smith et al. 2025 J Env Sci (placeholder)"),
    notes = "Template model to illustrate the standard interface."
  )
}

#' Input schema for example pesticide model
#'
#' @return Tibble describing required input fields
#' @export
model_example_pesticide_schema <- function() {
  tibble::tibble(
    field = c("species", "concentration", "toxicity_factor"),
    type = c("character", "numeric", "numeric"),
    units = c("-", "mg/L", "dimensionless"),
    mandatory = c(TRUE, TRUE, TRUE),
    description = c(
      "Species identifier",
      "Pesticide concentration in water",
      "Toxicity factor derived from toxicity data"
    )
  )
}
