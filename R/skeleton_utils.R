#' Create skeleton files for a new model
#'
#' This will:
#' - Create model_docs/<model_id>/<model_id>.qmd from the template
#' - Create R/models/model_<model_id>.R with run/metadata/schema stubs
#' - Create tests/test_<model_id>.R with a minimal test skeleton
#'
#' @param model_id Model identifier, e.g. "pesticide_rice_paddies"
#' @param title Human-readable model title
#' @export
new_model_skeleton <- function(model_id, title = NULL) {
  if (is.null(title)) {
    title <- model_id
  }

  # 1. Quarto doc
  dir.create(file.path("model_docs", model_id), showWarnings = FALSE, recursive = TRUE)
  template_path <- file.path("model_docs", "template", "model_doc_template.qmd")
  target_doc <- file.path("model_docs", model_id, paste0(model_id, ".qmd"))

  if (file.exists(template_path)) {
    txt <- readLines(template_path)
    # simple substitution of placeholder tokens
    txt <- gsub("MODEL_ID_PLACEHOLDER", model_id, txt, fixed = TRUE)
    txt <- gsub("MODEL_TITLE_PLACEHOLDER", title, txt, fixed = TRUE)
    writeLines(txt, target_doc)
  } else {
    warning("Template Quarto doc not found at ", template_path,
            ". Creating a very basic stub instead.")
    writeLines(c(
      "---",
      paste0("title: \"", title, " - Development & Documentation\""),
      "format: html",
      "---",
      "",
      "# 1. Overview",
      "",
      "Model ID: `", model_id, "`"
    ), target_doc)
  }

  # 2. R model file
  model_file <- file.path("R", "models", paste0("model_", model_id, ".R"))
  if (!file.exists(model_file)) {
    dir.create(dirname(model_file), showWarnings = FALSE, recursive = TRUE)
    writeLines(c(
      sprintf("# %s model --------------------------------------------------", model_id),
      "",
      "#' Run model",
      "#'",
      "#' @param data Input data frame",
      "#' @param params Named list of parameters",
      "#' @param config Optional config list",
      "#' @return Output tibble",
      "#' @export",
      sprintf("model_%s_run <- function(data, params = list(), config = list()) {", model_id),
      "  schema <- model_${ID}_schema()",
      "  validate_input(data, schema)",
      "",
      "  # TODO: implement model logic here",
      "  tibble::as_tibble(data)",
      "}",
      "",
      "#' Model metadata",
      "#'",
      "#' @return Named list of metadata",
      "#' @export",
      sprintf("model_%s_metadata <- function() {", model_id),
      "  list(",
      sprintf("    id        = \"%s\",", model_id),
      "    version   = \"0.0.1\",",
      sprintf("    title     = \"%s\",", title),
      "    status    = \"draft\",",
      "    owners    = character(),",
      "    risk_domain = NA_character_,",
      "    ecosystems  = character(),",
      "    inputs    = character(),",
      "    outputs   = character(),",
      sprintf("    doc_path  = \"model_docs/%s/%s.qmd\",", model_id, model_id),
      "    references = character(),",
      "    notes      = \"\"",
      "  )",
      "}",
      "",
      "#' Model input schema",
      "#'",
      "#' @return Tibble describing expected input columns",
      "#' @export",
      sprintf("model_%s_schema <- function() {", model_id),
      "  tibble::tibble(",
      "    field       = character(),",
      "    type        = character(),",
      "    units       = character(),",
      "    mandatory   = logical(),",
      "    description = character()",
      "  )",
      "}"
    ), model_file)
  } else {
    warning("Model file already exists: ", model_file)
  }

  # 3. Test file
  dir.create("tests", showWarnings = FALSE, recursive = TRUE)
  test_file <- file.path("tests", paste0("test_", model_id, ".R"))
  if (!file.exists(test_file)) {
    writeLines(c(
      sprintf("test_that(\"%s model basic run works\", {", model_id),
      "  # TODO: define a minimal example input",
      "  # data <- data.frame(...)",
      "",
      sprintf("  # res <- model_%s_run(data)", model_id),
      "  # expect_s3_class(res, \"tbl_df\")",
      "})"
    ), test_file)
  } else {
    warning("Test file already exists: ", test_file)
  }

  invisible(list(
    doc = target_doc,
    model_file = model_file,
    test_file = test_file
  ))
}
