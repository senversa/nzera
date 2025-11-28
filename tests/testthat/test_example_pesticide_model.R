test_that("example pesticide model runs and returns risk_score", {
  data <- data.frame(
    species = c("Fish", "Crab"),
    concentration = c(2.5, 1.1),
    toxicity_factor = c(0.8, 1.3)
  )

  res <- model_example_pesticide_run(data)
  expect_true("risk_score" %in% names(res))
  expect_equal(res$risk_score[1], 2.5 * 0.8)
})


test_that("validate_input catches missing/extra/NA", {
  schema <- tibble::tibble(
    field = c("a", "b"),
    type = c("numeric", "character"),
    units = c("", ""),
    mandatory = c(TRUE, TRUE),
    description = c("", "")
  )
  expect_error(
    validate_input(data.frame(a = 1:2), schema, strict = TRUE),
    "Missing required columns: b"
  )
  expect_error(
    validate_input(
      data.frame(a = 1:2, b = NA_character_),
      schema,
      na_check = TRUE
    ),
    "NA values"
  )
  expect_error(
    validate_input(data.frame(a = 1:2, b = "x", c = 3), schema, strict = TRUE),
    "Unexpected columns"
  )
})


test_that("list_models returns known example with valid doc_path", {
  meta <- list_models()
  expect_true("example_pesticide" %in% meta$id)
  row <- meta[meta$id == "example_pesticide", ]
  expect_false(is.na(row$doc_path))
})

test_that("new_model_skeleton writes files in expected locations", {
  tmp <- withr::local_tempdir()
  old <- getwd()
  setwd(tmp)
  on.exit({
    unlink(tmp, recursive = TRUE, force = TRUE)
    setwd(old)
  })
  file.copy(old, tmp, recursive = TRUE) # or create minimal structure
  setwd(file.path(tmp, basename(old)))

  res <- new_model_skeleton("foo", title = "Foo Model")
  expect_true(file.exists(file.path("model_docs", "foo", "foo.qmd")))
  expect_true(file.exists(file.path("R", "model_foo.R")))
  expect_true(file.exists(file.path("tests", "testthat", "test_foo.R"))) # after you adjust the path
})


test_that("API run returns 400 on bad input", {
  skip_on_cran()

  port <- httpuv::randomPort()
  srv <- callr::r_bg(
    function(port) {
      pr <- plumber::pr("app/api_plumber.R")
      pr$run(host = "127.0.0.1", port = port, swagger = FALSE, docs = FALSE)
    },
    args = list(port),
    supervise = TRUE
  )
  withr::defer(srv$kill(), envir = parent.frame())

  # wait for server to come up
  httr::RETRY(
    "GET",
    sprintf("http://127.0.0.1:%s/health", port),
    times = 10,
    pause_base = 0.1,
    quiet = TRUE
  )

  bad <- httr::POST(
    sprintf("http://127.0.0.1:%s/models/example_pesticide/run", port),
    body = list(input_data = list(list(a = 1))),
    encode = "json"
  )
  expect_equal(httr::status_code(bad), 400)
})
