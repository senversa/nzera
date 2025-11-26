test_that("example pesticide model runs and returns risk_score", {
  data <- data.frame(
    species        = c("Fish", "Crab"),
    concentration  = c(2.5, 1.1),
    toxicity_factor= c(0.8, 1.3)
  )

  res <- model_example_pesticide_run(data)
  expect_true("risk_score" %in% names(res))
  expect_equal(res$risk_score[1], 2.5 * 0.8)
})
