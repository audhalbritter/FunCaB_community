library(testthat)
library(glmmTMB)

# Create a small synthetic dataset
df <- data.frame(
  response = rnorm(40),
  predictor = rep(letters[1:2], each = 20),
  precipitation_scaled = rnorm(40),
  temperature_scaled = rnorm(40),
  siteID = rep(1:4, each = 10),
  blockID = rep(1:8, each = 5)
)
df$predictor <- as.factor(df$predictor)

# Note: Convergence warnings are expected with random data and are not a failure of the function itself.
test_that("compare_full_vs_2way_glmmTMB returns expected output structure", {
  result <- compare_full_vs_2way_glmmTMB(
    data = df,
    response = "response",
    predictor = "predictor",
    random_effect = "siteID/blockID"
  )
  expect_type(result, "list")
  expect_true(all(c("model_2way", "model_full", "lrt") %in% names(result)))
  expect_s3_class(result$model_2way, "glmmTMB")
  expect_s3_class(result$model_full, "glmmTMB")
  expect_s3_class(result$lrt, "anova")
})
