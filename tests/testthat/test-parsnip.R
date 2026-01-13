# Test tidymodels integration for rtabpfn

library(parsnip)

test_that("tab_pfn() creates a valid model spec", {
  skip_on_cran()

  spec <- tab_pfn(mode = "classification")

  expect_s3_class(spec, "tab_pfn")
  expect_equal(spec$mode, "classification")
  expect_equal(spec$engine, "tabpfn")
})

test_that("tab_pfn() works with regression mode", {
  skip_on_cran()

  spec <- tab_pfn(mode = "regression") %>%
    set_engine("tabpfn")

  expect_s3_class(spec, "tab_pfn")
  expect_equal(spec$mode, "regression")
  expect_equal(spec$engine, "tabpfn")
})

test_that("tab_pfn() can update parameters", {
  skip_on_cran()

  spec <- tab_pfn(mode = "regression")
  updated_spec <- spec %>%
    update(N_ensemble_configurations = 16)

  expect_s3_class(updated_spec, "tab_pfn")
  expect_equal(rlang::eval_tidy(updated_spec$args$N_ensemble_configurations), 16)
})

test_that("tab_pfn() can fit a classification model", {
  skip_on_cran()

  skip_if_not(check_shap_available())

  spec <- tab_pfn(mode = "classification") %>%
    set_engine("tabpfn")

  fit <- spec %>%
    fit(Species ~ Sepal.Length + Sepal.Width, data = iris)

  expect_s3_class(fit, "tab_pfn_fit")
  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$model, "tab_pfn")
})

test_that("tab_pfn() can make predictions", {
  skip_on_cran()

  skip_if_not(check_shap_available())

  spec <- tab_pfn(mode = "classification") %>%
    set_engine("tabpfn")

  fit <- spec %>%
    fit(Species ~ Sepal.Length + Sepal.Width, data = iris)

  preds_class <- predict(fit, iris[1:10, ], type = "class")
  expect_s3_class(preds_class, "tbl_df")
  expect_equal(colnames(preds_class), ".pred_class")
  expect_equal(nrow(preds_class), 10)

  preds_prob <- predict(fit, iris[1:10, ], type = "prob")
  expect_s3_class(preds_prob, "tbl_df")
  expect_true(all(startsWith(colnames(preds_prob), ".pred_")))
})

test_that("tab_pfn() can fit a regression model", {
  skip_on_cran()

  skip_if_not(check_shap_available())

  spec <- tab_pfn(mode = "regression") %>%
    set_engine("tabpfn")

  fit <- spec %>%
    fit(mpg ~ cyl + disp, data = mtcars)

  expect_s3_class(fit, "tab_pfn_fit")
  expect_s3_class(fit$model, "tab_pfn")
  expect_equal(fit$model$mode, "regression")
})

test_that("tab_pfn() regression predictions work", {
  skip_on_cran()

  skip_if_not(check_shap_available())

  spec <- tab_pfn(mode = "regression") %>%
    set_engine("tabpfn")

  fit <- spec %>%
    fit(mpg ~ cyl + disp, data = mtcars)

  preds <- predict(fit, mtcars[1:10, ], type = "numeric")
  expect_s3_class(preds, "tbl_df")
  expect_equal(colnames(preds), ".pred")
  expect_equal(nrow(preds), 10)
})
