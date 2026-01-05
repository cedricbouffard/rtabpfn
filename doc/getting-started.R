## ----setup, eval=FALSE--------------------------------------------------------
#  library(rtabpfn)
#  setup_tabpfn()

## ----regression, eval=FALSE---------------------------------------------------
#  # Load data
#  data(mtcars)
#  X <- mtcars[, c("cyl", "disp", "hp", "wt")]
#  y <- mtcars$mpg
#  
#  # Train model
#  model <- tab_pfn_regression(X, y)
#  
#  # Get quantile predictions
#  preds <- predict(model, X,
#                   type = "quantiles",
#                   quantiles = c(0.1, 0.5, 0.9))
#  
#  head(preds)

## ----visualization, eval=FALSE------------------------------------------------
#  library(ggplot2)
#  
#  # Combine predictions with actual values
#  df <- data.frame(
#    actual = y,
#    median = preds$.pred_q050,
#    lower = preds$.pred_q010,
#    upper = preds$.pred_q090
#  )
#  
#  # Plot
#  ggplot(df, aes(x = actual, y = median)) +
#    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#    geom_point() +
#    labs(title = "Predictions with 80% Uncertainty Intervals",
#         x = "Actual", y = "Predicted")

