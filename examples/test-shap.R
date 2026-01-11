library(rtabpfn)
library(ggplot2)

# Create sample data
set.seed(42)
X <- data.frame(
  cyl = sample(c(4, 6, 8), 100, replace = TRUE),
  disp = runif(100, 100, 500),
  hp = runif(100, 50, 350),
  wt = runif(100, 1.5, 5.5)
)
y <- 20 - 0.5 * X$cyl - 0.01 * X$disp - 0.05 * X$hp + X$wt + rnorm(100, sd = 1)

# Train model
cat("Training TabPFN regression model...\n")
model <- tab_pfn_regression(X, y)

# Calculate SHAP values
cat("\nCalculating SHAP values...\n")
shap_vals <- shap_values(model, X[1:5, ], verbose = TRUE)

# Print SHAP values
cat("\nSHAP values (first 5 observations):\n")
print(head(shap_vals))

# Plot SHAP summary
cat("\nCreating SHAP summary plot...\n")
p_summary <- plot_shap_summary(shap_vals, top_n = 4)
print(p_summary)

# Plot SHAP dependence for a feature
cat("\nCreating SHAP dependence plot for 'hp'...\n")
p_dep <- plot_shap_dependence(shap_vals, X[1:5, ], feature = "hp")
print(p_dep)

# Explain individual prediction
cat("\nExplaining individual prediction...\n")
explanation <- explain_prediction(model, X[1, , drop = FALSE])
print(explanation)

cat("\nSHAP extension successfully integrated!\n")

