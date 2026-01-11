# Unsupervised Anomaly Detection Examples

library(rtabpfn)
library(dplyr)

# Verify unsupervised extension is available
if (!check_unsupervised_available()) {
  cat("Please install tabpfn-extensions[unsupervised] first:\n")
  cat("  setup_tabpfn(install_unsupervised = TRUE)\n")
  stop("Unsupervised extension not available")
}

cat("=== Unsupervised Anomaly Detection Demo ===\n\n")

# Example 1: Detect anomalies in mtcars dataset
cat("Example 1: Anomaly detection on mtcars dataset\n")
cat("==============================================\n")

X <- mtcars[, c("cyl", "disp", "hp", "wt")]

cat("Data shape:", nrow(X), "x", ncol(X), "\n\n")

# Train unsupervised model
cat("Training unsupervised model...\n")
model <- tab_pfn_unsupervised(X, n_estimators = 4)

cat("\nModel summary:\n")
print(model)

# Calculate anomaly scores
cat("\nCalculating anomaly scores...\n")
scores <- anomaly_scores(model, X, n_permutations = 10)

cat("\nAnomaly scores (first 10 observations):\n")
print(head(scores, 10))

# Identify most anomalous observations
cat("\nMost anomalous observations (lowest scores):\n")
most_anomalous <- scores |>
  arrange(anomaly_score) |>
  head(5)
print(most_anomalous)

# Show full data for anomalous observations
cat("\nAnomalous observations (full data):\n")
anomalous_data <- mtcars[most_anomalous$observation, ]
print(anomalous_data)

# Example 2: Binary classification using threshold
cat("\n\nExample 2: Binary anomaly classification using threshold\n")
cat("========================================================\n")

# Determine threshold using percentile
threshold <- quantile(scores$anomaly_score, 0.1)
cat("Using threshold at 10th percentile:", round(threshold, 4), "\n")

predictions <- predict(model, X, threshold = threshold, n_permutations = 10)

cat("\nPredictions:\n")
print(head(predictions))

# Count anomalies
cat("\nNumber of anomalies detected:", sum(predictions$is_anomaly), "/", nrow(predictions), "\n")

# Example 3: Anomaly detection on a synthetic dataset
cat("\n\nExample 3: Anomaly detection on synthetic data\n")
cat("===============================================\n")

set.seed(42)
n_obs <- 100
n_features <- 4

# Generate normal data
X_normal <- matrix(rnorm(n_obs * n_features, mean = 0, sd = 1), nrow = n_obs, ncol = n_features)

# Add some anomalies
X_anomalies <- matrix(rnorm(10 * n_features, mean = 5, sd = 1), nrow = 10, ncol = n_features)
X_synthetic <- rbind(X_normal, X_anomalies)
X_synthetic <- as.data.frame(X_synthetic)
colnames(X_synthetic) <- paste0("feature_", 1:n_features)

cat("Synthetic data shape:", nrow(X_synthetic), "x", ncol(X_synthetic), "\n")

# Train model
cat("Training model on synthetic data...\n")
model_synthetic <- tab_pfn_unsupervised(X_synthetic, n_estimators = 4)

# Calculate scores
scores_synthetic <- anomaly_scores(model_synthetic, X_synthetic, n_permutations = 10)

# Check if anomalies are detected
cat("\nScores for last 10 observations (injected anomalies):\n")
print(tail(scores_synthetic, 10))

cat("\nScores for first 10 observations (normal data):\n")
print(head(scores_synthetic, 10))

cat("\n=== Demo Complete ===\n")
