library(rtabpfn)

# Create sample data
set.seed(42)
X <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = rnorm(100)
)
y <- 2 * X$x1 + 3 * X$x2 + rnorm(100, sd = 0.5)

# Train model
cat("Training TabPFN regression model...\n")
model <- tab_pfn_regression(X, y)

# Test 1: Basic quantile predictions
cat("\nTest 1: Basic quantile predictions...\n")
preds_q1 <- predict(model, X[1:5, ], type = "quantiles",
                    quantiles = c(0.1, 0.5, 0.9))
print(preds_q1)

# Test 2: Decimal quantile predictions (the original error case)
cat("\nTest 2: Decimal quantile predictions (c(0.025, 0.25, 0.5, 0.75, 0.975))...\n")
preds_q2 <- predict(model, X[1:5, ], type = "quantiles",
                    quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))
print(preds_q2)

cat("\nSuccess! The quantile format error has been fixed.\n")
