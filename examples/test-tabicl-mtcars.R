# Test TabICL avec mtcars
library(rtabpfn)

# Préparer les données
data(mtcars)

# Séparer en train/test
set.seed(42)
train_idx <- sample(1:nrow(mtcars), 20)
test_idx <- setdiff(1:nrow(mtcars), train_idx)

X_train <- mtcars[train_idx, c("disp", "hp", "wt")]
y_train <- mtcars[train_idx, "mpg"]
X_test <- mtcars[test_idx, c("disp", "hp", "wt")]
y_test <- mtcars[test_idx, "mpg"]

cat("=== Entraînement du modèle TabICL ===\n")
model <- tab_icl_regression(
  X = X_train,
  y = y_train,
  n_estimators = 4,
  device = "auto",
  verbose = FALSE
)

cat("\n=== Prédictions ponctuelles ===\n")
preds_point <- predict(model, X_test, type = "numeric")
print(head(preds_point))

# Comparer avec les vraies valeurs
results <- data.frame(
  actual = y_test,
  predicted = preds_point$.pred
)
cat("\nComparaison (premières lignes):\n")
print(head(results))

# Calculer l'erreur MAE
mae <- mean(abs(results$actual - results$predicted))
cat(sprintf("\nMAE: %.2f\n", mae))

cat("\n=== Prédictions quantiles (un seul: 0.1) ===\n")
preds_q1 <- predict(model, X_test, type = "quantiles", quantiles = 0.1)
print(head(preds_q1))
cat(sprintf("Dimensions: %d lignes x %d colonnes\n", nrow(preds_q1), ncol(preds_q1)))

cat("\n=== Prédictions quantiles (3 quantiles: 0.1, 0.5, 0.9) ===\n")
preds_q3 <- predict(model, X_test, type = "quantiles", quantiles = c(0.1, 0.5, 0.9))
print(head(preds_q3))
cat(sprintf("Dimensions: %d lignes x %d colonnes\n", nrow(preds_q3), ncol(preds_q3)))

cat("\n=== Prédictions quantiles (5 quantiles: 0.1, 0.25, 0.5, 0.75, 0.9) ===\n")
preds_q5 <- predict(model, X_test, type = "quantiles", quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
print(head(preds_q5))
cat(sprintf("Dimensions: %d lignes x %d colonnes\n", nrow(preds_q5), ncol(preds_q5)))

cat("\n=== Intervalles de prédiction (95%) ===\n")
preds_interval <- predict(model, X_test, type = "conf_int", level = 0.95)
print(head(preds_interval))
cat(sprintf("Dimensions: %d lignes x %d colonnes\n", nrow(preds_interval), ncol(preds_interval)))

cat("\n=== Test terminé avec succès! ===\n")
