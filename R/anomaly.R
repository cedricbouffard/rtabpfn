#' Unsupervised Anomaly Detection for TabPFN
#'
#' Functions to perform unsupervised anomaly detection using the TabPFN
#' unsupervised extension from tabpfn_extensions.
#'
#' @name anomaly
NULL

#' Check if Unsupervised Extension is available
#'
#' @description
#' Checks if tabpfn-extensions unsupervised module is installed and available.
#'
#' @return Logical indicating if unsupervised extension is available
#' @export
#'
#' @examples
#' \dontrun{
#' check_unsupervised_available()
#' }
check_unsupervised_available <- function() {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  has_ext <- reticulate::py_module_available("tabpfn_extensions")
  
  if (!has_ext) {
    return(FALSE)
  }

  tryCatch({
    unsup_module <- reticulate::import("tabpfn_extensions.unsupervised", convert = FALSE)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}


#' Train a TabPFN Unsupervised Anomaly Detection Model
#'
#' @description
#' Creates an unsupervised model for detecting anomalies using TabPFN's
#' joint probability estimation. The model uses both classifier and regressor
#' models to estimate the likelihood of samples under the learned distribution.
#'
#' @param X Predictor data frame or matrix
#' @param n_estimators Number of TabPFN models to use (default: 4)
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param categorical_features Vector of column indices or names to treat as categorical (NULL for auto-detection)
#' @param ... Additional arguments passed to TabPFNClassifier and TabPFNRegressor
#'
#' @return A tab_pfn_unsupervised model object
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#'
#' # Prepare data
#' X <- mtcars[, c("cyl", "disp", "hp", "wt")]
#'
#' # Train unsupervised model
#' model <- tab_pfn_unsupervised(X, n_estimators = 4)
#'
#' # Detect anomalies
#' scores <- anomaly_scores(model, X, n_permutations = 10)
#' }
tab_pfn_unsupervised <- function(X, n_estimators = 4, device = "auto", categorical_features = NULL, ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  if (!check_unsupervised_available()) {
    stop("Package 'tabpfn_extensions[unsupervised]' is required for anomaly detection.\n",
         "To install:\n",
         "1. Ensure Python environment is configured: setup_tabpfn()\n",
         "2. Or install manually: reticulate::py_install('tabpfn-extensions[unsupervised]', pip = TRUE)")
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }

  cat("Creating TabPFN Unsupervised Anomaly Detection Model\n\n")

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  tabpfn <- reticulate::import("tabpfn", convert = FALSE)

  cat("Initializing TabPFNClassifier with", n_estimators, "estimators...\n")
  clf <- suppressWarnings(tabpfn$TabPFNClassifier(device = device, n_estimators = as.integer(n_estimators), ...))

  cat("Initializing TabPFNRegressor with", n_estimators, "estimators...\n")
  reg <- suppressWarnings(tabpfn$TabPFNRegressor(device = device, n_estimators = as.integer(n_estimators), ...))

  cat("Initializing TabPFNUnsupervisedModel...\n")
  unsup_mod <- reticulate::import("tabpfn_extensions.unsupervised", convert = FALSE)
  model_unsupervised <- suppressWarnings(
    unsup_mod$TabPFNUnsupervisedModel(
      tabpfn_clf = clf,
      tabpfn_reg = reg
    )
  )

  # Convert categorical_features names to indices if necessary
  predictor_names <- colnames(X)
  if (!is.null(categorical_features)) {
    if (is.character(categorical_features)) {
      categorical_features <- match(categorical_features, predictor_names)
    }
    categorical_features <- as.integer(categorical_features) - 1L  # Convert to 0-based indices

    # Set categorical features on unsupervised model
    model_unsupervised$set_categorical_features(categorical_features)

    # Also set on TabPFN models if they support it
    tryCatch({
      clf$set_categorical_features(categorical_features)
    }, error = function(e) {
      # Ignore if method not available
    })

    tryCatch({
      reg$set_categorical_features(categorical_features)
    }, error = function(e) {
      # Ignore if method not available
    })
  }

  cat("Fitting unsupervised model...\n")

  np <- reticulate::import("numpy", convert = FALSE)
  X_np <- np$array(as.matrix(X), dtype = np$float32)
  suppressWarnings(model_unsupervised$fit(X_np))

  # Restore original environment variable
  if (old_do_not_track == "") {
    Sys.unsetenv("DO_NOT_TRACK")
  } else {
    Sys.setenv("DO_NOT_TRACK" = old_do_not_track)
  }

  model <- list(
    model_unsupervised = model_unsupervised,
    clf = clf,
    reg = reg,
    mode = "unsupervised",
    predictor_names = predictor_names,
    n_estimators = n_estimators,
    device = device,
    categorical_features = categorical_features
  )

  class(model) <- c("tab_pfn_unsupervised", "model_fit")

  cat("\nUnsupervised model training complete!\n")
  return(model)
}


#' Calculate Anomaly/Outlier Scores
#'
#' @description
#' Computes anomaly scores for observations using the fitted unsupervised model.
#' Lower scores indicate more anomalous observations (lower joint probability).
#'
#' @param object A fitted tab_pfn_unsupervised model object
#' @param new_data A data frame of observations to score
#' @param n_permutations Number of random feature orderings to average over (default: 10)
#' @param verbose Print progress information
#' @param ... Additional arguments passed to outliers method
#'
#' @return A tibble with anomaly scores for each observation
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#'
#' # Train model
#' X <- mtcars[, c("cyl", "disp", "hp", "wt")]
#' model <- tab_pfn_unsupervised(X)
#'
#' # Calculate anomaly scores
#' scores <- anomaly_scores(model, X, n_permutations = 10)
#'
#' # Get most anomalous observations (lowest scores)
#' most_anomalous <- scores |> dplyr::arrange(anomaly_score) |> head(5)
#' }
anomaly_scores <- function(object,
                          new_data,
                          n_permutations = 10,
                          verbose = TRUE,
                          ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }

  if (verbose) {
    cat("Computing anomaly scores...\n")
  }

  new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)

  if (!is.null(object$predictor_names)) {
    new_data <- new_data[, object$predictor_names, drop = FALSE]
  }

  for (col in colnames(new_data)) {
    if (is.factor(new_data[[col]])) {
      new_data[[col]] <- as.integer(as.numeric(new_data[[col]]))
    } else if (is.integer(new_data[[col]])) {
      new_data[[col]] <- as.integer(new_data[[col]])
    } else if (is.numeric(new_data[[col]])) {
      new_data[[col]] <- as.numeric(new_data[[col]])
    }
  }

  if (verbose) {
    cat("  Input data dimensions:", nrow(new_data), "x", ncol(new_data), "\n")
  }

  torch <- tryCatch({
    reticulate::import("torch", convert = FALSE)
  }, error = function(e) {
    stop("Package 'torch' is required. Install it with: pip install torch")
  })

  np <- reticulate::import("numpy", convert = FALSE)
  data_np <- np$array(as.matrix(new_data), dtype = np$float32)
  data_torch <- torch$from_numpy(data_np)

  if (verbose) {
    cat("  Computing with", n_permutations, "permutations...\n")
  }

  # Suppress PostHog analytics warnings
  old_do_not_track <- Sys.getenv("DO_NOT_TRACK")
  Sys.setenv("DO_NOT_TRACK" = "1")

  scores <- suppressWarnings(
    object$model_unsupervised$outliers(
      data_torch,
      n_permutations = as.integer(n_permutations),
      ...
    )
  )

  # Restore original environment variable
  if (old_do_not_track == "") {
    Sys.unsetenv("DO_NOT_TRACK")
  } else {
    Sys.setenv("DO_NOT_TRACK" = old_do_not_track)
  }

  # Convert torch.Tensor to numpy array first, then to R
  scores_np <- scores$numpy()
  scores_r <- reticulate::py_to_r(scores_np)

  if (is.matrix(scores_r) || is.array(scores_r)) {
    scores_vec <- as.numeric(scores_r)
  } else {
    scores_vec <- as.numeric(scores_r)
  }

  result <- tibble::tibble(
    observation = paste0(".obs_", seq_len(nrow(new_data))),
    anomaly_score = scores_vec
  )

  if (verbose) {
    cat("  Done! Score range:", min(scores_vec), "-", max(scores_vec), "\n")
  }

  return(result)
}


#' Predict method for TabPFN Unsupervised models
#'
#' @param object A fitted tab_pfn_unsupervised model object
#' @param new_data A data frame of observations
#' @param n_permutations Number of permutations for anomaly scoring (default: 10)
#' @param threshold Optional threshold for binary classification as anomaly
#' @param ... Additional arguments passed to anomaly_scores
#'
#' @return A tibble with anomaly scores and optional binary predictions
#' @export
predict.tab_pfn_unsupervised <- function(object,
                                           new_data,
                                           n_permutations = 10,
                                           threshold = NULL,
                                           ...) {

  scores_df <- anomaly_scores(
    object = object,
    new_data = new_data,
    n_permutations = n_permutations,
    verbose = FALSE,
    ...
  )

  if (!is.null(threshold)) {
    scores_df$is_anomaly <- scores_df$anomaly_score < threshold
  }

  return(scores_df)
}


#' Print method for TabPFN Unsupervised models
#'
#' @param x A tab_pfn_unsupervised model object
#' @param ... Additional arguments (not used)
#' @export
print.tab_pfn_unsupervised <- function(x, ...) {
  cat("TabPFN Unsupervised Anomaly Detection Model\n\n")
  
  if (!is.null(x$predictor_names)) {
    cat("Features:", length(x$predictor_names), "\n")
  }
  
  cat("Estimators:", x$n_estimators, "\n")
  cat("Device:", x$device, "\n")
  
  invisible(x)
}
