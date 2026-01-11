#' Predict method for TabPFN models
#'
#' @param object A fitted TabPFN model object
#' @param new_data A data frame of new predictors
#' @param type Type of prediction. For regression: "numeric" (default), "quantiles",
#'   "conf_int", or "raw". For classification: "class", "prob", or "raw"
#' @param output_type Python TabPFN output type. Options: "mean" (default), "quantiles",
#'   "full", "mode"
#' @param quantiles Numeric vector of quantiles to predict (used when output_type = "quantiles")
#' @param level Confidence level for prediction intervals (used when type = "conf_int")
#' @param ... Additional arguments passed to the Python predict method
#'
#' @return A tibble with predictions
#' @export
predict.tab_pfn <- function(object,
                            new_data,
                            type = NULL,
                            output_type = "mean",
                            quantiles = c(0.1, 0.5, 0.9),
                            level = 0.95,
                            ...) {

  # Load required packages
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  # Determine default type based on model mode
  if (is.null(type)) {
    type <- if (object$mode == "regression") "numeric" else "class"
  }

  # Validate type
  valid_reg_types <- c("numeric", "quantiles", "conf_int", "raw")
  valid_cls_types <- c("class", "prob", "raw")

  if (object$mode == "regression" && !type %in% valid_reg_types) {
    stop("For regression, type must be one of: ", paste(valid_reg_types, collapse = ", "))
  }
  if (object$mode == "classification" && !type %in% valid_cls_types) {
    stop("For classification, type must be one of: ", paste(valid_cls_types, collapse = ", "))
  }

  # Prepare new_data
  new_data <- as.data.frame(new_data)

  # Remove outcome variable if present
  if (!is.null(object$outcome_name) && object$outcome_name %in% names(new_data)) {
    new_data <- new_data[, names(new_data) != object$outcome_name, drop = FALSE]
  }

  # Reorder columns to match training data
  if (!is.null(object$predictor_names)) {
    new_data <- new_data[, object$predictor_names, drop = FALSE]
  }

  # Get predictions based on type and output_type
  if (type == "raw") {
    # Return raw Python object
    return(object$fit$predict(new_data, ...))
  }

  # Regression predictions
  if (object$mode == "regression") {
    if (type == "quantiles" || output_type == "quantiles") {
      # Quantile predictions
      preds <- object$fit$predict(
        new_data,
        output_type = "quantiles",
        quantiles = quantiles,
        ...
      )

      # Convert Python object to R matrix/array
      # TabPFN returns a list of arrays (one per quantile)
      preds_r <- reticulate::py_to_r(preds)

      # Handle different return formats
      if (is.list(preds_r) && !is.data.frame(preds_r)) {
        # If it's a list of arrays, combine into matrix
        pred_matrix <- do.call(cbind, lapply(preds_r, as.numeric))
      } else if (is.matrix(preds_r)) {
        pred_matrix <- preds_r
      } else {
        # Single array case
        pred_matrix <- as.matrix(preds_r)
      }

      # Ensure correct dimensions (rows = observations, cols = quantiles)
      if (ncol(pred_matrix) != length(quantiles)) {
        pred_matrix <- t(pred_matrix)
      }

      # Convert to data frame with named columns
      pred_df <- as.data.frame(pred_matrix)
      colnames(pred_df) <- paste0(".pred_q", sprintf("%03d", floor(quantiles * 100)))
      return(tibble::as_tibble(pred_df))

    } else if (type == "conf_int") {
      # Prediction intervals using quantiles
      lower_q <- (1 - level) / 2
      upper_q <- 1 - lower_q

      preds <- object$fit$predict(
        new_data,
        output_type = "quantiles",
        quantiles = c(lower_q, upper_q),
        ...
      )

      # Convert Python object to R
      preds_r <- reticulate::py_to_r(preds)

      # Handle different return formats
      if (is.list(preds_r) && !is.data.frame(preds_r)) {
        pred_matrix <- do.call(cbind, lapply(preds_r, as.numeric))
      } else if (is.matrix(preds_r)) {
        pred_matrix <- preds_r
      } else {
        pred_matrix <- as.matrix(preds_r)
      }

      # Ensure correct dimensions
      if (ncol(pred_matrix) != 2) {
        pred_matrix <- t(pred_matrix)
      }

      pred_df <- data.frame(
        .pred_lower = pred_matrix[, 1],
        .pred_upper = pred_matrix[, 2]
      )
      return(tibble::as_tibble(pred_df))

    } else {
      # Point predictions (mean)
      preds <- object$fit$predict(
        new_data,
        output_type = output_type,
        ...
      )

      # Convert Python object to R
      preds_r <- reticulate::py_to_r(preds)

      # Handle different output types
      if (output_type == "full") {
        # Full distribution - return all samples
        if (is.matrix(preds_r)) {
          pred_df <- as.data.frame(preds_r)
        } else {
          pred_df <- as.data.frame(matrix(preds_r, nrow = length(preds_r), ncol = 1))
        }
        colnames(pred_df) <- paste0(".pred_sample_", seq_len(ncol(pred_df)))
        return(tibble::as_tibble(pred_df))
      } else {
        # Mean or mode prediction
        preds_vec <- if (is.matrix(preds_r)) {
          as.numeric(preds_r[, 1])
        } else {
          as.numeric(preds_r)
        }
        return(tibble::tibble(.pred = preds_vec))
      }
    }
  }

  # Classification predictions
  if (object$mode == "classification") {
    if (type == "prob") {
      # Class probabilities
      probs <- object$fit$predict_proba(new_data, ...)

      # Convert Python object to R
      probs_r <- reticulate::py_to_r(probs)

      # Ensure it's a matrix
      if (!is.matrix(probs_r)) {
        probs_r <- as.matrix(probs_r)
      }

      # Convert to tibble with proper column names
      prob_df <- as.data.frame(probs_r)
      colnames(prob_df) <- paste0(".pred_", object$levels)
      return(tibble::as_tibble(prob_df))

    } else {
      # Class predictions
      preds <- object$fit$predict(new_data, ...)

      # Convert Python object to R
      preds_r <- reticulate::py_to_r(preds)

      # Convert to factor with proper levels
      pred_factor <- factor(as.character(preds_r), levels = object$levels)
      return(tibble::tibble(.pred_class = pred_factor))
    }
  }
}


#' Train a TabPFN regression model with enhanced predict options
#'
#' @param X Predictor data frame or matrix
#' @param y Response vector
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param test_size Proportion of data to use for internal validation
#' @param ... Additional arguments passed to TabPFNRegressor
#'
#' @return A tab_pfn model object with mode = "regression"
#' @export
tab_pfn_regression <- function(X, y, device = "auto", test_size = 0.33, ...) {

  # Load Python module
  tabpfn <- reticulate::import("tabpfn", convert = FALSE)

  # Create regressor
  reg <- tabpfn$TabPFNRegressor(device = device, ...)

  # Fit model
  reg$fit(X, y)

  # Create model object
  model <- list(
    fit = reg,
    mode = "regression",
    predictor_names = colnames(X),
    outcome_name = if (is.data.frame(y)) colnames(y)[1] else NULL,
    test_size = test_size,
    device = device
  )

  class(model) <- c("tab_pfn", "model_fit")
  return(model)
}


#' Train a TabPFN classification model
#'
#' @param X Predictor data frame or matrix
#' @param y Response vector (factor or character)
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param test_size Proportion of data to use for internal validation
#' @param ... Additional arguments passed to TabPFNClassifier
#'
#' @return A tab_pfn model object with mode = "classification"
#' @export
tab_pfn_classification <- function(X, y, device = "auto", test_size = 0.33, ...) {

  # Load Python module
  tabpfn <- reticulate::import("tabpfn", convert = FALSE)

  # Create classifier
  clf <- tabpfn$TabPFNClassifier(device = device, ...)

  # Get levels
  if (is.factor(y)) {
    levels_vec <- levels(y)
  } else {
    levels_vec <- unique(as.character(y))
  }

  # Fit model
  clf$fit(X, y)

  # Create model object
  model <- list(
    fit = clf,
    mode = "classification",
    levels = levels_vec,
    predictor_names = colnames(X),
    outcome_name = if (is.data.frame(y)) colnames(y)[1] else NULL,
    test_size = test_size,
    device = device
  )

  class(model) <- c("tab_pfn", "model_fit")
  return(model)
}


#' Print method for TabPFN models
#'
#' @param x A tab_pfn model object
#' @param ... Additional arguments (not used)
#' @export
print.tab_pfn <- function(x, ...) {
  cat("TabPFN", tools::toTitleCase(x$mode), "Model\n\n")

  if (!is.null(x$predictor_names)) {
    cat("Predictors:", length(x$predictor_names), "\n")
  }

  if (x$mode == "classification" && !is.null(x$levels)) {
    cat("Classes:", length(x$levels), "\n")
    cat(" ", paste(x$levels, collapse = ", "), "\n")
  }

  cat("Device:", x$device, "\n")

  invisible(x)
}
