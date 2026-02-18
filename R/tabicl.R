#' Predict method for TabICL models
#'
#' @param object A fitted TabICL model object
#' @param new_data A data frame of new predictors
#' @param type Type of prediction. For regression: "numeric" (default), "raw".
#'   For classification: "class", "prob", or "raw"
#' @param output_type Python TabICL output type. Options: "mean" (default), "full"
#' @param ... Additional arguments passed to the Python predict method
#'
#' @return A tibble with predictions
#' @export
predict.tab_icl <- function(object,
                            new_data,
                            type = NULL,
                            output_type = "mean",
                            ...) {

  rtabpfn:::ensure_python_env()

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
  valid_reg_types <- c("numeric", "raw")
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

  # Get predictions based on type
  if (type == "raw") {
    # Return raw Python object
    return(object$fit$predict(new_data, ...))
  }

  # Regression predictions
  if (object$mode == "regression") {
    preds <- object$fit$predict(new_data, ...)

    # Convert Python object to R
    preds_r <- reticulate::py_to_r(preds)

    # Handle different output types
    if (is.matrix(preds_r)) {
      pred_df <- as.data.frame(preds_r)
      colnames(pred_df) <- paste0(".pred_sample_", seq_len(ncol(pred_df)))
      return(tibble::as_tibble(pred_df))
    } else {
      # Mean prediction
      preds_vec <- as.numeric(preds_r)
      return(tibble::tibble(.pred = preds_vec))
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


# Helper function to resolve device for TabICL
resolve_tabicl_device <- function(device) {
  if (device != "auto") {
    return(device)
  }
  
  # Check if CUDA is available
  tryCatch({
    torch <- reticulate::import("torch", convert = FALSE)
    if (torch$cuda$is_available()) {
      return("cuda")
    } else {
      return("cpu")
    }
  }, error = function(e) {
    # If torch not available, default to cpu
    return("cpu")
  })
}


#' Train a TabICL regression model
#'
#' @param X Predictor data frame or matrix
#' @param y Response vector
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param n_estimators Number of ensemble members (default: 8)
#' @param batch_size Batch size for ensemble processing (default: 8)
#' @param random_state Random seed for reproducibility
#' @param ... Additional arguments passed to TabICLRegressor
#'
#' @return A tab_icl model object with mode = "regression"
#' @export
tab_icl_regression <- function(X, y, device = "auto", n_estimators = 8,
                                batch_size = 8, random_state = 42, ...) {

  rtabpfn:::ensure_python_env()
  
  # Resolve device (TabICL doesn't support "auto")
  resolved_device <- resolve_tabicl_device(device)

  # Load Python module
  tabicl <- reticulate::import("tabicl", convert = FALSE)

  # Create regressor
  reg <- tabicl$TabICLRegressor(
    device = resolved_device,
    n_estimators = as.integer(n_estimators),
    batch_size = as.integer(batch_size),
    random_state = as.integer(random_state),
    ...
  )

  # Fit model
  reg$fit(X, y)

  # Create model object
  model <- list(
    fit = reg,
    mode = "regression",
    predictor_names = colnames(X),
    outcome_name = if (is.data.frame(y)) colnames(y)[1] else NULL,
    device = device,
    n_estimators = n_estimators
  )

  class(model) <- c("tab_icl", "model_fit")
  return(model)
}


#' Train a TabICL classification model
#'
#' @param X Predictor data frame or matrix
#' @param y Response vector (factor or character)
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param n_estimators Number of ensemble members (default: 8)
#' @param batch_size Batch size for ensemble processing (default: 8)
#' @param random_state Random seed for reproducibility
#' @param ... Additional arguments passed to TabICLClassifier
#'
#' @return A tab_icl model object with mode = "classification"
#' @export
tab_icl_classification <- function(X, y, device = "auto", n_estimators = 8,
                                    batch_size = 8, random_state = 42, ...) {

  rtabpfn:::ensure_python_env()
  
  # Resolve device (TabICL doesn't support "auto")
  resolved_device <- resolve_tabicl_device(device)

  # Load Python module
  tabicl <- reticulate::import("tabicl", convert = FALSE)

  # Create classifier
  clf <- tabicl$TabICLClassifier(
    device = resolved_device,
    n_estimators = as.integer(n_estimators),
    batch_size = as.integer(batch_size),
    random_state = as.integer(random_state),
    ...
  )

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
    device = device,
    n_estimators = n_estimators
  )

  class(model) <- c("tab_icl", "model_fit")
  return(model)
}


#' Print method for TabICL models
#'
#' @param x A tab_icl model object or specification
#' @param ... Additional arguments (not used)
#' @export
print.tab_icl <- function(x, ...) {
  # Check if this is a model specification or a fitted model
  if ("model_spec" %in% class(x)) {
    # Print for model specification (from parsnip)
    cat("TabICL Model Specification (", x$mode, ")\n\n", sep = "")
    cat("Main Arguments:\n")
    cat("  n_estimators = ", rlang::eval_tidy(x$args$n_estimators), "\n", sep = "")
    cat("  batch_size = ", rlang::eval_tidy(x$args$batch_size), "\n", sep = "")
    cat("  device = '", rlang::eval_tidy(x$args$device), "'\n", sep = "")
    cat("  random_state = ", rlang::eval_tidy(x$args$random_state), "\n\n", sep = "")
    cat("Computational engine: ", x$engine, "\n", sep = "")
  } else {
    # Print for fitted model
    cat("TabICL", tools::toTitleCase(x$mode), "Model\n\n")

    if (!is.null(x$predictor_names)) {
      cat("Predictors:", length(x$predictor_names), "\n")
    }

    if (x$mode == "classification" && !is.null(x$levels)) {
      cat("Classes:", length(x$levels), "\n")
      cat(" ", paste(x$levels, collapse = ", "), "\n")
    }

    if (!is.null(x$n_estimators)) {
      cat("Ensemble size:", x$n_estimators, "\n")
    }

    cat("Device:", x$device, "\n")
  }

  invisible(x)
}


#' Check TabICL Installation
#'
#' @description
#' Helper function to check if TabICL is installed in the Python environment
#' and optionally install it if missing.
#'
#' @param install Logical. If TRUE, will attempt to install TabICL if not found.
#' @param envname Name of the virtual environment to use
#' @param method Installation method: "auto", "virtualenv", or "conda"
#'
#' @return Logical indicating if TabICL is available
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if TabICL is available
#' check_tabicl()
#'
#' # Install if not available
#' check_tabicl(install = TRUE)
#' }
check_tabicl <- function(install = FALSE,
                         envname = "tabpfn",
                         method = "auto") {

  rtabpfn:::ensure_python_env()

  has_tabicl <- reticulate::py_module_available("tabicl")

  if (!has_tabicl && install) {
    message("TabICL not found. Installing...")

    # Create virtual environment if it doesn't exist
    if (!envname %in% reticulate::virtualenv_list()) {
      reticulate::virtualenv_create(envname, python = NULL)
    }

    # Use the virtual environment
    reticulate::use_virtualenv(envname, required = FALSE)

    # Install TabICL
    reticulate::py_install("tabicl", envname = envname, method = method, pip = TRUE)

    has_tabicl <- reticulate::py_module_available("tabicl")

    if (has_tabicl) {
      message("TabICL installed successfully!")
    } else {
      warning("Failed to install TabICL. Please install manually.")
    }
  }

  invisible(has_tabicl)
}
