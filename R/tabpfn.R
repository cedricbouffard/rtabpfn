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
      # Ensure quantiles is a vector (handle single numeric input)
      quantiles_vec <- as.vector(quantiles)
      # Convert to list for Python
      quantiles_py <- as.list(quantiles_vec)
      
      preds <- object$fit$predict(
        new_data,
        output_type = "quantiles",
        quantiles = quantiles_py,
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
        # Single array case (single quantile)
        pred_matrix <- as.matrix(preds_r)
      }

      # Ensure correct dimensions (rows = observations, cols = quantiles)
      # Handle single quantile case where pred_matrix might be a vector
      if (is.null(dim(pred_matrix))) {
        pred_matrix <- as.matrix(pred_matrix)
      }
      if (ncol(pred_matrix) != length(quantiles_vec)) {
        pred_matrix <- t(pred_matrix)
      }

      # Convert to data frame with named columns
      pred_df <- as.data.frame(pred_matrix)
      # Generate column names using formatted quantile values (e.g., 0.5 -> q050)
      col_names <- paste0(".pred_q", sprintf("%03d", as.integer(round(quantiles_vec * 100))))
      colnames(pred_df) <- col_names
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
        # Cloud client returns the full predictive distribution as a dict.
        if (identical(object$backend, "client")) {
          return(reticulate::py_to_r(preds))
        }
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


# --- TabPFN 3.5 model version & endpoint helpers -----------------------------

# Map an R model_version string to a tabpfn ModelVersion enum member name (OSS).
.model_version_member <- function(model_version) {
  switch(model_version,
    "2" = "V2",
    "2.5" = "V2_5",
    "2.6" = "V2_6",
    "3" = "V3",
    "3.5" = "V3_5",
    "3.5-fast" = "V3_5_FAST",
    stop(
      "Unknown model_version: '", model_version, "'. ",
      "Valid values: \"2\", \"2.5\", \"2.6\", \"3\", \"3.5\", \"3.5-fast\".",
      call. = FALSE
    )
  )
}

# Map an R model_version string to a tabpfn-client version string (cloud).
.client_model_version <- function(model_version) {
  switch(model_version,
    "3.5" = "v3.5",
    "3.5-fast" = "v3.5-fast",
    "3" = "v3",
    "2.6" = "v2.6",
    "2.5" = "v2.5",
    "2" = "v2",
    stop(
      "Unknown model_version: '", model_version, "'. ",
      "Valid values for the cloud client: \"3.5\", \"3.5-fast\", \"3\", \"2.6\", \"2.5\".",
      call. = FALSE
    )
  )
}

# Whether the user requested the cloud "thinking" endpoint.
.thinking_requested <- function(thinking_mode, thinking_effort,
                                      thinking_timeout_s, thinking_metric,
                                      group_col, time_col, group_time_col) {
  isTRUE(thinking_mode) ||
    !is.null(thinking_effort) ||
    !is.null(thinking_timeout_s) ||
    !is.null(thinking_metric) ||
    !is.null(group_col) ||
    !is.null(time_col) ||
    !is.null(group_time_col)
}

.tabpfn_version_string <- function(tabpfn) {
  tryCatch(as.character(tabpfn$`__version__`), error = function(e) "unknown")
}

# Raise a friendly error for license/token failures (local or cloud).
.tabpfn_license_error <- function(msg) {
  if (grepl("license|token|401|403|authentication|api[ ]?key",
            msg, ignore.case = TRUE)) {
    stop(
      "TabPFN license/token error: ", msg, "\n",
      rtabpfn:::tabpfn_license_instructions(),
      call. = FALSE
    )
  }
  stop(msg, call. = FALSE)
}

.import_tabpfn_client <- function() {
  if (!reticulate::py_module_available("tabpfn_client")) {
    stop(
      "The cloud TabPFN client ('tabpfn-client') is not installed. ",
      "Thinking mode requires it. Run setup_tabpfn(install_client = TRUE).",
      call. = FALSE
    )
  }
  reticulate::import("tabpfn_client", convert = FALSE)
}

.build_client_kwargs <- function(n_estimators, thinking, thinking_effort,
                                 thinking_timeout_s, thinking_metric,
                                 group_col, time_col, group_time_col) {
  kwargs <- list()
  if (!is.null(n_estimators)) kwargs$n_estimators <- n_estimators
  if (isTRUE(thinking)) kwargs$thinking_mode <- TRUE
  if (!is.null(thinking_effort)) kwargs$thinking_effort <- thinking_effort
  if (!is.null(thinking_timeout_s)) kwargs$thinking_timeout_s <- thinking_timeout_s
  if (!is.null(thinking_metric)) kwargs$thinking_metric <- thinking_metric
  if (!is.null(group_col)) {
    kwargs$group_col <- if (length(group_col) == 1) group_col else as.list(group_col)
  }
  if (!is.null(time_col)) kwargs$time_col <- time_col
  if (!is.null(group_time_col)) kwargs$group_time_col <- group_time_col
  kwargs
}

.build_local_regressor <- function(model_version, device, n_estimators, dots) {
  tabpfn <- reticulate::import("tabpfn", convert = FALSE)
  ctor_args <- list(device = device)
  if (!is.null(n_estimators)) ctor_args$n_estimators <- n_estimators
  ctor_args <- c(ctor_args, dots)

  if (is.null(model_version) || identical(model_version, "auto")) {
    return(do.call(tabpfn$TabPFNRegressor, ctor_args))
  }

  member <- .model_version_member(model_version)
  mv <- tryCatch(
    reticulate::py_get_attr(tabpfn$constants$ModelVersion, member),
    error = function(e) {
      stop(
        "Model version '", model_version, "' requires tabpfn >= 9.0.0 ",
        "(installed: ", .tabpfn_version_string(tabpfn), "). ",
        "Run setup_tabpfn(upgrade = TRUE).",
        call. = FALSE
      )
    }
  )
  do.call(tabpfn$TabPFNRegressor$create_default_for_version, c(list(mv), ctor_args))
}

.build_local_classifier <- function(model_version, device, n_estimators, dots) {
  tabpfn <- reticulate::import("tabpfn", convert = FALSE)
  ctor_args <- list(device = device)
  if (!is.null(n_estimators)) ctor_args$n_estimators <- n_estimators
  ctor_args <- c(ctor_args, dots)

  if (is.null(model_version) || identical(model_version, "auto")) {
    return(do.call(tabpfn$TabPFNClassifier, ctor_args))
  }

  member <- .model_version_member(model_version)
  mv <- tryCatch(
    reticulate::py_get_attr(tabpfn$constants$ModelVersion, member),
    error = function(e) {
      stop(
        "Model version '", model_version, "' requires tabpfn >= 9.0.0 ",
        "(installed: ", .tabpfn_version_string(tabpfn), "). ",
        "Run setup_tabpfn(upgrade = TRUE).",
        call. = FALSE
      )
    }
  )
  do.call(tabpfn$TabPFNClassifier$create_default_for_version, c(list(mv), ctor_args))
}

.build_client_regressor <- function(model_version, n_estimators, thinking,
                                    thinking_effort, thinking_timeout_s,
                                    thinking_metric, group_col, time_col,
                                    group_time_col, dots) {
  client <- .import_tabpfn_client()
  kwargs <- .build_client_kwargs(n_estimators, thinking, thinking_effort,
                                 thinking_timeout_s, thinking_metric,
                                 group_col, time_col, group_time_col)
  if (is.null(model_version) || identical(model_version, "auto")) {
    return(do.call(client$TabPFNRegressor, c(kwargs, dots)))
  }
  ver <- .client_model_version(model_version)
  do.call(client$TabPFNRegressor$create_default_for_version, c(list(ver), c(kwargs, dots)))
}

.build_client_classifier <- function(model_version, n_estimators, thinking,
                                     thinking_effort, thinking_timeout_s,
                                     thinking_metric, group_col, time_col,
                                     group_time_col, dots) {
  client <- .import_tabpfn_client()
  kwargs <- .build_client_kwargs(n_estimators, thinking, thinking_effort,
                                 thinking_timeout_s, thinking_metric,
                                 group_col, time_col, group_time_col)
  if (is.null(model_version) || identical(model_version, "auto")) {
    return(do.call(client$TabPFNClassifier, c(kwargs, dots)))
  }
  ver <- .client_model_version(model_version)
  do.call(client$TabPFNClassifier$create_default_for_version, c(list(ver), c(kwargs, dots)))
}


#' Train a TabPFN regression model with enhanced predict options
#'
#' @param X Predictor data frame or matrix
#' @param y Response vector
#' @param device Device to use: "auto", "cpu", or "cuda"
#' @param test_size Proportion of data to use for internal validation
#' @param model_version Model version to use. One of "auto" (default), "3.5",
#'   "3.5-fast", "3", "2.6", "2.5", or "2". On the local OSS package,
#'   "3.5" selects the TabPFN-3.5 checkpoint and "3.5-fast" selects the
#'   TabPFN-3.5-Fast (alpha) checkpoint. Requires tabpfn >= 9.0.0.
#' @param thinking_mode Logical. If TRUE, route to the cloud "thinking" endpoint
#'   (requires tabpfn-client and a TABPFN_TOKEN). Not available in the local
#'   OSS package.
#' @param thinking_effort Effort level for thinking mode: "medium" (default) or
#'   "high". Setting this also enables thinking mode.
#' @param thinking_timeout_s Optional wall-clock budget (seconds) for thinking fits.
#' @param thinking_metric Metric to optimize during thinking (e.g. "rmse", "mae").
#' @param group_col Column(s) identifying groups of related rows (thinking only).
#' @param time_col Column holding time (thinking only).
#' @param group_time_col Temporal column within each group (thinking only).
#' @param ... Additional arguments passed to TabPFNRegressor
#'
#' @return A tab_pfn model object with mode = "regression"
#' @export
tab_pfn_regression <- function(X, y, device = "auto", test_size = 0.33,
                               model_version = "auto",
                               thinking_mode = FALSE,
                               thinking_effort = NULL,
                               thinking_timeout_s = NULL,
                               thinking_metric = NULL,
                               group_col = NULL,
                               time_col = NULL,
                               group_time_col = NULL,
                               ...) {

  rtabpfn:::ensure_python_env()

  dots <- list(...)
  n_estimators <- dots$n_estimators
  if (!is.null(n_estimators)) {
    n_estimators <- as.integer(n_estimators)
  }
  dots$n_estimators <- NULL

  use_thinking <- .thinking_requested(
    thinking_mode, thinking_effort, thinking_timeout_s, thinking_metric,
    group_col, time_col, group_time_col
  )
  backend <- if (use_thinking) "client" else "local"

  reg <- tryCatch({
    if (use_thinking) {
      .build_client_regressor(model_version, n_estimators, TRUE,
                              thinking_effort, thinking_timeout_s, thinking_metric,
                              group_col, time_col, group_time_col, dots)
    } else {
      .build_local_regressor(model_version, device, n_estimators, dots)
    }
  }, error = function(e) {
    .tabpfn_license_error(conditionMessage(e))
  })

  fit_y <- if (use_thinking) reticulate::np_array(as.numeric(y)) else y

  tryCatch({
    reg$fit(X, fit_y)
  }, error = function(e) {
    .tabpfn_license_error(conditionMessage(e))
  })

  # Create model object
  model <- list(
    fit = reg,
    mode = "regression",
    backend = backend,
    model_version = model_version,
    thinking_mode = use_thinking,
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
#' @param model_version Model version to use. One of "auto" (default), "3.5",
#'   "3.5-fast", "3", "2.6", "2.5", or "2". On the local OSS package,
#'   "3.5" selects the TabPFN-3.5 checkpoint and "3.5-fast" selects the
#'   TabPFN-3.5-Fast (alpha) checkpoint. Requires tabpfn >= 9.0.0.
#' @param thinking_mode Logical. If TRUE, route to the cloud "thinking" endpoint
#'   (requires tabpfn-client and a TABPFN_TOKEN). Not available in the local
#'   OSS package.
#' @param thinking_effort Effort level for thinking mode: "medium" (default) or
#'   "high". Setting this also enables thinking mode.
#' @param thinking_timeout_s Optional wall-clock budget (seconds) for thinking fits.
#' @param thinking_metric Metric to optimize during thinking (e.g. "accuracy", "roc_auc").
#' @param group_col Column(s) identifying groups of related rows (thinking only).
#' @param time_col Column holding time (thinking only).
#' @param group_time_col Temporal column within each group (thinking only).
#' @param ... Additional arguments passed to TabPFNClassifier
#'
#' @return A tab_pfn model object with mode = "classification"
#' @export
tab_pfn_classification <- function(X, y, device = "auto", test_size = 0.33,
                                   model_version = "auto",
                                   thinking_mode = FALSE,
                                   thinking_effort = NULL,
                                   thinking_timeout_s = NULL,
                                   thinking_metric = NULL,
                                   group_col = NULL,
                                   time_col = NULL,
                                   group_time_col = NULL,
                                   ...) {

  rtabpfn:::ensure_python_env()

  # Get levels
  if (is.factor(y)) {
    levels_vec <- levels(y)
  } else {
    levels_vec <- unique(as.character(y))
  }

  dots <- list(...)
  n_estimators <- dots$n_estimators
  if (!is.null(n_estimators)) {
    n_estimators <- as.integer(n_estimators)
  }
  dots$n_estimators <- NULL

  use_thinking <- .thinking_requested(
    thinking_mode, thinking_effort, thinking_timeout_s, thinking_metric,
    group_col, time_col, group_time_col
  )
  backend <- if (use_thinking) "client" else "local"

  clf <- tryCatch({
    if (use_thinking) {
      .build_client_classifier(model_version, n_estimators, TRUE,
                               thinking_effort, thinking_timeout_s, thinking_metric,
                               group_col, time_col, group_time_col, dots)
    } else {
      .build_local_classifier(model_version, device, n_estimators, dots)
    }
  }, error = function(e) {
    .tabpfn_license_error(conditionMessage(e))
  })

  fit_y <- if (use_thinking) reticulate::np_array(as.character(y)) else y

  tryCatch({
    clf$fit(X, fit_y)
  }, error = function(e) {
    .tabpfn_license_error(conditionMessage(e))
  })

  # Create model object
  model <- list(
    fit = clf,
    mode = "classification",
    backend = backend,
    model_version = model_version,
    thinking_mode = use_thinking,
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
#' @param x A tab_pfn model object (specification or fitted model)
#' @param ... Additional arguments (not used)
#' @export
print.tab_pfn <- function(x, ...) {
  # Check if this is a model specification or a fitted model
  if ("model_spec" %in% class(x)) {
    # Print for model specification (from parsnip)
    cat("TabPFN Model Specification (", x$mode, ")\n\n", sep = "")
    cat("Main Arguments:\n")
    if (!is.null(x$args$n_estimators)) {
      cat("  n_estimators = ", rlang::eval_tidy(x$args$n_estimators), "\n", sep = "")
    }
    if (!is.null(x$args$device)) {
      cat("  device = '", rlang::eval_tidy(x$args$device), "'\n", sep = "")
    }
    cat("\nComputational engine: ", x$engine, "\n", sep = "")
  } else {
    # Print for fitted model
    cat("TabPFN", tools::toTitleCase(x$mode), "Model\n\n")

    if (!is.null(x$predictor_names)) {
      cat("Predictors:", length(x$predictor_names), "\n")
    }

    if (x$mode == "classification" && !is.null(x$levels)) {
      cat("Classes:", length(x$levels), "\n")
      cat(" ", paste(x$levels, collapse = ", "), "\n")
    }

    if (!is.null(x$model_version) && !identical(x$model_version, "auto")) {
      cat("Model version:", x$model_version, "\n")
    }
    if (!is.null(x$backend)) {
      cat("Backend:", x$backend, if (isTRUE(x$thinking_mode)) " (thinking)" else "", "\n", sep = "")
    }

    cat("Device:", x$device, "\n")
  }

  invisible(x)
}
