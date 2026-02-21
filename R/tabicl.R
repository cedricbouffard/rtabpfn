#' Predict method for TabICL models
#'
#' @param object A fitted TabICL model object
#' @param new_data A data frame of new predictors
#' @param type Type of prediction. For regression: "numeric" (default), "quantiles",
#'   "conf_int", or "raw". For classification: "class", "prob", or "raw"
#' @param quantiles Numeric vector of quantiles to predict (used when type = "quantiles")
#' @param level Confidence level for prediction intervals (used when type = "conf_int")
#' @param ... Additional arguments passed to the Python predict method
#'
#' @return A tibble with predictions
#' @export
predict.tab_icl <- function(object,
                            new_data,
                            type = NULL,
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

  # Get predictions based on type
  if (type == "raw") {
    # Return raw Python object
    return(object$fit$predict(new_data, ...))
  }

  # Regression predictions
  if (object$mode == "regression") {
    if (type == "quantiles") {
      # Quantile predictions using TabICL
      quantiles_vec <- as.vector(quantiles)
      
      # TabICL predict with output_type="quantiles" and alphas parameter
      # The predict method handles the ensemble averaging internally
      preds <- object$fit$predict(
        new_data, 
        output_type = "quantiles", 
        alphas = as.list(quantiles_vec)
      )
      
      # Convert to R
      preds_r <- reticulate::py_to_r(preds)
      
      # TabICL returns (n_samples, n_quantiles) array
      if (is.matrix(preds_r)) {
        pred_matrix <- preds_r
      } else if (is.array(preds_r) && length(dim(preds_r)) == 2) {
        pred_matrix <- as.matrix(preds_r)
      } else {
        # Try to convert to matrix
        pred_matrix <- as.matrix(preds_r)
      }
      
      # Ensure correct dimensions - should be (n_samples, n_quantiles)
      if (ncol(pred_matrix) != length(quantiles_vec) && nrow(pred_matrix) == length(quantiles_vec)) {
        pred_matrix <- t(pred_matrix)
      }
      
      # Convert to data frame with named columns
      pred_df <- as.data.frame(pred_matrix)
      col_names <- paste0(".pred_q", quantiles_vec)
      colnames(pred_df) <- col_names
      return(tibble::as_tibble(pred_df))
      
    } else if (type == "conf_int") {
      # Prediction intervals using quantiles
      lower_q <- (1 - level) / 2
      upper_q <- 1 - lower_q
      
      # TabICL predict with output_type="quantiles" and alphas parameter
      preds <- object$fit$predict(
        new_data, 
        output_type = "quantiles", 
        alphas = list(lower_q, upper_q)
      )
      
      # Convert to R
      preds_r <- reticulate::py_to_r(preds)
      
      # Handle dimensions
      if (is.matrix(preds_r)) {
        pred_matrix <- preds_r
      } else {
        pred_matrix <- as.matrix(preds_r)
      }
      
      # Ensure correct dimensions - should be (n_samples, 2)
      if (ncol(pred_matrix) != 2 && nrow(pred_matrix) == 2) {
        pred_matrix <- t(pred_matrix)
      }
      
      pred_df <- data.frame(
        .pred_lower = pred_matrix[, 1],
        .pred_upper = pred_matrix[, 2]
      )
      return(tibble::as_tibble(pred_df))
      
    } else {
      # Point predictions (mean)
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
#' @param n_estimators Number of ensemble members (default: 8). More = better but slower.
#' @param norm_methods Normalization methods to try (default: NULL uses ["none", "power"]).
#' @param feat_shuffle_method Feature permutation strategy (default: "latin").
#' @param outlier_threshold Z-score threshold for outlier detection and clipping (default: 4.0).
#' @param batch_size Batch size for ensemble processing (default: 8). Lower to save memory.
#' @param model_path Path to checkpoint (default: NULL downloads from Hugging Face).
#' @param allow_auto_download Auto-download checkpoint if not found locally (default: TRUE).
#' @param checkpoint_version Pretrained checkpoint version (default: "tabicl-regressor-v2-20260212.ckpt").
#' @param device Device to use: "auto", "cpu", or "cuda" (default: "auto").
#' @param use_amp Automatic mixed precision for faster inference (default: "auto").
#' @param use_fa3 Flash Attention 3 for Hopper GPUs (default: "auto").
#' @param offload_mode Automatically decide when to use cpu/disk offloading (default: "auto").
#' @param disk_offload_dir Directory for disk offloading (default: NULL).
#' @param random_state Random seed for reproducibility (default: 42).
#' @param n_jobs Number of PyTorch threads for CPU inference (default: NULL).
#' @param verbose Print detailed information during inference (default: FALSE).
#' @param inference_config Fine-grained inference control for advanced users (default: NULL).
#' @param ... Additional arguments passed to TabICLRegressor
#'
#' @return A tab_icl model object with mode = "regression"
#' @export
tab_icl_regression <- function(X, y, 
                                n_estimators = 8,
                                norm_methods = NULL,
                                feat_shuffle_method = "latin",
                                outlier_threshold = 4.0,
                                batch_size = 8,
                                model_path = NULL,
                                allow_auto_download = TRUE,
                                checkpoint_version = "tabicl-regressor-v2-20260212.ckpt",
                                device = "auto",
                                use_amp = "auto",
                                use_fa3 = "auto",
                                offload_mode = "auto",
                                disk_offload_dir = NULL,
                                random_state = 42,
                                n_jobs = NULL,
                                verbose = FALSE,
                                inference_config = NULL,
                                ...) {

  rtabpfn:::ensure_python_env()
  
  # Resolve device (TabICL doesn't support "auto")
  resolved_device <- resolve_tabicl_device(device)

  # Load Python module
  tabicl <- reticulate::import("tabicl", convert = FALSE)

  # Create regressor with all parameters
  reg <- tabicl$TabICLRegressor(
    n_estimators = as.integer(n_estimators),
    norm_methods = norm_methods,
    feat_shuffle_method = feat_shuffle_method,
    outlier_threshold = outlier_threshold,
    batch_size = as.integer(batch_size),
    model_path = model_path,
    allow_auto_download = allow_auto_download,
    checkpoint_version = checkpoint_version,
    device = resolved_device,
    use_amp = use_amp,
    use_fa3 = use_fa3,
    offload_mode = offload_mode,
    disk_offload_dir = disk_offload_dir,
    random_state = as.integer(random_state),
    n_jobs = n_jobs,
    verbose = verbose,
    inference_config = inference_config,
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
#' @param n_estimators Number of ensemble members (default: 8). More = better but slower.
#' @param norm_methods Normalization methods to try (default: NULL uses ["none", "power"]).
#' @param feat_shuffle_method Feature permutation strategy (default: "latin").
#' @param class_shuffle_method Class permutation strategy (default: "shift").
#' @param outlier_threshold Z-score threshold for outlier detection and clipping (default: 4.0).
#' @param softmax_temperature Temperature to control prediction confidence (default: 0.9).
#' @param average_logits Average logits (TRUE) or probabilities (FALSE) (default: TRUE).
#' @param support_many_classes Handle >10 classes automatically (default: TRUE).
#' @param batch_size Batch size for ensemble processing (default: 8). Lower to save memory.
#' @param model_path Path to checkpoint (default: NULL downloads from Hugging Face).
#' @param allow_auto_download Auto-download checkpoint if not found locally (default: TRUE).
#' @param checkpoint_version Pretrained checkpoint version (default: "tabicl-classifier-v2-20260212.ckpt").
#' @param device Device to use: "auto", "cpu", or "cuda" (default: "auto").
#' @param use_amp Automatic mixed precision for faster inference (default: "auto").
#' @param use_fa3 Flash Attention 3 for Hopper GPUs (default: "auto").
#' @param offload_mode Automatically decide when to use cpu/disk offloading (default: "auto").
#' @param disk_offload_dir Directory for disk offloading (default: NULL).
#' @param random_state Random seed for reproducibility (default: 42).
#' @param n_jobs Number of PyTorch threads for CPU inference (default: NULL).
#' @param verbose Print detailed information during inference (default: FALSE).
#' @param inference_config Fine-grained inference control for advanced users (default: NULL).
#' @param ... Additional arguments passed to TabICLClassifier
#'
#' @return A tab_icl model object with mode = "classification"
#' @export
tab_icl_classification <- function(X, y, 
                                    n_estimators = 8,
                                    norm_methods = NULL,
                                    feat_shuffle_method = "latin",
                                    class_shuffle_method = "shift",
                                    outlier_threshold = 4.0,
                                    softmax_temperature = 0.9,
                                    average_logits = TRUE,
                                    support_many_classes = TRUE,
                                    batch_size = 8,
                                    model_path = NULL,
                                    allow_auto_download = TRUE,
                                    checkpoint_version = "tabicl-classifier-v2-20260212.ckpt",
                                    device = "auto",
                                    use_amp = "auto",
                                    use_fa3 = "auto",
                                    offload_mode = "auto",
                                    disk_offload_dir = NULL,
                                    random_state = 42,
                                    n_jobs = NULL,
                                    verbose = FALSE,
                                    inference_config = NULL,
                                    ...) {

  rtabpfn:::ensure_python_env()
  
  # Resolve device (TabICL doesn't support "auto")
  resolved_device <- resolve_tabicl_device(device)

  # Load Python module
  tabicl <- reticulate::import("tabicl", convert = FALSE)

  # Create classifier with all parameters
  clf <- tabicl$TabICLClassifier(
    n_estimators = as.integer(n_estimators),
    norm_methods = norm_methods,
    feat_shuffle_method = feat_shuffle_method,
    class_shuffle_method = class_shuffle_method,
    outlier_threshold = outlier_threshold,
    softmax_temperature = softmax_temperature,
    average_logits = average_logits,
    support_many_classes = support_many_classes,
    batch_size = as.integer(batch_size),
    model_path = model_path,
    allow_auto_download = allow_auto_download,
    checkpoint_version = checkpoint_version,
    device = resolved_device,
    use_amp = use_amp,
    use_fa3 = use_fa3,
    offload_mode = offload_mode,
    disk_offload_dir = disk_offload_dir,
    random_state = as.integer(random_state),
    n_jobs = n_jobs,
    verbose = verbose,
    inference_config = inference_config,
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
