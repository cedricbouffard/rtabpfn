# Debug and Troubleshooting Functions for TabPFN
# Add these to your R/utils.R file or source separately

#' Debug Python object structure
#'
#' @param py_obj A Python object from reticulate
#' @return Prints diagnostic information about the object
#' @export
debug_python_object <- function(py_obj) {
  cat("=== Python Object Debug Info ===\n")
  cat("Class:", paste(class(py_obj), collapse = ", "), "\n")
  cat("Type:", typeof(py_obj), "\n")

  # Try to get Python type
  tryCatch({
    cat("Python type:", reticulate::py_str(reticulate::py_get_attr(py_obj, "__class__")), "\n")
  }, error = function(e) {
    cat("Python type: <unavailable>\n")
  })

  # Check if it's a list
  if (inherits(py_obj, "python.builtin.list")) {
    cat("Length:", reticulate::py_len(py_obj), "\n")
    cat("First element class:", class(py_obj[[1]]), "\n")

    # Try to inspect first element
    tryCatch({
      first_elem <- reticulate::py_to_r(py_obj[[1]])
      cat("First element (R):", str(first_elem), "\n")
    }, error = function(e) {
      cat("Could not convert first element\n")
    })
  }

  # Try converting to R
  tryCatch({
    r_obj <- reticulate::py_to_r(py_obj)
    cat("Converted to R successfully\n")
    cat("R class:", paste(class(r_obj), collapse = ", "), "\n")
    cat("R structure:\n")
    str(r_obj, max.level = 2)
  }, error = function(e) {
    cat("Conversion error:", e$message, "\n")
  })

  invisible(NULL)
}


#' Alternative predict function with detailed error handling
#'
#' @param object A fitted TabPFN model object
#' @param new_data A data frame of new predictors
#' @param type Type of prediction
#' @param output_type Python TabPFN output type
#' @param quantiles Numeric vector of quantiles
#' @param verbose Print debug information
#' @param ... Additional arguments
#'
#' @return A tibble with predictions
#' @export
predict_tabpfn_verbose <- function(object,
                                   new_data,
                                   type = NULL,
                                   output_type = "mean",
                                   quantiles = c(0.1, 0.5, 0.9),
                                   verbose = TRUE,
                                   ...) {

  if (is.null(type)) {
    type <- if (object$mode == "regression") "numeric" else "class"
  }

  new_data <- as.data.frame(new_data)

  if (verbose) {
    cat("=== Prediction Settings ===\n")
    cat("Mode:", object$mode, "\n")
    cat("Type:", type, "\n")
    cat("Output type:", output_type, "\n")
    cat("New data dims:", nrow(new_data), "x", ncol(new_data), "\n\n")
  }

  # For quantile predictions
  if (object$mode == "regression" && (type == "quantiles" || output_type == "quantiles")) {

    if (verbose) cat("Calling Python predict with quantiles...\n")

    # Get raw predictions
    preds_py <- object$fit$predict(
      new_data,
      output_type = "quantiles",
      quantiles = quantiles,
      ...
    )

    if (verbose) {
      cat("\n=== Raw Python Output ===\n")
      debug_python_object(preds_py)
      cat("\n")
    }

    # Try different conversion strategies
    preds_r <- tryCatch({
      # Strategy 1: Direct conversion
      reticulate::py_to_r(preds_py)
    }, error = function(e) {
      if (verbose) cat("Strategy 1 failed, trying strategy 2...\n")

      # Strategy 2: Convert as numpy array
      tryCatch({
        np <- reticulate::import("numpy", convert = TRUE)
        preds_np <- np$array(preds_py)
        reticulate::py_to_r(preds_np)
      }, error = function(e2) {
        if (verbose) cat("Strategy 2 failed, trying strategy 3...\n")

        # Strategy 3: Manual iteration
        n_quantiles <- length(quantiles)
        n_obs <- nrow(new_data)

        result <- matrix(NA, nrow = n_obs, ncol = n_quantiles)

        for (i in seq_along(quantiles)) {
          result[, i] <- reticulate::py_to_r(preds_py[[i - 1]])  # Python 0-indexed
        }
        result
      })
    })

    if (verbose) {
      cat("=== Converted R Object ===\n")
      cat("Class:", paste(class(preds_r), collapse = ", "), "\n")
      cat("Dimensions:", paste(dim(preds_r), collapse = " x "), "\n")
      cat("Structure:\n")
      str(preds_r, max.level = 1)
      cat("\n")
    }

    # Ensure it's a matrix with correct dimensions
    if (is.list(preds_r) && !is.data.frame(preds_r)) {
      if (verbose) cat("Converting list to matrix...\n")
      preds_r <- do.call(cbind, lapply(preds_r, as.numeric))
    } else if (!is.matrix(preds_r)) {
      if (verbose) cat("Converting to matrix...\n")
      preds_r <- as.matrix(preds_r)
    }

    # Check and fix dimensions
    if (ncol(preds_r) != length(quantiles)) {
      if (verbose) cat("Transposing matrix (wrong orientation)...\n")
      preds_r <- t(preds_r)
    }

    if (verbose) {
      cat("Final dimensions:", paste(dim(preds_r), collapse = " x "), "\n")
      cat("First few rows:\n")
      print(head(preds_r))
      cat("\n")
    }

    # Convert to data frame
    pred_df <- as.data.frame(preds_r)
    colnames(pred_df) <- paste0(".pred_q", sprintf("%03d", quantiles * 100))

    return(tibble::as_tibble(pred_df))
  }

  # For other prediction types, fall back to standard predict
  predict.tab_pfn(object, new_data, type = type, output_type = output_type,
                  quantiles = quantiles, ...)
}


#' Test TabPFN installation and basic functionality
#'
#' @param verbose Print detailed output
#' @return Logical indicating if tests passed
#' @export
test_tabpfn_setup <- function(verbose = TRUE) {

  tests_passed <- TRUE

  # Test 1: Check if TabPFN is available
  if (verbose) cat("Test 1: Checking TabPFN availability...\n")
  has_tabpfn <- reticulate::py_module_available("tabpfn")

  if (has_tabpfn) {
    if (verbose) cat("  ✓ TabPFN is available\n")
  } else {
    if (verbose) cat("  ✗ TabPFN is NOT available\n")
    tests_passed <- FALSE
    return(tests_passed)
  }

  # Test 2: Try to import TabPFN
  if (verbose) cat("\nTest 2: Importing TabPFN...\n")
  tabpfn <- tryCatch({
    reticulate::import("tabpfn", convert = FALSE)
  }, error = function(e) {
    if (verbose) cat("  ✗ Failed to import TabPFN:", e$message, "\n")
    tests_passed <<- FALSE
    return(NULL)
  })

  if (!is.null(tabpfn)) {
    if (verbose) cat("  ✓ TabPFN imported successfully\n")
  }

  # Test 3: Create a simple model
  if (verbose) cat("\nTest 3: Creating simple regression model...\n")

  X_test <- data.frame(x1 = 1:10, x2 = 11:20)
  y_test <- 1:10

  model <- tryCatch({
    tab_pfn_regression(X_test, y_test)
  }, error = function(e) {
    if (verbose) cat("  ✗ Failed to create model:", e$message, "\n")
    tests_passed <<- FALSE
    return(NULL)
  })

  if (!is.null(model)) {
    if (verbose) cat("  ✓ Model created successfully\n")
  }

  # Test 4: Make predictions
  if (verbose) cat("\nTest 4: Making standard predictions...\n")

  preds <- tryCatch({
    predict(model, X_test, type = "numeric")
  }, error = function(e) {
    if (verbose) cat("  ✗ Failed to predict:", e$message, "\n")
    tests_passed <<- FALSE
    return(NULL)
  })

  if (!is.null(preds)) {
    if (verbose) {
      cat("  ✓ Predictions successful\n")
      cat("    Prediction dimensions:", paste(dim(preds), collapse = " x "), "\n")
    }
  }

  # Test 5: Quantile predictions
  if (verbose) cat("\nTest 5: Making quantile predictions...\n")

  preds_q <- tryCatch({
    predict(model, X_test, type = "quantiles", quantiles = c(0.1, 0.5, 0.9))
  }, error = function(e) {
    if (verbose) cat("  ✗ Failed quantile predictions:", e$message, "\n")
    tests_passed <<- FALSE
    return(NULL)
  })

  if (!is.null(preds_q)) {
    if (verbose) {
      cat("  ✓ Quantile predictions successful\n")
      cat("    Dimensions:", paste(dim(preds_q), collapse = " x "), "\n")
      cat("    Columns:", paste(colnames(preds_q), collapse = ", "), "\n")
    }
  }

  if (verbose) {
    cat("\n=== Summary ===\n")
    if (tests_passed) {
      cat("✓ All tests passed!\n")
    } else {
      cat("✗ Some tests failed. See details above.\n")
    }
  }

  invisible(tests_passed)
}


#' Convert TabPFN quantile predictions safely
#'
#' @param preds_py Raw Python predictions object
#' @param quantiles Vector of quantiles that were requested
#' @param n_obs Number of observations
#'
#' @return Matrix of predictions (n_obs x n_quantiles)
#' @export
convert_quantile_predictions <- function(preds_py, quantiles, n_obs) {

  n_quantiles <- length(quantiles)

  # Try numpy conversion first (most reliable)
  tryCatch({
    np <- reticulate::import("numpy", convert = TRUE)
    preds_np <- np$array(preds_py)
    preds_r <- reticulate::py_to_r(preds_np)

    # Ensure correct shape
    if (is.vector(preds_r)) {
      preds_r <- matrix(preds_r, nrow = n_obs, ncol = n_quantiles)
    }

    # Check orientation
    if (ncol(preds_r) != n_quantiles) {
      preds_r <- t(preds_r)
    }

    return(preds_r)

  }, error = function(e) {
    # Fall back to manual conversion
    result <- matrix(NA, nrow = n_obs, ncol = n_quantiles)

    for (i in seq_along(quantiles)) {
      # Python uses 0-indexing
      result[, i] <- as.numeric(reticulate::py_to_r(preds_py[[i - 1]]))
    }

    return(result)
  })
}
