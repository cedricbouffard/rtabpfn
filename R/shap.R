#' SHAP Values for TabPFN Models
#'
#' Functions to calculate SHAP (SHapley Additive exPlanations) values for
#' TabPFN models using tabpfn_extensions package.
#'
#' @name shap
NULL

#' Check if SHAP is available
#'
#' @description
#' Checks if tabpfn-extensions is installed and available for SHAP calculations.
#'
#' @return Logical indicating if SHAP is available
#' @export
#'
#' @examples
#' \dontrun{
#' check_shap_available()
#' }
check_shap_available <- function() {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }

  rtabpfn:::ensure_python_env()

  reticulate::py_module_available("tabpfn_extensions")
}


#' Calculate SHAP Values for a TabPFN Model
#'
#' @param object A fitted TabPFN model object
#' @param new_data A data frame of observations to explain
#' @param verbose Print progress information
#' @param ... Additional arguments passed to get_shap_values
#'
#' @return A tibble with SHAP values for each observation and feature
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#'
#' # Train model
#' X <- mtcars[, c("cyl", "disp", "hp", "wt")]
#' y <- mtcars$mpg
#' model <- tab_pfn_regression(X, y)
#'
#' # Calculate SHAP values
#' shap_vals <- shap_values(model, X[1:5, ])
#' }
shap_values <- function(object,
                        new_data,
                        verbose = TRUE,
                        ...) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  rtabpfn:::ensure_python_env()

  if (!check_shap_available()) {
    stop("Package 'tabpfn_extensions' is required for SHAP values.\n",
         "To install:\n",
         "1. Ensure Python environment is configured: setup_tabpfn()\n",
         "2. Or install manually: reticulate::py_install('tabpfn-extensions', pip = TRUE)")
  }

  if (verbose) {
    cat("Calculating SHAP values for TabPFN model...\n")
  }

  # Prepare new_data
  new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)

  # Remove outcome variable if present
  if (!is.null(object$outcome_name) && object$outcome_name %in% names(new_data)) {
    new_data <- new_data[, names(new_data) != object$outcome_name, drop = FALSE]
  }

  # Reorder columns to match training data
  if (!is.null(object$predictor_names)) {
    new_data <- new_data[, object$predictor_names, drop = FALSE]
  }

  # Convert numeric columns to double, handle factors
  for (col in colnames(new_data)) {
    if (is.factor(new_data[[col]])) {
      if (verbose) {
        cat("  Converting factor column '", col, "' to numeric...\n")
      }
      new_data[[col]] <- as.numeric(new_data[[col]])
    } else if (is.numeric(new_data[[col]])) {
      new_data[[col]] <- as.double(new_data[[col]])
    }
  }

  if (verbose) {
    cat("  Input data dimensions:", nrow(new_data), "x", ncol(new_data), "\n")
    cat("  Column names and types:\n")
    for (col in colnames(new_data)) {
      cat("    ", col, ":", class(new_data[[col]]), "\n")
    }
  }

  # Import numpy and pandas (without auto-conversion)
  np <- tryCatch({
    reticulate::import("numpy", convert = FALSE)
  }, error = function(e) {
    stop("Package 'numpy' is required. Install it with: pip install numpy")
  })

  pd <- tryCatch({
    reticulate::import("pandas", convert = FALSE)
  }, error = function(e) {
    stop("Package 'pandas' is required. Install it with: pip install pandas")
  })

  # Import get_shap_values from tabpfn_extensions
  if (verbose) {
    cat("Importing SHAP function from tabpfn_extensions.interpretability.shap...\n")
  }

  # Import shap module
  shap_module <- tryCatch({
    reticulate::import("tabpfn_extensions.interpretability.shap", convert = FALSE)
  }, error = function(e) {
    stop("Failed to import tabpfn_extensions.interpretability.shap.\n",
         "Error: ", e$message, "\n",
         "Make sure you installed: pip install 'tabpfn-extensions[all] @ git+https://github.com/PriorLabs/tabpfn-extensions.git'")
  })

  if (verbose) {
    cat("  Checking model object type...\n")
    cat("  object$fit type:", class(object$fit), "\n")
    if (!is.null(object$fit$predict)) {
      cat("  object$fit has predict method: TRUE\n")
    }
  }

  # Convert to Python pandas DataFrame explicitly
  if (verbose) {
    cat("Converting data to pandas DataFrame...\n")
  }

  # Create numpy arrays for numeric columns
  data_dict <- reticulate::dict()

  for (col in colnames(new_data)) {
    if (is.numeric(new_data[[col]])) {
      # Convert to numpy array as float64
      col_values <- as.double(new_data[[col]])

      # Ensure we create a 1D array even for single observations
      if (length(col_values) == 1) {
        arr <- np$array(list(col_values), dtype = np$float64)
      } else {
        arr <- np$array(col_values, dtype = np$float64)
      }
      data_dict[[col]] <- arr
    } else {
      # Handle character values
      col_values <- as.character(new_data[[col]])
      data_dict[[col]] <- list(col_values)
    }
  }

  # Create pandas DataFrame from dict
  new_data_py <- pd$DataFrame(data_dict)

  if (verbose) {
    shape <- reticulate::py_to_r(new_data_py$shape)
    cat("  Created pandas DataFrame with shape:", unlist(shape), "\n")
    cat("  DataFrame dtypes:\n")
    for (col in colnames(new_data)) {
      dtype <- reticulate::py_to_r(new_data_py[[col]]$dtype)
      cat("    ", col, ":", as.character(dtype), "\n")
    }
  }

  # Calculate SHAP values using tabpfn_extensions
  if (verbose) {
    cat("Computing SHAP values...\n")
  }

  # Call function
  if (verbose) {
    cat("  Calling get_shap_values with fit object and DataFrame...\n")
    cat("  Sample of data:\n")
    sample_data <- reticulate::py_to_r(new_data_py)
    print(head(sample_data, 1))
  }

  # Suppress sklearn warnings during SHAP calculation
  warnings <- reticulate::import("warnings", convert = FALSE)
  warnings$filterwarnings("ignore")

  shap_result <- shap_module$get_shap_values(
    object$fit,
    new_data_py
  )

  warnings$filterwarnings("default")

  # Extract SHAP values directly from Python object
  shap_values <- reticulate::py_to_r(shap_result$values)
  base_values <- reticulate::py_to_r(shap_result$base_values)

  # Get feature names from the Explanation object or use model's predictor names
  feature_names <- tryCatch({
    names_from_py <- reticulate::py_to_r(shap_result$feature_names)
    if (!is.null(names_from_py)) {
      names_from_py
    } else {
      object$predictor_names
    }
  }, error = function(e) {
    object$predictor_names
  })

  # Handle multi-dimensional SHAP arrays (e.g., for classification)
  shap_array <- shap_values
  shap_dims <- dim(shap_array)
  
  if (length(shap_dims) == 3) {
    # Classification: average over classes to get 2D array
    if (verbose) {
      cat("  Detected 3D SHAP array (classification model)\n")
      cat("  Averaging over", shap_dims[3], "classes\n")
    }
    # Average over classes (third dimension)
    shap_matrix <- apply(shap_array, c(1, 2), mean)
  } else if (length(shap_dims) == 2) {
    # Regression or already 2D
    shap_matrix <- shap_array
  } else if (!is.matrix(shap_array)) {
    # Try to convert to matrix
    shap_matrix <- as.matrix(shap_array)
  } else {
    shap_matrix <- shap_array
  }

  # Set feature names as column names
  colnames(shap_matrix) <- feature_names

  # Add row names for observations
  rownames(shap_matrix) <- paste0(".obs_", seq_len(nrow(shap_matrix)))

  # Convert to tibble
  shap_df <- tibble::as_tibble(shap_matrix, rownames = "observation")

  # Add base values
  shap_df$.base_value <- base_values

  if (verbose) {
    cat("  Done! SHAP shape:", paste(dim(shap_matrix), collapse = " x "), "\n")
  }

  return(shap_df)
}


#' Plot SHAP Summary
#'
#' @param shap_df SHAP values tibble from shap_values()
#' @param top_n Number of top features to show (default: 10)
#' @param ... Additional arguments passed to ggplot2
#'
#' @return A ggplot2 object showing feature importance
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#' library(ggplot2)
#'
#' # Train model and get SHAP values
#' model <- tab_pfn_regression(X, y)
#' shap_vals <- shap_values(model, X)
#'
#' # Plot summary
#' plot_shap_summary(shap_vals)
#' }
plot_shap_summary <- function(shap_df, top_n = 10, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  # Get feature columns (exclude observation and base_value)
  feature_cols <- setdiff(names(shap_df), c("observation", ".base_value"))

  # Reshape to long format
  shap_long <- tidyr::pivot_longer(
    shap_df,
    cols = dplyr::all_of(feature_cols),
    names_to = "feature",
    values_to = "shap_value"
  )

  # Calculate mean absolute SHAP values for each feature
  feature_importance <- shap_long |>
    dplyr::group_by(feature) |>
    dplyr::summarize(
      mean_abs_shap = mean(abs(shap_value)),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(mean_abs_shap)) |>
    dplyr::slice_head(n = top_n)

  # Create plot
  p <- ggplot2::ggplot(feature_importance, ggplot2::aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "SHAP Feature Importance",
      x = "Feature",
      y = "Mean |SHAP Value|"
    ) +
    ggplot2::theme_minimal()

  return(p)
}


#' Plot SHAP Dependence Plot
#'
#' @param shap_df SHAP values tibble from shap_values()
#' @param feature_data Original feature data (data frame with the feature values)
#' @param feature Feature name to plot
#' @param color_feature Optional feature name to use for coloring points
#' @param ... Additional arguments passed to ggplot2
#'
#' @return A ggplot2 object showing SHAP dependence
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#' library(ggplot2)
#'
#' model <- tab_pfn_regression(X, y)
#' shap_vals <- shap_values(model, X)
#'
#' # Plot SHAP dependence for a feature
#' plot_shap_dependence(shap_vals, X, feature = "hp")
#' }
plot_shap_dependence <- function(shap_df, feature_data, feature, color_feature = NULL, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  if (!feature %in% names(shap_df)) {
    stop("Feature '", feature, "' not found in SHAP values.")
  }

  if (!feature %in% names(feature_data)) {
    stop("Feature '", feature, "' not found in feature data.")
  }

  # Create plot data combining feature values and SHAP values
  plot_data <- data.frame(
    feature_value = feature_data[[feature]],
    shap_value = shap_df[[feature]]
  )

  # Check if color feature should be included
  has_color <- !is.null(color_feature) && color_feature %in% names(feature_data)
  
  if (has_color) {
    plot_data$color_feature <- feature_data[[color_feature]]
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = feature_value, y = shap_value, color = color_feature)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_color_gradientn(colors = c("blue", "red"))
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = feature_value, y = shap_value)) +
      ggplot2::geom_point(alpha = 0.5)
  }
  
  p <- p + ggplot2::labs(
    title = paste("SHAP Dependence:", feature),
    x = feature,
    y = paste0("SHAP value for ", feature)
  ) +
  ggplot2::theme_minimal()

  return(p)
}



#' Explain Individual Prediction with SHAP
#'
#' @param object A fitted TabPFN model object
#' @param new_data A single row data frame to explain
#' @param ... Additional arguments passed to shap_values
#'
#' @return A list with SHAP values and base value for prediction
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#'
#' model <- tab_pfn_regression(X, y)
#'
#' # Explain a single prediction
#' explanation <- explain_prediction(model, X[1, , drop = FALSE])
#'
#' # View SHAP values
#' print(explanation$shap_values)
#' }
explain_prediction <- function(object,
                               new_data,
                               ...) {

  # Calculate SHAP values for single observation
  shap_df <- shap_values(
    object = object,
    new_data = new_data,
    verbose = FALSE,
    ...
  )

  # Extract SHAP values for first observation
  shap_vals <- as.numeric(shap_df[1, -which(names(shap_df) %in% c("observation", ".base_value"))])
  names(shap_vals) <- setdiff(names(shap_df), c("observation", ".base_value"))
  base_value <- shap_df$.base_value[1]

  # Calculate prediction
  prediction <- sum(shap_vals) + base_value

  # Get feature values
  feature_vals <- as.numeric(new_data[1, ])
  names(feature_vals) <- names(new_data)

  result <- list(
    shap_values = shap_vals,
    base_value = base_value,
    prediction = prediction,
    feature_values = feature_vals
  )

  class(result) <- "shap_explanation"

  return(result)
}


#' Print method for SHAP explanation
#'
#' @param x A shap_explanation object
#' @param ... Additional arguments (not used)
#' @export
print.shap_explanation <- function(x, ...) {
  cat("=== SHAP Explanation ===\n\n")
  cat("Prediction:", round(x$prediction, 4), "\n")
  cat("Base value:", round(x$base_value, 4), "\n\n")

  # Sort by absolute SHAP value
  shap_sorted <- sort(abs(x$shap_values), decreasing = TRUE)

  cat("Top feature contributions:\n")
  for (feat in names(shap_sorted)[1:min(5, length(shap_sorted))]) {
    val <- x$shap_values[feat]
    feat_val <- x$feature_values[feat]
    direction <- if (val > 0) "+" else "-"
    cat(sprintf("  %s: %s%.4f (feature value: %.2f)\n", feat, direction, abs(val), feat_val))
  }

  invisible(x)
}
