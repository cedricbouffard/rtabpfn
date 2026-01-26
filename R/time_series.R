#' Check if TabPFN Time Series is available
#'
#' @description
#' Checks if tabpfn-time-series package is installed and available.
#'
#' @return Logical indicating if tabpfn-time-series is available
#' @export
#'
#' @examples
#' \dontrun{
#' check_time_series_available()
#' }
check_time_series_available <- function() {
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(FALSE)
  }
  
  rtabpfn:::ensure_python_env()
  
  has_ts <- reticulate::py_module_available("tabpfn_time_series")
  
  if (!has_ts) {
    tryCatch({
      py_config <- reticulate::py_config()
      message("tabpfn-time-series not available in current Python environment: ", py_config$python)
      message("To install tabpfn-time-series:")
      message("  1. setup_tabpfn(install_time_series = TRUE)")
      message("  2. Or manually: pip install tabpfn-time-series")
      message("  3. Or switch Python environment: setup_tabpfn(python_path = 'path/to/venv/Scripts/python.exe')")
    }, error = function(e) {
      message("tabpfn-time-series not available. Configure Python with setup_tabpfn().")
    })
    return(FALSE)
  }
  
  tryCatch({
    ts_module <- reticulate::import("tabpfn_time_series", convert = FALSE)
    return(TRUE)
  }, error = function(e) {
    tryCatch({
      py_config <- reticulate::py_config()
      message("Error importing tabpfn_time_series module: ", e$message)
      message("Python: ", py_config$python)
      message("Version: ", py_config$version)
    }, error = function(e2) {
      message("Error importing tabpfn_time_series module: ", e$message)
    })
    return(FALSE)
  })
}


#' Train a TabPFN Time Series Forecasting Model
#'
#' @description
#' Creates a zero-shot time series forecasting model using TabPFN-TS. The model
#' requires no training and can generate forecasts for both point predictions
#' and probabilistic forecasts using quantiles.
#'
#' @param train_df A data frame with time series training data. Must contain:
#'   - A date/datetime column (automatically detected or specified with date_col)
#'   - A value column (automatically detected or specified with value_col)
#'   - Optionally, an item_id column for multiple time series
#' @param prediction_length Integer. Number of steps to forecast ahead
#' @param quantiles Numeric vector of quantiles for probabilistic forecasting (default: c(0.1, 0.5, 0.9))
#' @param date_col Character. Name of the date/datetime column (NULL for auto-detection)
#' @param value_col Character. Name of the value column (NULL for auto-detection)
#' @param item_id_col Character. Name of the item_id column for multiple time series (NULL for single series)
#' @param tabpfn_mode Character. TabPFN mode: "local" (default) or "client"
#' @param tabpfn_output_selection Character. Output selection: "median" or "mean"
#' @param features List of features to extract. NULL for automatic features.
#'   Available features: "running_index", "calendar", "seasonal"
#' @param verbose Logical. Print progress information (default: TRUE)
#' @param ... Additional arguments passed to TabPFNTimeSeriesPredictor
#'
#' @return A tab_pfn_time_series model object
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#' library(dplyr)
#' library(lubridate)
#'
#' # Create sample time series data
#' dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
#' values <- sin(seq(0, 2*pi, length.out = length(dates))) * 10 + rnorm(length(dates), 0, 1)
#' ts_data <- tibble(date = dates, value = values)
#'
#' # Train forecasting model
#' model <- tab_pfn_time_series(ts_data, prediction_length = 30)
#'
#' # Make forecasts
#' forecasts <- predict(model, ts_data)
#' }
tab_pfn_time_series <- function(train_df,
                           prediction_length,
                           quantiles = c(0.1, 0.5, 0.9),
                           date_col = NULL,
                           value_col = NULL,
                           item_id_col = NULL,
                           tabpfn_mode = "local",
                           tabpfn_output_selection = "median",
                           features = NULL,
                           verbose = TRUE) {

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required but not installed.")
  }

  rtabpfn:::ensure_python_env()

  if (!check_time_series_available()) {
    stop("Package 'tabpfn-time-series' is required for time series forecasting.\\n",
         "To install:\\n",
         "1. Ensure Python environment is configured: setup_tabpfn()\\n",
         "2. Or install manually: pip install tabpfn-time-series")
  }

  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed.")
  }

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed.")
  }

  train_df <- as.data.frame(train_df, stringsAsFactors = FALSE)

  col_names <- names(train_df)

  if (is.null(date_col)) {
    date_col <- find_date_column(train_df)
    if (is.null(date_col)) {
      stop("Could not detect date/datetime column. Please specify 'date_col'.")
    }
    if (verbose) {
      message("Auto-detected date column: ", date_col)
    }
  }

  if (is.null(value_col)) {
    value_col <- find_value_column(train_df, date_col, item_id_col)
    if (is.null(value_col)) {
      stop("Could not detect value column. Please specify 'value_col'.")
    }
    if (verbose) {
      message("Auto-detected value column: ", value_col)
    }
  }

  if (verbose) {
    cat("Creating TabPFN Time Series Forecasting Model\\n\\n")
  }

  np <- reticulate::import("numpy", convert = FALSE)
  pd <- reticulate::import("pandas", convert = FALSE)

  train_df[[date_col]] <- lubridate::as_datetime(train_df[[date_col]])

  if (is.null(features)) {
    if (verbose) {
      cat("Adding automatic temporal features...\\n")
    }
    features <- list(
      list(type = "running_index"),
      list(type = "calendar"),
      list(type = "seasonal")
    )
  }

  # For LOCAL mode, we need to manually add features to the data
  if (tabpfn_mode == "local") {
    if (verbose) {
      cat("Computing temporal features for local mode...\n")
    }

    # Add running index
    train_df$row_number <- seq_len(nrow(train_df))

    # Add calendar features
    train_df$year <- lubridate::year(train_df[[date_col]])
    train_df$month <- lubridate::month(train_df[[date_col]])
    train_df$day <- lubridate::day(train_df[[date_col]])
    train_df$day_of_week <- lubridate::wday(train_df[[date_col]], week_start = 1)

    # Add seasonal features (sine/cosine encoding)
    train_df$sin_year <- sin(2 * pi * train_df$row_number / nrow(train_df))
    train_df$cos_year <- cos(2 * pi * train_df$row_number / nrow(train_df))
  }

  # Create TimeSeriesDataFrame AFTER adding features
  tsdf <- create_timeseries_dataframe(train_df, date_col, value_col, item_id_col, tabpfn_mode = tabpfn_mode)

  if (verbose) {
    cat("Initializing TabPFNTimeSeriesPredictor...\n")
    cat("  Mode:", tabpfn_mode, "\n")
    cat("  Output selection:", tabpfn_output_selection, "\n")
  }

  suppressWarnings(
    Sys.setenv("DO_NOT_TRACK" = "1")
  )

  tryCatch({
    tabpfn_ts <- reticulate::import("tabpfn_time_series", convert = FALSE)

    tabpfn_mode_enum <- if (tabpfn_mode == "client") {
      tabpfn_ts$TabPFNMode$CLIENT
    } else if (tabpfn_mode == "local") {
      tabpfn_ts$TabPFNMode$LOCAL
    } else {
      stop("tabpfn_mode must be 'client' or 'local'")
    }

    predictor <- suppressWarnings(
      tabpfn_ts$TabPFNTimeSeriesPredictor(
        tabpfn_mode = tabpfn_mode_enum,
        tabpfn_output_selection = tabpfn_output_selection
      )
    )

  }, error = function(e) {
    stop("Error initializing TabPFNTimeSeriesPredictor: ", e$message)
  })

  model <- list(
    predictor = predictor,
    train_tsdf = tsdf,
    train_df = train_df,
    prediction_length = prediction_length,
    quantiles = quantiles,
    date_col = date_col,
    value_col = value_col,
    item_id_col = item_id_col,
    features = features,
    tabpfn_mode = tabpfn_mode,
    tabpfn_output_selection = tabpfn_output_selection,
    mode = "time_series"
  )

  class(model) <- c("tab_pfn_time_series", "model_fit")

  if (verbose) {
    cat("\\nTime series model ready!\\n")
  }

  return(model)
}


#' Predict method for TabPFN Time Series models
#'
#' @description
#' Generates forecasts for time series using the fitted TabPFN-TS model.
#' Returns point forecasts and probabilistic quantile forecasts.
#'
#' @param object A fitted tab_pfn_time_series model object
#' @param new_data A data frame with time series data (typically the training data)
#' @param prediction_length Integer. Number of steps to forecast (uses model default if NULL)
#' @param quantiles Numeric vector of quantiles for probabilistic forecasting
#' @param verbose Logical. Print progress information
#' @param ... Additional arguments passed to the predict method
#'
#' @return A tibble with forecasts containing:
#'   - item_id (if multiple series)
#'   - timestamp
#'   - point forecast (mean/median based on model configuration)
#'   - Quantile forecasts (e.g., .pred_q100, .pred_q500, .pred_q900)
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#' library(dplyr)
#' library(lubridate)
#'
#' # Train model
#' dates <- seq(as.Date("2020-01-01"), as.Date("2022-12-31"), by = "day")
#' values <- sin(seq(0, 2*pi, length.out = length(dates))) * 10 + rnorm(length(dates), 0, 1)
#' ts_data <- tibble(date = dates, value = values)
#' model <- tab_pfn_time_series(ts_data, prediction_length = 30)
#'
#' # Generate forecasts
#' forecasts <- predict(model, ts_data)
#' }
predict.tab_pfn_time_series <- function(object,
                                    new_data,
                                    prediction_length = NULL,
                                    quantiles = NULL,
                                    verbose = TRUE,
                                    ...) {
  
  rtabpfn:::ensure_python_env()
  
  if (verbose) {
    cat("Generating forecasts...\\n")
  }
  
  new_data <- as.data.frame(new_data, stringsAsFactors = FALSE)
  
  if (!is.null(object$date_col)) {
    new_data[[object$date_col]] <- lubridate::as_datetime(new_data[[object$date_col]])
  }
  
  if (is.null(prediction_length)) {
    prediction_length <- object$prediction_length
  }
  
  if (is.null(quantiles)) {
    quantiles <- object$quantiles
  }
  
  test_df <- create_test_dataframe(
    object$train_df,
    prediction_length,
    object$date_col,
    object$value_col,
    object$item_id_col,
    object$tabpfn_mode
  )
  
  test_tsdf <- create_timeseries_dataframe(
    test_df,
    object$date_col,
    object$value_col,
    object$item_id_col,
    object$tabpfn_mode
  )

  suppressWarnings(
    Sys.setenv("DO_NOT_TRACK" = "1")
  )

  tryCatch({
    forecasts <- suppressWarnings(
      object$predictor$predict(
        train_tsdf = object$train_tsdf,
        test_tsdf = test_tsdf,
        quantiles = as.list(quantiles)
      )
    )
  }, error = function(e) {
    stop("Error generating forecasts: ", e$message)
  })

  result <- convert_forecasts_to_tibble(forecasts, object$item_id_col)

  # Rename timestamp to original date column name if available
  if ("timestamp" %in% names(result) && !is.null(object$date_col)) {
    names(result)[names(result) == "timestamp"] <- object$date_col
  }

  if (verbose) {
    cat("Forecasts generated successfully!\\n")
  }

  return(result)
}


#' Find date/datetime column in data frame
#'
#' @param df Data frame to search
#' @return Name of the first date/datetime column or NULL
#' @keywords internal
find_date_column <- function(df) {
  for (col in names(df)) {
    if (inherits(df[[col]], c("Date", "POSIXct", "POSIXt"))) {
      return(col)
    }
  }
  return(NULL)
}


#' Find value column in data frame
#'
#' @param df Data frame to search
#' @param date_col Name of date column to exclude
#' @param item_id_col Name of item_id column to exclude
#' @return Name of the first numeric column or NULL
#' @keywords internal
find_value_column <- function(df, date_col, item_id_col) {
  excluded_cols <- c(date_col, item_id_col)
  excluded_cols <- excluded_cols[!is.na(excluded_cols)]

  for (col in names(df)) {
    if (!(col %in% excluded_cols) && is.numeric(df[[col]])) {
      return(col)
    }
  }
  return(NULL)
}


#' Create TimeSeriesDataFrame from R data frame
#'
#' @param df R data frame
#' @param date_col Name of date column
#' @param value_col Name of value column
#' @param item_id_col Name of item_id column
#' @return Python TimeSeriesDataFrame object
#' @keywords internal
create_timeseries_dataframe <- function(df, date_col, value_col, item_id_col, tabpfn_mode = NULL) {

  pd <- reticulate::import("pandas", convert = FALSE)

  # Define feature columns for LOCAL mode
  feature_cols <- c("row_number", "year", "month", "day", "day_of_week", "sin_year", "cos_year")

  # For LOCAL mode, include pre-computed features
  has_features <- FALSE
  if (!is.null(tabpfn_mode) && tabpfn_mode == "local") {
    has_features <- all(feature_cols %in% names(df))
  }

  # Build select_cols using the actual column names in df
  if (has_features) {
    # Local mode with features: include item_id, date_col, value_col, and features
    select_cols <- c("item_id", date_col, value_col, feature_cols)
  } else {
    # Client mode or no features: basic columns
    select_cols <- c("item_id", date_col, value_col)
    if (!is.null(tabpfn_mode) && tabpfn_mode == "local" && getOption("rtabpfn.verbose", FALSE)) {
      message("DEBUG: Missing feature cols. df columns: ", paste(names(df), collapse=", "))
      message("DEBUG: Expected feature cols: ", paste(feature_cols, collapse=", "))
    }
  }

  # Ensure required columns are in the dataframe before subsetting
  # Add item_id if not present (for single time series)
  if (!("item_id" %in% names(df))) {
    df$item_id <- "item_0"
  }

  # Only include columns that actually exist in df
  # Build the column list explicitly using original names
  if (has_features) {
    select_cols <- c("item_id", date_col, value_col, feature_cols)
  } else {
    select_cols <- c("item_id", date_col, value_col)
  }

  # Filter to only columns that exist
  select_cols <- select_cols[select_cols %in% names(df)]
  df_subset <- df[, select_cols, drop = FALSE]

  # Rename columns to match TimeSeriesDataFrame expected format
  col_mapping <- c(
    "date" = "timestamp",
    "datetime" = "timestamp",
    "time" = "timestamp",
    "ts" = "timestamp"
  )

  # Rename date column if it matches a mapping key
  if (date_col %in% names(col_mapping) && date_col %in% names(df_subset)) {
    names(df_subset)[names(df_subset) == date_col] <- col_mapping[date_col]
  }

  # Rename item_id column if present with original name
  if (!is.null(item_id_col) && item_id_col %in% names(df_subset)) {
    names(df_subset)[names(df_subset) == item_id_col] <- "item_id"
  }

  # Rename target column
  if (value_col %in% names(df_subset)) {
    names(df_subset)[names(df_subset) == value_col] <- "target"
  }

  df_pandas <- pd$DataFrame(df_subset)

  tabpfn_ts <- reticulate::import("tabpfn_time_series", convert = FALSE)
  tsdf <- tabpfn_ts$TimeSeriesDataFrame$from_data_frame(df_pandas)

  return(tsdf)
}


#' Create test data frame for forecasting horizon
#'
#' @param train_df Training data frame
#' @param prediction_length Number of forecast steps
#' @param date_col Name of date column
#' @param value_col Name of value column
#' @param item_id_col Name of item_id column
#' @param tabpfn_mode TabPFN mode ("local" or "client")
#' @return Data frame with forecast horizon
#' @keywords internal
create_test_dataframe <- function(train_df, prediction_length, date_col, value_col, item_id_col, tabpfn_mode = "client") {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required.")
  }

  train_df[[date_col]] <- lubridate::as_datetime(train_df[[date_col]])

  if (is.null(item_id_col) || !item_id_col %in% names(train_df)) {
    last_timestamp <- max(train_df[[date_col]], na.rm = TRUE)
    freq <- infer_frequency(train_df[[date_col]])
    forecast_dates <- seq(last_timestamp, by = freq, length.out = prediction_length + 1)[-1]

    test_df <- data.frame(
      item_id = rep("item_0", prediction_length),
      timestamp = forecast_dates,
      target = rep(NA_real_, prediction_length)
    )

  } else {
    item_ids <- unique(train_df[[item_id_col]])
    test_dfs <- list()

    for (item_id in item_ids) {
      item_data <- train_df[train_df[[item_id_col]] == item_id, ]
      last_timestamp <- max(item_data[[date_col]], na.rm = TRUE)
      freq <- infer_frequency(item_data[[date_col]])
      forecast_dates <- seq(last_timestamp, by = freq, length.out = prediction_length + 1)[-1]

      test_df_item <- data.frame(
        item_id = rep(item_id, prediction_length),
        timestamp = forecast_dates,
        target = rep(NA_real_, prediction_length)
      )
      test_dfs[[item_id]] <- test_df_item
    }

    test_df <- do.call(rbind, test_dfs)
  }

  # For LOCAL mode, add temporal features (before renaming)
  if (tabpfn_mode == "local") {
    test_df$row_number <- (nrow(train_df) + 1):(nrow(train_df) + prediction_length)
    test_df$year <- lubridate::year(test_df$timestamp)
    test_df$month <- lubridate::month(test_df$timestamp)
    test_df$day <- lubridate::day(test_df$timestamp)
    test_df$day_of_week <- lubridate::wday(test_df$timestamp, week_start = 1)
    n_total <- nrow(train_df) + prediction_length
    test_df$sin_year <- sin(2 * pi * test_df$row_number / n_total)
    test_df$cos_year <- cos(2 * pi * test_df$row_number / n_total)
  }

  names(test_df)[names(test_df) == "timestamp"] <- date_col
  names(test_df)[names(test_df) == "target"] <- value_col

  return(test_df)
}


#' Infer time series frequency
#'
#' @param dates Date/datetime vector
#' @return String representing frequency (compatible with seq() by parameter)
#' @keywords internal
infer_frequency <- function(dates) {
  diffs <- as.numeric(diff(dates), units = "days")

  freq_table <- table(round(diffs, 2))
  most_common <- as.numeric(names(freq_table)[which.max(freq_table)])

  if (most_common <= 1.5) {
    return("day")
  } else if (most_common <= 7.5) {
    return("week")
  } else if (most_common <= 32) {
    return("month")
  } else if (most_common <= 95) {
    return("quarter")
  } else {
    return("year")
  }
}


#' Convert Python forecasts to R tibble
#'
#' @param forecasts Python TimeSeriesDataFrame with forecasts
#' @param item_id_col Name of item_id column
#' @return R tibble with forecasts
#' @keywords internal
convert_forecasts_to_tibble <- function(forecasts, item_id_col) {

  pd <- reticulate::import("pandas", convert = FALSE)

  if (inherits(forecasts, "python.builtin.object")) {
    df_pandas <- pd$DataFrame(forecasts)
  } else if (has_method(forecasts, "to_pandas")) {
    df_pandas <- forecasts$to_pandas()
  } else {
    df_pandas <- forecasts
  }

  df_r <- reticulate::py_to_r(df_pandas)

  if (!is.data.frame(df_r)) {
    df_r <- as.data.frame(df_r)
  }

  df_r <- tibble::as_tibble(df_r)

  # Extract timestamp from pandas MultiIndex
  if (reticulate::py_has_attr(df_pandas, "index")) {
    tryCatch({
      # Try to reset index to get timestamp as column
      df_with_index <- df_pandas$reset_index()
      df_with_index_r <- reticulate::py_to_r(df_with_index)
      if ("timestamp" %in% names(df_with_index_r)) {
        df_r$timestamp <- lubridate::as_datetime(df_with_index_r$timestamp)
      }
    }, error = function(e) {
      # Fallback: try to get index values directly
      py_index <- df_pandas$index
      if (has_method(py_index, "get_level_values")) {
        tryCatch({
          timestamp_values <- py_index$get_level_values("timestamp")
          if (length(timestamp_values) > 0) {
            df_r$timestamp <- lubridate::as_datetime(reticulate::py_to_r(timestamp_values))
          }
        }, error = function(e2) {})
      }
    })
  } else if ("timestamp" %in% names(df_r)) {
    df_r$timestamp <- lubridate::as_datetime(df_r$timestamp)
  }

  return(df_r)
}


#' Check if object has a method
#'
#' @param object Object to check
#' @param method Method name
#' @return Logical
#' @keywords internal
has_method <- function(object, method) {
  tryCatch({
    reticulate::py_has_attr(object, method)
  }, error = function(e) {
    FALSE
  })
}


#' Print method for TabPFN Time Series models
#'
#' @param x A tab_pfn_time_series model object
#' @param ... Additional arguments (not used)
#' @export
print.tab_pfn_time_series <- function(x, ...) {
  cat("TabPFN Time Series Forecasting Model\\n\\n")

  cat("Prediction Length:", x$prediction_length, "\\n")
  cat("Quantiles:", paste(x$quantiles, collapse = ", "), "\\n")
  cat("Mode:", x$tabpfn_mode, "\\n")
  cat("Output Selection:", x$tabpfn_output_selection, "\\n")

  if (!is.null(x$item_id_col)) {
    cat("Multiple Time Series: TRUE\\n")
  } else {
    cat("Multiple Time Series: FALSE\\n")
  }

  invisible(x)
}
