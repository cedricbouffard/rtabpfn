#' @name tab_pfn_ts
#' @title TabPFN Time Series Model for tidymodels
#' @description A TabPFN Time Series (TabPFN-TS) model for zero-shot forecasting
#'   with the tidymodels ecosystem (parsnip, workflows, tune, etc.)

#' Get encoding for tab_pfn_ts model
#'
#' @return A tibble with encoding information
#' @keywords internal
get_encoding.tab_pfn_ts <- function() {
  tibble::tibble(
    model = c("tab_pfn_ts"),
    engine = c("tabpfn_ts"),
    mode = c("regression"),
    predictor_indicators = c("none"),
    compute_intercept = c(FALSE),
    remove_intercept = c(FALSE),
    allow_sparse_x = c(FALSE)
  )
}

NULL

#' TabPFN Time Series model specification
#'
#' @param mode A single character string for the prediction mode. Only "regression" is supported for time series.
#' @param engine A single character string specifying the computational engine. Always "tabpfn_ts" for time series.
#' @param prediction_length Number of steps to forecast ahead (integer)
#' @param quantiles Numeric vector of quantiles for probabilistic forecasting
#' @param tabpfn_output_selection Output selection: "median" or "mean"
#' @param date_col Name of the date/datetime column (NULL for auto-detection)
#' @param value_col Name of the value column (NULL for auto-detection)
#' @param item_id_col Name of the item_id column for multiple time series (NULL for single series)
#' @param ... Additional engine-specific arguments
#'
#' @return A model specification object
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#' library(rtabpfn)
#'
#' # Time series forecasting model
#' ts_spec <- tab_pfn_ts(mode = "regression") %>%
#'   set_engine("tabpfn_ts") %>%
#'   set_args(prediction_length = 12, quantiles = c(0.1, 0.5, 0.9))
#' }
tab_pfn_ts <- function(
    mode = "regression",
    engine = "tabpfn_ts",
    prediction_length = 12,
    quantiles = c(0.1, 0.5, 0.9),
    tabpfn_output_selection = "median",
    date_col = NULL,
    value_col = NULL,
    item_id_col = NULL,
    ...
) {
  if (!mode %in% "regression") {
    stop("For time series forecasting, mode must be 'regression'")
  }

  args <- list(
    prediction_length = rlang::enquo(prediction_length),
    quantiles = rlang::enquo(quantiles),
    tabpfn_output_selection = rlang::enquo(tabpfn_output_selection),
    date_col = rlang::enquo(date_col),
    value_col = rlang::enquo(value_col),
    item_id_col = rlang::enquo(item_id_col),
    ...
  )

  result <- parsnip::new_model_spec(
    cls = "tab_pfn_ts",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = "tab_pfn_ts",
    engine = engine
  )

  result$encoding <- list(
    predictors = "numeric",
    outcomes = "numeric"
  )

  result
}

#' @export
#' @rdname tab_pfn_ts
update.tab_pfn_ts <- function(object, parameters = NULL, prediction_length = NULL,
                          quantiles = NULL,
                          tabpfn_output_selection = NULL, date_col = NULL,
                          value_col = NULL, item_id_col = NULL,
                          fresh = FALSE, ...) {
  parsnip::update_dot_check(...)

  if (fresh) {
    object$args <- list(
      prediction_length = rlang::enquo(prediction_length),
      quantiles = rlang::enquo(quantiles),
      tabpfn_output_selection = rlang::enquo(tabpfn_output_selection),
      date_col = rlang::enquo(date_col),
      value_col = rlang::enquo(value_col),
      item_id_col = rlang::enquo(item_id_col)
    )
  } else {
    if (!is.null(prediction_length)) {
      object$args$prediction_length <- rlang::enquo(prediction_length)
    }
    if (!is.null(quantiles)) {
      object$args$quantiles <- rlang::enquo(quantiles)
    }
    if (!is.null(tabpfn_output_selection)) {
      object$args$tabpfn_output_selection <- rlang::enquo(tabpfn_output_selection)
    }
    if (!is.null(date_col)) {
      object$args$date_col <- rlang::enquo(date_col)
    }
    if (!is.null(value_col)) {
      object$args$value_col <- rlang::enquo(value_col)
    }
    if (!is.null(item_id_col)) {
      object$args$item_id_col <- rlang::enquo(item_id_col)
    }
    object$args <- c(object$args, parameters)
  }

  result <- parsnip::new_model_spec(
    cls = "tab_pfn_ts",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = object$method,
    engine = object$engine
  )

  result$encoding <- list(
    predictors = "numeric",
    outcomes = "numeric"
  )

  result
}

#' Set the model engine for TabPFN Time Series
#'
#' @param x A model specification
#' @param eng A single character string for the engine (always "tabpfn_ts")
#' @param ... Additional engine-specific arguments
#'
#' @return An updated model specification
#' @export
set_engine.tab_pfn_ts <- function(x, eng = c("tabpfn_ts"), ...) {
  eng <- match.arg(eng)
  x$engine <- eng
  x$eng_args <- list(...)
  x
}

#' Set mode for TabPFN Time Series
#'
#' @param object A model specification
#' @param mode A single character string for prediction mode
#' @param ... Not used
#'
#' @return An updated model specification
#' @export
set_mode.tab_pfn_ts <- function(object, mode, ...) {
  if (mode != "regression") {
    stop("`mode` should be 'regression' for time series forecasting", call. = FALSE)
  }

  object$mode <- mode

  object$encoding <- list(
    predictors = "numeric",
    outcomes = "numeric"
  )

  object
}

#' Check that the required arguments are available for fitting
#'
#' @param object A model specification
#' @param x A data frame or matrix of predictors
#' @return TRUE
#' @keywords internal
required_pkgs.tab_pfn_ts <- function(object, ...) {
  c("rtabpfn", "reticulate", "tibble", "lubridate")
}

#' Fit TabPFN Time Series model specification
#'
#' Wrapper function to fit tab_pfn_ts model specifications,
#' working around tidymodels S3 dispatch conflicts.
#'
#' @param model_spec A tab_pfn_ts model specification created by [tab_pfn_ts()]
#' @param data A data frame containing the training data with date and value columns
#' @param ... Additional arguments passed to the fitting function
#'
#' @return A fitted model object of class tab_pfn_ts_fit
#' @export
#'
#' @examples
#' \dontrun{
#' library(rtabpfn)
#' library(tidymodels)
#'
#' # Create time series data
#' ts_data <- tibble(
#'   date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
#'   value = sin(1:100 * 0.1) + rnorm(100, 0, 0.1)
#' )
#'
#' # Create model specification
#' ts_spec <- tab_pfn_ts(mode = "regression") %>%
#'   set_engine("tabpfn_ts") %>%
#'   set_args(prediction_length = 10, quantiles = c(0.1, 0.5, 0.9))
#'
#' # Fit using the wrapper function (avoids tidymodels S3 dispatch issues)
#' ts_fit <- fit_tabpfn_ts(ts_spec, data = ts_data)
#' }
fit_tabpfn_ts <- function(model_spec, data, ...) {
  rtabpfn:::ensure_python_env()

  if (is.null(data)) {
    stop("Training data 'data' is required for time series forecasting.")
  }

  # Auto-detect columns
  date_col <- rtabpfn:::find_date_column(data)
  value_col <- rtabpfn:::find_value_column(data, date_col, NULL)

  args <- model_spec$args

  prediction_length <- rlang::eval_tidy(args$prediction_length)
  quantiles <- rlang::eval_tidy(args$quantiles)
  tabpfn_output_selection <- rlang::eval_tidy(args$tabpfn_output_selection)
  date_col <- rlang::eval_tidy(args$date_col)
  value_col <- rlang::eval_tidy(args$value_col)
  item_id_col <- rlang::eval_tidy(args$item_id_col)

  suppressWarnings(
    Sys.setenv("DO_NOT_TRACK" = "1")
  )

  tryCatch({
    model <- rtabpfn::tab_pfn_time_series(
      train_df = data,
      prediction_length = prediction_length,
      quantiles = quantiles,
      date_col = date_col,
      value_col = value_col,
      item_id_col = item_id_col,
      tabpfn_output_selection = tabpfn_output_selection,
      verbose = FALSE
    )

    class(model) <- c("tab_pfn_ts")

  }, error = function(e) {
    stop("Error fitting TabPFN Time Series model: ", e$message)
  })

  fit <- list(
    model = model,
    spec = model_spec,
    preproc = NULL,
    elapsed = NA_real_
  )
  class(fit) <- c("tab_pfn_ts_fit", "model_fit")
  fit
}

#' Fit a TabPFN Time Series model with xy interface
#'
#' @param object A model specification
#' @param x A data frame with training data (must contain date and value columns)
#' @param y Not used (value column extracted from x)
#' @param control A `parsnip::control_fit()` object
#' @param ... Additional arguments
#'
#' @return A fitted model object
#' @export
fit_xy.tab_pfn_ts <- function(object, x, y = NULL, control = parsnip::control_fit(), ...) {
  rtabpfn:::ensure_python_env()

  args <- object$args

  prediction_length <- rlang::eval_tidy(args$prediction_length)
  quantiles <- rlang::eval_tidy(args$quantiles)
  tabpfn_output_selection <- rlang::eval_tidy(args$tabpfn_output_selection)
  date_col <- rlang::eval_tidy(args$date_col)
  value_col <- rlang::eval_tidy(args$value_col)
  item_id_col <- rlang::eval_tidy(args$item_id_col)

  suppressWarnings(
    Sys.setenv("DO_NOT_TRACK" = "1")
  )

  tryCatch({
    model <- rtabpfn::tab_pfn_time_series(
      train_df = x,
      prediction_length = prediction_length,
      quantiles = quantiles,
      date_col = date_col,
      value_col = value_col,
      item_id_col = item_id_col,
      tabpfn_output_selection = tabpfn_output_selection,
      verbose = FALSE
    )

    class(model) <- c("tab_pfn_ts")

  }, error = function(e) {
    stop("Error fitting TabPFN Time Series model: ", e$message)
  })

  fit <- list(
    model = model,
    spec = object,
    preproc = NULL,
    elapsed = NA_real_
  )
  class(fit) <- c("tab_pfn_ts_fit", "model_fit")
  fit
}

#' Make predictions from a fitted TabPFN Time Series model
#'
#' @param object A fitted model object
#' @param new_data A data frame of time series data
#' @param type A single character string for the prediction type
#' @param ... Additional arguments
#'
#' @return A tibble of forecasts
#' @export
#' @keywords internal
predict.tab_pfn_ts_fit <- function(object, new_data, type = NULL, ...) {
  rtabpfn:::ensure_python_env()

  spec <- object$spec

  new_data <- as.data.frame(new_data)

  preds <- predict(
    object$model,
    new_data = new_data,
    prediction_length = rlang::eval_tidy(spec$args$prediction_length),
    quantiles = rlang::eval_tidy(spec$args$quantiles),
    verbose = FALSE
  )

  tibble::as_tibble(preds)
}

#' @export
#' @keywords internal
required_pkgs.tab_pfn_ts_fit <- function(object, ...) {
  required_pkgs(object$spec)
}

#' Print method for TabPFN Time Series model specification
#'
#' @description
#' Prints the model specification details for a TabPFN Time Series model.
#'
#' @param x A model specification
#' @param ... Additional arguments (not used)
#'
#' @return The model specification (invisibly)
#' @export
print.tab_pfn_ts <- function(x, ...) {
  cat("TabPFN Time Series Model Specification\n\n")
  cat("Main Arguments:\n")
  cat("  prediction_length = ", rlang::eval_tidy(x$args$prediction_length), "\n", sep = "")
  cat("  quantiles = ", paste(rlang::eval_tidy(x$args$quantiles), collapse = ", "), "\n", sep = "")
  cat("  tabpfn_output_selection = '", rlang::eval_tidy(x$args$tabpfn_output_selection), "'\n", sep = "")
  cat("\nComputational engine: ", x$engine, "\n", sep = "")

  invisible(x)
}

#' Print method for fitted TabPFN Time Series model
#'
#' @description
#' Prints details of a fitted TabPFN Time Series model.
#'
#' @param x A fitted model object
#' @param ... Additional arguments (not used)
#'
#' @return The model object (invisibly)
#' @export
print.tab_pfn_ts_fit <- function(x, ...) {
  print(x$spec, ...)
  cat("\\nModel fit:\\n")
  print(x$model)
}
