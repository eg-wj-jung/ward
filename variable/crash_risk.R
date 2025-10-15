#' Crash risk metrics based on Chen et al. (2001)
#'
#' This module provides helper functions to compute the firm-level crash risk
#' measures NCSKEW and DUVOL from weekly return data. The implementation follows
#' the definitions in Chen, Hong, and Stein (2001), "Forecasting crashes: Trading
#' volume, past returns, and conditional skewness in stock prices".
#'
#' The workflow implemented here mirrors the description in the provided
#' reference image and consists of three major steps:
#'   1. Fit the three-factor market model with the market return at t-1, t, and
#'      t+1 to obtain firm-specific weekly residual returns (\\eqn{W_{i,t}}).
#'   2. Compute the negative conditional skewness (NCSKEW) from the residuals.
#'   3. Compute the down-to-up volatility measure (DUVOL) from the residuals.
#'
#' @section Usage:
#' Supply a data frame that contains at least the firm identifier, a week/date
#' column, the firm's raw return, and the corresponding market return. Optionally
#' a period column (e.g. year-quarter) can be provided to compute the measures
#' over sub-periods.
#'
#' @examples
#' \dontrun{
#' crash_data <- compute_crash_risk(data = weekly_returns,
#'                                   firm_col = "permno",
#'                                   date_col = "week",
#'                                   ret_col = "ret",
#'                                   market_col = "market_ret",
#'                                   period_col = "fiscal_quarter")
#' }
NULL

#' Compute the normalized negative skewness (NCSKEW).
#'
#' @param residuals Numeric vector of firm-specific weekly residual returns.
#' @param min_obs Minimum number of observations required to return a finite
#'   value. Defaults to 8 which is customary in the crash-risk literature.
#'
#' @return A single numeric value equal to the negative of the normalized sample
#'   skewness. Returns `NA_real_` if the requirements are not satisfied.
calc_ncskew <- function(residuals, min_obs = 8L) {
  residuals <- residuals[is.finite(residuals)]
  n <- length(residuals)

  if (n < min_obs || n < 3L) {
    return(NA_real_)
  }

  mean_resid <- mean(residuals)
  centered <- residuals - mean_resid

  sum_sq <- sum(centered^2)
  sum_cu <- sum(centered^3)

  if (!is.finite(sum_sq) || sum_sq <= 0 || n <= 2L) {
    return(NA_real_)
  }

  skew <- (n * sqrt(n - 1)) / (n - 2) * (sum_cu / (sum_sq^(3 / 2)))
  -skew
}

#' Compute the down-to-up volatility ratio (DUVOL).
#'
#' @param residuals Numeric vector of firm-specific weekly residual returns.
#' @param min_obs Minimum number of observations required to return a finite
#'   value. Defaults to 8.
#'
#' @return A single numeric value representing the log ratio of the volatility
#'   of down weeks to up weeks. Returns `NA_real_` when the computation is not
#'   feasible.
calc_duvol <- function(residuals, min_obs = 8L) {
  residuals <- residuals[is.finite(residuals)]
  n <- length(residuals)

  if (n < min_obs || n < 4L) {
    return(NA_real_)
  }

  mean_resid <- mean(residuals)
  up_idx <- residuals >= mean_resid
  up_resid <- residuals[up_idx]
  down_resid <- residuals[!up_idx]

  n_up <- length(up_resid)
  n_down <- length(down_resid)

  if (n_up < 2L || n_down < 2L) {
    return(NA_real_)
  }

  var_up <- sum((up_resid - mean(up_resid))^2) / (n_up - 1)
  var_down <- sum((down_resid - mean(down_resid))^2) / (n_down - 1)

  if (!is.finite(var_up) || !is.finite(var_down) || var_up <= 0 || var_down <= 0) {
    return(NA_real_)
  }

  log(var_down / var_up)
}

#' Compute firm-level crash risk metrics.
#'
#' @param data A data frame containing at least the columns specified by
#'   `firm_col`, `date_col`, `ret_col`, and `market_col`.
#' @param firm_col Column name (string) identifying the firm/security.
#' @param date_col Column name (string) that orders the weekly observations
#'   chronologically (e.g. a Date or integer week identifier).
#' @param ret_col Column name (string) of the firm's weekly return.
#' @param market_col Column name (string) of the corresponding market return.
#' @param period_col Optional column name (string). When provided, NCSKEW and
#'   DUVOL are computed separately for each unique combination of `firm_col` and
#'   `period_col`.
#' @param min_obs Minimum number of residual observations required for each
#'   measure. Defaults to 8.
#'
#' @return A data frame with the grouping columns, the number of observations
#'   used in the regression (`n_model`), the number of residuals used in the
#'   crash-risk statistics (`n_residual`), and the resulting `ncskew` and
#'   `duvol` metrics.
#'
#' @export
compute_crash_risk <- function(data,
                               firm_col = "firm_id",
                               date_col = "date",
                               ret_col = "ret",
                               market_col = "market_ret",
                               period_col = NULL,
                               min_obs = 8L) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  required_cols <- c(firm_col, date_col, ret_col, market_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from `data`: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!is.null(period_col) && !(period_col %in% names(data))) {
    stop("`period_col` was supplied but is not present in `data`.")
  }

  if (!is.numeric(min_obs) || length(min_obs) != 1L || is.na(min_obs)) {
    stop("`min_obs` must be a single, non-missing numeric value.")
  }

  min_obs <- as.integer(min_obs)
  if (min_obs < 3L) {
    stop("`min_obs` must be at least 3 to allow the computations.")
  }

  data <- data[order(data[[date_col]]), ]

  market_df <- unique(data[, c(date_col, market_col)])
  market_df <- market_df[order(market_df[[date_col]]), ]

  if (nrow(market_df) > 1L) {
    market_df$market_lag <- c(NA, market_df[[market_col]][-nrow(market_df)])
    market_df$market_lead <- c(market_df[[market_col]][-1L], NA)
  } else {
    market_df$market_lag <- NA_real_
    market_df$market_lead <- NA_real_
  }

  match_idx <- match(data[[date_col]], market_df[[date_col]])
  data$market_lag <- market_df$market_lag[match_idx]
  data$market_lead <- market_df$market_lead[match_idx]

  group_cols <- c(firm_col, if (!is.null(period_col)) period_col)

  if (length(group_cols) == 0) {
    stop("At least one grouping column must be specified.")
  }

  split_key <- interaction(data[group_cols], drop = TRUE, sep = "\u0001")
  groups <- split(seq_len(nrow(data)), f = split_key, drop = TRUE)

  build_result <- function(indices) {
    df <- data[indices, , drop = FALSE]
    df <- df[order(df[[date_col]]), , drop = FALSE]

    model_idx <- is.finite(df[[ret_col]]) &
      is.finite(df[[market_col]]) &
      is.finite(df$market_lag) &
      is.finite(df$market_lead)

    model_df <- df[model_idx, , drop = FALSE]
    n_model <- nrow(model_df)

    result <- data.frame(n_model = n_model,
                         n_residual = NA_integer_,
                         ncskew = NA_real_,
                         duvol = NA_real_,
                         stringsAsFactors = FALSE)

    if (n_model < 4L) {
      return(result)
    }

    regressors <- data.frame(
      ret = as.numeric(model_df[[ret_col]]),
      lag = as.numeric(model_df$market_lag),
      curr = as.numeric(model_df[[market_col]]),
      lead = as.numeric(model_df$market_lead)
    )

    if (any(!is.finite(as.matrix(regressors)))) {
      return(result)
    }

    fit <- stats::lm(ret ~ lag + curr + lead, data = regressors)
    resid <- stats::residuals(fit)
    resid <- as.numeric(resid)

    result$n_residual <- length(resid)
    result$ncskew <- calc_ncskew(resid, min_obs = min_obs)
    result$duvol <- calc_duvol(resid, min_obs = min_obs)
    result
  }

  res_list <- lapply(groups, build_result)
  result_df <- do.call(rbind, res_list)

  group_values <- data[match(names(groups), split_key), group_cols, drop = FALSE]
  rownames(group_values) <- NULL

  final <- cbind(group_values, result_df)
  rownames(final) <- NULL
  final
}
