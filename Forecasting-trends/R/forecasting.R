# ---------------------------------------------------------------------------
# forecasting.R
#
# Helpers for the Search Trends forecasting app: cleaning what Google Trends
# returns, fitting Prophet at whatever granularity the data actually has, and
# scoring the result.
#
# This file belongs to this app (it is not part of shared/).
# ---------------------------------------------------------------------------

#' Google Trends reports very low interest as the string "<1".
#'
#' Left as-is that turns the whole column into text and every downstream
#' calculation into NA, so strip the marker and coerce to a number.
clean_hits <- function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- gsub("<", "", as.character(x), fixed = TRUE)
    x <- gsub("^\\s*$", NA, x)
  }
  suppressWarnings(as.numeric(x))
}

#' Median spacing between observations, in seconds.
#'
#' Google Trends changes granularity with the time span (hourly for "last
#' day", weekly for "last 5 years"), so nothing downstream may assume days.
infer_step_seconds <- function(ds) {
  if (length(ds) < 2L) return(86400)
  step <- stats::median(diff(as.numeric(as.POSIXct(ds, tz = "UTC"))), na.rm = TRUE)
  if (!is.finite(step) || step <= 0) 86400 else step
}

#' Human name for a spacing, used for slider labels and the AI briefing.
describe_step <- function(step_seconds) {
  if (step_seconds < 3600 * 2) return("hour")
  if (step_seconds < 86400 * 2) return("day")
  if (step_seconds < 86400 * 10) return("week")
  if (step_seconds < 86400 * 45) return("month")
  "period"
}

#' A sensible forecast horizon, in periods, for this granularity.
suggest_horizon <- function(step_seconds) {
  switch(describe_step(step_seconds),
         hour  = c(value = 48,  max = 336),
         day   = c(value = 90,  max = 730),
         week  = c(value = 26,  max = 156),
         month = c(value = 12,  max = 60),
         c(value = 12, max = 60))
}

#' Drop the final observation when it covers a period that has not finished.
#'
#' The old code always filtered on `Sys.Date() - 1`, which silently deleted
#' every row of an hourly series and left nothing to plot.
drop_incomplete_tail <- function(df, step_seconds) {
  if (!nrow(df)) return(df)
  latest <- max(df$ds, na.rm = TRUE)
  if (as.numeric(Sys.time()) - as.numeric(latest) < step_seconds) {
    df <- df[df$ds < latest, , drop = FALSE]
  }
  df
}

#' Fit Prophet with the options the UI exposes.
fit_prophet_model <- function(history,
                              growth = "linear",
                              seasonality_mode = "additive",
                              changepoint_prior_scale = 0.05,
                              holidays_country = NULL) {

  model <- prophet::prophet(
    growth = growth,
    seasonality.mode = seasonality_mode,
    changepoint.prior.scale = changepoint_prior_scale,
    interval.width = 0.8
  )

  if (!is.null(holidays_country) && nzchar(holidays_country)) {
    # Prophet only ships holiday tables for some countries; an unknown one
    # should cost the user the holidays, not the forecast.
    model <- tryCatch(
      prophet::add_country_holidays(model, country_name = holidays_country),
      error = function(e) model
    )
  }

  prophet::fit.prophet(model, history)
}

#' Hold out the tail of the series, refit, and score the predictions.
#'
#' Gives the forecast an honest error bar instead of asking the user to trust
#' an unvalidated curve.
prophet_backtest <- function(history, model_args = list(), min_points = 30L) {
  n <- nrow(history)
  if (n < min_points) return(NULL)

  holdout <- max(3L, min(60L, floor(n * 0.2)))
  train <- history[seq_len(n - holdout), , drop = FALSE]
  test  <- history[seq.int(n - holdout + 1L, n), , drop = FALSE]

  fitted <- tryCatch(
    do.call(fit_prophet_model, c(list(history = train), model_args)),
    error = function(e) NULL
  )
  if (is.null(fitted)) return(NULL)

  predicted <- tryCatch(stats::predict(fitted, test[, "ds", drop = FALSE]),
                        error = function(e) NULL)
  if (is.null(predicted)) return(NULL)

  errors <- test$y - predicted$yhat
  denominator <- ifelse(test$y == 0, NA_real_, test$y)

  list(
    holdout  = holdout,
    mae      = mean(abs(errors), na.rm = TRUE),
    rmse     = sqrt(mean(errors^2, na.rm = TRUE)),
    mape     = mean(abs(errors / denominator), na.rm = TRUE) * 100,
    coverage = mean(test$y >= predicted$yhat_lower &
                      test$y <= predicted$yhat_upper, na.rm = TRUE) * 100
  )
}

# --- Anomaly detection -----------------------------------------------------

#' A spread estimate that a single outlier cannot inflate.
#'
#' MAD first. On a flat series MAD collapses to zero (most Google Trends
#' series sit on a plateau for long stretches), and falling straight back to
#' sd would re-introduce the masking problem, because the spike being looked
#' for is itself most of the sd. The mean absolute deviation from the median
#' sits in between: still centred on the median, but non-zero as soon as
#' anything at all moves.
robust_scale <- function(x, centre) {
  spread <- stats::mad(x, center = centre, na.rm = TRUE)
  if (is.finite(spread) && spread > 0) return(spread)
  spread <- mean(abs(x - centre), na.rm = TRUE)
  if (is.finite(spread) && spread > 0) return(spread)
  spread <- stats::sd(x, na.rm = TRUE)
  if (is.finite(spread) && spread > 0) return(spread)
  0
}

#' Robust z-score: median and MAD instead of mean and sd.
#'
#' A spike big enough to be worth flagging also drags a mean/sd score toward
#' itself and hides. The median/MAD pair does not move.
robust_z <- function(x) {
  centre <- stats::median(x, na.rm = TRUE)
  spread <- robust_scale(x, centre)
  if (spread == 0) return(rep(0, length(x)))
  (x - centre) / spread
}

#' Robust z-score against a local window rather than the whole series.
#'
#' Catches a spike that is unremarkable next to the series maximum but far
#' out of line with the weeks either side of it.
rolling_robust_z <- function(x, window = 15L) {
  n <- length(x)
  if (n == 0L) return(numeric(0))
  half <- max(1L, as.integer(window) %/% 2L)
  vapply(seq_len(n), function(i) {
    reference <- x[max(1L, i - half):min(n, i + half)]
    centre <- stats::median(reference, na.rm = TRUE)
    spread <- robust_scale(reference, centre)
    if (spread == 0) return(0)
    (x[i] - centre) / spread
  }, numeric(1))
}

ANOMALY_METHODS <- c(
  "Local window (robust)"     = "rolling",
  "Whole series (robust)"     = "global",
  "Against the Prophet fit"   = "residual"
)

#' Flag unusual observations.
#'
#' @param history data frame with ds and y.
#' @param method one of ANOMALY_METHODS.
#' @param sensitivity threshold in robust z units; lower flags more.
#' @param fitted optional data frame with ds and yhat, for method "residual".
#' @return `history` plus `score` and `anomaly` columns, or NULL.
detect_anomalies <- function(history, method = "rolling", sensitivity = 3.5,
                             window = 15L, fitted = NULL) {
  if (is.null(history) || !nrow(history)) return(NULL)

  score <- switch(
    method,
    global = robust_z(history$y),
    residual = {
      if (is.null(fitted)) return(NULL)
      matched <- fitted$yhat[match(history$ds, fitted$ds)]
      if (all(is.na(matched))) return(NULL)
      robust_z(history$y - matched)
    },
    rolling_robust_z(history$y, window = window)
  )

  history$score <- score
  history$anomaly <- is.finite(score) & abs(score) >= sensitivity
  history
}

# --- Countries and time spans ----------------------------------------------

#' Country list for the region selector, worldwide first.
#'
#' "Worldwide" has to map to an empty geo string. The old app used the literal
#' "Worldwide", which Google Trends rejects, so that option never worked.
trends_regions <- function() {
  regions <- countrycode::codelist
  regions <- regions[!is.na(regions$iso2c), c("iso2c", "country.name.en")]
  regions <- regions[order(regions$country.name.en), ]
  c(c("Worldwide" = ""), stats::setNames(regions$iso2c, regions$country.name.en))
}

TIME_CHOICES <- c(
  "Last hour"      = "now 1-H",
  "Last 4 hours"   = "now 4-H",
  "Last day"       = "now 1-d",
  "Last 7 days"    = "now 7-d",
  "Last month"     = "today 1-m",
  "Last 3 months"  = "today 3-m",
  "Last 12 months" = "today 12-m",
  "Last 5 years"   = "today+5-y",
  "Since 2004"     = "all"
)
