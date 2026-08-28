# Checks for Forecasting-trends/R/forecasting.R
# Run from the repository root:  Rscript tests/run_all.R

source("Forecasting-trends/R/forecasting.R")

ok <- function(label) cat("  ok -", label, "\n")

# --- clean_hits ------------------------------------------------------------
# Google Trends reports very low interest as the string "<1", which turns the
# whole column into text.
stopifnot(identical(clean_hits(c("5", "<1", "100")), c(5, 1, 100)))
stopifnot(identical(clean_hits(c(5L, 7L)), c(5, 7)))
stopifnot(is.na(clean_hits("   ")))
ok("clean_hits strips the \"<1\" marker and always returns numbers")

# --- granularity -----------------------------------------------------------
hourly <- as.POSIXct("2026-01-01", tz = "UTC") + (0:47) * 3600
daily  <- as.POSIXct("2026-01-01", tz = "UTC") + (0:59) * 86400
weekly <- as.POSIXct("2026-01-01", tz = "UTC") + (0:51) * 7 * 86400

stopifnot(infer_step_seconds(hourly) == 3600)
stopifnot(infer_step_seconds(daily) == 86400)
stopifnot(infer_step_seconds(weekly) == 7 * 86400)
stopifnot(infer_step_seconds(daily[1]) == 86400)  # single point falls back
ok("infer_step_seconds reads hourly, daily and weekly spacing")

stopifnot(describe_step(3600) == "hour",
          describe_step(86400) == "day",
          describe_step(7 * 86400) == "week",
          describe_step(30 * 86400) == "month")
ok("describe_step names each granularity")

stopifnot(suggest_horizon(3600)[["value"]] == 48,
          suggest_horizon(7 * 86400)[["max"]] == 156)
ok("suggest_horizon scales the default forecast length")

# --- drop_incomplete_tail --------------------------------------------------
# The previous app filtered on Sys.Date() - 1 regardless of granularity, which
# deleted every row of an hourly series.
recent_hourly <- data.frame(
  ds = seq(Sys.time() - 10 * 3600, Sys.time(), by = 3600),
  y = 1:11
)
trimmed <- drop_incomplete_tail(recent_hourly, 3600)
stopifnot(nrow(trimmed) == 10, max(trimmed$ds) < max(recent_hourly$ds))
ok("drop_incomplete_tail removes only the in-progress period")

old_daily <- data.frame(ds = daily, y = seq_along(daily))
stopifnot(nrow(drop_incomplete_tail(old_daily, 86400)) == nrow(old_daily))
stopifnot(nrow(drop_incomplete_tail(old_daily[0, ], 86400)) == 0)
ok("a series that already ended keeps all of its rows")

# --- robust scoring --------------------------------------------------------
# A mean/sd score is dragged toward a big spike and hides it; median/MAD is not.
series <- c(rep(10, 40), 400, rep(10, 40))
z_robust <- robust_z(series)
stopifnot(abs(z_robust[41]) > 10)
stopifnot(all(abs(z_robust[-41]) < 1e-8))
ok("robust_z exposes a spike that a mean/sd score would mask")

stopifnot(all(robust_z(rep(7, 20)) == 0))  # zero spread must not divide by zero
ok("robust_z survives a constant series")

# A step change is unremarkable globally but obvious against its neighbours.
local_spike <- c(seq(10, 50, length.out = 40), 95, seq(50, 90, length.out = 40))
rolling <- rolling_robust_z(local_spike, window = 15)
stopifnot(which.max(abs(rolling)) == 41)
stopifnot(length(rolling_robust_z(numeric(0))) == 0)
ok("rolling_robust_z finds a locally unusual point")

# --- detect_anomalies ------------------------------------------------------
history <- data.frame(ds = daily[1:41], y = c(rep(10, 40), 400))

flagged <- detect_anomalies(history, method = "global", sensitivity = 3.5)
stopifnot(sum(flagged$anomaly) == 1, flagged$anomaly[41])
ok("detect_anomalies flags the spike and nothing else")

loose <- detect_anomalies(history, method = "global", sensitivity = 1.5)
tight <- detect_anomalies(history, method = "global", sensitivity = 8)
stopifnot(sum(loose$anomaly) >= sum(tight$anomaly))
ok("the sensitivity slider actually changes how much is flagged")

fitted <- data.frame(ds = history$ds, yhat = rep(10, 41))
residual <- detect_anomalies(history, method = "residual", sensitivity = 3.5,
                             fitted = fitted)
stopifnot(sum(residual$anomaly) == 1, residual$anomaly[41])
stopifnot(is.null(detect_anomalies(history, method = "residual", fitted = NULL)))
ok("residual mode scores against the model and refuses to guess without it")

stopifnot(is.null(detect_anomalies(NULL)))
stopifnot(is.null(detect_anomalies(history[0, ])))
ok("empty input returns NULL rather than erroring")

stopifnot(length(TIME_CHOICES) == 9, "all" %in% TIME_CHOICES)
stopifnot(setequal(ANOMALY_METHODS, c("rolling", "global", "residual")))
ok("choice vectors are intact")
