# ---------------------------------------------------------------------------
# Search Trends Forecasting
#
# Pulls Google Trends interest over time, forecasts it with Prophet, flags
# unusual observations, and (optionally) has an OpenAI model write the
# read-out.
#
# Originally by Abel Hernandez Garcia // hi@abelhga.com // www.abelhga.com
#
# Packages: shiny, bslib, dplyr, plotly, DT, gtrendsR, prophet (whose ggplot2
#           themes the seasonality plots), lubridate, countrycode,
#           shinycssloaders, httr, jsonlite.
# See ../install_dependencies.R
# ---------------------------------------------------------------------------

library(shiny)
library(dplyr)

# Shiny sources everything in R/ automatically when the app is launched from
# its own folder. This makes `source("app.R")` work too.
for (.f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(.f)

MAX_KEYWORDS <- 5L  # Google Trends compares at most five terms at a time

# The site's accents first (teal, magenta, sand), then two compatible extras
# for four- and five-term comparisons.
SERIES_COLOURS <- c("#0e7c6b", "#b8336a", "#c8973f", "#4C78A8", "#8C6BB1")

series_colour <- function(i) SERIES_COLOURS[((i - 1) %% length(SERIES_COLOURS)) + 1]

#' Hex colour to a plotly rgba() string, for translucent ribbons.
rgba <- function(hex, alpha) {
  v <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

# Sentinel for "every fetched term at once" in the series and insights
# selectors. Not a plausible search term, so it cannot collide with one.
ALL_SERIES <- "__all_terms__"

REGIONS <- trends_regions()

# --- UI --------------------------------------------------------------------

ui <- bslib::page_sidebar(
  title = "Search Trends Forecasting",
  theme = app_theme(FALSE),
  fillable = TRUE,

  sidebar = bslib::sidebar(
    width = 340,
    open = "desktop",

    shiny::textInput("keywords", "Keywords (comma separated)",
                     value = "Lavender, Daffodil, Bluebell"),
    shiny::helpText(sprintf("Up to %d terms. Interest is relative to the busiest
                             point across the terms you compare.", MAX_KEYWORDS)),

    shiny::selectInput("geo", "Region", choices = REGIONS, selected = "GB"),
    shiny::selectInput("time", "Time span", choices = TIME_CHOICES, selected = "today+5-y"),
    shiny::helpText("Google returns hourly data for short spans and weekly data for
                     long ones; the app adapts the forecast to whichever it gets."),
    shiny::checkboxInput("fetch_related", "Also fetch related queries", FALSE),
    shiny::helpText("Adds a Related queries tab (top and rising searches). It is a
                     second, heavier request, so it hits Google's rate limit sooner."),

    shiny::actionButton("fetch", "Fetch trends", class = "btn btn-primary w-100",
                        icon = shiny::icon("cloud-arrow-down")),

    shiny::hr(),

    shiny::selectInput("series", "Series to forecast", choices = character(0)),

    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Forecast model",
        shiny::sliderInput("horizon", "Forecast horizon (periods)",
                           min = 1, max = 730, value = 90, step = 1),
        shiny::selectInput("growth", "Trend",
                           choices = c("Linear" = "linear",
                                       "Logistic (saturates near 100)" = "logistic",
                                       "Flat" = "flat")),
        shiny::helpText("Logistic respects the 0-100 ceiling of the Trends index,
                         so a rising forecast levels off instead of leaving the scale."),
        shiny::selectInput("seasonality_mode", "Seasonality",
                           choices = c("Additive" = "additive",
                                       "Multiplicative" = "multiplicative")),
        shiny::sliderInput("changepoint_prior", "Trend flexibility",
                           min = 0.01, max = 0.5, value = 0.05, step = 0.01),
        shiny::helpText("Higher values let the trend bend more sharply to follow
                         recent movement."),
        shiny::checkboxInput("holidays", "Add public holidays for the selected region", FALSE),
        shiny::checkboxInput("backtest", "Score the model on held-out data", TRUE)
      ),
      bslib::accordion_panel(
        "Anomalies",
        shiny::selectInput("anomaly_method", "Compare each point against",
                           choices = ANOMALY_METHODS, selected = "rolling"),
        shiny::sliderInput("anomaly_sensitivity", "Threshold (robust z)",
                           min = 1.5, max = 8, value = 3.5, step = 0.5),
        shiny::sliderInput("anomaly_window", "Local window (periods)",
                           min = 5, max = 61, value = 15, step = 2),
        shiny::helpText("Lower the threshold to flag more points. The old fixed
                         cut-off of 20 points ignored how noisy the series was.")
      ),
      bslib::accordion_panel(
        "Display",
        shiny::checkboxInput("dark_mode", "Dark mode", FALSE)
      )
    )
  ),

  shiny::tags$head(shiny::tags$style(app_styles())),

  shiny::div(
    class = "app-subtitle mb-2",
    "Relative search interest from Google Trends, forecast with Prophet."
  ),

  shiny::uiOutput("kpis"),

  bslib::navset_card_tab(
    id = "tabs",
    height = "740px",

    bslib::nav_panel(
      "Interest over time",
      icon = shiny::icon("chart-line"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("trends_plot", height = "600px"),
        type = 8, color = "#0e7c6b"
      )
    ),

    bslib::nav_panel(
      "Forecast",
      icon = shiny::icon("arrow-trend-up"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("forecast_plot", height = "540px"),
        type = 8, color = "#0e7c6b"
      ),
      shiny::uiOutput("accuracy_note")
    ),

    bslib::nav_panel(
      "Seasonality",
      icon = shiny::icon("calendar-days"),
      shiny::div(class = "status-note mb-2",
                 "How Prophet splits the series into trend and repeating cycles."),
      shinycssloaders::withSpinner(
        shiny::plotOutput("components_plot", height = "620px"),
        type = 8, color = "#0e7c6b"
      )
    ),

    bslib::nav_panel(
      "Anomalies",
      icon = shiny::icon("triangle-exclamation"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("anomaly_plot", height = "420px"),
        type = 8, color = "#0e7c6b"
      ),
      DT::DTOutput("anomaly_table")
    ),

    bslib::nav_panel(
      "Related queries",
      icon = shiny::icon("magnifying-glass-plus"),
      shiny::uiOutput("related_note"),
      shiny::div(
        class = "d-flex gap-2 mb-2",
        shiny::downloadButton("dl_related", "Related CSV",
                              class = "btn-sm btn-outline-secondary")
      ),
      DT::DTOutput("related_table")
    ),

    bslib::nav_panel(
      "Data",
      icon = shiny::icon("table"),
      shiny::div(
        class = "d-flex gap-2 mb-2",
        shiny::downloadButton("dl_history", "Observed CSV",
                              class = "btn-sm btn-outline-secondary"),
        shiny::downloadButton("dl_forecast", "Forecast CSV",
                              class = "btn-sm btn-outline-secondary")
      ),
      DT::DTOutput("data_table")
    ),

    bslib::nav_panel(
      "AI insights",
      icon = shiny::icon("robot"),
      shiny::selectInput("ai_scope", "Insights about", choices = character(0),
                         width = "260px"),
      aiInsightsUI("ai", label = "Interpret this forecast")
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  shiny::observeEvent(input$dark_mode, {
    session$setCurrentTheme(app_theme(isTRUE(input$dark_mode)))
  }, ignoreInit = TRUE)

  requested_keywords <- shiny::reactive({
    parts <- trimws(unlist(strsplit(input$keywords %||% "", ",")))
    unique(parts[nzchar(parts)])
  })

  # --- data ----------------------------------------------------------------
  trends <- shiny::eventReactive(input$fetch, {
    keywords <- requested_keywords()

    if (!length(keywords)) {
      shiny::showNotification("Enter at least one keyword.", type = "warning")
      return(NULL)
    }

    # Five terms per request is Google Trends' own comparison limit; say so
    # instead of silently dropping what the user typed.
    if (length(keywords) > MAX_KEYWORDS) {
      shiny::showNotification(
        sprintf("Google Trends compares at most %d terms per request; using the first %d (%s).",
                MAX_KEYWORDS, MAX_KEYWORDS,
                paste(utils::head(keywords, MAX_KEYWORDS), collapse = ", ")),
        type = "warning", duration = 8
      )
      keywords <- utils::head(keywords, MAX_KEYWORDS)
    }

    result <- shiny::withProgress(message = "Asking Google Trends", value = 0.4, {
      tryCatch(
        gtrendsR::gtrends(keyword = keywords, geo = input$geo, time = input$time,
                          tz = 0, onlyInterest = !isTRUE(input$fetch_related)),
        error = function(e) e
      )
    })

    if (inherits(result, "error")) {
      shiny::showNotification(
        paste("Google Trends refused the request:", conditionMessage(result),
              "- it rate limits heavily, so wait a moment and try again."),
        type = "error", duration = 10
      )
      return(NULL)
    }

    interest <- result$interest_over_time
    if (is.null(interest) || !nrow(interest)) {
      shiny::showNotification(
        "Google Trends returned no data for that combination of keywords, region and span.",
        type = "warning", duration = 8
      )
      return(NULL)
    }

    interest <- interest %>%
      mutate(
        ds = as.POSIXct(.data$date, tz = "UTC"),
        y = clean_hits(.data$hits),
        keyword = as.character(.data$keyword)
      ) %>%
      filter(!is.na(.data$ds), !is.na(.data$y)) %>%
      arrange(.data$keyword, .data$ds)

    if (!nrow(interest)) return(NULL)

    step <- infer_step_seconds(sort(unique(interest$ds)))

    # Trim the period that is still in progress, whatever its length. The old
    # `date < Sys.Date() - 1` rule deleted every row of an hourly series.
    interest <- interest %>%
      group_by(.data$keyword) %>%
      group_modify(~ drop_incomplete_tail(as.data.frame(.x), step)) %>%
      ungroup()

    attr(interest, "step_seconds") <- step
    attr(interest, "related") <- result$related_queries
    interest
  })

  related_queries <- shiny::reactive({
    data <- trends()
    if (is.null(data)) return(NULL)
    related <- attr(data, "related")
    if (is.null(related) || !nrow(related)) return(NULL)
    data.frame(
      keyword = as.character(related$keyword),
      type    = as.character(related$related_queries),
      query   = as.character(related$value),
      value   = as.character(related$subject),
      stringsAsFactors = FALSE
    )
  })

  # Point the series picker and the horizon slider at what actually came back.
  # With several terms, both pickers gain an "all terms" option and start
  # there: a comparison is what asking for several terms means.
  shiny::observeEvent(trends(), {
    data <- trends()
    shiny::req(data)

    available <- unique(data$keyword)
    series_choices <- if (length(available) > 1) {
      stats::setNames(c(ALL_SERIES, available),
                      c("All terms (one forecast each)", available))
    } else available
    shiny::updateSelectInput(session, "series", choices = series_choices,
                             selected = series_choices[[1]])

    scope_choices <- if (length(available) > 1) {
      stats::setNames(c(ALL_SERIES, available),
                      c("All terms, compared", available))
    } else available
    shiny::updateSelectInput(session, "ai_scope", choices = scope_choices,
                             selected = scope_choices[[1]])

    horizon <- suggest_horizon(attr(data, "step_seconds"))
    unit <- describe_step(attr(data, "step_seconds"))
    shiny::updateSliderInput(
      session, "horizon",
      label = sprintf("Forecast horizon (%ss)", unit),
      min = 1, max = as.integer(horizon[["max"]]),
      value = as.integer(horizon[["value"]])
    )
  })

  step_seconds <- shiny::reactive({
    data <- trends()
    if (is.null(data)) 86400 else attr(data, "step_seconds")
  })

  all_mode <- shiny::reactive(identical(input$series, ALL_SERIES))

  # One history per fetched term, oldest first, terms too short to model
  # dropped. Everything per-term downstream reads from this.
  histories <- shiny::reactive({
    data <- trends()
    shiny::req(data)
    parts <- split(as.data.frame(data[, c("ds", "y")]), data$keyword)
    parts <- lapply(parts, function(df) df[order(df$ds), , drop = FALSE])
    parts[vapply(parts, nrow, integer(1)) >= 5]
  })

  history <- shiny::reactive({
    shiny::req(input$series, !all_mode())
    chosen <- histories()[[input$series]]
    shiny::validate(shiny::need(!is.null(chosen),
                                "Not enough observations for this series to model."))
    chosen
  })

  model_args <- shiny::reactive({
    list(
      growth = input$growth,
      seasonality_mode = input$seasonality_mode,
      changepoint_prior_scale = input$changepoint_prior,
      holidays_country = if (isTRUE(input$holidays) && nzchar(input$geo)) input$geo else NULL
    )
  })

  # One Prophet model per term, so the Forecast tab and the AI briefing can
  # cover every term, not just the selected one. Fitting is the slow part and
  # only reruns when the data or the model settings change; moving the
  # horizon slider re-predicts without refitting.
  models_all <- shiny::reactive({
    hs <- histories()
    shiny::req(length(hs) > 0)
    args <- model_args()
    shiny::withProgress(message = "Fitting Prophet", value = 0, {
      models <- vector("list", length(hs))
      names(models) <- names(hs)
      for (kw in names(hs)) {
        shiny::incProgress(1 / length(hs), detail = kw)
        models[[kw]] <- tryCatch(
          do.call(fit_prophet_model, c(list(history = hs[[kw]]), args)),
          error = function(e) NULL
        )
      }
      models[!vapply(models, is.null, logical(1))]
    })
  })

  forecasts_all <- shiny::reactive({
    hs <- histories()
    models <- models_all()
    step <- step_seconds()
    predictions <- list()
    for (kw in names(models)) {
      future <- prophet::make_future_dataframe(
        models[[kw]],
        periods = as.integer(input$horizon),
        freq = step,
        include_history = TRUE
      )
      future <- with_capacity(future, input$growth, hs[[kw]]$y)
      predicted <- stats::predict(models[[kw]], future)
      predicted$ds <- as.POSIXct(predicted$ds, tz = "UTC")
      predictions[[kw]] <- predicted
    }
    predictions
  })

  model <- shiny::reactive({
    shiny::req(!all_mode())
    fitted <- models_all()[[input$series]]
    shiny::validate(shiny::need(!is.null(fitted),
                                "The model could not be fitted for this series."))
    fitted
  })

  forecast <- shiny::reactive({
    shiny::req(!all_mode())
    predicted <- forecasts_all()[[input$series]]
    shiny::validate(shiny::need(!is.null(predicted),
                                "No forecast is available for this series."))
    predicted
  })

  backtest <- shiny::reactive({
    if (!isTRUE(input$backtest)) return(NULL)
    shiny::withProgress(message = "Backtesting", value = 0.5, {
      prophet_backtest(history(), model_args())
    })
  })

  anomalies_for <- function(kw) {
    detect_anomalies(
      histories()[[kw]],
      method = input$anomaly_method,
      sensitivity = input$anomaly_sensitivity,
      window = input$anomaly_window,
      fitted = if (identical(input$anomaly_method, "residual")) forecasts_all()[[kw]] else NULL
    )
  }

  anomalies <- shiny::reactive({
    shiny::req(!all_mode())
    anomalies_for(input$series)
  })

  slope_for <- function(kw) {
    past <- histories()[[kw]]
    if (is.null(past) || nrow(past) < 3) return(NA_real_)
    fit <- stats::lm(y ~ as.numeric(ds), data = past)
    # Per period rather than per second, so the number means something.
    unname(stats::coef(fit)[2]) * step_seconds()
  }

  trend_slope <- shiny::reactive(slope_for(input$series))

  direction_of <- function(slope) {
    if (!is.finite(slope)) "unknown"
    else if (slope > 0.01) "rising"
    else if (slope < -0.01) "falling"
    else "flat"
  }

  #' Forecast change over the horizon, in percent, for one term.
  change_for <- function(kw) {
    past <- histories()[[kw]]
    predicted <- forecasts_all()[[kw]]
    if (is.null(past) || is.null(predicted)) return(NA_real_)
    future_rows <- predicted[predicted$ds > max(past$ds), , drop = FALSE]
    if (!nrow(future_rows)) return(NA_real_)
    last_actual <- past$y[nrow(past)]
    if (last_actual > 0) {
      (future_rows$yhat[nrow(future_rows)] - last_actual) / last_actual * 100
    } else NA_real_
  }

  # --- KPIs ----------------------------------------------------------------
  output$kpis <- shiny::renderUI({
    data <- trends()
    if (is.null(data)) {
      return(shiny::div(
        class = "ai-empty mb-3",
        "Enter keywords and press Fetch trends. Google Trends rate limits
         aggressively, so if nothing comes back, wait a minute and retry."
      ))
    }

    unit <- describe_step(step_seconds())

    # All-terms mode: one tile per term, forecast change as the headline.
    if (isTRUE(all_mode())) {
      kws <- names(histories())
      tiles <- lapply(kws, function(kw) {
        past <- histories()[[kw]]
        change <- tryCatch(change_for(kw), error = function(e) NA_real_)
        metric_card(
          kw,
          if (is.finite(change)) paste0(fmt_num(change, 1), "%") else "-",
          sprintf("mean %s · %s", fmt_num(mean(past$y), 1),
                  direction_of(slope_for(kw)))
        )
      })
      return(shiny::tagList(
        shiny::div(class = "status-note mb-2",
                   sprintf("Forecast change per term over the next %d %ss.",
                           as.integer(input$horizon), unit)),
        shiny::div(class = "metric-grid mb-3", tiles)
      ))
    }

    past <- history()
    slope <- trend_slope()
    direction <- direction_of(slope)
    change <- tryCatch(change_for(input$series), error = function(e) NA_real_)
    scores <- backtest()

    shiny::div(
      class = "metric-grid mb-3",
      metric_card("Observations", format(nrow(past), big.mark = ","),
                  paste("one per", unit)),
      metric_card("Average interest", fmt_num(mean(past$y), 1),
                  paste("median", fmt_num(stats::median(past$y), 1))),
      metric_card("Peak", fmt_num(max(past$y), 0),
                  format(past$ds[which.max(past$y)], "%d %b %Y")),
      metric_card("Trend", direction,
                  paste(fmt_num(slope, 3), "points per", unit)),
      metric_card("Forecast change",
                  if (is.finite(change)) paste0(fmt_num(change, 1), "%") else "-",
                  paste("over", input$horizon, paste0(unit, "s"))),
      metric_card("Holdout MAPE",
                  if (!is.null(scores) && is.finite(scores$mape)) {
                    paste0(fmt_num(scores$mape, 1), "%")
                  } else "-",
                  if (!is.null(scores)) paste("last", scores$holdout, paste0(unit, "s"))
                  else "backtest off")
    )
  })

  # --- plots ---------------------------------------------------------------
  output$trends_plot <- plotly::renderPlotly({
    data <- trends()
    shiny::req(data)

    figure <- plotly::plot_ly()
    series <- unique(data$keyword)
    for (i in seq_along(series)) {
      rows <- data[data$keyword == series[i], , drop = FALSE]
      figure <- plotly::add_lines(
        figure, x = rows$ds, y = rows$y, name = series[i],
        line = list(width = 2,
                    color = SERIES_COLOURS[((i - 1) %% length(SERIES_COLOURS)) + 1]),
        hovertemplate = paste0("<b>", series[i], "</b><br>%{x}<br>%{y}<extra></extra>")
      )
    }

    figure <- plotly::layout(
      figure,
      xaxis = list(title = "", rangeslider = list(visible = TRUE)),
      yaxis = list(title = "Relative search interest", range = c(0, 105))
    )
    plotly_theme(figure, isTRUE(input$dark_mode))
  })

  output$forecast_plot <- plotly::renderPlotly({
    # All-terms mode: observed line, dotted forecast and translucent interval
    # per term, in the term's colour, toggled together from the legend.
    if (isTRUE(all_mode())) {
      hs <- histories()
      predictions <- forecasts_all()
      shiny::req(length(predictions) > 0)
      pal <- app_palette(isTRUE(input$dark_mode))

      kws <- names(predictions)
      cutoff <- do.call(max, lapply(hs[kws], function(h) max(h$ds)))
      cutoff_label <- format(cutoff, "%Y-%m-%d %H:%M:%S")

      figure <- plotly::plot_ly()
      for (i in seq_along(kws)) {
        kw <- kws[i]
        past <- hs[[kw]]
        predicted <- predictions[[kw]]
        colour <- series_colour(i)
        future_rows <- predicted[predicted$ds >= max(past$ds), , drop = FALSE]

        figure <- plotly::add_ribbons(
          figure, x = future_rows$ds,
          ymin = future_rows$yhat_lower, ymax = future_rows$yhat_upper,
          name = kw, legendgroup = kw, showlegend = FALSE,
          line = list(width = 0), fillcolor = rgba(colour, 0.14),
          hoverinfo = "skip"
        )
        figure <- plotly::add_lines(
          figure, x = past$ds, y = past$y, name = kw, legendgroup = kw,
          line = list(color = colour, width = 1.3),
          hovertemplate = paste0("<b>", kw, "</b><br>%{x}<br>observed %{y}<extra></extra>")
        )
        figure <- plotly::add_lines(
          figure, x = future_rows$ds, y = future_rows$yhat,
          name = kw, legendgroup = kw, showlegend = FALSE,
          line = list(color = colour, width = 2.2, dash = "dot"),
          hovertemplate = paste0("<b>", kw, "</b><br>%{x}<br>forecast %{y:.1f}<extra></extra>")
        )
      }

      figure <- plotly::layout(
        figure,
        xaxis = list(title = ""),
        yaxis = list(title = "Relative search interest"),
        shapes = list(list(
          type = "line", x0 = cutoff_label, x1 = cutoff_label,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = pal$sand, width = 1.5, dash = "dash")
        )),
        annotations = list(list(
          x = cutoff_label, y = 1, yref = "paper", text = "forecast starts",
          showarrow = FALSE, xanchor = "left", yanchor = "bottom",
          font = list(size = 11, color = pal$sand)
        ))
      )
      return(plotly_theme(figure, isTRUE(input$dark_mode)))
    }

    past <- history()
    predicted <- forecast()
    shiny::req(predicted)

    cutoff <- max(past$ds)
    cutoff_label <- format(cutoff, "%Y-%m-%d %H:%M:%S")
    future_rows <- predicted[predicted$ds >= cutoff, , drop = FALSE]
    pal <- app_palette(isTRUE(input$dark_mode))

    figure <- plotly::plot_ly()
    figure <- plotly::add_ribbons(
      figure, x = future_rows$ds,
      ymin = future_rows$yhat_lower, ymax = future_rows$yhat_upper,
      name = "80% interval", line = list(width = 0),
      fillcolor = "rgba(14,124,107,0.18)", hoverinfo = "skip"
    )
    figure <- plotly::add_lines(
      figure, x = past$ds, y = past$y, name = "Observed",
      line = list(color = pal$ink, width = 1.6),
      hovertemplate = "%{x}<br>observed %{y}<extra></extra>"
    )
    figure <- plotly::add_lines(
      figure, x = future_rows$ds, y = future_rows$yhat, name = "Forecast",
      line = list(color = pal$teal, width = 2.5, dash = "dot"),
      hovertemplate = "%{x}<br>forecast %{y:.1f}<extra></extra>"
    )

    figure <- plotly::layout(
      figure,
      xaxis = list(title = ""),
      yaxis = list(title = "Relative search interest"),
      shapes = list(list(
        type = "line", x0 = cutoff_label, x1 = cutoff_label,
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = pal$sand, width = 1.5, dash = "dash")
      )),
      annotations = list(list(
        x = cutoff_label, y = 1, yref = "paper", text = "forecast starts",
        showarrow = FALSE, xanchor = "left", yanchor = "bottom",
        font = list(size = 11, color = pal$sand)
      ))
    )
    plotly_theme(figure, isTRUE(input$dark_mode))
  })

  output$accuracy_note <- shiny::renderUI({
    if (isTRUE(all_mode())) {
      return(shiny::div(
        class = "status-note mt-2",
        "Backtesting scores one model at a time - pick a single term under
         \"Series to forecast\" to see its holdout accuracy."
      ))
    }
    scores <- backtest()
    if (is.null(scores)) {
      return(shiny::div(
        class = "status-note mt-2",
        "Turn on \"Score the model on held-out data\" to see how the same settings
         performed on the most recent stretch of real data."
      ))
    }
    shiny::div(
      class = "status-note mt-2",
      sprintf(paste("Refit without the last %d observations, the model predicted them",
                    "with a mean absolute error of %s (%s%% MAPE, RMSE %s), and %s%% of",
                    "them fell inside the 80%% interval."),
              scores$holdout, fmt_num(scores$mae, 1), fmt_num(scores$mape, 1),
              fmt_num(scores$rmse, 1), fmt_num(scores$coverage, 0))
    )
  })

  output$components_plot <- shiny::renderPlot({
    shiny::validate(shiny::need(
      !isTRUE(all_mode()),
      "Seasonality is decomposed one model at a time - pick a single term under \"Series to forecast\"."
    ))
    # Reuses the fitted model and its predictions instead of refitting, which
    # is what the previous version did on every redraw. Prophet draws with
    # ggplot's default theme, which is a white rectangle in dark mode, so the
    # theme is swapped for the app palette while these plots render.
    pal <- app_palette(isTRUE(input$dark_mode))
    old <- ggplot2::theme_set(
      ggplot2::theme_minimal() +
        ggplot2::theme(
          text = ggplot2::element_text(colour = pal$ink),
          axis.text = ggplot2::element_text(colour = pal$ink_soft),
          axis.title = ggplot2::element_text(colour = pal$ink_soft),
          panel.grid.major = ggplot2::element_line(colour = pal$rule),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
    )
    on.exit(ggplot2::theme_set(old), add = TRUE)
    prophet::prophet_plot_components(model(), forecast(), uncertainty = TRUE)
  }, bg = "transparent")

  output$anomaly_plot <- plotly::renderPlotly({
    if (isTRUE(all_mode())) {
      hs <- histories()
      shiny::req(length(hs) > 0)
      pal <- app_palette(isTRUE(input$dark_mode))
      kws <- names(hs)

      figure <- plotly::plot_ly()
      for (i in seq_along(kws)) {
        kw <- kws[i]
        flagged <- tryCatch(anomalies_for(kw), error = function(e) NULL)
        if (is.null(flagged)) next
        colour <- series_colour(i)
        figure <- plotly::add_lines(
          figure, x = flagged$ds, y = flagged$y, name = kw, legendgroup = kw,
          line = list(color = colour, width = 1.2),
          hovertemplate = paste0("<b>", kw, "</b><br>%{x}<br>%{y}<extra></extra>")
        )
        hits <- flagged[flagged$anomaly, , drop = FALSE]
        if (nrow(hits)) {
          figure <- plotly::add_markers(
            figure, x = hits$ds, y = hits$y,
            name = kw, legendgroup = kw, showlegend = FALSE,
            marker = list(color = colour, size = 9,
                          line = list(color = pal$surface, width = 1)),
            text = round(hits$score, 2),
            hovertemplate = paste0("<b>", kw, "</b><br>%{x}<br>%{y} (z %{text})<extra></extra>")
          )
        }
      }
      figure <- plotly::layout(
        figure, xaxis = list(title = ""),
        yaxis = list(title = "Relative search interest")
      )
      return(plotly_theme(figure, isTRUE(input$dark_mode)))
    }

    flagged <- anomalies()
    shiny::req(flagged)
    hits <- flagged[flagged$anomaly, , drop = FALSE]
    pal <- app_palette(isTRUE(input$dark_mode))

    figure <- plotly::plot_ly()
    figure <- plotly::add_lines(
      figure, x = flagged$ds, y = flagged$y, name = "Observed",
      line = list(color = pal$ink, width = 1.4),
      hovertemplate = "%{x}<br>%{y}<extra></extra>"
    )
    if (nrow(hits)) {
      figure <- plotly::add_markers(
        figure, x = hits$ds, y = hits$y,
        name = sprintf("Flagged (%d)", nrow(hits)),
        marker = list(color = pal$magenta, size = 9,
                      line = list(color = pal$surface, width = 1)),
        text = round(hits$score, 2),
        hovertemplate = "%{x}<br>%{y} (z %{text})<extra></extra>"
      )
    }
    figure <- plotly::layout(
      figure, xaxis = list(title = ""),
      yaxis = list(title = "Relative search interest")
    )
    plotly_theme(figure, isTRUE(input$dark_mode))
  })

  output$anomaly_table <- DT::renderDT({
    if (isTRUE(all_mode())) {
      total <- 0L
      combined <- do.call(rbind, lapply(names(histories()), function(kw) {
        flagged <- tryCatch(anomalies_for(kw), error = function(e) NULL)
        if (is.null(flagged)) return(NULL)
        total <<- total + nrow(flagged)
        hits <- flagged[flagged$anomaly, , drop = FALSE]
        if (!nrow(hits)) return(NULL)
        hits$keyword <- kw
        hits
      }))
      shiny::validate(shiny::need(
        !is.null(combined) && nrow(combined),
        sprintf("No observations flagged for any term at a threshold of %.1f.",
                input$anomaly_sensitivity)
      ))
      combined <- combined[order(-abs(combined$score)), , drop = FALSE]
      combined$score <- round(combined$score, 2)
      combined$ds <- format(combined$ds, "%Y-%m-%d %H:%M")

      return(DT::datatable(
        combined[, c("keyword", "ds", "y", "score")],
        rownames = FALSE,
        colnames = c("Term", "When", "Interest", "Robust z"),
        options = list(pageLength = 8, order = list(list(3, "desc"))),
        caption = sprintf("%d of %d observations flagged at a threshold of %.1f, across %d terms.",
                          nrow(combined), total, input$anomaly_sensitivity,
                          length(histories()))
      ))
    }

    flagged <- anomalies()
    shiny::req(flagged)
    hits <- flagged[flagged$anomaly, , drop = FALSE]
    hits <- hits[order(-abs(hits$score)), , drop = FALSE]
    hits$score <- round(hits$score, 2)
    hits$ds <- format(hits$ds, "%Y-%m-%d %H:%M")

    DT::datatable(
      hits[, c("ds", "y", "score")],
      rownames = FALSE,
      colnames = c("When", "Interest", "Robust z"),
      options = list(pageLength = 8, order = list(list(2, "desc"))),
      caption = sprintf("%d of %d observations flagged at a threshold of %.1f.",
                        nrow(hits), nrow(flagged), input$anomaly_sensitivity)
    )
  })

  output$related_note <- shiny::renderUI({
    if (!is.null(related_queries())) return(NULL)
    note <- if (is.null(trends())) {
      "Fetch trends first."
    } else if (!isTRUE(input$fetch_related)) {
      "Turn on \"Also fetch related queries\" in the sidebar and fetch again to fill this tab."
    } else {
      "Google Trends returned no related queries for this combination."
    }
    shiny::div(class = "ai-empty mb-2", note)
  })

  output$related_table <- DT::renderDT({
    related <- related_queries()
    shiny::req(related)
    DT::datatable(
      related,
      rownames = FALSE, filter = "top",
      colnames = c("Keyword", "Type", "Related query", "Interest"),
      options = list(pageLength = 15, scrollX = TRUE),
      caption = paste("\"Top\" queries are the most searched alongside each keyword;",
                      "\"rising\" ones grew fastest (\"Breakout\" means >5000%).")
    )
  })

  output$data_table <- DT::renderDT({
    if (isTRUE(all_mode())) {
      hs <- histories()
      predictions <- forecasts_all()
      shiny::req(length(predictions) > 0)
      combined <- do.call(rbind, lapply(names(predictions), function(kw) {
        predicted <- predictions[[kw]]
        past <- hs[[kw]]
        data.frame(
          keyword = kw,
          ds = format(predicted$ds, "%Y-%m-%d %H:%M"),
          segment = ifelse(predicted$ds > max(past$ds), "forecast", "fitted"),
          observed = past$y[match(predicted$ds, past$ds)],
          yhat = round(predicted$yhat, 2),
          yhat_lower = round(predicted$yhat_lower, 2),
          yhat_upper = round(predicted$yhat_upper, 2),
          stringsAsFactors = FALSE
        )
      }))
      return(DT::datatable(
        combined,
        rownames = FALSE, filter = "top",
        colnames = c("Term", "When", "Segment", "Observed", "Forecast", "Lower", "Upper"),
        options = list(pageLength = 15, scrollX = TRUE)
      ))
    }

    predicted <- forecast()
    past <- history()
    shiny::req(predicted)

    combined <- data.frame(
      ds = predicted$ds,
      yhat = round(predicted$yhat, 2),
      yhat_lower = round(predicted$yhat_lower, 2),
      yhat_upper = round(predicted$yhat_upper, 2),
      stringsAsFactors = FALSE
    )
    combined$observed <- past$y[match(combined$ds, past$ds)]
    combined$segment <- ifelse(combined$ds > max(past$ds), "forecast", "fitted")
    combined$ds <- format(combined$ds, "%Y-%m-%d %H:%M")

    DT::datatable(
      combined[, c("ds", "segment", "observed", "yhat", "yhat_lower", "yhat_upper")],
      rownames = FALSE, filter = "top",
      colnames = c("When", "Segment", "Observed", "Forecast", "Lower", "Upper"),
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  # --- downloads -----------------------------------------------------------
  stamp <- function(what) {
    series <- if (isTRUE(all_mode())) "all-terms" else input$series %||% "series"
    paste0(gsub("[^A-Za-z0-9]+", "-", series),
           "-", what, "-", format(Sys.Date()), ".csv")
  }

  output$dl_history <- shiny::downloadHandler(
    filename = function() stamp("observed"),
    content = function(file) {
      shiny::req(trends())
      utils::write.csv(trends()[, c("keyword", "ds", "y")], file, row.names = FALSE)
    }
  )
  output$dl_forecast <- shiny::downloadHandler(
    filename = function() stamp("forecast"),
    content = function(file) {
      predictions <- forecasts_all()
      shiny::req(length(predictions) > 0)
      # Every term's forecast, whatever is on screen: the CSV is the record.
      combined <- do.call(rbind, lapply(names(predictions), function(kw) {
        cbind(keyword = kw,
              predictions[[kw]][, c("ds", "yhat", "yhat_lower", "yhat_upper", "trend")])
      }))
      utils::write.csv(combined, file, row.names = FALSE)
    }
  )
  output$dl_related <- shiny::downloadHandler(
    filename = function() stamp("related-queries"),
    content = function(file) {
      shiny::req(related_queries())
      utils::write.csv(related_queries(), file, row.names = FALSE)
    }
  )

  # --- AI briefing ---------------------------------------------------------

  #' Everything worth saying about one term, as briefing prose: history,
  #' slope, forecast over the horizon, flagged dates and related queries.
  term_briefing <- function(kw) {
    past <- histories()[[kw]]
    if (is.null(past)) {
      return(sprintf("Term \"%s\": too few observations to model.", kw))
    }
    unit <- describe_step(step_seconds())

    lines <- sprintf(
      "Term \"%s\": %d observations from %s to %s. Mean %.1f, median %.1f, min %.0f, max %.0f (peak on %s). Linear trend %.3f points per %s (%s).",
      kw, nrow(past),
      format(min(past$ds), "%Y-%m-%d"), format(max(past$ds), "%Y-%m-%d"),
      mean(past$y), stats::median(past$y), min(past$y), max(past$y),
      format(past$ds[which.max(past$y)], "%Y-%m-%d"),
      slope_for(kw), unit, direction_of(slope_for(kw))
    )

    predicted <- forecasts_all()[[kw]]
    lines <- c(lines, if (!is.null(predicted)) {
      future_rows <- predicted[predicted$ds > max(past$ds), , drop = FALSE]
      if (nrow(future_rows)) {
        sprintf("Forecast %d %ss ahead: %.1f now to %.1f at the end (80%% interval %.1f to %.1f), a change of %s%%.",
                nrow(future_rows), unit, past$y[nrow(past)],
                future_rows$yhat[nrow(future_rows)],
                future_rows$yhat_lower[nrow(future_rows)],
                future_rows$yhat_upper[nrow(future_rows)],
                fmt_num(change_for(kw), 1))
      } else "No forecast horizon was requested."
    } else "The model could not be fitted for this term.")

    flagged <- tryCatch(anomalies_for(kw), error = function(e) NULL)
    hits <- if (is.null(flagged)) NULL else flagged[flagged$anomaly, , drop = FALSE]
    lines <- c(lines, if (!is.null(hits) && nrow(hits)) {
      top <- utils::head(hits[order(-abs(hits$score)), , drop = FALSE], 5)
      paste0(sprintf("%d observations flagged as unusual. Largest: ", nrow(hits)),
             paste(sprintf("%s (%.0f, z %.1f)", format(top$ds, "%Y-%m-%d"),
                           top$y, top$score), collapse = "; "), ".")
    } else "No observations flagged as unusual.")

    related <- related_queries()
    if (!is.null(related)) {
      mine <- related[related$keyword == kw, , drop = FALSE]
      if (nrow(mine)) {
        rising <- utils::head(mine$query[mine$type == "rising"], 8)
        top <- utils::head(mine$query[mine$type == "top"], 8)
        lines <- c(lines, paste0(
          "Related searches - top: ",
          if (length(top)) paste(top, collapse = ", ") else "none",
          "; rising: ",
          if (length(rising)) paste(rising, collapse = ", ") else "none", "."
        ))
      }
    }

    paste(lines, collapse = " ")
  }

  aiInsightsServer(
    "ai",
    context = shiny::reactive({
      hs <- tryCatch(histories(), error = function(e) NULL)
      if (is.null(hs) || !length(hs)) return(NULL)

      unit <- describe_step(step_seconds())
      region <- names(REGIONS)[match(input$geo, REGIONS)]
      scope <- input$ai_scope %||% ALL_SERIES

      header <- sprintf(
        paste("Google Trends relative search interest in %s, %s, one observation per %s.",
              "Values share one 0-100 index scaled to the busiest point across all",
              "compared terms, so levels are directly comparable between terms.",
              "Prophet settings: %s trend, %s seasonality, flexibility %.2f, horizon %d %ss."),
        region, names(TIME_CHOICES)[match(input$time, TIME_CHOICES)], unit,
        input$growth, input$seasonality_mode, input$changepoint_prior,
        as.integer(input$horizon), unit
      )

      if (identical(scope, ALL_SERIES) && length(hs) > 1) {
        kws <- names(hs)
        blocks <- vapply(kws, term_briefing, character(1))

        means <- sort(vapply(kws, function(kw) mean(hs[[kw]]$y), numeric(1)),
                      decreasing = TRUE)
        changes <- vapply(kws, function(kw) {
          tryCatch(change_for(kw), error = function(e) NA_real_)
        }, numeric(1))
        changes <- sort(changes[is.finite(changes)], decreasing = TRUE)

        paste(c(
          header,
          sprintf("%d terms are compared: %s.", length(kws), paste(kws, collapse = ", ")),
          blocks,
          paste0("Ranking by average interest: ",
                 paste(sprintf("%s (%.1f)", names(means), means), collapse = ", "), "."),
          if (length(changes)) {
            paste0("Ranking by forecast change over the horizon: ",
                   paste(sprintf("%s (%+.1f%%)", names(changes), changes), collapse = ", "), ".")
          }
        ), collapse = "\n")
      } else {
        kw <- if (identical(scope, ALL_SERIES)) names(hs)[1] else scope
        if (!kw %in% names(hs)) return(NULL)

        scores <- tryCatch(
          if (identical(kw, input$series)) backtest() else NULL,
          error = function(e) NULL
        )
        others <- setdiff(names(hs), kw)

        paste(c(
          header,
          term_briefing(kw),
          if (!is.null(scores)) {
            sprintf("Held-out accuracy on the last %d observations: MAE %.1f, MAPE %.1f%%, RMSE %.1f, interval coverage %.0f%%.",
                    scores$holdout, scores$mae, scores$mape, scores$rmse, scores$coverage)
          } else "The model was not backtested.",
          if (length(others)) {
            means <- vapply(others, function(k) mean(hs[[k]]$y), numeric(1))
            paste0("Other compared terms and their mean interest: ",
                   paste(sprintf("%s (%.1f)", others, means), collapse = ", "), ".")
          } else "No other terms were compared."
        ), collapse = "\n")
      }
    }),
    system_prompt = paste(
      "You are a demand analyst briefing a marketing team on search-interest",
      "forecasts. The briefing covers either one term or several compared",
      "terms. With several terms, lead with the comparison: which terms",
      "dominate, which are gaining or fading, how their forecasts and their",
      "seasonal peaks differ, and which one deserves attention first; then",
      "any flagged dates worth explaining. With a single term, structure the",
      "answer as: 1) what the series has actually been doing; 2) what the",
      "forecast implies and how much to trust it, using the held-out error",
      "and interval width when given; 3) what the flagged dates most likely",
      "were, if anything in the data suggests it; 4) one caveat. Google",
      "Trends values are relative (0-100, on one shared scale across compared",
      "terms), never absolute volumes - say so if it matters. Use only the",
      "numbers in the briefing and keep it under 400 words."
    )
  )
}

shinyApp(ui = ui, server = server)
