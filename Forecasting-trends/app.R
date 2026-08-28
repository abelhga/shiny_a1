# ---------------------------------------------------------------------------
# Search Trends Forecasting
#
# Pulls Google Trends interest over time, forecasts it with Prophet, flags
# unusual observations, and (optionally) has an OpenAI model write the
# read-out.
#
# Originally by Abel Hernandez Garcia // hi@abelhga.com // www.abelhga.com
#
# Packages: shiny, bslib, dplyr, plotly, DT, gtrendsR, prophet, lubridate,
#           countrycode, shinycssloaders, httr, jsonlite.
# See ../install_dependencies.R
# ---------------------------------------------------------------------------

library(shiny)
library(dplyr)

# Shiny sources everything in R/ automatically when the app is launched from
# its own folder. This makes `source("app.R")` work too.
for (.f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(.f)

MAX_KEYWORDS <- 5L  # Google Trends compares at most five terms at a time

SERIES_COLOURS <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#B279A2")

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
                           choices = c("Linear" = "linear", "Flat" = "flat")),
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
        type = 8, color = "#4C78A8"
      )
    ),

    bslib::nav_panel(
      "Forecast",
      icon = shiny::icon("arrow-trend-up"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("forecast_plot", height = "540px"),
        type = 8, color = "#4C78A8"
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
        type = 8, color = "#4C78A8"
      )
    ),

    bslib::nav_panel(
      "Anomalies",
      icon = shiny::icon("triangle-exclamation"),
      shinycssloaders::withSpinner(
        plotly::plotlyOutput("anomaly_plot", height = "420px"),
        type = 8, color = "#4C78A8"
      ),
      DT::DTOutput("anomaly_table")
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
    parts <- unique(parts[nzchar(parts)])
    utils::head(parts, MAX_KEYWORDS)
  })

  # --- data ----------------------------------------------------------------
  trends <- shiny::eventReactive(input$fetch, {
    keywords <- requested_keywords()

    if (!length(keywords)) {
      shiny::showNotification("Enter at least one keyword.", type = "warning")
      return(NULL)
    }

    result <- shiny::withProgress(message = "Asking Google Trends", value = 0.4, {
      tryCatch(
        gtrendsR::gtrends(keyword = keywords, geo = input$geo, time = input$time,
                          tz = 0, onlyInterest = TRUE),
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
    interest
  })

  # Point the series picker and the horizon slider at what actually came back.
  shiny::observeEvent(trends(), {
    data <- trends()
    shiny::req(data)

    available <- unique(data$keyword)
    shiny::updateSelectInput(session, "series", choices = available,
                             selected = available[1])

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

  history <- shiny::reactive({
    data <- trends()
    shiny::req(data, input$series)
    chosen <- data[data$keyword == input$series, c("ds", "y"), drop = FALSE]
    chosen <- as.data.frame(chosen[order(chosen$ds), , drop = FALSE])
    shiny::validate(shiny::need(nrow(chosen) >= 5,
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

  model <- shiny::reactive({
    past <- history()
    shiny::req(nrow(past) >= 5)
    shiny::withProgress(message = "Fitting Prophet", value = 0.5, {
      do.call(fit_prophet_model, c(list(history = past), model_args()))
    })
  })

  forecast <- shiny::reactive({
    fitted <- model()
    future <- prophet::make_future_dataframe(
      fitted,
      periods = as.integer(input$horizon),
      freq = step_seconds(),
      include_history = TRUE
    )
    predicted <- stats::predict(fitted, future)
    predicted$ds <- as.POSIXct(predicted$ds, tz = "UTC")
    predicted
  })

  backtest <- shiny::reactive({
    if (!isTRUE(input$backtest)) return(NULL)
    shiny::withProgress(message = "Backtesting", value = 0.5, {
      prophet_backtest(history(), model_args())
    })
  })

  anomalies <- shiny::reactive({
    detect_anomalies(
      history(),
      method = input$anomaly_method,
      sensitivity = input$anomaly_sensitivity,
      window = input$anomaly_window,
      fitted = if (identical(input$anomaly_method, "residual")) forecast() else NULL
    )
  })

  trend_slope <- shiny::reactive({
    past <- history()
    if (nrow(past) < 3) return(NA_real_)
    fit <- stats::lm(y ~ as.numeric(ds), data = past)
    # Per period rather than per second, so the number means something.
    unname(stats::coef(fit)[2]) * step_seconds()
  })

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

    past <- history()
    slope <- trend_slope()
    unit <- describe_step(step_seconds())

    direction <- if (!is.finite(slope)) "unknown"
    else if (slope > 0.01) "rising"
    else if (slope < -0.01) "falling"
    else "flat"

    predicted <- tryCatch(forecast(), error = function(e) NULL)
    change <- if (!is.null(predicted)) {
      future_rows <- predicted[predicted$ds > max(past$ds), , drop = FALSE]
      if (nrow(future_rows)) {
        last_actual <- past$y[nrow(past)]
        if (last_actual > 0) {
          (future_rows$yhat[nrow(future_rows)] - last_actual) / last_actual * 100
        } else NA_real_
      } else NA_real_
    } else NA_real_

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
    past <- history()
    predicted <- forecast()
    shiny::req(predicted)

    cutoff <- max(past$ds)
    cutoff_label <- format(cutoff, "%Y-%m-%d %H:%M:%S")
    future_rows <- predicted[predicted$ds >= cutoff, , drop = FALSE]

    figure <- plotly::plot_ly()
    figure <- plotly::add_ribbons(
      figure, x = future_rows$ds,
      ymin = future_rows$yhat_lower, ymax = future_rows$yhat_upper,
      name = "80% interval", line = list(width = 0),
      fillcolor = "rgba(76,120,168,0.22)", hoverinfo = "skip"
    )
    figure <- plotly::add_lines(
      figure, x = past$ds, y = past$y, name = "Observed",
      line = list(color = if (isTRUE(input$dark_mode)) "#e6edf3" else "#1b1f24",
                  width = 1.6),
      hovertemplate = "%{x}<br>observed %{y}<extra></extra>"
    )
    figure <- plotly::add_lines(
      figure, x = future_rows$ds, y = future_rows$yhat, name = "Forecast",
      line = list(color = "#4C78A8", width = 2.5, dash = "dot"),
      hovertemplate = "%{x}<br>forecast %{y:.1f}<extra></extra>"
    )

    figure <- plotly::layout(
      figure,
      xaxis = list(title = ""),
      yaxis = list(title = "Relative search interest"),
      shapes = list(list(
        type = "line", x0 = cutoff_label, x1 = cutoff_label,
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "rgba(245,133,24,0.8)", width = 1.5, dash = "dash")
      )),
      annotations = list(list(
        x = cutoff_label, y = 1, yref = "paper", text = "forecast starts",
        showarrow = FALSE, xanchor = "left", yanchor = "bottom",
        font = list(size = 11, color = "#F58518")
      ))
    )
    plotly_theme(figure, isTRUE(input$dark_mode))
  })

  output$accuracy_note <- shiny::renderUI({
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
    # Reuses the fitted model and its predictions instead of refitting, which
    # is what the previous version did on every redraw.
    prophet::prophet_plot_components(model(), forecast(), uncertainty = TRUE)
  })

  output$anomaly_plot <- plotly::renderPlotly({
    flagged <- anomalies()
    shiny::req(flagged)
    hits <- flagged[flagged$anomaly, , drop = FALSE]

    figure <- plotly::plot_ly()
    figure <- plotly::add_lines(
      figure, x = flagged$ds, y = flagged$y, name = "Observed",
      line = list(color = if (isTRUE(input$dark_mode)) "#e6edf3" else "#1b1f24",
                  width = 1.4),
      hovertemplate = "%{x}<br>%{y}<extra></extra>"
    )
    if (nrow(hits)) {
      figure <- plotly::add_markers(
        figure, x = hits$ds, y = hits$y,
        name = sprintf("Flagged (%d)", nrow(hits)),
        marker = list(color = "#E45756", size = 9,
                      line = list(color = "#ffffff", width = 1)),
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

  output$data_table <- DT::renderDT({
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
    paste0(gsub("[^A-Za-z0-9]+", "-", input$series %||% "series"),
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
      shiny::req(forecast())
      utils::write.csv(
        forecast()[, c("ds", "yhat", "yhat_lower", "yhat_upper", "trend")],
        file, row.names = FALSE
      )
    }
  )

  # --- AI briefing ---------------------------------------------------------
  aiInsightsServer(
    "ai",
    context = shiny::reactive({
      data <- trends()
      if (is.null(data)) return(NULL)

      past <- history()
      predicted <- tryCatch(forecast(), error = function(e) NULL)
      if (is.null(predicted)) return(NULL)

      unit <- describe_step(step_seconds())
      future_rows <- predicted[predicted$ds > max(past$ds), , drop = FALSE]
      scores <- backtest()
      flagged <- anomalies()
      hits <- if (is.null(flagged)) NULL else flagged[flagged$anomaly, , drop = FALSE]

      region <- names(REGIONS)[match(input$geo, REGIONS)]
      others <- setdiff(unique(data$keyword), input$series)

      comparison <- if (length(others)) {
        means <- vapply(others, function(k) mean(data$y[data$keyword == k]), numeric(1))
        paste0("Compared terms and their mean interest: ",
               paste(sprintf("%s (%.1f)", others, means), collapse = ", "), ".")
      } else "No other terms were compared."

      paste(
        sprintf("Google Trends relative search interest for \"%s\" in %s, %s, one observation per %s.",
                input$series, region,
                names(TIME_CHOICES)[match(input$time, TIME_CHOICES)], unit),
        sprintf("%d observations from %s to %s. Mean %.1f, median %.1f, min %.0f, max %.0f (peak on %s).",
                nrow(past), format(min(past$ds), "%Y-%m-%d"), format(max(past$ds), "%Y-%m-%d"),
                mean(past$y), stats::median(past$y), min(past$y), max(past$y),
                format(past$ds[which.max(past$y)], "%Y-%m-%d")),
        sprintf("Linear trend: %.3f points per %s.", trend_slope(), unit),
        comparison,
        if (nrow(future_rows)) {
          sprintf("Prophet (%s trend, %s seasonality, flexibility %.2f) forecasts %d %ss ahead: %.1f now to %.1f at the end, interval %.1f to %.1f.",
                  input$growth, input$seasonality_mode, input$changepoint_prior,
                  nrow(future_rows), unit, past$y[nrow(past)],
                  future_rows$yhat[nrow(future_rows)],
                  future_rows$yhat_lower[nrow(future_rows)],
                  future_rows$yhat_upper[nrow(future_rows)])
        } else "No forecast horizon was requested.",
        if (!is.null(scores)) {
          sprintf("Held-out accuracy on the last %d observations: MAE %.1f, MAPE %.1f%%, RMSE %.1f, interval coverage %.0f%%.",
                  scores$holdout, scores$mae, scores$mape, scores$rmse, scores$coverage)
        } else "The model was not backtested.",
        if (!is.null(hits) && nrow(hits)) {
          top <- hits[order(-abs(hits$score)), , drop = FALSE]
          top <- utils::head(top, 10)
          paste0(sprintf("%d observations flagged as unusual (%s, threshold %.1f). Largest: ",
                         nrow(hits),
                         names(ANOMALY_METHODS)[match(input$anomaly_method, ANOMALY_METHODS)],
                         input$anomaly_sensitivity),
                 paste(sprintf("%s (%.0f, z %.1f)", format(top$ds, "%Y-%m-%d"),
                               top$y, top$score), collapse = "; "), ".")
        } else "No observations were flagged as unusual.",
        sep = "\n"
      )
    }),
    system_prompt = paste(
      "You are a demand analyst briefing a marketing team on a search-interest",
      "forecast. Structure the answer as: 1) what the series has actually been",
      "doing; 2) what the forecast implies and how much to trust it, using the",
      "held-out error and interval width; 3) what the flagged dates most likely",
      "were, if anything in the data suggests it; 4) one caveat. Google Trends",
      "values are relative (0-100), never absolute volumes - say so if it",
      "matters. Use only the numbers in the briefing and keep it under 350 words."
    )
  )
}

shinyApp(ui = ui, server = server)
