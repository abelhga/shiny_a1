# ---------------------------------------------------------------------------
# ui_kit.R
#
# Look and feel shared by every app in this repo: the Bootstrap theme (light
# and dark), the extra CSS, and the small KPI tile helper.
#
# Source of truth: shared/ui_kit.R
# Copies live in <app>/R/ui_kit.R. Run tools/sync_shared.sh after editing.
# ---------------------------------------------------------------------------

# --- Small helpers ---------------------------------------------------------

#' Fall back to `y` when `x` is missing (NULL or a single NA).
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

#' Bootstrap 5 theme, light or dark.
#'
#' Swapped at runtime through `session$setCurrentTheme()`, so the dark-mode
#' switch needs no page reload and no extra JavaScript dependency.
app_theme <- function(dark = FALSE) {
  bslib::bs_theme(
    version = 5,
    bootswatch = if (isTRUE(dark)) "darkly" else "flatly",
    base_font = bslib::font_collection(
      bslib::font_google("Inter", local = FALSE), "system-ui", "sans-serif"
    )
  )
}

#' CSS shared by the three network apps (and reused by the forecasting app).
app_styles <- function() {
  shiny::HTML("
    .app-subtitle { color: var(--bs-secondary-color, #6c757d); margin-bottom: 0; }
    .metric-grid { display: grid; gap: .75rem;
                   grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); }
    .metric-card { background: var(--bs-body-bg, #fff);
                   border: 1px solid var(--bs-border-color, #dee2e6);
                   border-radius: .75rem; padding: .75rem 1rem; }
    .metric-value { font-size: 1.5rem; font-weight: 700; line-height: 1.15; }
    .metric-label { font-size: .72rem; text-transform: uppercase;
                    letter-spacing: .06em; color: var(--bs-secondary-color, #6c757d); }
    .metric-hint { font-size: .75rem; color: var(--bs-secondary-color, #6c757d); }
    .ai-panel { display: flex; flex-direction: column; gap: .6rem; }
    .ai-controls { display: flex; gap: .75rem; align-items: flex-end; flex-wrap: wrap; }
    .ai-controls .form-group,
    .ai-controls .shiny-input-container { margin-bottom: 0; min-width: 190px; }
    .ai-run { white-space: nowrap; }
    .ai-empty, .ai-error, .ai-answer { border-radius: .75rem; padding: .85rem 1rem; }
    .ai-empty { background: rgba(76, 120, 168, .10); font-size: .9rem; }
    .ai-error { background: rgba(228, 87, 86, .14); font-size: .9rem; }
    .ai-answer { background: rgba(84, 162, 75, .10); }
    .ai-answer > :last-child { margin-bottom: 0; }
    .status-note { font-size: .85rem; color: var(--bs-secondary-color, #6c757d); }
    .vis-network { outline: none; }
  ")
}

#' One KPI tile.
metric_card <- function(label, value, hint = NULL) {
  shiny::div(
    class = "metric-card",
    shiny::div(class = "metric-label", label),
    shiny::div(class = "metric-value", value),
    if (!is.null(hint)) shiny::div(class = "metric-hint", hint)
  )
}

#' Format a number for a KPI tile.
fmt_num <- function(x, digits = 2) {
  if (is.null(x) || !length(x) || all(is.na(x))) return("-")
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

#' Make a plotly figure follow the app's light/dark theme.
#'
#' Plotly draws on its own white canvas, which turns into a bright rectangle
#' in the middle of a dark page unless the backgrounds are cleared explicitly.
plotly_theme <- function(p, dark = FALSE, legend_position = "top") {
  ink <- if (isTRUE(dark)) "#e6edf3" else "#1b1f24"
  grid <- if (isTRUE(dark)) "rgba(230,237,243,0.12)" else "rgba(27,31,36,0.10)"

  plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = ink, family = "Inter, system-ui, sans-serif"),
    xaxis = list(gridcolor = grid, zerolinecolor = grid),
    yaxis = list(gridcolor = grid, zerolinecolor = grid),
    hoverlabel = list(font = list(family = "Inter, system-ui, sans-serif")),
    legend = if (identical(legend_position, "top")) {
      list(orientation = "h", y = 1.12, x = 0)
    } else {
      list(orientation = "v")
    },
    margin = list(l = 50, r = 20, t = 30, b = 40)
  )
}
