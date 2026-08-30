# ---------------------------------------------------------------------------
# ui_kit.R
#
# Look and feel shared by every app in this repo: the Bootstrap theme (light
# and dark), the extra CSS, and the small KPI tile helper.
#
# The visual language mirrors www.abelhga.com ("coordinate field"): a cool
# green paper, teal/magenta/sand accents, Fraunces for headings, IBM Plex
# Sans for body text and IBM Plex Mono for data labels, near-square corners,
# and a faint background grid.
#
# Source of truth: shared/ui_kit.R
# Copies live in <app>/R/ui_kit.R. Run tools/sync_shared.sh after editing.
# ---------------------------------------------------------------------------

# --- Small helpers ---------------------------------------------------------

#' Fall back to `y` when `x` is missing (NULL or a single NA).
`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}

#' The site palette, light or dark. Values copied from the website's
#' tokens.css so the apps and the site read as one design.
app_palette <- function(dark = FALSE) {
  if (isTRUE(dark)) {
    list(
      paper    = "#0c1512", surface = "#131f1b", rule = "#26332e",
      ink      = "#e3e9e5", ink_soft = "#a9b8b1", muted = "#7f8f87",
      teal     = "#3fbfa5", magenta = "#e0709e", sand = "#dcae5c"
    )
  } else {
    list(
      paper    = "#edf0ec", surface = "#ffffff", rule = "#cdd6d0",
      ink      = "#10231f", ink_soft = "#3d534c", muted = "#6b7a72",
      teal     = "#0e7c6b", magenta = "#b8336a", sand = "#c8973f"
    )
  }
}

#' Bootstrap 5 theme, light or dark.
#'
#' Swapped at runtime through `session$setCurrentTheme()`, so the dark-mode
#' switch needs no page reload and no extra JavaScript dependency. The
#' palette CSS variables are emitted as part of the theme, so everything in
#' `app_styles()` recolours itself on the same switch.
app_theme <- function(dark = FALSE) {
  pal <- app_palette(dark)
  theme <- bslib::bs_theme(
    version = 5,
    bg = pal$paper,
    fg = pal$ink,
    primary   = pal$teal,
    secondary = pal$muted,
    success   = pal$teal,
    danger    = pal$magenta,
    warning   = pal$sand,
    info      = pal$teal,
    "border-color"       = pal$rule,
    "card-bg"            = pal$surface,
    "border-radius"      = "3px",
    "card-border-radius" = "3px",
    base_font = bslib::font_collection(
      bslib::font_google("IBM Plex Sans", local = FALSE), "system-ui", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      bslib::font_google("Fraunces", local = FALSE), "Georgia", "serif"
    ),
    code_font = bslib::font_collection(
      bslib::font_google("IBM Plex Mono", local = FALSE), "ui-monospace", "monospace"
    )
  )
  bslib::bs_add_rules(theme, sprintf(
    ":root { --app-paper: %s; --app-surface: %s; --app-rule: %s;
             --app-ink: %s; --app-ink-soft: %s; --app-muted: %s;
             --app-teal: %s; --app-magenta: %s; --app-sand: %s; }",
    pal$paper, pal$surface, pal$rule,
    pal$ink, pal$ink_soft, pal$muted,
    pal$teal, pal$magenta, pal$sand
  ))
}

#' CSS shared by the three network apps (and reused by the forecasting app).
#' Colours come from the --app-* variables the theme defines, so this sheet
#' is static and still follows the light/dark switch.
app_styles <- function() {
  shiny::HTML("
    /* The coordinate field: the site's signature background grid. Fixed and
       behind everything; page content is lifted above it. Off on phones. */
    body::before { content: ''; position: fixed; inset: 0; z-index: 0;
                   pointer-events: none;
                   background-image:
                     linear-gradient(to right, var(--app-rule) 1px, transparent 1px),
                     linear-gradient(to bottom, var(--app-rule) 1px, transparent 1px);
                   background-size: 32px 32px; opacity: .34;
                   -webkit-mask-image: radial-gradient(ellipse 120% 80% at 50% 0%, #000 20%, transparent 78%);
                   mask-image: radial-gradient(ellipse 120% 80% at 50% 0%, #000 20%, transparent 78%); }
    body > * { position: relative; }
    @media (max-width: 640px) { body::before { display: none; } }

    .app-subtitle { color: var(--app-ink-soft); margin-bottom: 0; }

    /* KPI tiles, shaped like the site's stat blocks: mono uppercase label,
       mono tabular figure, teal top rule on a plain surface. */
    .metric-grid { display: grid; gap: .75rem;
                   grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); }
    .metric-card { background: var(--app-surface);
                   border: 1px solid var(--app-rule);
                   border-radius: var(--bs-border-radius, 3px);
                   padding: .75rem 1rem;
                   border-top: 2px solid var(--app-teal); }
    .metric-value { font-family: var(--bs-font-monospace); font-size: 1.4rem;
                    font-weight: 500; line-height: 1.15;
                    font-variant-numeric: tabular-nums; }
    .metric-label { font-family: var(--bs-font-monospace); font-size: .68rem;
                    text-transform: uppercase; letter-spacing: .08em;
                    color: var(--app-muted); }
    .metric-hint { font-size: .75rem; color: var(--app-muted); }

    .card { border-color: var(--app-rule); box-shadow: none; }
    .nav-tabs .nav-link { border-top-left-radius: var(--bs-border-radius, 3px);
                          border-top-right-radius: var(--bs-border-radius, 3px); }
    .nav-tabs .nav-link.active { font-weight: 600; }
    .accordion-button { font-weight: 600; font-size: .9rem; }
    .dataTable { font-size: .9rem; }

    .ai-panel { display: flex; flex-direction: column; gap: .6rem; }
    .ai-controls { display: flex; gap: .75rem; align-items: flex-end; flex-wrap: wrap; }
    .ai-controls .form-group,
    .ai-controls .shiny-input-container { margin-bottom: 0; min-width: 190px; }
    .ai-run { white-space: nowrap; }
    .ai-panel > .shiny-input-container { margin-bottom: 0; }
    .ai-empty, .ai-error, .ai-answer { border-radius: var(--bs-border-radius, 3px);
                                       padding: .85rem 1rem;
                                       background: var(--app-surface);
                                       border: 1px solid var(--app-rule); }
    .ai-empty  { border-left: 3px solid var(--app-teal); font-size: .9rem; }
    .ai-error  { border-left: 3px solid var(--app-magenta); font-size: .9rem; }
    .ai-answer { border-left: 3px solid var(--app-teal); }
    .ai-answer > :last-child { margin-bottom: 0; }
    .status-note { font-size: .85rem; color: var(--app-muted); }
    .vis-network { outline: none; }

    /* Phones: one swipeable row of tabs instead of a tall stack, comfortable
       tap targets, and no plotly toolbar crowding the legend. */
    @media (max-width: 768px) {
      .nav-tabs, .card-header .nav { flex-wrap: nowrap; overflow-x: auto;
                                     -webkit-overflow-scrolling: touch;
                                     scrollbar-width: none; }
      .nav-tabs::-webkit-scrollbar, .card-header .nav::-webkit-scrollbar { display: none; }
      .nav-tabs .nav-link { white-space: nowrap; min-height: 44px;
                            display: inline-flex; align-items: center; gap: .35rem; }
      .modebar { display: none !important; }
    }
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
#' Also trims the modebar to the buttons that earn their place and keeps the
#' figure responsive, so the toolbar no longer collides with the legend.
plotly_theme <- function(p, dark = FALSE, legend_position = "top") {
  pal <- app_palette(dark)
  grid <- if (isTRUE(dark)) "rgba(227,233,229,0.12)" else "rgba(16,35,31,0.10)"
  mono <- "IBM Plex Mono, ui-monospace, monospace"
  body <- "IBM Plex Sans, system-ui, sans-serif"

  p <- plotly::layout(
    p,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = pal$ink, family = body),
    xaxis = list(gridcolor = grid, zerolinecolor = grid,
                 tickfont = list(family = mono, size = 11, color = pal$muted)),
    yaxis = list(gridcolor = grid, zerolinecolor = grid,
                 tickfont = list(family = mono, size = 11, color = pal$muted)),
    hoverlabel = list(font = list(family = body)),
    legend = if (identical(legend_position, "top")) {
      list(orientation = "h", y = 1.14, x = 0)
    } else {
      list(orientation = "v")
    },
    margin = list(l = 50, r = 20, t = 48, b = 40)
  )
  plotly::config(
    p,
    displaylogo = FALSE,
    responsive = TRUE,
    modeBarButtonsToRemove = c(
      "lasso2d", "select2d", "autoScale2d", "zoomIn2d", "zoomOut2d",
      "pan2d", "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    )
  )
}
