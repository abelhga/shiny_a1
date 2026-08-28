# ---------------------------------------------------------------------------
# Installs everything the apps in this repository need.
#
#   Rscript install_dependencies.R
#
# Prophet pulls in rstan and takes a while the first time. Everything else is
# quick.
# ---------------------------------------------------------------------------

required <- c(
  # shared across all four apps
  "shiny",            # >= 1.7.0 for shiny::markdown()
  "bslib",            # >= 0.5.0 for page_sidebar()/navset_card_tab()
  "dplyr",
  "httr",
  "jsonlite",
  "DT",
  "plotly",
  "shinycssloaders",

  # keyword network apps
  "igraph",           # >= 2.0 recommended

  # forecasting app
  "gtrendsR",
  "prophet",
  "lubridate",
  "countrycode"
)

# Nice to have, not required: the apps fall back cleanly when these are absent.
#   tm       richer stop-word lists (bundled lists are used otherwise)
#   stringi  locale-independent lower-casing of accented words
optional <- c("tm", "stringi")

installed <- rownames(utils::installed.packages())

missing_required <- setdiff(required, installed)
if (length(missing_required)) {
  message("Installing: ", paste(missing_required, collapse = ", "))
  utils::install.packages(missing_required)
} else {
  message("All required packages are already installed.")
}

missing_optional <- setdiff(optional, installed)
if (length(missing_optional)) {
  message("Installing optional extras: ", paste(missing_optional, collapse = ", "))
  tryCatch(utils::install.packages(missing_optional),
           error = function(e) message("Optional packages skipped: ", conditionMessage(e)))
}

# Minimum versions the apps rely on.
minimums <- list(shiny = "1.7.0", bslib = "0.5.0", igraph = "1.3.0")
for (pkg in names(minimums)) {
  if (pkg %in% rownames(utils::installed.packages())) {
    have <- as.character(utils::packageVersion(pkg))
    if (utils::compareVersion(have, minimums[[pkg]]) < 0) {
      warning(sprintf("%s %s is older than the %s these apps expect; run install.packages(\"%s\").",
                      pkg, have, minimums[[pkg]], pkg), call. = FALSE)
    }
  }
}

message("Done. Copy .Renviron.example to .Renviron and add your OPENAI_API_KEY ",
        "to enable the AI insights panels (everything else works without one).")
