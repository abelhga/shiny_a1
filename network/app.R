# ---------------------------------------------------------------------------
# Keyword Network Analysis - Google Suggest
#
# Turns Google's autocomplete suggestions for a seed keyword into an
# interactive co-occurrence network.
#
# Packages: shiny, bslib, dplyr, igraph, visNetwork, httr, jsonlite, DT,
#           plotly, shinycssloaders (tm is optional, for richer stop words).
# See ../install_dependencies.R
#
# Shared logic lives in R/ (a copy of ../shared). Run tools/sync_shared.sh
# after editing the originals.
# ---------------------------------------------------------------------------

library(shiny)
library(dplyr)

# Shiny sources everything in R/ automatically when the app is launched from
# its own folder. This makes `source("app.R")` work too.
for (.f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(.f)

GOOGLE_USER_AGENT <- "shiny_a1 keyword-network (https://github.com/abelhga/shiny_a1)"

GOOGLE_LANGUAGES <- c(
  "English"    = "en",
  "Spanish"    = "es",
  "French"     = "fr",
  "German"     = "de",
  "Italian"    = "it",
  "Portuguese" = "pt",
  "Dutch"      = "nl",
  "Any / unset" = ""
)

#' One Google Suggest lookup.
#'
#' Uses the JSON endpoint rather than the XML one the app used to call: it
#' needs no XML parser, and a malformed or throttled response fails cleanly
#' instead of blowing up mid-render.
google_suggest <- function(query, lang = "en") {
  query <- trimws(query)
  if (!nzchar(query)) return(character(0))

  url <- paste0(
    "https://suggestqueries.google.com/complete/search",
    "?client=firefox",
    "&hl=", utils::URLencode(lang, reserved = TRUE),
    "&q=", utils::URLencode(query, reserved = TRUE)
  )

  response <- tryCatch(
    httr::GET(url, httr::user_agent(GOOGLE_USER_AGENT), httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::http_error(response)) return(character(0))

  body <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || length(parsed) < 2L) return(character(0))

  suggestions <- parsed[[2L]]
  if (is.null(suggestions)) return(character(0))
  as.character(unlist(suggestions, use.names = FALSE))
}

config <- list(
  title = "Keyword Network Analysis - Google Suggest",
  subtitle = paste("Expand a seed keyword through Google's autocomplete, then read the",
                   "result as a network of the words people search together."),
  source_label = "Google Suggest",
  seed_default = "where can i buy",
  scope_label = "Language",
  scope_choices = GOOGLE_LANGUAGES,
  scope_default = "en",
  fetcher = function(query, scope) google_suggest(query, scope),
  scope_lang = function(scope) scope,
  footer_note = paste("Suggestions are tailored to the location Google sees you from.",
                      "To explore another market, use a VPN as well as changing the language.")
)

ui <- keyword_network_ui(config)
server <- keyword_network_server(config)

shinyApp(ui = ui, server = server)
