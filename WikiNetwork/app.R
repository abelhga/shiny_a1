# ---------------------------------------------------------------------------
# Keyword Network Analysis - Wikipedia
#
# Turns Wikipedia's search suggestions (the MediaWiki OpenSearch API) for a
# seed term into an interactive co-occurrence network of article titles.
#
# This app previously shipped as a verbatim copy of the Amazon one - same
# endpoint, same marketplace picker, same "for Amazon" heading - so it never
# actually queried Wikipedia. It does now.
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

# The Wikimedia API policy asks for a descriptive User-Agent that identifies
# the tool; requests without one get rate limited or refused.
WIKI_USER_AGENT <- "shiny_a1 keyword-network/1.0 (https://github.com/abelhga/shiny_a1)"

WIKI_LANGUAGES <- c(
  "English (en.wikipedia.org)"    = "en",
  "Spanish (es.wikipedia.org)"    = "es",
  "French (fr.wikipedia.org)"     = "fr",
  "German (de.wikipedia.org)"     = "de",
  "Italian (it.wikipedia.org)"    = "it",
  "Portuguese (pt.wikipedia.org)" = "pt",
  "Dutch (nl.wikipedia.org)"      = "nl"
)

#' One Wikipedia OpenSearch lookup.
#'
#' Returns matching article titles. The response is a four-element array
#' (search term, titles, descriptions, urls); only the titles are used.
wikipedia_suggest <- function(query, lang = "en", limit = 50L) {
  query <- trimws(query)
  if (!nzchar(query)) return(character(0))

  url <- paste0(
    "https://", lang, ".wikipedia.org/w/api.php",
    "?action=opensearch",
    "&format=json",
    "&namespace=0",
    "&limit=", as.integer(limit),
    "&redirects=resolve",
    "&search=", utils::URLencode(query, reserved = TRUE)
  )

  response <- tryCatch(
    httr::GET(url, httr::user_agent(WIKI_USER_AGENT), httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::http_error(response)) return(character(0))

  body <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                     error = function(e) NULL)
  if (is.null(parsed) || length(parsed) < 2L) return(character(0))

  titles <- parsed[[2L]]
  if (is.null(titles)) return(character(0))
  as.character(unlist(titles, use.names = FALSE))
}

config <- list(
  title = "Keyword Network Analysis - Wikipedia",
  subtitle = paste("Expand a seed term through Wikipedia's article search, then read the",
                   "result as a network of the concepts its titles share."),
  source_label = "Wikipedia OpenSearch",
  seed_default = "machine learning",
  scope_label = "Wikipedia edition",
  scope_choices = WIKI_LANGUAGES,
  scope_default = "en",
  fetcher = function(query, scope) wikipedia_suggest(query, scope),
  scope_lang = function(scope) scope,
  footer_note = paste("Nodes are words taken from article titles, so clusters tend to map",
                      "onto subject areas. \"By vector\" depth follows titles into their own",
                      "searches, which is the quickest way to see how a topic branches.")
)

ui <- keyword_network_ui(config)
server <- keyword_network_server(config)

shinyApp(ui = ui, server = server)
