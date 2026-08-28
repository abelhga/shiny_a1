# ---------------------------------------------------------------------------
# Keyword Network Analysis - Amazon
#
# Turns Amazon's search autocomplete for a seed keyword into an interactive
# co-occurrence network, per marketplace.
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

AMAZON_USER_AGENT <- "shiny_a1 keyword-network (https://github.com/abelhga/shiny_a1)"

# Marketplace id -> language, so stop words match the market being queried
# instead of always being English.
AMAZON_MARKETPLACES <- data.frame(
  name   = c("Amazon.com.br (Brazil)", "Amazon.ca (Canada)", "Amazon.com.mx (Mexico)",
             "Amazon.com (United States)", "Amazon.de (Germany)",
             "Amazon.co.uk (United Kingdom)", "Amazon.fr (France)",
             "Amazon.es (Spain)", "Amazon.it (Italy)", "Amazon.nl (Netherlands)"),
  id     = c("A2Q3Y263D00KWC", "A2EUQ1WTGCTBG2", "A1AM78C64UM0Y8",
             "ATVPDKIKX0DER", "A1PA6795UKMFR9", "A1F83G8C2ARO7P",
             "A13V1IB3VIYZZH", "A1RKKUPIHCS9HS", "APJ6JRA9NG5V4",
             "A1805IZSGTT6HS"),
  lang   = c("pt", "en", "es", "en", "de", "en", "fr", "es", "it", "nl"),
  stringsAsFactors = FALSE
)

AMAZON_CHOICES <- setNames(AMAZON_MARKETPLACES$id, AMAZON_MARKETPLACES$name)

amazon_language <- function(market_id) {
  hit <- AMAZON_MARKETPLACES$lang[match(market_id, AMAZON_MARKETPLACES$id)]
  if (length(hit) && !is.na(hit)) hit else "en"
}

#' One Amazon autocomplete lookup.
#'
#' The endpoint changes shape when it is throttled or when a marketplace
#' returns nothing, so every step is guarded and an unusable response becomes
#' an empty result rather than an error that stops the whole harvest.
amazon_suggest <- function(query, market_id) {
  query <- trimws(query)
  if (!nzchar(query)) return(character(0))

  url <- paste0(
    "https://completion.amazon.com/api/2017/suggestions",
    "?mid=", utils::URLencode(market_id, reserved = TRUE),
    "&alias=aps",
    "&prefix=", utils::URLencode(query, reserved = TRUE)
  )

  response <- tryCatch(
    httr::GET(url, httr::user_agent(AMAZON_USER_AGENT), httr::timeout(15)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::http_error(response)) return(character(0))

  body <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE),
                     error = function(e) NULL)

  values <- tryCatch(parsed$suggestions$value, error = function(e) NULL)
  if (is.null(values)) return(character(0))
  as.character(unlist(values, use.names = FALSE))
}

config <- list(
  title = "Keyword Network Analysis - Amazon",
  subtitle = paste("Expand a seed keyword through Amazon's search autocomplete, then read",
                   "the result as a network of the words shoppers type together."),
  source_label = "Amazon autocomplete",
  seed_default = "iphone",
  scope_label = "Marketplace",
  scope_choices = AMAZON_CHOICES,
  scope_default = "ATVPDKIKX0DER",
  fetcher = function(query, scope) amazon_suggest(query, scope),
  scope_lang = amazon_language,
  footer_note = paste("Results reflect the selected marketplace. Stop words follow that",
                      "marketplace's main language, so switching markets changes what is",
                      "filtered out as well as what is returned.")
)

ui <- keyword_network_ui(config)
server <- keyword_network_server(config)

shinyApp(ui = ui, server = server)
