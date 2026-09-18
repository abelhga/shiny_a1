# ---------------------------------------------------------------------------
# ai_insights.R
#
# Small Shiny module that turns the numbers an app has just computed into a
# short written commentary, using the OpenAI Responses API.
#
# Source of truth: shared/ai_insights.R
# Copies live in <app>/R/ai_insights.R so that every app folder stays
# self-contained and deployable on its own. Run tools/sync_shared.sh after
# editing this file.
#
# Configuration (never hard-code a key):
#   OPENAI_API_KEY    required, e.g. in ~/.Renviron or the app's .Renviron
#   OPENAI_MODEL      optional, overrides the default model
#   OPENAI_BASE_URL   optional, for Azure/proxy/compatible endpoints
# ---------------------------------------------------------------------------

# Current OpenAI text models (GPT-5.6 family). `gpt-5.6` is the alias that
# routes to Sol; the explicit ids are pinned here so a silent alias change
# cannot alter what the apps cost or how fast they answer.
OPENAI_MODELS <- c(
  "GPT-5.6 Luna  — fastest, cheapest"   = "gpt-5.6-luna",
  "GPT-5.6 Terra — balanced"            = "gpt-5.6-terra",
  "GPT-5.6 Sol   — highest quality"     = "gpt-5.6-sol"
)

# Reasoning effort accepted by the GPT-5.6 family. The apps default to "low":
# these are short summarisation prompts and latency is what the user feels.
OPENAI_EFFORTS <- c(
  "None (fastest)" = "none",
  "Low"            = "low",
  "Medium"         = "medium",
  "High"           = "high"
)

OPENAI_DEFAULT_MODEL  <- "gpt-5.6-luna"
OPENAI_DEFAULT_EFFORT <- "low"

# --- Spend guards ------------------------------------------------------------
# The key is one server-side env var shared by every visitor, and on a public
# host anyone can press the button. Two ceilings bound what a stranger can
# spend: per browser session (a refresh starts a new one, so this alone is
# only friction) and per calendar day for the whole process (the real wall,
# short of the monthly limit set on the key itself). Both are env-tunable so a
# private deployment can lift them.
AI_MAX_CALLS_PER_SESSION <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("AI_MAX_CALLS", "5")))
  if (is.na(v) || v < 0L) 5L else v
}
AI_MAX_CALLS_PER_DAY <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("AI_MAX_CALLS_PER_DAY", "200")))
  if (is.na(v) || v < 0L) 200L else v
}

# Process-wide daily counter. Lives in this environment (not in a session) on
# purpose: it is meant to be shared by every session the process serves.
.ai_daily <- new.env(parent = emptyenv())
.ai_daily$day   <- NA_character_
.ai_daily$calls <- 0L

#' Count one attempted call against today's ceiling.
#' @return TRUE if the call may proceed, FALSE if the daily ceiling is spent.
ai_daily_take <- function(now = Sys.Date()) {
  today <- as.character(now)
  if (!identical(.ai_daily$day, today)) {
    .ai_daily$day   <- today
    .ai_daily$calls <- 0L
  }
  if (.ai_daily$calls >= AI_MAX_CALLS_PER_DAY()) return(FALSE)
  .ai_daily$calls <- .ai_daily$calls + 1L
  TRUE
}

#' Reset the daily counter (tests).
ai_daily_reset <- function() {
  .ai_daily$day   <- NA_character_
  .ai_daily$calls <- 0L
  invisible(NULL)
}

openai_api_key <- function() trimws(Sys.getenv("OPENAI_API_KEY", ""))

openai_available <- function() nzchar(openai_api_key())

openai_default_model <- function() {
  from_env <- trimws(Sys.getenv("OPENAI_MODEL", ""))
  if (nzchar(from_env)) from_env else OPENAI_DEFAULT_MODEL
}

openai_endpoint <- function() {
  base <- trimws(Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1"))
  paste0(sub("/+$", "", base), "/responses")
}

# The Responses API returns a list of output items; assistant prose lives in
# the `output_text` chunks of the `message` items. Reasoning items are also in
# there and must be skipped, which is why this walks the structure rather than
# grabbing the first element.
openai_output_text <- function(parsed) {
  items <- parsed$output
  if (is.null(items) || !length(items)) return("")
  chunks <- character(0)
  for (item in items) {
    if (!identical(item$type, "message") || is.null(item$content)) next
    for (part in item$content) {
      if (isTRUE(part$type %in% c("output_text", "text")) && !is.null(part$text)) {
        chunks <- c(chunks, part$text)
      }
    }
  }
  paste(chunks, collapse = "\n")
}

openai_error_message <- function(response) {
  body <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  msg <- tryCatch(body$error$message, error = function(e) NULL)
  if (is.null(msg) || !nzchar(msg)) {
    msg <- paste("HTTP", httr::status_code(response))
  }
  msg
}

#' Ask an OpenAI model for a short piece of commentary.
#'
#' @return list(ok = logical, text = character, error = character)
openai_complete <- function(user_prompt,
                            system_prompt = "You are a concise data analyst.",
                            model = openai_default_model(),
                            effort = OPENAI_DEFAULT_EFFORT,
                            verbosity = "medium",
                            max_output_tokens = 1200L,
                            timeout = 120,
                            retries = 2L) {

  key <- openai_api_key()
  if (!nzchar(key)) {
    return(list(ok = FALSE, text = "",
                error = paste("No OPENAI_API_KEY found. Add it to your .Renviron",
                              "(OPENAI_API_KEY=sk-...) and restart R.")))
  }

  payload <- list(
    model = model,
    input = list(
      list(role = "system", content = system_prompt),
      list(role = "user",   content = user_prompt)
    ),
    max_output_tokens = as.integer(max_output_tokens)
  )
  if (!is.null(effort)    && nzchar(effort))    payload$reasoning <- list(effort = effort)
  if (!is.null(verbosity) && nzchar(verbosity)) payload$text      <- list(verbosity = verbosity)

  post_once <- function(body) {
    tryCatch(
      httr::POST(
        openai_endpoint(),
        httr::add_headers(Authorization = paste("Bearer", key)),
        httr::content_type_json(),
        httr::timeout(timeout),
        body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
        encode = "raw"
      ),
      error = function(e) e
    )
  }

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    response <- post_once(payload)

    if (inherits(response, "error")) {
      if (attempt > retries) {
        return(list(ok = FALSE, text = "",
                    error = paste("Could not reach the OpenAI API:", conditionMessage(response))))
      }
      Sys.sleep(2^attempt)
      next
    }

    status <- httr::status_code(response)

    if (status >= 200 && status < 300) break

    # Some deployments (older proxies, Azure passthrough) reject the newer
    # tuning fields. Retry once with a plain request rather than failing.
    if (status == 400 && (!is.null(payload$reasoning) || !is.null(payload$text))) {
      payload$reasoning <- NULL
      payload$text <- NULL
      next
    }

    retryable <- status == 429 || status >= 500
    if (retryable && attempt <= retries) {
      Sys.sleep(2^attempt)
      next
    }

    return(list(ok = FALSE, text = "", error = openai_error_message(response)))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed)) {
    return(list(ok = FALSE, text = "", error = "The OpenAI response could not be parsed."))
  }

  text <- openai_output_text(parsed)
  if (!nzchar(text)) {
    reason <- tryCatch(parsed$incomplete_details$reason, error = function(e) NULL)
    if (identical(parsed$status, "incomplete")) {
      return(list(ok = FALSE, text = "",
                  error = paste0("The model ran out of budget before answering",
                                 if (!is.null(reason)) paste0(" (", reason, ")") else "",
                                 ". Try a lower reasoning effort or a larger token budget.")))
    }
    return(list(ok = FALSE, text = "", error = "The model returned an empty answer."))
  }

  list(ok = TRUE, text = text, error = "")
}

# --- Shiny module ----------------------------------------------------------

aiInsightsUI <- function(id, label = "Ask the model") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "ai-panel",
    shiny::div(
      class = "ai-controls",
      shiny::selectInput(ns("model"), "OpenAI model",
                         choices = OPENAI_MODELS, selected = openai_default_model()),
      shiny::selectInput(ns("effort"), "Reasoning effort",
                         choices = OPENAI_EFFORTS, selected = OPENAI_DEFAULT_EFFORT),
      shiny::actionButton(ns("run"), label, class = "btn btn-primary ai-run",
                          icon = shiny::icon("wand-magic-sparkles"))
    ),
    shiny::textInput(ns("focus"), NULL, "",
                     placeholder = paste("Optional: a question or angle to focus on,",
                                         "e.g. \"which cluster should I target first?\""),
                     width = "100%"),
    shiny::uiOutput(ns("quota")),
    shiny::uiOutput(ns("answer"))
  )
}

#' @param context reactive returning the plain-text briefing sent to the model,
#'   or NULL/"" when the app has nothing to talk about yet.
#' @param system_prompt persona/instructions for the model.
aiInsightsServer <- function(id, context,
                             system_prompt = "You are a concise data analyst.",
                             max_output_tokens = 1200L) {
  shiny::moduleServer(id, function(input, output, session) {

    result <- shiny::reactiveVal(NULL)
    # Attempts this session, counted before the request goes out so that a
    # failing call still spends one: retrying into a 429 is exactly the loop
    # the ceiling exists to stop.
    calls  <- shiny::reactiveVal(0L)

    shiny::observeEvent(input$run, {
      per_session <- AI_MAX_CALLS_PER_SESSION()
      if (calls() >= per_session) {
        result(list(ok = FALSE, text = "",
                    error = sprintf(paste("This session has used its %d read-outs.",
                                          "Reload the app to start a new session."),
                                    per_session)))
        return()
      }
      calls(calls() + 1L)

      if (!ai_daily_take()) {
        result(list(ok = FALSE, text = "",
                    error = paste("The AI panel has reached its daily limit on this host.",
                                  "Everything else keeps working; try again tomorrow.")))
        return()
      }

      briefing <- tryCatch(context(), error = function(e) NULL)

      if (is.null(briefing) || !nzchar(paste(briefing, collapse = ""))) {
        result(list(ok = FALSE, text = "",
                    error = "Run the analysis first — there is nothing to summarise yet."))
        return()
      }

      focus <- trimws(input$focus %||% "")
      user_prompt <- paste(briefing, collapse = "\n")
      if (nzchar(focus)) {
        user_prompt <- paste0(
          user_prompt,
          "\n\nThe user specifically asks: \"", focus, "\". Address this",
          " question directly as part of the commentary, still using only the",
          " data in the briefing."
        )
      }

      shiny::withProgress(message = "Asking OpenAI…", value = 0.4, {
        result(openai_complete(
          user_prompt       = user_prompt,
          system_prompt     = system_prompt,
          model             = input$model,
          effort            = input$effort,
          max_output_tokens = max_output_tokens
        ))
      })
    })

    output$quota <- shiny::renderUI({
      per_session <- AI_MAX_CALLS_PER_SESSION()
      if (!openai_available() || per_session <= 0L) return(NULL)
      left <- max(0L, per_session - calls())
      shiny::div(class = "ai-quota small text-muted",
                 sprintf("%d of %d read-outs left this session.", left, per_session))
    })

    output$answer <- shiny::renderUI({
      res <- result()

      if (is.null(res)) {
        note <- if (openai_available()) {
          "Press the button to get a written read-out of the results above."
        } else {
          paste("Set OPENAI_API_KEY in your .Renviron to enable this panel.",
                "Everything else in the app works without it.")
        }
        return(shiny::div(class = "ai-empty", note))
      }

      if (!isTRUE(res$ok)) {
        return(shiny::div(class = "ai-error", shiny::strong("OpenAI: "), res$error))
      }

      shiny::div(class = "ai-answer", shiny::markdown(res$text))
    })

    result
  })
}
