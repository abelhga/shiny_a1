# Checks for shared/ai_insights.R
# Run from the repository root:  Rscript tests/run_all.R
#
# Nothing here calls the OpenAI API. It checks the request that would be sent
# and how a response is read back.

suppressPackageStartupMessages({
  library(jsonlite)
  library(httr)
})
source("shared/ai_insights.R")

ok <- function(label) cat("  ok -", label, "\n")

# --- the request the apps send ---------------------------------------------
payload <- list(
  model = "gpt-5.6-luna",
  input = list(
    list(role = "system", content = "You are a concise data analyst."),
    list(role = "user", content = "Summarise this.")
  ),
  max_output_tokens = 1200L,
  reasoning = list(effort = "low"),
  text = list(verbosity = "medium")
)
body <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))

# `input` must serialise as an array of role/content objects, and the scalars
# must not come out as one-element arrays.
stopifnot(grepl('"model":"gpt-5.6-luna"', body, fixed = TRUE))
stopifnot(grepl('"input":[{"role":"system"', body, fixed = TRUE))
stopifnot(grepl('"max_output_tokens":1200', body, fixed = TRUE))
stopifnot(grepl('"reasoning":{"effort":"low"}', body, fixed = TRUE))
ok("the Responses API request body has the shape the API expects")

# --- reading the response --------------------------------------------------
# Reasoning items sit alongside the message in `output`; taking the first item
# would return nothing.
response <- jsonlite::fromJSON('{
  "status": "completed",
  "output": [
    {"type": "reasoning", "summary": []},
    {"type": "message", "content": [
      {"type": "output_text", "text": "Line one."},
      {"type": "output_text", "text": "Line two."}
    ]}
  ]
}', simplifyVector = FALSE)
stopifnot(identical(openai_output_text(response), "Line one.\nLine two."))
ok("output text is collected past the reasoning items")

stopifnot(identical(openai_output_text(jsonlite::fromJSON('{"output":[]}',
                                                          simplifyVector = FALSE)), ""))
stopifnot(identical(openai_output_text(list()), ""))
ok("an empty response reads as empty rather than erroring")

# --- configuration ---------------------------------------------------------
old_key <- Sys.getenv("OPENAI_API_KEY")
old_model <- Sys.getenv("OPENAI_MODEL")
old_base <- Sys.getenv("OPENAI_BASE_URL")
on.exit({
  Sys.setenv(OPENAI_API_KEY = old_key, OPENAI_MODEL = old_model,
             OPENAI_BASE_URL = old_base)
}, add = TRUE)

Sys.setenv(OPENAI_API_KEY = "")
stopifnot(!openai_available())
result <- openai_complete("hi")
stopifnot(!result$ok, grepl("OPENAI_API_KEY", result$error))
ok("a missing key is reported, not thrown")

Sys.setenv(OPENAI_BASE_URL = "https://example.test/v1")
stopifnot(openai_endpoint() == "https://example.test/v1/responses")
Sys.setenv(OPENAI_BASE_URL = "https://example.test/v1/")
stopifnot(openai_endpoint() == "https://example.test/v1/responses")
Sys.unsetenv("OPENAI_BASE_URL")
stopifnot(openai_endpoint() == "https://api.openai.com/v1/responses")
ok("OPENAI_BASE_URL is honoured, with or without a trailing slash")

Sys.setenv(OPENAI_MODEL = "gpt-5.6-terra")
stopifnot(openai_default_model() == "gpt-5.6-terra")
Sys.unsetenv("OPENAI_MODEL")
stopifnot(openai_default_model() == OPENAI_DEFAULT_MODEL)
ok("OPENAI_MODEL overrides the default model")

stopifnot(setequal(OPENAI_MODELS, c("gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol")))
stopifnot(OPENAI_DEFAULT_MODEL %in% OPENAI_MODELS)
stopifnot(OPENAI_DEFAULT_EFFORT %in% OPENAI_EFFORTS)
ok("the model and effort menus are self-consistent")
