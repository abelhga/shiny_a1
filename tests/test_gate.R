# Checks for shared/gate.R — the funnel. No network: the Supabase call is
# never made (no SUPABASE_URL), and the modals are not rendered.
# Run from the repository root:  Rscript tests/run_all.R

suppressPackageStartupMessages(library(shiny))
source("shared/gate.R")

ok <- function(label) cat("  ok -", label, "\n")

# --- the decision table -------------------------------------------------------
d <- function(...) gate_decide(...)
stopifnot(d(1, FALSE, FALSE)$allowed,  d(1, FALSE, FALSE)$stage == "free")
stopifnot(!d(2, FALSE, FALSE)$allowed, d(2, FALSE, FALSE)$stage == "email")
stopifnot(d(2, TRUE, FALSE)$allowed,   d(2, TRUE, FALSE)$stage == "unlocked")
stopifnot(d(3, TRUE, FALSE)$allowed)
stopifnot(!d(4, TRUE, FALSE)$allowed,  d(4, TRUE, FALSE)$stage == "contact")
stopifnot(d(99, FALSE, TRUE)$allowed,  d(99, FALSE, TRUE)$stage == "owner")
stopifnot(d(NA, FALSE, FALSE)$allowed)   # a broken cookie is a first visit, not a lockout
ok("one free go, email unlocks two more, then contact; the owner never waits")

# --- counting visits ----------------------------------------------------------
now <- 1e9
c1 <- gate_count_visit(0, 0, now)
stopifnot(c1$uses == 1L, c1$counted, c1$last == now)
c2 <- gate_count_visit(1, now, now + 5 * 60)          # reload five minutes later
stopifnot(c2$uses == 1L, !c2$counted, c2$last == now)
c3 <- gate_count_visit(1, now, now + 31 * 60)         # back after the window
stopifnot(c3$uses == 2L, c3$counted, c3$last == now + 31 * 60)
stopifnot(gate_count_visit("abc", "xyz", now)$uses == 1L)
ok("a reconnect inside the window is the same visit; after it, a new one")

# --- owner detection ----------------------------------------------------------
stopifnot(gate_query_param("?owner=abc&x=1", "owner") == "abc")
stopifnot(gate_query_param("x=1", "owner") == "")
stopifnot(gate_query_param("?owner=a%20b", "owner") == "a b")
stopifnot(gate_is_owner("k", "", "k"), gate_is_owner("", "k", "k"))
stopifnot(!gate_is_owner("k", "k", ""))   # no key configured: nobody is the owner
stopifnot(!gate_is_owner("nope", "", "k"))
ok("the owner key is read from cookie or URL, and an empty key matches nobody")

# --- the lead --------------------------------------------------------------------
stopifnot(gate_valid_email("Ana@Example.com"), !gate_valid_email("ana"),
          !gate_valid_email("a @b.c"), !gate_valid_email(""))
body <- gate_lead_body("  Ana@Example.COM ", "forecasting", "es", "https://linkedin.com/", "UA")
stopifnot(identical(body$email, "ana@example.com"), body$app == "forecasting",
          body$lang == "es")
json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
stopifnot(grepl('"email":"ana@example.com"', json, fixed = TRUE))
# Without Supabase configured the lead is not stored but the funnel goes on.
old <- c(Sys.getenv("SUPABASE_URL"), Sys.getenv("SUPABASE_ANON_KEY"))
Sys.setenv(SUPABASE_URL = "", SUPABASE_ANON_KEY = "")
res <- gate_post_lead(gate_config(), body)
stopifnot(isTRUE(res$ok), !isTRUE(res$stored))
ok("the lead is normalised, serialised, and never blocks the visitor")

# --- the server, end to end without a browser -----------------------------------
# No on.exit here: at the top level of a file loaded with source(), on.exit
# runs immediately and would restore GATE_ENABLED before the test below sees
# it. The environment is put back at the end of the file instead.
old_gate <- Sys.getenv("GATE_ENABLED"); old_key <- Sys.getenv("OWNER_KEY")
Sys.setenv(GATE_ENABLED = "1", OWNER_KEY = "secret")
options(gate.render_modals = FALSE)

visit <- function(uses, last, email = FALSE, owner = "", query = "") {
  list(uses = uses, last = last, email = email, owner = owner, lang = "es-MX",
       query = query, referrer = "", ua = "test", nonce = 1)
}

shiny::testServer(
  function(input, output, session) {
    gate <- gate_server(input, output, session, app_id = "network")
    session$userData$gate <- gate
  },
  {
    g <- session$userData$gate
    session$setInputs(gate_state = visit(0, 0))
    stopifnot(isTRUE(g$allowed), g$stage == "free", g$lang == "es", g$uses == 1L)

    session$setInputs(gate_state = visit(1, 0))          # second visit, no email
    stopifnot(!isTRUE(g$allowed), g$stage == "email", g$uses == 2L)

    session$setInputs(gate_email = "not an email")
    session$setInputs(gate_email_submit = 1)
    stopifnot(!isTRUE(g$allowed))

    session$setInputs(gate_email = "ana@example.com")
    session$setInputs(gate_email_submit = 2)
    stopifnot(isTRUE(g$allowed), g$stage == "unlocked", isTRUE(g$email))

    session$setInputs(gate_state = visit(3, 0, email = TRUE))   # the fourth
    stopifnot(!isTRUE(g$allowed), g$stage == "contact")

    session$setInputs(gate_state = visit(9, 0, query = "?owner=secret"))
    stopifnot(isTRUE(g$allowed), g$stage == "owner", isTRUE(session$userData$owner))

    session$setInputs(gate_state = visit(9, 0, owner = "secret"))
    stopifnot(isTRUE(g$allowed), g$stage == "owner")
  }
)
ok("the server walks the whole funnel, and the owner key opens it from cookie or URL")

Sys.setenv(GATE_ENABLED = "")
shiny::testServer(
  function(input, output, session) session$userData$gate <- gate_server(input, output, session, "x"),
  {
    g <- session$userData$gate
    stopifnot(isTRUE(g$allowed), g$stage == "off")
    session$setInputs(gate_state = visit(50, 0))
    stopifnot(isTRUE(g$allowed))
  }
)
ok("with GATE_ENABLED unset the gate does nothing at all")

Sys.setenv(GATE_ENABLED = old_gate, OWNER_KEY = old_key,
           SUPABASE_URL = old[1], SUPABASE_ANON_KEY = old[2])
options(gate.render_modals = NULL)

# --- the URL as state, and the cache -------------------------------------------
q <- gate_build_query(list(time = "today+5-y", kw = "lavanda, jazmín", geo = "MX", empty = ""))
stopifnot(startsWith(q, "?geo=MX&kw="), grepl("time=today%2B5-y$", q))
back <- gate_parse_query(q)
stopifnot(identical(back$kw, "lavanda, jazmín"), identical(back$time, "today+5-y"), is.null(back$empty))
stopifnot(identical(gate_parse_query(""), list()), identical(gate_parse_query("?a")$a, ""))
ok("the analysis survives a round trip through the URL, with accents and pluses intact")

old_dir <- Sys.getenv("GATE_CACHE_DIR")
Sys.setenv(GATE_CACHE_DIR = file.path(tempdir(), paste0("gate-test-", Sys.getpid())))
gate_cache_clear()
n <- 0L
fetch <- function() { n <<- n + 1L; "value" }
stopifnot(gate_cached(c("k", "1"), fetch, now = 1000) == "value", n == 1L)
stopifnot(gate_cached(c("k", "1"), fetch, now = 1000 + 3600) == "value", n == 1L)   # memory hit
rm(list = ls(.gate_cache), envir = .gate_cache)                                      # new R process
stopifnot(gate_cached(c("k", "1"), fetch, now = 1000 + 3600) == "value", n == 1L)   # disk hit
stopifnot(gate_cached(c("k", "1"), fetch, now = 1000 + 7 * 3600) == "value", n == 2L) # expired
boom <- function() simpleError("rate limited")
stopifnot(inherits(gate_cached("err", boom, now = 1), "error"), inherits(gate_cached("err", boom, now = 1), "error"))
gate_cache_clear()
if (nzchar(old_dir)) Sys.setenv(GATE_CACHE_DIR = old_dir) else Sys.unsetenv("GATE_CACHE_DIR")
ok("the cache serves from memory, then from disk across R restarts, expires, and never caches an error")

stopifnot(grepl("^mailto:hi@abelhga.com\\?subject=", gate_mailto(GATE_COPY$es, "lavanda")),
          grepl("lavanda", utils::URLdecode(gate_mailto(GATE_COPY$en, "lavanda"))))
Sys.unsetenv("FEATURED_KW"); stopifnot(is.null(gate_featured()))
Sys.setenv(FEATURED_KW = "a, b ,", FEATURED_GEO = "MX")
stopifnot(identical(gate_featured()$kw, c("a", "b")), gate_featured()$geo == "MX")
Sys.unsetenv("FEATURED_KW"); Sys.unsetenv("FEATURED_GEO")
ok("mailto and the featured analysis read their configuration")
