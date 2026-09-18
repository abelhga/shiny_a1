# ---------------------------------------------------------------------------
# gate.R
#
# The funnel: one free go, two more for an email address, then "write to me
# on LinkedIn". Later, paying. It exists because these apps are the landing
# page of a LinkedIn post, and a landing page that gives everything away for
# free forever teaches nobody anything about who was interested.
#
# Source of truth: shared/gate.R. Copies live in <app>/R/gate.R so that every
# app folder stays self-contained. Run tools/sync_shared.sh after editing.
#
# How it works
#   - State lives in first-party cookies on the app's own host, one year:
#       apps_uses   how many occasions this browser has spent
#       apps_last   when the last one was counted (unix seconds)
#       apps_email  "1" once an email was left
#       apps_owner  the owner's key, which switches the whole thing off
#     A small script in <head> reads them on connect and hands them to Shiny;
#     Shiny decides, and writes them back through a custom message. No shinyjs,
#     no extra dependencies.
#   - "One occasion" is one visit to an app. A reload or a dropped websocket
#     within GATE_REVISIT_MINUTES is the same visit, not a new one: nobody
#     should lose a go to a flaky connection.
#   - It is a funnel, not a fortress. Clearing cookies starts over. That is
#     fine: the point is to learn who cares enough to leave an address, not to
#     stop a determined person.
#
# Configuration (environment; nothing set means the gate is OFF, so a local
# run or the Posit mirror behave as before):
#   GATE_ENABLED=1          turn it on
#   OWNER_KEY               ?owner=<key> in the URL sets a cookie that bypasses
#                           the gate, the request budget and the AI quota
#   OWNER_REQUEST_BUDGET    the owner's crawl ceiling (default 5000)
#   SUPABASE_URL            where leads go (PostgREST); unset = not stored,
#   SUPABASE_ANON_KEY       the funnel still unlocks, and the log says so
#   LINKEDIN_URL            the "write to me" destination
#   PUBLIC_SITE_URL         the site the apps belong to (for the copy and OG)
# ---------------------------------------------------------------------------

GATE_FREE_USES      <- 1L   # occasions before the email ask
GATE_TOTAL_USES     <- 3L   # occasions in total once an email was left
GATE_REVISIT_MINUTES <- 30  # a reconnect within this window is the same visit

gate_config <- function() {
  list(
    enabled      = identical(trimws(Sys.getenv("GATE_ENABLED", "")), "1"),
    owner_key    = trimws(Sys.getenv("OWNER_KEY", "")),
    supabase_url = sub("/+$", "", trimws(Sys.getenv("SUPABASE_URL", ""))),
    supabase_key = trimws(Sys.getenv("SUPABASE_ANON_KEY", "")),
    linkedin     = trimws(Sys.getenv("LINKEDIN_URL", "https://www.linkedin.com/in/abelhga/")),
    site         = sub("/+$", "", trimws(Sys.getenv("PUBLIC_SITE_URL", "https://www.abelhga.com")))
  )
}

# One line at start-up saying what this process actually sees. Environment
# variables do not reach R on their own under Shiny Server (su --login drops
# them); this is how a deployment proves they did. No secrets: only whether
# each one is set.
local({
  cfg <- gate_config()
  message(sprintf("[gate] config: enabled=%s owner_key=%s supabase=%s featured=%s openai=%s",
                  cfg$enabled, nzchar(cfg$owner_key), nzchar(cfg$supabase_url),
                  nzchar(Sys.getenv("FEATURED_KW", "")), nzchar(Sys.getenv("OPENAI_API_KEY", ""))))
})

OWNER_REQUEST_BUDGET <- function() {
  v <- suppressWarnings(as.integer(Sys.getenv("OWNER_REQUEST_BUDGET", "5000")))
  if (is.na(v) || v < 1L) 5000L else v
}

# --- pure logic (tested without Shiny) ----------------------------------------

#' Given what the cookies say, is this visitor allowed to run the app, and
#' which stage of the funnel are they at?
#'
#' `uses` already includes the current visit.
#' @return list(allowed = logical, stage = "owner"|"free"|"email"|"unlocked"|"contact")
gate_decide <- function(uses, has_email, owner) {
  uses <- suppressWarnings(as.integer(uses))
  if (is.na(uses)) uses <- 1L
  if (isTRUE(owner))              return(list(allowed = TRUE,  stage = "owner"))
  if (uses <= GATE_FREE_USES)     return(list(allowed = TRUE,  stage = "free"))
  if (!isTRUE(has_email))         return(list(allowed = FALSE, stage = "email"))
  if (uses <= GATE_TOTAL_USES)    return(list(allowed = TRUE,  stage = "unlocked"))
  list(allowed = FALSE, stage = "contact")
}

#' Count this connection as a visit, unless the last one was counted moments
#' ago (a reload, a reconnect). Returns the cookie values to write back.
gate_count_visit <- function(uses, last, now = as.numeric(Sys.time()),
                             window_minutes = GATE_REVISIT_MINUTES) {
  uses <- suppressWarnings(as.integer(uses)); if (is.na(uses) || uses < 0L) uses <- 0L
  last <- suppressWarnings(as.numeric(last)); if (is.na(last)) last <- 0
  same_visit <- uses > 0L && (now - last) < window_minutes * 60
  if (!same_visit) uses <- uses + 1L
  list(uses = uses, last = if (same_visit) last else now, counted = !same_visit)
}

#' One query-string parameter, decoded, or "" when absent.
gate_query_param <- function(query, name) {
  query <- sub("^\\?", "", as.character(query %||% ""))
  if (!nzchar(query)) return("")
  for (pair in strsplit(query, "&", fixed = TRUE)[[1]]) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 1L && identical(kv[[1]], name)) {
      return(utils::URLdecode(if (length(kv) >= 2L) kv[[2]] else ""))
    }
  }
  ""
}

#' Owner if either the cookie or the URL carries the key. An empty key never
#' matches anything: with OWNER_KEY unset there is no owner.
gate_is_owner <- function(cookie_value, query_value, owner_key) {
  if (!nzchar(owner_key)) return(FALSE)
  identical(cookie_value, owner_key) || identical(query_value, owner_key)
}

#' A deliberately loose email check: something, an @, something with a dot.
#' It exists to catch typos, not to police addresses.
gate_valid_email <- function(email) {
  email <- trimws(as.character(email %||% ""))
  nchar(email) <= 254L && grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)
}

#' The row that goes to Supabase. Lower-cased and trimmed so the unique index
#' on email does its job.
gate_lead_body <- function(email, app, lang = "en", referrer = "", user_agent = "",
                           context = "") {
  list(
    email      = tolower(trimws(email)),
    app        = substr(as.character(app), 1L, 40L),
    lang       = substr(as.character(lang), 1L, 8L),
    # What they were analysing when they left the address. What someone
    # forecasts says what they work on: it qualifies the contact.
    context    = substr(as.character(context %||% ""), 1L, 300L),
    referrer   = substr(as.character(referrer %||% ""), 1L, 500L),
    user_agent = substr(as.character(user_agent %||% ""), 1L, 300L)
  )
}

#' Store a lead through PostgREST with the publishable key. Row-level security
#' on the table lets that key insert and nothing else, which is why it can
#' live in the container. A 409 means the address is already there: for the
#' visitor that is a success, so it is reported as one.
gate_post_lead <- function(cfg, body, timeout = 10) {
  if (!nzchar(cfg$supabase_url) || !nzchar(cfg$supabase_key)) {
    message("[gate] SUPABASE_URL/SUPABASE_ANON_KEY not set: lead for ",
            body$email, " NOT stored")
    return(list(ok = TRUE, stored = FALSE))
  }
  res <- tryCatch(
    httr::POST(
      paste0(cfg$supabase_url, "/rest/v1/web_leads"),
      httr::add_headers(
        apikey        = cfg$supabase_key,
        Authorization = paste("Bearer", cfg$supabase_key),
        Prefer        = "return=minimal"
      ),
      httr::content_type_json(),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(timeout)
    ),
    error = function(e) NULL
  )
  if (is.null(res)) return(list(ok = FALSE, stored = FALSE, error = "network"))
  code <- httr::status_code(res)
  if (code %in% c(200L, 201L)) return(list(ok = TRUE, stored = TRUE))
  if (code == 409L)            return(list(ok = TRUE, stored = FALSE, error = "duplicate"))
  message("[gate] Supabase answered ", code, ": ",
          substr(httr::content(res, as = "text", encoding = "UTF-8"), 1L, 200L))
  list(ok = FALSE, stored = FALSE, error = paste0("http_", code))
}

#' One funnel event (visit, email_asked, email_left, contact_shown,
#' contact_click, share). Fire and forget: a failure is logged and never
#' surfaces to the visitor. The structured log line is the fallback when
#' Supabase is not configured, and the quick answer in Railway's logs.
gate_post_event <- function(cfg, event, app, stage = "", lang = "en", timeout = 3) {
  message(sprintf("[gate] event=%s app=%s stage=%s lang=%s", event, app, stage, lang))
  if (!nzchar(cfg$supabase_url) || !nzchar(cfg$supabase_key)) return(invisible(FALSE))
  body <- list(event = substr(event, 1L, 40L), app = substr(app, 1L, 40L),
               stage = substr(as.character(stage %||% ""), 1L, 40L),
               lang = substr(as.character(lang %||% ""), 1L, 8L))
  res <- tryCatch(
    httr::POST(
      paste0(cfg$supabase_url, "/rest/v1/web_events"),
      httr::add_headers(apikey = cfg$supabase_key,
                        Authorization = paste("Bearer", cfg$supabase_key),
                        Prefer = "return=minimal"),
      httr::content_type_json(),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::timeout(timeout)
    ),
    error = function(e) NULL
  )
  ok <- !is.null(res) && httr::status_code(res) %in% c(200L, 201L)
  if (!ok) message("[gate] event not stored: ", if (is.null(res)) "network" else httr::status_code(res))
  invisible(ok)
}

# --- the URL as state: deep links, sharing, surviving a reload -------------------

#' Serialise the analysis parameters into a query string. Only non-empty
#' values travel; keys are sorted so the same analysis always gives the same
#' link (and the same cache key).
gate_build_query <- function(params) {
  params <- params[vapply(params, function(v) !is.null(v) && nzchar(as.character(v)[1]), logical(1))]
  if (!length(params)) return("")
  params <- params[order(names(params))]
  paste0("?", paste(
    vapply(names(params), function(k) paste0(k, "=", utils::URLencode(as.character(params[[k]])[1], reserved = TRUE)),
           character(1)),
    collapse = "&"))
}

#' The inverse: "?a=1&b=x%20y" -> list(a = "1", b = "x y"). Unknown keys are
#' kept; the caller picks what it understands.
gate_parse_query <- function(query) {
  query <- sub("^\\?", "", as.character(query %||% ""))
  if (!nzchar(query)) return(list())
  out <- list()
  for (pair in strsplit(query, "&", fixed = TRUE)[[1]]) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 1L && nzchar(kv[[1]])) {
      out[[utils::URLdecode(kv[[1]])]] <- utils::URLdecode(if (length(kv) >= 2L) paste(kv[-1], collapse = "=") else "")
    }
  }
  out
}

#' Write the current analysis into the address bar without reloading. A
#' forced reload then reproduces the analysis instead of losing it, and the
#' same URL is what the share button copies.
gate_set_url <- function(session, params) {
  q <- gate_build_query(params)
  shiny::updateQueryString(if (nzchar(q)) q else "?", mode = "replace", session = session)
  invisible(q)
}

#' The share button: Web Share API on phones, clipboard elsewhere. The label
#' is bilingual because the app is English and the LinkedIn audience is not.
gate_share_button <- function() {
  shiny::tags$button(
    id = "gate_share", type = "button",
    class = "btn btn-outline-secondary w-100 mt-2 gate-share",
    onclick = "window.gateShare && window.gateShare()",
    shiny::icon("share-nodes"), " Compartir este análisis · Share"
  )
}

# --- a small cache for upstream calls ------------------------------------------
# The visitors of one post mostly ask for the same terms, and Google rate
# limits a datacenter IP. Keeping each answer for a few hours means the first
# click after the post never has to reach Google at all.
#
# Two layers: memory for this R process, and disk (GATE_CACHE_DIR) because
# Shiny Server kills an idle R process and starts a fresh one for the next
# visitor - a memory-only cache would be empty exactly when it matters most.
.gate_cache <- new.env(parent = emptyenv())

gate_cache_dir <- function() {
  d <- Sys.getenv("GATE_CACHE_DIR", file.path(tempdir(), "gate-cache"))
  if (!dir.exists(d)) tryCatch(dir.create(d, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  d
}

gate_cache_file <- function(key) {
  safe <- substr(gsub("[^A-Za-z0-9]+", "_", key), 1L, 120L)
  file.path(gate_cache_dir(), paste0(safe, "_", nchar(key), ".rds"))
}

gate_cached <- function(key, fetch, ttl_seconds = 6 * 3600, now = as.numeric(Sys.time())) {
  key <- paste(key, collapse = "\u001f")
  hit <- .gate_cache[[key]]
  if (!is.null(hit) && (now - hit$at) < ttl_seconds) return(hit$value)
  f <- gate_cache_file(key)
  if (file.exists(f)) {
    hit <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(hit) && (now - hit$at) < ttl_seconds) {
      assign(key, hit, envir = .gate_cache)
      return(hit$value)
    }
  }
  value <- fetch()
  # Errors are not cached: the next visitor gets a fresh attempt.
  if (!inherits(value, "error") && !is.null(value)) {
    entry <- list(at = now, value = value)
    assign(key, entry, envir = .gate_cache)
    tryCatch(saveRDS(entry, f), error = function(e) NULL)
  }
  value
}

gate_cache_clear <- function() {
  rm(list = ls(.gate_cache), envir = .gate_cache)
  unlink(list.files(gate_cache_dir(), full.names = TRUE))
  invisible(NULL)
}

#' The featured analysis: the one the LinkedIn post links to. Warmed at app
#' start so the first click after the post is served from cache, not from
#' Google. Env: FEATURED_KW (comma separated), FEATURED_GEO, FEATURED_TIME.
gate_featured <- function() {
  kw <- trimws(unlist(strsplit(Sys.getenv("FEATURED_KW", ""), ",")))
  kw <- kw[nzchar(kw)]
  if (!length(kw)) return(NULL)
  list(kw = kw, geo = Sys.getenv("FEATURED_GEO", "MX"), time = Sys.getenv("FEATURED_TIME", "today+5-y"))
}

# --- copy, in the visitor's language ------------------------------------------

GATE_COPY <- list(
  en = list(
    email_title = "Liked it? Two more goes for your email",
    email_body  = paste(
      "The first run is on the house. Leave an email and you get two more,",
      "plus a heads-up when the tool goes public. Nothing else, no spam."),
    email_label = "Email",
    email_button = "Unlock two more",
    email_consent = "Your address is used only to tell you about this tool.",
    email_invalid = "That does not look like an email address.",
    email_failed  = "Could not save that right now. Try again in a moment.",
    contact_title = "You have used your three goes",
    contact_body  = paste(
      "That is the free tour. If this is useful for your work, tell me what",
      "you would do with it: I read every message."),
    contact_button = "Write to me on LinkedIn",
    contact_mail = "or send an email",
    mail_subject = "About the search tools",
    blocked = "This session is locked: see the message on screen."
  ),
  es = list(
    email_title = "¿Te sirvió? Dos usos más por tu correo",
    email_body  = paste(
      "El primer uso va por la casa. Deja un correo y te desbloqueo dos más,",
      "y te aviso cuando la herramienta sea pública. Nada más, sin spam."),
    email_label = "Correo",
    email_button = "Desbloquear dos más",
    email_consent = "Tu correo se usa solo para avisarte de esta herramienta.",
    email_invalid = "Eso no parece un correo.",
    email_failed  = "No se pudo guardar ahora mismo. Inténtalo en un momento.",
    contact_title = "Ya usaste tus tres ocasiones",
    contact_body  = paste(
      "Hasta aquí llega la prueba gratis. Si esto te sirve para tu trabajo,",
      "cuéntame qué harías con ello: leo todos los mensajes."),
    contact_button = "Escríbeme por LinkedIn",
    contact_mail = "o mándame un correo",
    mail_subject = "Sobre las herramientas de búsqueda",
    blocked = "Esta sesión está bloqueada: mira el aviso en pantalla."
  )
)

gate_lang <- function(x) if (identical(substr(as.character(x %||% "en"), 1L, 2L), "es")) "es" else "en"

# --- head: the script and the Open Graph card ---------------------------------

#' What goes in <head>: the cookie/Shiny bridge and the Open Graph tags that
#' make a LinkedIn link show a card instead of a bare URL.
gate_head <- function(app_id, title, description, image = NULL) {
  cfg <- gate_config()
  image <- image %||% paste0(cfg$site, "/og.png")
  js <- "
(function () {
  function cookies() {
    var out = {};
    document.cookie.split(';').forEach(function (c) {
      var i = c.indexOf('=');
      if (i > 0) out[c.slice(0, i).trim()] = decodeURIComponent(c.slice(i + 1));
    });
    return out;
  }
  function state() {
    var c = cookies();
    return {
      uses: +(c.apps_uses || 0), last: +(c.apps_last || 0),
      email: c.apps_email === '1', owner: c.apps_owner || '',
      lang: (navigator.language || 'en').slice(0, 2),
      query: location.search, referrer: document.referrer,
      ua: navigator.userAgent, nonce: Date.now()
    };
  }
  /* On every (re)connect, not only the first: a reconnect is a new server
     session with no memory, and the 30-minute rule keeps it from counting. */
  $(document).on('shiny:connected', function () {
    Shiny.setInputValue('gate_state', state(), { priority: 'event' });
  });
  Shiny.addCustomMessageHandler('gate_set', function (m) {
    var secure = location.protocol === 'https:' ? '; Secure' : '';
    Object.keys(m).forEach(function (k) {
      document.cookie = k + '=' + encodeURIComponent(m[k]) +
        '; path=/; max-age=31536000; SameSite=Lax' + secure;
    });
  });
  Shiny.addCustomMessageHandler('gate_lock', function (m) {
    document.querySelectorAll(m.selector).forEach(function (el) {
      el.disabled = !!m.lock;
      el.classList.toggle('gate-locked', !!m.lock);
    });
  });
  /* Share the current URL (the app keeps the analysis in it). Phones get
     the native sheet; desktops get the clipboard and a short confirmation. */
  window.gateShare = function () {
    var url = location.href, title = document.title;
    Shiny.setInputValue('gate_share_click', Date.now(), { priority: 'event' });
    if (navigator.share) { navigator.share({ title: title, url: url }).catch(function () {}); return; }
    var done = function () {
      var b = document.getElementById('gate_share'); if (!b) return;
      var old = b.innerHTML; b.innerHTML = '\u2713 Copiado \u00b7 Copied';
      setTimeout(function () { b.innerHTML = old; }, 1800);
    };
    if (navigator.clipboard) navigator.clipboard.writeText(url).then(done, done);
    else window.prompt('URL', url);
  };
  $(document).on('click', '.gate-contact', function () {
    Shiny.setInputValue('gate_contact_click', Date.now(), { priority: 'event' });
  });
})();
"
  shiny::tagList(
    shiny::tags$meta(property = "og:type", content = "website"),
    shiny::tags$meta(property = "og:title", content = title),
    shiny::tags$meta(property = "og:description", content = description),
    shiny::tags$meta(property = "og:image", content = image),
    shiny::tags$meta(property = "og:site_name", content = "Abel Hernández García"),
    shiny::tags$meta(name = "twitter:card", content = "summary_large_image"),
    shiny::tags$meta(name = "description", content = description),
    shiny::tags$meta(name = "gate-app", content = app_id),
    shiny::tags$style(shiny::HTML("
      .gate-locked { opacity: .55; cursor: not-allowed !important; }
      .gate-modal .modal-content { border-radius: 3px; }
      .gate-modal .gate-consent { font-size: .85em; opacity: .75; margin-top: .6rem; }
      .gate-modal .gate-alt { font-size: .9em; margin-top: 1rem; }
    ")),
    shiny::tags$script(shiny::HTML(js))
  )
}

# --- server ---------------------------------------------------------------------

gate_modal_email <- function(copy, cfg, error = NULL) {
  shiny::modalDialog(
    title = copy$email_title,
    easyClose = FALSE, footer = NULL, size = "m",
    shiny::div(
      class = "gate-modal-body",
      shiny::p(copy$email_body),
      shiny::textInput("gate_email", copy$email_label, "", width = "100%",
                       placeholder = "you@example.com"),
      if (!is.null(error)) shiny::div(class = "text-danger mb-2", error),
      shiny::actionButton("gate_email_submit", copy$email_button,
                          class = "btn btn-primary w-100"),
      shiny::div(class = "gate-consent", copy$email_consent),
      shiny::div(class = "gate-alt",
                 shiny::a(href = cfg$linkedin, target = "_blank", rel = "noopener",
                          class = "gate-contact", copy$contact_button))
    )
  )
}

gate_modal_contact <- function(copy, cfg, context = "") {
  shiny::modalDialog(
    title = copy$contact_title,
    easyClose = FALSE, footer = NULL, size = "m",
    shiny::div(
      class = "gate-modal-body",
      shiny::p(copy$contact_body),
      shiny::a(href = cfg$linkedin, target = "_blank", rel = "noopener",
               class = "btn btn-primary w-100 gate-contact", copy$contact_button),
      shiny::div(class = "gate-alt",
                 copy$contact_mail, " ",
                 shiny::a(href = gate_mailto(copy, context), class = "gate-contact", "hi@abelhga.com"))
    )
  )
}

#' A mailto with the subject pre-filled: LinkedIn cannot pre-fill a message,
#' email can, and the subject tells the owner what they were looking at.
gate_mailto <- function(copy, context = "") {
  subject <- copy$mail_subject
  if (nzchar(context %||% "")) subject <- paste0(subject, ": ", substr(context, 1L, 80L))
  paste0("mailto:hi@abelhga.com?subject=", utils::URLencode(subject, reserved = TRUE))
}

#' Wire the funnel into an app's server. Returns reactiveValues with
#' `allowed`, `stage`, `lang`, `owner`; the app's main handler checks
#' `isTRUE(gate$allowed)` before doing anything expensive.
#'
#' @param app_id short name stored with the lead ("forecasting", "network"...)
#' @param main_button CSS selector of the button(s) to disable while locked
gate_server <- function(input, output, session, app_id, main_button = "#generate") {
  cfg  <- gate_config()
  gate <- shiny::reactiveValues(allowed = !cfg$enabled,
                                stage = if (cfg$enabled) "pending" else "off",
                                lang = "en", owner = FALSE, uses = 0L, email = FALSE,
                                referrer = "", ua = "",
                                context = "")   # the app sets this: what is being analysed
  session$userData$owner <- FALSE
  if (!cfg$enabled) return(gate)

  render_modals <- isTRUE(getOption("gate.render_modals", TRUE))

  apply_stage <- function() {
    copy <- GATE_COPY[[gate$lang]]
    if (render_modals) {
      if (gate$stage == "email")        shiny::showModal(gate_modal_email(copy, cfg))
      else if (gate$stage == "contact") shiny::showModal(gate_modal_contact(copy, cfg, gate$context))
      else                              shiny::removeModal()
    }
    session$sendCustomMessage("gate_lock", list(selector = main_button, lock = !gate$allowed))
    if (gate$stage == "email")   gate_post_event(cfg, "email_asked", app_id, gate$stage, gate$lang)
    if (gate$stage == "contact") gate_post_event(cfg, "contact_shown", app_id, gate$stage, gate$lang)
  }

  shiny::observeEvent(input$gate_state, {
    s <- input$gate_state
    gate$lang     <- gate_lang(s$lang)
    gate$referrer <- as.character(s$referrer %||% "")
    gate$ua       <- as.character(s$ua %||% "")
    owner <- gate_is_owner(as.character(s$owner %||% ""),
                           gate_query_param(s$query, "owner"), cfg$owner_key)
    counted <- gate_count_visit(s$uses, s$last)
    gate$uses  <- counted$uses
    gate$email <- isTRUE(s$email)
    gate$owner <- owner
    session$userData$owner <- owner

    set <- list(apps_uses = counted$uses, apps_last = format(counted$last, scientific = FALSE))
    if (owner) set$apps_owner <- cfg$owner_key
    session$sendCustomMessage("gate_set", set)

    d <- gate_decide(gate$uses, gate$email, owner)
    gate$allowed <- d$allowed
    gate$stage   <- d$stage
    if (counted$counted) gate_post_event(cfg, "visit", app_id, d$stage, gate$lang)
    apply_stage()
  })

  shiny::observeEvent(input$gate_contact_click, {
    gate_post_event(cfg, "contact_click", app_id, gate$stage, gate$lang)
  })
  shiny::observeEvent(input$gate_share_click, {
    gate_post_event(cfg, "share", app_id, gate$stage, gate$lang)
  })

  shiny::observeEvent(input$gate_email_submit, {
    copy  <- GATE_COPY[[gate$lang]]
    email <- input$gate_email %||% ""
    if (!gate_valid_email(email)) {
      if (render_modals) shiny::showModal(gate_modal_email(copy, cfg, error = copy$email_invalid))
      return()
    }
    res <- gate_post_lead(cfg, gate_lead_body(email, app_id, gate$lang, gate$referrer, gate$ua,
                                              context = gate$context))
    if (!isTRUE(res$ok)) {
      if (render_modals) shiny::showModal(gate_modal_email(copy, cfg, error = copy$email_failed))
      return()
    }
    message("[gate] lead from ", app_id, " (", gate$lang, ")",
            if (isTRUE(res$stored)) " stored" else " not stored")
    gate$email <- TRUE
    gate_post_event(cfg, "email_left", app_id, gate$stage, gate$lang)
    session$sendCustomMessage("gate_set", list(apps_email = "1"))
    d <- gate_decide(gate$uses, TRUE, gate$owner)
    gate$allowed <- d$allowed
    gate$stage   <- d$stage
    apply_stage()
  })

  gate
}

#' The request ceiling for this session: the owner's, or the public one.
gate_request_ceiling <- function(session) {
  if (isTRUE(session$userData$owner)) OWNER_REQUEST_BUDGET() else MAX_REQUEST_BUDGET()
}

#' What the main handler says when it refuses to run.
gate_refuse <- function(gate) {
  shiny::showNotification(GATE_COPY[[gate$lang]]$blocked, type = "warning")
}

# ui_kit.R defines `%||%` for every app; this fallback is only for sourcing
# gate.R on its own (the tests do).
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}
