# ---------------------------------------------------------------------------
# keyword_network.R
#
# Everything the three "keyword network" apps (Google Suggest, Amazon,
# Wikipedia) have in common: fetching autocomplete suggestions, turning them
# into a co-occurrence graph, scoring it, and drawing it.
#
# Source of truth: shared/keyword_network.R
# Copies live in <app>/R/keyword_network.R so that every app folder stays
# self-contained and deployable on its own. Run tools/sync_shared.sh after
# editing this file.
# ---------------------------------------------------------------------------

# --- Stop words ------------------------------------------------------------
# Bundled so the apps do not hard-depend on {tm}. When {tm} is installed its
# (larger) list is merged in on top of these.
BUILTIN_STOPWORDS <- list(
  en = c("a", "about", "after", "all", "also", "am", "an", "and", "any", "are",
         "as", "at", "be", "because", "been", "before", "being", "best",
         "between", "both", "but", "by", "can", "do", "does", "doing", "down",
         "during", "each", "few", "for", "from", "further", "had", "has",
         "have", "having", "he", "her", "here", "hers", "him", "his", "how",
         "i", "if", "in", "into", "is", "it", "its", "just", "me", "more",
         "most", "my", "near", "no", "nor", "not", "now", "of", "off", "on",
         "once", "only", "or", "other", "our", "out", "over", "own", "same",
         "she", "should", "so", "some", "such", "than", "that", "the", "their",
         "them", "then", "there", "these", "they", "this", "those", "through",
         "to", "too", "under", "until", "up", "very", "was", "we", "were",
         "what", "when", "where", "which", "while", "who", "whom", "why",
         "will", "with", "you", "your"),
  es = c("al", "algo", "ante", "antes", "aqui", "como", "con", "contra",
         "cual", "cuando", "de", "del", "desde", "donde", "dos", "el", "ella",
         "ellas", "ellos", "en", "entre", "era", "es", "esa", "ese", "eso",
         "esta", "este", "esto", "ha", "hasta", "hay", "la", "las", "le",
         "les", "lo", "los", "mas", "me", "mi", "mucho", "muy", "nada", "ni",
         "no", "nos", "otra", "otro", "para", "pero", "poco", "por", "porque",
         "que", "quien", "se", "ser", "si", "sin", "sobre", "son", "su",
         "sus", "tan", "te", "tiene", "todo", "tu", "un", "una", "uno",
         "unos", "ya"),
  fr = c("au", "aux", "avec", "ce", "ces", "dans", "de", "des", "du", "elle",
         "en", "est", "et", "eux", "il", "je", "la", "le", "les", "leur",
         "lui", "ma", "mais", "me", "meme", "mes", "moi", "mon", "ne", "nos",
         "notre", "nous", "on", "ou", "par", "pas", "plus", "pour", "que",
         "qui", "sa", "sans", "se", "ses", "son", "sur", "ta", "te", "tes",
         "toi", "ton", "tu", "un", "une", "vos", "votre", "vous"),
  de = c("aber", "alle", "als", "also", "am", "an", "auch", "auf", "aus",
         "bei", "bin", "bis", "bist", "da", "dass", "dein", "dem", "den",
         "der", "des", "die", "dies", "doch", "dort", "du", "ein", "eine",
         "einem", "einen", "einer", "eines", "er", "es", "euer", "hat",
         "hatte", "hier", "ich", "ihr", "im", "in", "ist", "ja", "kann",
         "kein", "mein", "mit", "nach", "nicht", "noch", "nun", "nur",
         "oder", "sein", "sie", "sind", "so", "um", "und", "uns", "unser",
         "von", "vor", "war", "was", "wenn", "wer", "wie", "wir", "wird",
         "zu", "zum", "zur"),
  it = c("ad", "agli", "ai", "al", "alla", "alle", "allo", "anche", "che",
         "chi", "col", "come", "con", "cui", "da", "dal", "dei", "del",
         "della", "delle", "dello", "di", "dove", "ed", "gli", "ha", "hanno",
         "il", "in", "io", "la", "le", "lo", "ma", "mi", "ne", "nel", "nella",
         "non", "per", "quando", "se", "si", "sono", "su", "sul", "sulla",
         "tra", "tu", "un", "una", "uno", "voi"),
  pt = c("ao", "aos", "as", "com", "como", "da", "das", "de", "do", "dos",
         "ela", "ele", "em", "entre", "essa", "esse", "esta", "este", "eu",
         "foi", "isso", "mais", "mas", "me", "mesmo", "meu", "muito", "na",
         "nao", "nas", "no", "nos", "os", "ou", "para", "pela", "pelo", "por",
         "qual", "quando", "que", "se", "sem", "ser", "seu", "sua", "tem",
         "um", "uma", "voce"),
  nl = c("aan", "al", "als", "bij", "dan", "dat", "de", "der", "deze", "die",
         "dit", "door", "een", "en", "er", "het", "hij", "hoe", "ik", "in",
         "is", "je", "met", "na", "naar", "niet", "nog", "of", "om", "onder",
         "ook", "op", "over", "te", "tot", "uit", "van", "voor", "waar",
         "was", "wat", "we", "wie", "zijn", "zo")
)

#' Stop words for a language code, merging the bundled list with {tm}'s.
get_stopwords <- function(lang) {
  lang <- tolower(substr(as.character(lang), 1L, 2L))
  if (!nzchar(lang)) return(character(0))
  builtin <- BUILTIN_STOPWORDS[[lang]]
  if (is.null(builtin)) builtin <- character(0)
  from_tm <- tryCatch(tm::stopwords(lang), error = function(e) character(0))
  sort(unique(c(builtin, from_tm)))
}

# --- Question / preposition modifiers --------------------------------------
# Used by the "by questions" expansion method: each modifier is combined with
# the seed ("how seed", "seed for", ...) the way keyword-research tools do, so
# the harvest surfaces intent (questions, comparisons, prepositions) rather
# than the alphabet.
QUESTION_MODIFIERS <- list(
  en = c("who", "what", "where", "when", "why", "how", "which", "can", "will",
         "is", "are", "vs", "versus", "or", "and", "for", "with", "without",
         "near", "like", "to", "best", "cheap", "free", "review"),
  es = c("quien", "que", "donde", "cuando", "por que", "como", "cual", "puede",
         "es", "son", "vs", "o", "y", "para", "con", "sin", "cerca", "como",
         "mejor", "barato", "gratis", "opiniones"),
  fr = c("qui", "quoi", "ou", "quand", "pourquoi", "comment", "quel", "peut",
         "est", "sont", "vs", "ou", "et", "pour", "avec", "sans", "proche",
         "comme", "meilleur", "pas cher", "gratuit", "avis"),
  de = c("wer", "was", "wo", "wann", "warum", "wie", "welche", "kann", "wird",
         "ist", "sind", "vs", "oder", "und", "fur", "mit", "ohne", "nahe",
         "wie", "beste", "gunstig", "kostenlos", "test"),
  it = c("chi", "cosa", "dove", "quando", "perche", "come", "quale", "puo",
         "e", "sono", "vs", "o", "e", "per", "con", "senza", "vicino",
         "come", "migliore", "economico", "gratis", "recensioni"),
  pt = c("quem", "o que", "onde", "quando", "por que", "como", "qual", "pode",
         "e", "sao", "vs", "ou", "e", "para", "com", "sem", "perto", "como",
         "melhor", "barato", "gratis", "avaliacao"),
  nl = c("wie", "wat", "waar", "wanneer", "waarom", "hoe", "welke", "kan",
         "wordt", "is", "zijn", "vs", "of", "en", "voor", "met", "zonder",
         "dichtbij", "zoals", "beste", "goedkoop", "gratis", "review")
)

#' Modifier list for a language code, falling back to English.
get_question_modifiers <- function(lang) {
  lang <- tolower(substr(as.character(lang), 1L, 2L))
  mods <- QUESTION_MODIFIERS[[lang]]
  if (is.null(mods)) mods <- QUESTION_MODIFIERS$en
  unique(mods)
}

#' Label valid UTF-8 text as UTF-8.
#'
#' Suggestions arrive as UTF-8 from the APIs, but a session running in the C
#' locale leaves the strings marked "unknown". PCRE then sees raw bytes and
#' `\\p{L}` cuts accented words in half. Marking them first keeps "cafe" with
#' an accent as a single token wherever the app is deployed.
mark_utf8 <- function(x) {
  encodings <- Encoding(x)
  needs <- !is.na(x) & encodings != "UTF-8" & validUTF8(x)
  if (any(needs)) {
    encodings[needs] <- "UTF-8"
    Encoding(x) <- encodings
  }
  x
}

#' Lower-case text without depending on the session locale.
#'
#' base::tolower() case-folds through the C library, so in a C-locale session
#' it leaves accented capitals alone and "CAFE" with an accent would end up as
#' a separate node from "cafe". {stringi} folds Unicode the same way
#' everywhere; it is used when installed and tolower() is the fallback.
lower_utf8 <- function(x) {
  x <- mark_utf8(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    return(stringi::stri_trans_tolower(x))
  }
  tolower(x)
}

# --- Tokenising ------------------------------------------------------------

#' Split a suggestion into comparable tokens.
#'
#' Punctuation is stripped but accented characters are kept, so "cafe" with an
#' accent stays one word instead of being cut in two.
tokenise <- function(x, min_chars = 2L, keep_numbers = FALSE) {
  if (!length(x)) return(character(0))
  x <- lower_utf8(as.character(x))
  x <- gsub("[^\\p{L}\\p{N}\\s'-]+", " ", x, perl = TRUE)
  tokens <- unlist(strsplit(x, "\\s+"), use.names = FALSE)
  tokens <- gsub("^['-]+|['-]+$", "", tokens)
  tokens <- tokens[nzchar(tokens)]
  tokens <- tokens[nchar(tokens) >= min_chars]
  if (!keep_numbers) tokens <- tokens[!grepl("^\\p{N}+$", tokens, perl = TRUE)]
  tokens
}

# --- Graph building --------------------------------------------------------

#' Build a weighted co-occurrence graph from a vector of suggestions.
#'
#' Tokens are de-duplicated inside each suggestion, so a phrase like
#' "iphone case iphone charger" contributes one iphone-case edge rather than
#' two, and never a self-loop.
#'
#' @return NULL when there is not enough signal to draw anything, otherwise a
#'   list(graph, edges, terms, n_queries, n_terms).
build_cooccurrence <- function(queries,
                               ignore_words = character(0),
                               min_chars = 2L,
                               keep_numbers = FALSE,
                               min_edge_weight = 1L) {

  queries <- unique(queries[!is.na(queries) & nzchar(queries)])
  if (length(queries) < 2L) return(NULL)

  ignore_words <- unique(tolower(ignore_words))

  token_sets <- lapply(queries, function(q) {
    tk <- unique(tokenise(q, min_chars = min_chars, keep_numbers = keep_numbers))
    setdiff(tk, ignore_words)
  })

  frequency <- table(unlist(token_sets, use.names = FALSE))

  token_sets <- token_sets[vapply(token_sets, length, integer(1)) >= 2L]
  if (!length(token_sets)) return(NULL)

  # Pair up each suggestion's tokens. Building the edge list directly is what
  # keeps this usable at scale: the previous full |V| x |V| dense matrix
  # allocated gigabytes once a few thousand distinct words showed up.
  pairs <- lapply(token_sets, function(tk) {
    combos <- utils::combn(sort(tk), 2L)
    data.frame(from = combos[1L, ], to = combos[2L, ], stringsAsFactors = FALSE)
  })

  edges <- dplyr::count(dplyr::bind_rows(pairs), from, to, name = "weight")
  edges <- as.data.frame(edges)
  edges <- edges[edges$weight >= max(1L, as.integer(min_edge_weight)), , drop = FALSE]
  if (!nrow(edges)) return(NULL)

  graph <- igraph::graph_from_data_frame(edges, directed = FALSE)

  igraph::V(graph)$frequency <- as.integer(frequency[igraph::V(graph)$name])
  igraph::V(graph)$degree    <- igraph::degree(graph)
  igraph::V(graph)$strength  <- igraph::strength(graph, weights = igraph::E(graph)$weight)
  igraph::V(graph)$betweenness <- igraph::betweenness(
    graph, weights = 1 / igraph::E(graph)$weight, normalized = TRUE
  )

  communities <- tryCatch(
    igraph::cluster_louvain(graph, weights = igraph::E(graph)$weight),
    error = function(e) tryCatch(igraph::cluster_fast_greedy(graph),
                                 error = function(e2) NULL)
  )
  igraph::V(graph)$community <- if (is.null(communities)) {
    rep(1L, igraph::gorder(graph))
  } else {
    as.integer(igraph::membership(communities))
  }

  terms <- data.frame(
    term        = igraph::V(graph)$name,
    frequency   = igraph::V(graph)$frequency,
    degree      = igraph::V(graph)$degree,
    strength    = igraph::V(graph)$strength,
    betweenness = round(igraph::V(graph)$betweenness, 4),
    community   = igraph::V(graph)$community,
    stringsAsFactors = FALSE
  )
  terms <- terms[order(-terms$strength, -terms$degree), , drop = FALSE]

  list(
    graph     = graph,
    edges     = edges,
    terms     = terms,
    n_queries = length(queries),
    n_terms   = nrow(terms)
  )
}

# --- Adaptive defaults -----------------------------------------------------

#' Pick a physics solver that will actually settle for a graph this size.
suggest_solver <- function(n_nodes) {
  if (n_nodes > 400L) "barnesHut" else if (n_nodes > 120L) "forceAtlas2Based" else "repulsion"
}

#' Suggest how many nodes to show before the picture turns into a hairball.
suggest_top_n <- function(n_nodes) {
  if (n_nodes <= 150L) n_nodes else if (n_nodes <= 600L) 150L else 250L
}

#' Suggest an edge-weight floor that keeps roughly `target` edges.
suggest_min_edge_weight <- function(edges, target = 1500L) {
  if (is.null(edges) || !nrow(edges) || nrow(edges) <= target) return(1L)
  cutoff <- stats::quantile(edges$weight, probs = 1 - target / nrow(edges), names = FALSE)
  max(1L, as.integer(ceiling(cutoff)))
}

#' Label size that stays readable without swamping a dense graph.
suggest_label_size <- function(n_nodes) {
  if (n_nodes <= 60L) 26L else if (n_nodes <= 200L) 20L else 15L
}

# --- Drawing ---------------------------------------------------------------

# Colour-blind-safe qualitative palette, recycled across communities.
NETWORK_PALETTE <- c(
  "#4C78A8", "#F58518", "#54A24B", "#E45756", "#72B7B2", "#EECA3B",
  "#B279A2", "#FF9DA6", "#9D755D", "#66C2A5", "#8C6BB1", "#BAB0AC"
)

#' Reduce a scored graph to the visNetwork node/edge frames to draw.
#'
#' @param size_by one of "strength", "degree", "frequency", "betweenness".
network_frames <- function(scored, top_n = 150L, min_edge_weight = 1L,
                           size_by = "strength", communities = NULL) {

  if (is.null(scored)) return(NULL)
  terms <- scored$terms
  edges <- scored$edges

  if (!is.null(communities) && length(communities)) {
    terms <- terms[terms$community %in% as.integer(communities), , drop = FALSE]
  }
  if (!nrow(terms)) return(NULL)

  if (!size_by %in% names(terms)) size_by <- "strength"
  terms <- terms[order(-terms[[size_by]]), , drop = FALSE]
  keep <- utils::head(terms$term, max(2L, as.integer(top_n)))
  terms <- terms[terms$term %in% keep, , drop = FALSE]

  edges <- edges[edges$weight >= max(1L, as.integer(min_edge_weight)) &
                   edges$from %in% keep & edges$to %in% keep, , drop = FALSE]

  colours <- NETWORK_PALETTE[((terms$community - 1L) %% length(NETWORK_PALETTE)) + 1L]

  # Normalise the sizing metric to 1..100 for vis.js. The old pmax(x, 1)
  # flattened betweenness (a 0..1 number) to a constant, so every node drew
  # at the same size whenever that metric was selected.
  raw <- terms[[size_by]]
  rng <- suppressWarnings(range(raw[is.finite(raw)]))
  value <- if (all(is.finite(rng)) && diff(rng) > 0) {
    1 + 99 * (raw - rng[1L]) / diff(rng)
  } else {
    rep(50, nrow(terms))
  }

  nodes <- data.frame(
    id    = terms$term,
    label = terms$term,
    value = value,
    group = paste("Cluster", terms$community),
    color = colours,
    title = paste0(
      "<b>", terms$term, "</b><br>",
      "Appears in ", terms$frequency, " suggestions<br>",
      "Connections: ", terms$degree, "<br>",
      "Weighted strength: ", terms$strength, "<br>",
      "Bridging (betweenness): ", terms$betweenness, "<br>",
      "Cluster ", terms$community
    ),
    stringsAsFactors = FALSE
  )

  vis_edges <- if (nrow(edges)) {
    data.frame(
      from  = edges$from,
      to    = edges$to,
      value = edges$weight,
      title = paste0(edges$from, " + ", edges$to, ": ", edges$weight, " suggestions"),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(from = character(0), to = character(0),
               value = numeric(0), title = character(0), stringsAsFactors = FALSE)
  }

  list(nodes = nodes, edges = vis_edges)
}

#' Draw the network.
#'
#' Note the stabilisation settings: the earlier version asked visNetwork to
#' skip stabilisation and then waited for `stabilizationIterationsDone` to
#' switch physics off. That event never fires when stabilisation is disabled,
#' so the graph drifted forever. Stabilisation is now on with a bounded
#' iteration count, and the callback does what it was always meant to do.
render_keyword_network <- function(frames,
                                   solver = "forceAtlas2Based",
                                   label_size = 20L,
                                   show_labels = TRUE,
                                   dark = FALSE,
                                   seed = 11L) {

  if (is.null(frames) || !nrow(frames$nodes)) return(NULL)

  n_nodes <- nrow(frames$nodes)
  font_colour   <- if (dark) "#e6edf3" else "#1b1f24"
  stroke_colour <- if (dark) "#0d1117" else "#ffffff"
  edge_colour   <- if (dark) "#3d444d" else "#c9d1d9"

  # Tuning for the chosen solver only. visPhysics() means to reject more than
  # one solver block at a time (its own guard misfires on lists, so passing
  # all three happens to slip through today), and vis.js ignores the blocks
  # that do not match `solver` anyway.
  solver_options <- switch(
    solver,
    barnesHut = list(barnesHut = list(gravitationalConstant = -8000,
                                      springLength = 130,
                                      springConstant = 0.04,
                                      avoidOverlap = 0.2)),
    repulsion = list(repulsion = list(nodeDistance = 160, springLength = 200)),
    list(forceAtlas2Based = list(gravitationalConstant = -60,
                                 springLength = 110,
                                 springConstant = 0.05,
                                 avoidOverlap = 0.4))
  )

  widget <- visNetwork::visNetwork(frames$nodes, frames$edges) %>%
    visNetwork::visNodes(
      shape = "dot",
      borderWidth = 1.5,
      scaling = list(min = 10, max = 55,
                     label = list(enabled = show_labels,
                                  min = label_size, max = label_size + 16L)),
      font = list(size = label_size, color = font_colour, face = "Helvetica",
                  strokeWidth = 4, strokeColor = stroke_colour),
      shadow = list(enabled = TRUE, size = 6, x = 1, y = 1)
    ) %>%
    visNetwork::visEdges(
      # Undirected co-occurrence: "A appears next to B" is the same fact as
      # "B appears next to A", so there are no arrowheads.
      smooth = list(enabled = TRUE, type = "continuous"),
      color = list(color = edge_colour, highlight = "#F58518", opacity = 0.6),
      scaling = list(min = 1, max = 9)
    ) %>%
    visNetwork::visEvents(
      stabilizationIterationsDone = "function () { this.setOptions({ physics: false }); }"
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE, dragView = TRUE, zoomView = TRUE,
      hover = TRUE, tooltipDelay = 120, keyboard = TRUE,
      navigationButtons = TRUE, multiselect = TRUE,
      hideEdgesOnDrag = n_nodes > 250L
    ) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = list(enabled = TRUE, useLabels = TRUE, main = "Find a term"),
      selectedBy = list(variable = "group", multiple = TRUE, main = "Filter by cluster")
    ) %>%
    visNetwork::visLayout(randomSeed = seed, improvedLayout = n_nodes <= 300L)

  do.call(
    visNetwork::visPhysics,
    c(list(graph = widget,
           solver = solver,
           stabilization = list(enabled = TRUE, iterations = 250)),
      solver_options)
  )
}

# --- Suggestion harvesting -------------------------------------------------

.suggest_cache <- new.env(parent = emptyenv())

#' Cache one autocomplete lookup for the life of the R session.
cached_fetch <- function(key, fetcher) {
  hit <- .suggest_cache[[key]]
  if (!is.null(hit)) return(hit)
  value <- fetcher()
  .suggest_cache[[key]] <- value
  value
}

clear_suggest_cache <- function() {
  rm(list = ls(.suggest_cache), envir = .suggest_cache)
}

#' How many HTTP calls a given setting will make, so the UI can warn first.
#'
#' Works for any depth: the alphabetical crawl appends one more letter per
#' level (1 + 26 + 26^2 + ...), "by questions" pairs the seed with each
#' modifier on both sides, and "by vector" widens by `branch` per level.
estimate_requests <- function(level, method, alphabet_size = 26L, branch = 12L,
                              modifiers = 25L) {
  level <- max(1L, as.integer(level))
  if (identical(method, "alphabetically")) {
    total <- 1
    if (level > 1L) {
      for (d in seq_len(level - 1L)) total <- total + alphabet_size^d
    }
    return(as.integer(min(total, .Machine$integer.max)))
  }
  if (identical(method, "by_questions")) {
    return(as.integer(1L + 2L * as.integer(modifiers)))
  }
  total <- 1L
  frontier <- 1L
  if (level > 1L) {
    for (i in seq_len(level - 1L)) {
      frontier <- frontier * branch
      total <- total + frontier
    }
  }
  as.integer(total)
}

#' Harvest autocomplete suggestions around a seed keyword.
#'
#' @param fetcher function(query) returning a character vector of suggestions.
#' @param method "alphabetically" (seed + a..z, one more letter per level),
#'   "by_questions" (seed paired with question/preposition modifiers) or
#'   "by_vector" (feed each suggestion back in as a new seed).
#' @param modifiers character vector for "by_questions"; defaults to English.
#' @param progress function(done, total, label) called between requests.
#' @return list(suggestions, requests, failures, truncated)
expand_suggestions <- function(seed, fetcher, level = 2L, method = "alphabetically",
                               max_requests = 250L, pause = 0.1,
                               alphabet = letters, modifiers = NULL,
                               progress = NULL) {

  level <- max(1L, as.integer(level))
  seed <- trimws(seed)
  if (!nzchar(seed)) {
    return(list(suggestions = character(0), requests = 0L,
                failures = 0L, truncated = FALSE))
  }

  failures <- 0L
  safe_fetch <- function(query) {
    out <- cached_fetch(query, function() {
      res <- tryCatch(fetcher(query), error = function(e) NULL)
      if (is.null(res)) NA_character_ else as.character(res)
    })
    if (length(out) == 1L && is.na(out[1L])) {
      failures <<- failures + 1L
      return(character(0))
    }
    out[!is.na(out) & nzchar(out)]
  }

  truncated <- FALSE
  collected <- character(0)
  requests <- 0L

  if (method %in% c("alphabetically", "by_questions")) {
    if (identical(method, "alphabetically")) {
      queries <- seed
      if (level > 1L) {
        suffixes <- alphabet
        for (d in seq.int(2L, level)) {
          queries <- c(queries, paste0(seed, " ", suffixes))
          if (d < level) {
            suffixes <- as.vector(outer(suffixes, alphabet, paste0))
          }
        }
      }
    } else {
      mods <- modifiers
      if (is.null(mods) || !length(mods)) mods <- QUESTION_MODIFIERS$en
      mods <- unique(trimws(as.character(mods)))
      mods <- mods[nzchar(mods)]
      queries <- unique(c(seed, paste(mods, seed), paste(seed, mods)))
    }
    if (length(queries) > max_requests) {
      queries <- queries[seq_len(max_requests)]
      truncated <- TRUE
    }
    total <- length(queries)
    for (i in seq_along(queries)) {
      collected <- c(collected, safe_fetch(queries[i]))
      requests <- requests + 1L
      if (is.function(progress)) progress(i, total, queries[i])
      if (pause > 0 && i < total) Sys.sleep(pause)
    }
  } else {
    frontier <- seed
    total <- min(max_requests, estimate_requests(level, "by_vector"))
    for (depth in seq_len(level)) {
      next_frontier <- character(0)
      for (query in frontier) {
        if (requests >= max_requests) {
          truncated <- TRUE
          break
        }
        found <- safe_fetch(query)
        requests <- requests + 1L
        collected <- c(collected, found)
        next_frontier <- c(next_frontier, found)
        if (is.function(progress)) progress(requests, max(total, requests), query)
        if (pause > 0) Sys.sleep(pause)
      }
      if (truncated) break
      # Widening on every suggestion explodes fast; keep the most promising
      # slice of the frontier so deeper levels stay affordable.
      frontier <- utils::head(unique(next_frontier), 12L)
      if (!length(frontier)) break
    }
  }

  list(suggestions = unique(collected), requests = requests,
       failures = failures, truncated = truncated)
}
