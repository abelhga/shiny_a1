# Checks for shared/keyword_network.R
# Run from the repository root:  Rscript tests/run_all.R

suppressPackageStartupMessages(library(dplyr))
source("shared/ui_kit.R")
source("shared/keyword_network.R")

# Accented words must survive whole (they used to be cut at the accent), and
# the trailing number is dropped by default. tolower() cannot fold non-ASCII
# in a C-locale session, so compare case-insensitively.
tk <- tokenise("Where can I buy a CAFÉ-machine, 2024?")
stopifnot(length(tk) == 4L,
          identical(tk[1:3], c("where", "can", "buy")),
          grepl("^caf.-machine$", tk[4]))
stopifnot(identical(tokenise("iphone 15 pro", keep_numbers = TRUE),
                    c("iphone","15","pro")))
cat("  ok - tokenise\n")

# Duplicate token inside one suggestion must not create a self-loop or a
# double-counted edge (the old code did both).
sug <- c("iphone case iphone charger", "iphone case leather",
         "iphone charger fast", "leather case brown", "brown leather bag")
built <- build_cooccurrence(sug)
stopifnot(!any(built$edges$from == built$edges$to))
w <- built$edges$weight[built$edges$from == "case" & built$edges$to == "iphone"]
stopifnot(length(w) == 1L, w == 2L)   # two suggestions, not three
cat("  ok - build_cooccurrence:", built$n_terms, "terms /", nrow(built$edges), "links\n")

stopifnot(is.null(build_cooccurrence(character(0))))
stopifnot(is.null(build_cooccurrence(c("solo"))))
stopifnot(is.null(build_cooccurrence(c("the a", "a the"), ignore_words = get_stopwords("en"))))
cat("  ok - degenerate inputs return NULL, not an error\n")

f <- network_frames(built, top_n = 3, size_by = "degree")
stopifnot(nrow(f$nodes) == 3, all(f$edges$from %in% f$nodes$id))
stopifnot(all(c("id","label","value","group","color","title") %in% names(f$nodes)))
cat("  ok - network_frames\n")

stopifnot(estimate_requests(1, "alphabetically") == 1)
stopifnot(estimate_requests(2, "alphabetically") == 27)
stopifnot(estimate_requests(3, "alphabetically") == 703)
cat("  ok - estimate_requests\n")

# Harvesting: budget respected, failures counted, results cached.
calls <- 0L
fake <- function(q) { calls <<- calls + 1L; if (grepl(" z$", q)) stop("boom") else paste(q, c("red","blue")) }
r <- expand_suggestions("shoe", fake, level = 2, method = "alphabetically",
                        max_requests = 10, pause = 0)
stopifnot(r$requests == 10, r$truncated, length(r$suggestions) == 20)
r2 <- expand_suggestions("shoe", fake, level = 2, method = "alphabetically",
                         max_requests = 10, pause = 0)
stopifnot(calls == 10L)  # second run served entirely from cache
cat("  ok - expand_suggestions respects budget, truncation and cache\n")

clear_suggest_cache()
r3 <- expand_suggestions("shoe", function(q) stop("nope"), level = 1, pause = 0)
stopifnot(r3$failures == 1L, length(r3$suggestions) == 0L)
cat("  ok - failing fetcher degrades gracefully\n")

clear_suggest_cache()
r4 <- expand_suggestions("shoe", function(q) paste(q, c("x","y")), level = 3,
                         method = "by_vector", max_requests = 20, pause = 0)
stopifnot(r4$requests <= 20)
cat("  ok - by_vector respects the budget\n")

stopifnot(suggest_solver(500) == "barnesHut", suggest_solver(200) == "forceAtlas2Based",
          suggest_solver(50) == "repulsion")
stopifnot(`%||%`(NULL, 5) == 5, `%||%`(NA, 5) == 5, `%||%`(3, 5) == 3)
cat("  ok - every graph-building check passed\n")
