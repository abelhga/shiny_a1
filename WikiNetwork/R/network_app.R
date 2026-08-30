# ---------------------------------------------------------------------------
# network_app.R
#
# The UI and server for a keyword-network app. The three apps in this repo
# (Google Suggest, Amazon, Wikipedia) differ only in where the autocomplete
# suggestions come from, so they all call `keyword_network_ui()` /
# `keyword_network_server()` with a small config list instead of each keeping
# its own near-identical copy.
#
# Source of truth: shared/network_app.R
# Copies live in <app>/R/network_app.R. Run tools/sync_shared.sh after editing.
#
# config fields
#   title         page title
#   subtitle      one-line description under the title
#   source_label  human name of the suggestion source ("Google Suggest", ...)
#   seed_default  starting keyword
#   scope_choices named vector for the source selector (market, language, ...)
#   scope_label   label for that selector
#   scope_default selected value
#   fetcher       function(query, scope) -> character vector of suggestions
#   scope_lang    function(scope) -> two-letter code used for stop words
#   footer_note   caveat shown under the graph
# ---------------------------------------------------------------------------

SIZE_CHOICES <- c(
  "Weighted strength" = "strength",
  "Number of connections" = "degree",
  "Raw frequency" = "frequency",
  "Bridging (betweenness)" = "betweenness"
)

SOLVER_CHOICES <- c(
  "Automatic (based on size)" = "auto",
  "Barnes-Hut (large graphs)" = "barnesHut",
  "ForceAtlas2 (organic)" = "forceAtlas2Based",
  "Repulsion (small graphs)" = "repulsion"
)

keyword_network_ui <- function(config) {
  bslib::page_sidebar(
    title = config$title,
    theme = app_theme(FALSE),
    fillable = TRUE,

    sidebar = bslib::sidebar(
      width = 340,
      open = "desktop",

      shiny::textInput("keyword", "Seed keyword", value = config$seed_default,
                       placeholder = "e.g. running shoes"),

      shiny::selectInput("scope", config$scope_label,
                         choices = config$scope_choices,
                         selected = config$scope_default),

      shiny::selectInput("method", "Expansion method",
                         choices = c("By vector (recursive)" = "by_vector",
                                     "Alphabetically (a-z)" = "alphabetically",
                                     "By questions & prepositions" = "by_questions")),

      shiny::conditionalPanel(
        "input.method != 'by_questions'",
        shiny::sliderInput("level", "Depth", min = 1, max = 4, value = 2, step = 1)
      ),

      shiny::numericInput("max_requests", "Request budget",
                          value = 60, min = 1, max = 5000, step = 10),
      shiny::uiOutput("request_estimate"),

      shiny::actionButton("generate", "Generate network",
                          class = "btn btn-primary w-100",
                          icon = shiny::icon("diagram-project")),
      shiny::div(
        class = "text-end mt-1",
        shiny::actionLink("clear_cache", "Clear cached lookups",
                          class = "small text-muted")
      ),

      shiny::hr(),

      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "Text cleaning",
          shiny::checkboxInput("remove_stopwords", "Remove stop words", TRUE),
          shiny::checkboxInput("remove_seed", "Remove the seed keyword's own words", TRUE),
          shiny::checkboxInput("keep_numbers", "Keep numbers", FALSE),
          shiny::sliderInput("min_chars", "Minimum word length",
                             min = 1, max = 6, value = 2, step = 1),
          shiny::textInput("extra_stopwords", "Extra words to drop (comma separated)", "")
        ),
        bslib::accordion_panel(
          "Graph shape",
          shiny::sliderInput("top_n", "Terms to show", min = 5, max = 300,
                             value = 120, step = 5),
          shiny::sliderInput("min_edge_weight", "Minimum co-occurrences per link",
                             min = 1, max = 10, value = 1, step = 1),
          shiny::selectInput("size_by", "Size nodes by", choices = SIZE_CHOICES,
                             selected = "strength"),
          shiny::selectInput("solver", "Layout solver", choices = SOLVER_CHOICES,
                             selected = "auto")
        ),
        bslib::accordion_panel(
          "Display",
          shiny::checkboxInput("show_labels", "Show labels", TRUE),
          shiny::checkboxInput("physics", "Physics running", FALSE),
          shiny::checkboxInput("dark_mode", "Dark mode", FALSE),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton("fit_view", "Fit view", class = "btn btn-sm btn-outline-secondary"),
            shiny::actionButton("restabilise", "Re-run layout", class = "btn btn-sm btn-outline-secondary")
          )
        )
      )
    ),

    shiny::tags$head(shiny::tags$style(app_styles())),

    shiny::div(class = "app-subtitle mb-2", config$subtitle),

    shiny::uiOutput("kpis"),

    bslib::navset_card_tab(
      id = "tabs",
      height = "760px",

      bslib::nav_panel(
        "Network",
        icon = shiny::icon("circle-nodes"),
        shinycssloaders::withSpinner(
          visNetwork::visNetworkOutput("network", height = "620px"),
          type = 8, color = "#4C78A8"
        ),
        shiny::uiOutput("selected_term"),
        shiny::div(class = "status-note mt-2", config$footer_note)
      ),

      bslib::nav_panel(
        "Terms",
        icon = shiny::icon("table"),
        shiny::div(
          class = "d-flex gap-2 mb-2 flex-wrap",
          shiny::downloadButton("dl_terms", "Terms CSV", class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton("dl_edges", "Links CSV", class = "btn-sm btn-outline-secondary"),
          shiny::downloadButton("dl_graphml", "Graph (GraphML, for Gephi)",
                                class = "btn-sm btn-outline-secondary")
        ),
        DT::DTOutput("terms_table")
      ),

      bslib::nav_panel(
        "Charts",
        icon = shiny::icon("chart-simple"),
        bslib::layout_columns(
          col_widths = c(7, 5),
          bslib::card(bslib::card_header("Most connected terms"),
                      plotly::plotlyOutput("top_terms_plot", height = "540px")),
          bslib::card(bslib::card_header("Cluster sizes"),
                      plotly::plotlyOutput("cluster_plot", height = "540px"))
        )
      ),

      bslib::nav_panel(
        "Suggestions",
        icon = shiny::icon("list"),
        shiny::div(
          class = "d-flex gap-2 mb-2",
          shiny::downloadButton("dl_suggestions", "Suggestions CSV",
                                class = "btn-sm btn-outline-secondary")
        ),
        DT::DTOutput("suggestions_table")
      ),

      bslib::nav_panel(
        "AI insights",
        icon = shiny::icon("robot"),
        aiInsightsUI("ai", label = "Interpret this network")
      )
    )
  )
}

keyword_network_server <- function(config) {
  function(input, output, session) {

    harvest <- shiny::reactiveVal(NULL)  # expand_suggestions() result
    scored  <- shiny::reactiveVal(NULL)  # build_cooccurrence() result

    # --- theme --------------------------------------------------------------
    shiny::observeEvent(input$dark_mode, {
      session$setCurrentTheme(app_theme(isTRUE(input$dark_mode)))
    }, ignoreInit = TRUE)

    # --- budget preview -----------------------------------------------------
    scope_modifiers <- shiny::reactive({
      get_question_modifiers(config$scope_lang(input$scope))
    })

    output$request_estimate <- shiny::renderUI({
      wanted <- estimate_requests(input$level, input$method,
                                  modifiers = length(scope_modifiers()))
      budget <- as.integer(input$max_requests %||% 60L)
      actual <- min(wanted, budget)
      shiny::div(
        class = "status-note mb-2",
        sprintf("These settings want %s request%s; the budget caps it at %s.",
                format(wanted, big.mark = ","), if (wanted == 1L) "" else "s",
                format(actual, big.mark = ",")),
        if (wanted > budget) {
          shiny::span(shiny::tags$br(),
                      "Raise the budget for a fuller picture, or lower the depth.")
        }
      )
    })

    shiny::observeEvent(input$clear_cache, {
      clear_suggest_cache()
      shiny::showNotification("Cached lookups cleared - the next crawl re-fetches everything.",
                              type = "message")
    })

    # --- harvesting ---------------------------------------------------------
    shiny::observeEvent(input$generate, {
      keyword <- trimws(input$keyword %||% "")

      if (!nzchar(keyword)) {
        shiny::showNotification("Type a seed keyword first.", type = "warning")
        return()
      }

      budget <- max(1L, as.integer(input$max_requests %||% 60L))

      result <- shiny::withProgress(
        message = paste("Querying", config$source_label), value = 0,
        {
          expand_suggestions(
            seed = keyword,
            fetcher = function(query) config$fetcher(query, input$scope),
            level = input$level,
            method = input$method,
            modifiers = scope_modifiers(),
            max_requests = budget,
            progress = function(done, total, label) {
              shiny::setProgress(value = done / max(total, 1),
                                 detail = sprintf("%d/%d - %s", done, total, label))
            }
          )
        }
      )

      harvest(result)

      if (!length(result$suggestions)) {
        scored(NULL)
        shiny::showNotification(
          paste0("No suggestions came back from ", config$source_label,
                 if (result$failures > 0) " (all requests failed - check your connection)" else "",
                 "."),
          type = "error", duration = 8
        )
        return()
      }

      if (result$failures > 0) {
        shiny::showNotification(
          sprintf("%d of %d requests failed and were skipped.",
                  result$failures, result$requests),
          type = "warning", duration = 6
        )
      }
      if (isTRUE(result$truncated)) {
        shiny::showNotification("Stopped at the request budget.", type = "message")
      }

      ignore <- character(0)
      if (isTRUE(input$remove_seed)) {
        ignore <- c(ignore, tokenise(keyword, min_chars = 1L, keep_numbers = TRUE))
      }
      if (isTRUE(input$remove_stopwords)) {
        ignore <- c(ignore, get_stopwords(config$scope_lang(input$scope)))
      }
      extra <- trimws(unlist(strsplit(input$extra_stopwords %||% "", "[,;]")))
      ignore <- c(ignore, tolower(extra[nzchar(extra)]))

      built <- build_cooccurrence(
        result$suggestions,
        ignore_words = ignore,
        min_chars = input$min_chars,
        keep_numbers = isTRUE(input$keep_numbers)
      )

      if (is.null(built)) {
        scored(NULL)
        shiny::showNotification(
          paste("The suggestions did not contain enough distinct words to build a",
                "network. Try a broader keyword, a deeper level, or turn off",
                "some of the text cleaning."),
          type = "warning", duration = 9
        )
        return()
      }

      scored(built)

      # Adaptive defaults: re-aim the shape controls at this particular graph.
      shiny::updateSliderInput(session, "top_n",
                               max = max(10L, built$n_terms),
                               value = suggest_top_n(built$n_terms))
      shiny::updateSliderInput(session, "min_edge_weight",
                               max = max(3L, max(built$edges$weight)),
                               value = suggest_min_edge_weight(built$edges))
    })

    frames <- shiny::reactive({
      shiny::req(scored())
      network_frames(
        scored(),
        top_n = input$top_n,
        min_edge_weight = input$min_edge_weight,
        size_by = input$size_by
      )
    })

    # --- network ------------------------------------------------------------
    output$network <- visNetwork::renderVisNetwork({
      current <- frames()
      shiny::req(current)

      solver <- if (identical(input$solver, "auto")) {
        suggest_solver(nrow(current$nodes))
      } else {
        input$solver
      }

      render_keyword_network(
        current,
        solver = solver,
        label_size = suggest_label_size(nrow(current$nodes)),
        show_labels = isTRUE(input$show_labels),
        dark = isTRUE(input$dark_mode)
      )
    })

    # Live controls: these talk to the running widget instead of rebuilding it.
    shiny::observeEvent(input$physics, {
      visNetwork::visPhysics(visNetwork::visNetworkProxy("network"),
                             enabled = isTRUE(input$physics))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$fit_view, {
      visNetwork::visFit(visNetwork::visNetworkProxy("network"))
    })

    shiny::observeEvent(input$restabilise, {
      proxy <- visNetwork::visNetworkProxy("network")
      visNetwork::visPhysics(proxy, enabled = TRUE)
      visNetwork::visStabilize(proxy)
    })

    output$selected_term <- shiny::renderUI({
      selected <- input$network_selected
      built <- scored()
      if (is.null(built) || is.null(selected) || !nzchar(selected)) {
        return(shiny::div(class = "status-note mt-2",
                          "Click a node (or use the search box above the graph) to see what it connects to."))
      }

      edges <- built$edges
      linked <- rbind(
        data.frame(term = edges$to[edges$from == selected],
                   weight = edges$weight[edges$from == selected],
                   stringsAsFactors = FALSE),
        data.frame(term = edges$from[edges$to == selected],
                   weight = edges$weight[edges$to == selected],
                   stringsAsFactors = FALSE)
      )
      linked <- linked[order(-linked$weight), , drop = FALSE]
      row <- built$terms[built$terms$term == selected, , drop = FALSE]

      shiny::div(
        class = "mt-3",
        shiny::h5(shiny::tags$code(selected)),
        shiny::div(
          class = "status-note",
          sprintf("Cluster %s - appears in %s suggestions - %s connections - top links: %s",
                  if (nrow(row)) row$community[1] else "?",
                  if (nrow(row)) row$frequency[1] else "?",
                  if (nrow(row)) row$degree[1] else "?",
                  paste(utils::head(paste0(linked$term, " (", linked$weight, ")"), 8),
                        collapse = ", "))
        )
      )
    })

    # --- tables -------------------------------------------------------------
    output$terms_table <- DT::renderDT({
      built <- scored()
      shiny::req(built)
      DT::datatable(
        built$terms,
        rownames = FALSE,
        filter = "top",
        colnames = c("Term", "Suggestions", "Connections", "Strength",
                     "Bridging", "Cluster"),
        options = list(pageLength = 15, order = list(list(3, "desc")),
                       scrollX = TRUE)
      )
    })

    output$suggestions_table <- DT::renderDT({
      result <- harvest()
      shiny::req(result)
      DT::datatable(
        data.frame(suggestion = result$suggestions, stringsAsFactors = FALSE),
        rownames = FALSE, filter = "top", colnames = "Suggestion",
        options = list(pageLength = 20, scrollX = TRUE)
      )
    })

    # --- charts -------------------------------------------------------------
    output$top_terms_plot <- plotly::renderPlotly({
      built <- scored()
      shiny::req(built)
      metric <- input$size_by
      if (!metric %in% names(built$terms)) metric <- "strength"

      top <- built$terms[order(-built$terms[[metric]]), , drop = FALSE]
      top <- utils::head(top, 25)
      top <- top[order(top[[metric]]), , drop = FALSE]
      colours <- NETWORK_PALETTE[((top$community - 1L) %% length(NETWORK_PALETTE)) + 1L]

      figure <- plotly::layout(
        plotly::plot_ly(
          x = top[[metric]],
          y = factor(top$term, levels = top$term),
          type = "bar", orientation = "h",
          marker = list(color = colours),
          hovertemplate = paste0("<b>%{y}</b><br>",
                                 names(SIZE_CHOICES)[match(metric, SIZE_CHOICES)],
                                 ": %{x}<extra></extra>")
        ),
        xaxis = list(title = names(SIZE_CHOICES)[match(metric, SIZE_CHOICES)]),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 40)
      )
      plotly_theme(figure, isTRUE(input$dark_mode))
    })

    output$cluster_plot <- plotly::renderPlotly({
      built <- scored()
      shiny::req(built)
      sizes <- as.data.frame(table(built$terms$community), stringsAsFactors = FALSE)
      names(sizes) <- c("community", "terms")
      sizes$community <- as.integer(sizes$community)
      sizes <- sizes[order(-sizes$terms), , drop = FALSE]
      sizes <- utils::head(sizes, 15)
      sizes <- sizes[order(sizes$terms), , drop = FALSE]

      labels <- vapply(sizes$community, function(cl) {
        members <- built$terms$term[built$terms$community == cl]
        paste(utils::head(members, 3), collapse = ", ")
      }, character(1))

      colours <- NETWORK_PALETTE[((sizes$community - 1L) %% length(NETWORK_PALETTE)) + 1L]

      figure <- plotly::layout(
        plotly::plot_ly(
          x = sizes$terms,
          y = factor(paste("Cluster", sizes$community),
                     levels = paste("Cluster", sizes$community)),
          type = "bar", orientation = "h",
          marker = list(color = colours),
          text = labels, hovertemplate = "<b>%{y}</b><br>%{x} terms<br>%{text}<extra></extra>"
        ),
        xaxis = list(title = "Terms in cluster"),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, t = 10, b = 40)
      )
      plotly_theme(figure, isTRUE(input$dark_mode))
    })

    # --- KPIs ---------------------------------------------------------------
    output$kpis <- shiny::renderUI({
      built <- scored()
      result <- harvest()
      if (is.null(built) || is.null(result)) {
        return(shiny::div(
          class = "ai-empty mb-3",
          sprintf("Pick a keyword and press Generate network to pull live suggestions from %s.",
                  config$source_label)
        ))
      }
      shiny::div(
        class = "metric-grid mb-3",
        metric_card("Suggestions", format(length(result$suggestions), big.mark = ",")),
        metric_card("Unique terms", format(built$n_terms, big.mark = ",")),
        metric_card("Links", format(nrow(built$edges), big.mark = ",")),
        metric_card("Clusters", length(unique(built$terms$community))),
        metric_card("Requests", format(result$requests, big.mark = ","),
                    if (result$failures > 0) paste(result$failures, "failed") else "all succeeded")
      )
    })

    # --- downloads ----------------------------------------------------------
    stamp <- function(what) {
      paste0(gsub("[^A-Za-z0-9]+", "-", trimws(input$keyword %||% "network")),
             "-", what, "-", format(Sys.Date()), ".csv")
    }

    output$dl_terms <- shiny::downloadHandler(
      filename = function() stamp("terms"),
      content = function(file) {
        shiny::req(scored())
        utils::write.csv(scored()$terms, file, row.names = FALSE)
      }
    )
    output$dl_edges <- shiny::downloadHandler(
      filename = function() stamp("links"),
      content = function(file) {
        shiny::req(scored())
        utils::write.csv(scored()$edges, file, row.names = FALSE)
      }
    )
    output$dl_graphml <- shiny::downloadHandler(
      filename = function() sub("[.]csv$", ".graphml", stamp("graph")),
      content = function(file) {
        shiny::req(scored())
        igraph::write_graph(scored()$graph, file, format = "graphml")
      }
    )
    output$dl_suggestions <- shiny::downloadHandler(
      filename = function() stamp("suggestions"),
      content = function(file) {
        shiny::req(harvest())
        utils::write.csv(data.frame(suggestion = harvest()$suggestions),
                         file, row.names = FALSE)
      }
    )

    # --- AI briefing --------------------------------------------------------
    aiInsightsServer(
      "ai",
      context = shiny::reactive({
        built <- scored()
        result <- harvest()
        if (is.null(built) || is.null(result)) return(NULL)

        top_terms <- utils::head(built$terms, 30)
        clusters <- split(built$terms$term, built$terms$community)
        cluster_lines <- vapply(names(clusters), function(cl) {
          sprintf("- Cluster %s (%d terms): %s", cl, length(clusters[[cl]]),
                  paste(utils::head(clusters[[cl]], 12), collapse = ", "))
        }, character(1))

        top_edges <- built$edges[order(-built$edges$weight), , drop = FALSE]
        top_edges <- utils::head(top_edges, 20)

        paste(
          sprintf("Source: %s. Seed keyword: \"%s\". Scope: %s.",
                  config$source_label, trimws(input$keyword),
                  names(config$scope_choices)[match(input$scope, config$scope_choices)]),
          sprintf("%d autocomplete suggestions produced %d terms, %d links and %d clusters.",
                  length(result$suggestions), built$n_terms, nrow(built$edges),
                  length(unique(built$terms$community))),
          "",
          "Top terms (term | suggestions | connections | strength | bridging):",
          paste(sprintf("- %s | %d | %d | %d | %.3f", top_terms$term,
                        top_terms$frequency, top_terms$degree,
                        top_terms$strength, top_terms$betweenness),
                collapse = "\n"),
          "",
          "Clusters:",
          paste(utils::head(cluster_lines, 12), collapse = "\n"),
          "",
          "Strongest links:",
          paste(sprintf("- %s + %s (%d)", top_edges$from, top_edges$to, top_edges$weight),
                collapse = "\n"),
          sep = "\n"
        )
      }),
      system_prompt = paste(
        "You are a search and SEO strategist reading a co-occurrence network",
        "built from live autocomplete suggestions. Write for a marketer, not a",
        "data scientist. Structure the answer as: 1) what the clusters mean in",
        "plain language, naming them; 2) three to five concrete content or",
        "product opportunities the data supports; 3) anything that looks like",
        "noise or a data-quality problem. Refer only to terms that appear in",
        "the briefing, be specific, and keep it under 350 words."
      )
    )
  }
}
