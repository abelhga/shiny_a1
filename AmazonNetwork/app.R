library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(igraph)
library(tm)
library(slam)
library(httr)
library(visNetwork)
library(shinycssloaders)
library(XML)
library(ggplot2)
library(reshape2)
library(DT)
library(SnowballC)
library(wordcloud2)
library(jsonlite)

# Function to get Google Suggest queries
getGSQueries <- function (search_query, code_lang) {
  query <- URLencode(search_query)
  url <- paste0("http://suggestqueries.google.com/complete/search?output=toolbar&hl=", code_lang, "&q=", query)
  req <- GET(url)
  xml <- content(req, "text", encoding = "UTF-8")
  doc <- xmlParse(xml, encoding = "UTF-8")
  list <- xpathSApply(doc, "//CompleteSuggestion/suggestion", xmlGetAttr, 'data')
  return(list)
}

# Function to get Amazon Suggest queries
getAmazonQueries <- function(search_query, market_id) {
  query <- URLencode(search_query)
  url <- paste0("https://completion.amazon.com/api/2017/suggestions?mid=", market_id, "&alias=aps&prefix=", query)
  req <- GET(url)
  json_content <- content(req, as = "text")
  data <- fromJSON(json_content)
  suggestions <- data$suggestions$value
  return(suggestions)
}

# Function to handle suggestions based on level and method (alphabetically or by vector)
suggestQueries <- function(search_query, code_lang, level, method = "alphabetically", source = "Google", market_id = NULL) {
  if (source == "Google") {
    getQueries <- function(query) getGSQueries(query, code_lang)
  } else if (source == "Amazon") {
    getQueries <- function(query) getAmazonQueries(query, market_id)
  } else {
    stop("Invalid source")
  }
  
  if (method == "alphabetically") {
    # Default alphabetical suggestion method
    all_suggestion <- getQueries(search_query)
    if (level > 1) {
      for (l in letters) {
        local_suggestion <- getQueries(paste0(search_query, " ", l))
        all_suggestion <- c(all_suggestion, local_suggestion)
      }
      if (level > 2) {
        for (l1 in letters) {
          for (l2 in letters) {
            local_suggestion <- getQueries(paste0(search_query, " ", l1, l2))
            all_suggestion <- c(all_suggestion, local_suggestion)
          }
        }
      }
    }
  } else if (method == "by_vector") {
    # By vector suggestion method
    all_suggestion <- getQueries(search_query)
    if (level > 1) {
      for (i in 2:level) {
        all_suggestion <- unique(unlist(lapply(all_suggestion, function(q) getQueries(q))))
      }
    }
  }
  return(unique(all_suggestion))
}

# List of Marketplaces with their codes and IDs
marketplace_list <- data.frame(
  Marketplace = c("United States", "Canada", "United Kingdom", "Germany", "France", "Spain", "Italy", "Mexico", "Brazil"),
  MarketID = c("ATVPDKIKX0DER", "A2EUQ1WTGCTBG2", "A1F83G8C2ARO7P", "A1PA6795UKMFR9", 
               "A13V1IB3VIYZZH", "A1RKKUPIHCS9HS", "APJ6JRA9NG5V4", "A1AM78C64UM0Y8", "A2Q3Y263D00KWC"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  
  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
      }
      .container-fluid {
        padding: 20px;
      }
      .title-panel {
        text-align: center;
        font-size: 28px;
        font-weight: bold;
        color: #333;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 0 15px rgba(0, 0, 0, 0.1);
      }
      .main-panel {
        padding: 20px;
      }
      .btn-update {
        width: 100%;
        background-color: #007bff;
        color: white;
        font-size: 16px;
        padding: 10px;
        border-radius: 8px;
        border: none;
      }
      .btn-update:hover {
        background-color: #0056b3;
      }
      #networkPlot {
        height: 900px;
      }
    "))
  ),
  
  div(class = "title-panel", "Keyword Network Analysis"),
  
  navbarPage(
    title = "",
    tabPanel("Network",
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar",
                 textInput("keyword", "Enter Keyword:", value = "where can I buy"),
                 selectInput("source", "Data Source:", choices = c("Google", "Amazon")),
                 conditionalPanel(
                   condition = "input.source == 'Google'",
                   selectInput("lang", "Language:", 
                               choices = c("English" = "en", "Spanish" = "es", "French" = "fr", "German" = "de"," " = ""))
                 ),
                 conditionalPanel(
                   condition = "input.source == 'Amazon'",
                   selectInput("market", "Select Marketplace:", 
                               choices = setNames(marketplace_list$MarketID, marketplace_list$Marketplace),selected = "ATVPDKIKX0DER")
                 ),
                 selectInput("method", "Suggestion Method:", choices = c("By Vector" = "by_vector","Alphabetically" = "alphabetically")),
                 selectInput("level", "Suggestion Level:", choices = 1:3, selected = 2),
                 checkboxInput("remove_stopwords", "Remove Stopwords", value = TRUE),  # Add checkbox for stopwords
                 selectInput("solver", "Select Solver:", choices = c("forceAtlas2Based","barnesHut","repulsion")),
                 
                 helpText("Suggestion Methods:",
                          tags$ul(
                            tags$li(tags$b("By Vector:"), " A recursive method where suggestions are generated by taking each suggestion from the previous level and using it as a new keyword. This approach is ideal for deep-diving into related search terms and exploring how suggestions evolve based on earlier results."),
                            tags$li(tags$b("Alphabetically:"), " The default method where search suggestions are generated by appending letters (a-z) to the keyword. This method is useful for exploring all possible autocomplete suggestions from A to Z.")
                          )),
                 helpText("Solvers:",
                          tags$ul(
                            tags$li(tags$b("barnesHut:"), " Suitable for large networks, fast and efficient."),
                            tags$li(tags$b("forceAtlas2Based:"), " Mimics physical forces, good for organic and aesthetic layouts. Recommended when the network cannot be stabilized."),
                            tags$li(tags$b("repulsion:"), " Good for small to medium networks with strong node repulsion.")
                          )),
                 
                 actionButton("update", "Generate Network", class = "btn-update")
               ),
               mainPanel(
                 class = "main-panel",
                 visNetworkOutput("networkPlot", width = "100%", height = "850px"),
                 "These results are tailored to your current location or selected marketplace."
               )
             )
    ),
    tabPanel("Heat Map",
             fluidRow(
               column(12,
                      plotOutput("heatMapPlot") %>% withSpinner()
               )
             )
    ),
    tabPanel("Word Cloud",
             fluidRow(
               column(12,
                      wordcloud2Output("wordCloud") %>% withSpinner()
               )
             )
    ),
    tabPanel("Phrases with Metrics",
             fluidRow(
               column(12,
                      dataTableOutput("phrasesTable") %>% withSpinner()
               )
             )
    ),
    tabPanel("Data Comparison",
             fluidRow(
               column(12,
                      plotOutput("comparisonPlot") %>% withSpinner()
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  data_reactive <- eventReactive(input$update, {
    keyword <- input$keyword
    level <- as.numeric(input$level)
    method <- input$method
    solver <- input$solver  # Get the selected solver
    source <- input$source
    
    # Determine language or market ID based on source
    if (source == "Google") {
      lang <- input$lang
      market_id <- NULL
    } else if (source == "Amazon") {
      lang <- "en"  # Default to English for stopwords
      market_id <- input$market
    }
    
    # Fetch suggestions based on the selected source
    suggested_queries <- suggestQueries(keyword, lang, level, method, source, market_id)
    
    # Split the keyword into individual words
    palabras_keyword <- unlist(strsplit(tolower(keyword), " "))
    
    # Create a list of words to ignore including stopwords and the keyword's words
    # Checking if the user wants to remove stopwords
    if (input$remove_stopwords && lang != "") {
      palabras_a_ignorar <- c(palabras_keyword, stopwords(lang))
    } else {
      palabras_a_ignorar <- palabras_keyword  # Only remove the keyword words, not stopwords
    }
    
    palabras_list <- lapply(suggested_queries, function(x) {
      palabras <- strsplit(tolower(x), " ")[[1]]
      palabras <- palabras[!palabras %in% palabras_a_ignorar]
      return(palabras)
    })
    
    palabras_unicas <- unique(unlist(palabras_list))
    matriz_co_ocurrencia <- matrix(0, length(palabras_unicas), length(palabras_unicas),
                                   dimnames = list(palabras_unicas, palabras_unicas))
    
    for (palabras in palabras_list) {
      if(length(unique(palabras)) >= 2) {
        combinaciones <- combn(palabras, 2)
        for (i in seq_len(ncol(combinaciones))) {
          par <- combinaciones[, i]
          if (!any(par %in% palabras_a_ignorar)) {
            matriz_co_ocurrencia[par[1], par[2]] <- matriz_co_ocurrencia[par[1], par[2]] + 1
            matriz_co_ocurrencia[par[2], par[1]] <- matriz_co_ocurrencia[par[2], par[1]] + 1
          }
        }
      }
    }
    
    red_semantica <- graph.adjacency(as.matrix(matriz_co_ocurrencia), mode = "undirected", weighted = TRUE)
    nodos <- data.frame(id = V(red_semantica)$name, label = V(red_semantica)$name, size = degree(red_semantica))
    aristas <- get.data.frame(red_semantica, what = "edges")
    
    comunidades <- cluster_louvain(red_semantica)
    nodos$group <- membership(comunidades)
    
    # Text mining for term frequencies
    # Create a corpus
    corpus <- Corpus(VectorSource(suggested_queries))
    # Clean the text
    corpus <- tm_map(corpus, content_transformer(tolower))
    # Remove punctuation, numbers, etc.
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    # Remove stopwords if selected
    if (input$remove_stopwords && lang != "") {
      corpus <- tm_map(corpus, removeWords, stopwords(lang))
    }
    # Remove keyword words
    corpus <- tm_map(corpus, removeWords, palabras_keyword)
    
    # Create term-document matrix
    tdm <- TermDocumentMatrix(corpus)
    tdm_matrix <- as.matrix(tdm)
    term_freq <- rowSums(tdm_matrix)
    term_freq <- sort(term_freq, decreasing = TRUE)
    
    return(list(
      nodos = nodos,
      aristas = aristas,
      matriz_co_ocurrencia = matriz_co_ocurrencia,
      suggested_queries = suggested_queries,
      term_freq = term_freq,
      tdm_matrix = tdm_matrix,
      source = source
    ))
  })
  
  output$networkPlot <- renderVisNetwork({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    nodos <- data$nodos
    aristas <- data$aristas
    solver <- input$solver
    
    visNetwork(nodos, aristas) %>%
      visPhysics(solver = solver, stabilization = FALSE) %>% #TRUE, FALSE or number of maximum iteractions
      visInteraction(dragNodes = TRUE) %>%
      visEvents(stabilizationIterationsDone = "function () {this.setOptions( { physics: false } );}") %>%
      visNodes(
        shape = "dot",
        scaling = list(min = 10, max = 30, label = list(enabled = TRUE, min = 30, max = 50)),
        font = list(size = 30)
      ) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 11)
  })
  
  output$heatMapPlot <- renderPlot({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    co_occur_matrix <- data$matriz_co_ocurrencia
    
    # Get top terms that are present in both term_freq and co_occur_matrix
    term_freq <- data$term_freq
    top_terms <- names(term_freq)
    top_terms <- intersect(top_terms, rownames(co_occur_matrix))
    top_terms <- top_terms[1:min(20, length(top_terms))]
    
    if (length(top_terms) < 2) {
      plot.new()
      text(0.5, 0.5, "Not enough data to generate heat map.")
      return()
    }
    
    # Subset the co-occurrence matrix
    co_occur_matrix_subset <- co_occur_matrix[top_terms, top_terms]
    
    # Convert matrix to long format
    co_occur_df <- melt(co_occur_matrix_subset)
    colnames(co_occur_df) <- c("Word1", "Word2", "Count")
    
    # Remove zeros
    co_occur_df <- co_occur_df[co_occur_df$Count > 0,]
    
    ggplot(co_occur_df, aes(x=Word1, y=Word2, fill=Count)) +
      geom_tile() +
      scale_fill_gradient(low="white", high="red") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$wordCloud <- renderWordcloud2({
    data <- data_reactive()
    if (is.null(data))
      return(NULL)
    
    term_freq <- data$term_freq
    term_freq_df <- data.frame(word = names(term_freq), freq = term_freq)
    
    if (nrow(term_freq_df) == 0) {
      return(NULL)
    }
    
    wordcloud2(term_freq_df, size = 1)
  })
  
  # ... [rest of the code including other outputs and the app's UI remains unchanged] ...
}

# Run the application 
shinyApp(ui = ui, server = server)