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
library(jsonlite)

# Function to get Wikipedia Suggest queries
getWikiQueries <- function(search_query) {
  query <- URLencode(search_query)
  url <- paste0("https://en.wikipedia.org/w/api.php?action=opensearch&search=", query, "&limit=10&format=json")
  req <- GET(url)
  json_content <- content(req, as = "text")
  data <- fromJSON(json_content)
  suggestions <- data[[2]] # The second element contains the suggestions
  return(suggestions)
}

# Function to handle suggestions based on level and method (alphabetically or by vector)
suggestWikiQueries <- function (search_query, level, method = "alphabetically") {
  if (method == "alphabetically") {
    # Default alphabetical suggestion method
    all_suggestion <- getWikiQueries(search_query)
    if (level > 1) {
      for (l in letters) {
        local_suggestion <- getWikiQueries(paste0(search_query, " ", l))
        all_suggestion <- c(all_suggestion, local_suggestion)
      }
      if (level > 2) {
        for (l1 in letters) {
          for (l2 in letters) {
            local_suggestion <- getWikiQueries(paste0(search_query, " ", l1, l2))
            all_suggestion <- c(all_suggestion, local_suggestion)
          }
        }
      }
    }
  } else if (method == "by_vector") {
    # By vector suggestion method
    all_suggestion <- getWikiQueries(search_query)
    if (level > 1) {
      for (i in 2:level) {
        all_suggestion <- unlist(lapply(all_suggestion, function(q) getWikiQueries(q)))
      }
    }
  }
  return(unique(all_suggestion))
}

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
  
  div(class = "title-panel", "Keyword Network Analysis for Wikipedia"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      textInput("keyword", "Enter Keyword:", value = "madonna"),
      selectInput("method", "Suggestion Method:", choices = c("By Vector" = "by_vector", "Alphabetically" = "alphabetically")),
      selectInput("level", "Suggestion Level:", choices = 1:3, selected = 2),
      checkboxInput("remove_stopwords", "Remove Stopwords", value = TRUE),
      selectInput("solver", "Select Solver:", choices = c("barnesHut", "forceAtlas2Based", "repulsion")),
      
      actionButton("update", "Generate Network", class = "btn-update")
    ),
    mainPanel(
      class = "main-panel",
      visNetworkOutput("networkPlot", width = "100%", height = "850px"), "These results are based on Wikipedia suggestions."
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$update, {
    keyword <- input$keyword
    level <- as.numeric(input$level)
    method <- input$method
    solver <- input$solver
    
    # Use the selected suggestion method for Wikipedia
    suggested_queries <- suggestWikiQueries(keyword, level, method)
    
    # Split the keyword into individual words
    palabras_keyword <- unlist(strsplit(tolower(keyword), " "))
    
    # Create a list of words to ignore, including stopwords if selected
    if (input$remove_stopwords) {
      palabras_a_ignorar <- c(palabras_keyword, stopwords("en"))
    } else {
      palabras_a_ignorar <- palabras_keyword
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
    
    output$networkPlot <- renderVisNetwork({
      visNetwork(nodos, aristas) %>%
        visPhysics(solver = solver, stabilization = FALSE) %>%
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
