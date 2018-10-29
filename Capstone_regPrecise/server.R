library(shinyWidgets)
library(shiny)
library("igraph")
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(DT)
library(tools)
library(readxl)
library(reshape2)
library(tidyr)
library (scales)
library(visNetwork)
library(tidyverse)
library(httr)
library(jsonlite)
source("helpers.R")

selectedIds <- "60"
printouts <- ""
i <- 1
selected <- vector("list", 20)
regulatedTF <- vector("list", 0)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #Genomes
  genomes_df <- read_excel("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/genomes.xlsx")
  genomes_df1 <- read_excel("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/genomes1.xlsx")
  
  #Selection Menu for Genomes
  output$genomeSelection <- renderUI({
    selectizeInput("selectedGenome", label = "Select your model genome", 
                   choices = as.list(genomes_df$name), options = list(create = TRUE), selected = "Acetobacter pasteurianus IFO 3283-01")
  }) 
  
  output$Selectiongenome2 <- renderUI ({ 
    
    #Choices Initialisation
    choices <- vector("list", 10)
    j <- 1
  
    #Loop through all the Genomes
    for (genomesID in genomes_df$genomeId) {
      filename <- paste("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/",
                        paste(paste("nodes_G", genomesID, sep = ""), ".xlsx", sep = ""), sep="")
      network <-read_excel(filename) 
      network <- data.frame(from = network$from, to = network$to)
      selectedss <- levels(as.factor(selectedId()))
      choice = FALSE
        if (all(selectedss %in% unique(tolower(network$from)))) {
        choice = TRUE
        }
      
    if (choice) {
      choices[j] <- genomesID
      j <- j + 1
    }
    }
    choices2 <- vector("list", (j - 1))
    for (k in 1:(j-1)) {
      choices2[k] <- genomes_df[match(choices[k], genomes_df$genomeId), 2]
    }
    
    #Select Input
    selectizeInput("Selectiongenomes2", label = "Select the genome you would like to compare to: ", 
                   choices = choices2, options = list(create = TRUE), selected = "Acetobacter pasteurianus IFO 3283-01")
    
  })
  
  #Get the file for the Chosen Genome
  filenames1 <- reactive({
    if(is.null(input$selectedGenome)) {
      selectedIds <<- "60"
    } else {
      selectedIds <<- toString(genomes_df[match(input$selectedGenome, genomes_df$name), 1])
    }
    # if (selectedIds == "334") {
    #   filename <- paste("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/",
    #                     paste(paste("nodes_G", selectedIds, sep = ""), ".xls", sep = ""), sep="")
    # } else {
    filename <- paste("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/",
                      paste(paste("nodes_G", selectedIds, sep = ""), ".xlsx", sep = ""), sep="")
    # }
    dataframes1 <- read_excel(filename)
    dataframes <- data.frame(from = dataframes1$from, to = dataframes1$to)
    dataframes$to <- tolower(dataframes$to)
    dataframes$from <- tolower(dataframes$from)
    #Return the data 
    dataframes
  })
  #Function for removing lone TFs
  uniques <- reactive({
    edges <-
      data.frame(from = filenames1()$from, to = filenames1()$to)
    graph <- graph.data.frame(edges, directed = T)
    degree_value <- degree(graph, mode = "out")
    nodes1 <- unique(filenames1()$from)
    edges1 <- filter(filenames1(), to %in% nodes1)
    nodes3 <- unique(edges1$to)
    nodes4 <- unique(edges1$from)
    nodes2 <- append(nodes3, nodes4)
    nodes2 <- unique(nodes2)
    if (!input$viewNonRegulation) {
      nodes <-
        data.frame(
          id = nodes1,
          label = nodes1,
          group = NA, 
          value = degree_value[match(nodes1, names(degree_value))]
        )
    } else {
      nodes <-
        data.frame(
          id = nodes2,
          label = nodes2,
          group = NA,
          value = degree_value[match(nodes2, names(degree_value))]
        )
    }
    nodes
  })
  
  loops <- reactive({
    #Initial Code
    edges <- filenames1()
    graph <- graph.data.frame(edges, directed = T)
    degree_value <- degree(graph, mode = "out")
    nodes1 <- unique(filenames1()$from)
    edges1 <- filter(filenames1(), to %in% nodes1)
    edges <-
      data.frame(
        from = edges1$from,
        to = edges1$to
      )
    
    print(input$removeEdges)
    #Conditionals for the edges
    if (!input$removeLoops) {
      vizualisation <-
        visNetwork(uniques(), edges, width = "100%")
      
     } 
    else if (input$removeLoops && input$viewNonRegulation && !input$removeEdges) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      nodes3 <- unique(append(unique(edges2$from), unique(edges2$to)))
      nodes <- data.frame(id = nodes3, name = nodes3, group = NA, value = degree_value[match(nodes3, names(degree_value))])
      vizualisation <-
        visNetwork(nodes, edges, width = "100%")
    }
    else if (input$removeEdges) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from == to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      nodes3 <- unique(append(unique(edges2$from), unique(edges2$to)))
      nodes <- data.frame(id = nodes3, name = nodes3, group = NA, value = degree_value[match(nodes3, names(degree_value))])
      vizualisation <-
        visNetwork(nodes, edges, width = "100%")
    }
    else {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      
      vizualisation <-
        visNetwork(uniques(), edges, width = "100%") 
    }
    vizualisation
  })
  
  output$network1 <- renderVisNetwork({
    layouts()
  })
  
  layouts <- reactive ({
    networks <- loops() %>%
      visEdges(arrows = c("to")) %>%
      visOptions(
        highlightNearest = list(
          enabled = T,
          algorithm = "hierarchical",
          hover = T,
          degree = 1
        ),
        collapse = T
      ) %>%
      visInteraction(multiselect = T) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}") %>%
      visGroups(
        groupname = "regulatedTF",
        color = list(background = "orange"),
        shape = "circle", 
        value = 7
      )
    if (input$layoutSelect == "Layout Nicely") {
        networks <- networks %>% visIgraphLayout(layout = "layout_nicely")
      } else if (input$layoutSelect == "Layout with Fruchterman-Reingold") {
        networks %>% visIgraphLayout(layout = "layout_with_fr", randomSeed = 3)
      } else if (input$layoutSelect == "Layout with Kamada Kawai") {
        networks %>% visIgraphLayout(layout = "layout_with_kk", randomSeed = 3)
      } else if (input$layoutSelect == "Layout with LGL") {
        networks %>% visIgraphLayout(layout = "layout_with_lgl", randomSeed = 3)
      } else if (input$layoutSelect == "Layout with Davidson Harel") {
        networks %>% visIgraphLayout(layout = "layout_with_dh", randomSeed = 3)
      } else if (input$layoutSelect == "Layout with Sugiyama") {
        networks %>% visIgraphLayout(layout = "layout_with_sugiyama", randomSeed = 3)
      } else {
       networks <- networks %>% visHierarchicalLayout(
        direction = "UD",
        edgeMinimization = FALSE,
        levelSeparation = 70
      ) 
      }
    networks
  })
  selectedId <- reactive({
    selected[i] <<- input$current_node_id
    print(selected)
  })
  
  nodesId <- reactive ({
    selected[i] <<- input$current_node_id
    if (i == 1) {
      printouts <<- paste(printouts, input$current_node_id, sep = "")
    }
    else if (is.null(input$current_node_id)) {
      printouts
    }
    else {
      printouts <<- paste(printouts, input$current_node_id, sep = " -> ")
    }
    i <<- i + 1
    printouts
  })
  
  #Update TFs that regulate searched gene
  observe({
    inputgene <- tolower(input$genesearch)
    print(inputgene)
    j = 1
    for (i in 1:nrow(filenames1())) {
      if (filenames1()[i, 2] == inputgene) {
        regulatedTF[j] <<- filenames1()[i, 1]
        j <- j + 1
      }
    }
    if (input$removeLoops && input$viewNonRegulation) {
      edges <- filenames1()
      graph <- graph.data.frame(edges, directed = T)
      degree_value <- degree(graph, mode = "out")
      nodes1 <- unique(filenames1()$from)
      edges1 <- filter(filenames1(), to %in% nodes1)
      edges <-
        data.frame(
          from = edges1$from,
          to = edges1$to
        )
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      nodes3 <- unique(append(unique(edges2$from), unique(edges2$to)))
      nodes <- data.frame(id = nodes3, name = nodes3, group = NA, value = degree_value[match(nodes3, names(degree_value))])
    } else {
    nodes <- uniques()
    }
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% regulatedTF) {
        nodes[i, 3] <- "regulatedTF"
      }
    }
    visNetworkProxy("network1") %>% visUpdateNodes(nodes)
  })
  
  #TFsearch
  observe({
    TF <- tolower(input$TFsearch)
    if (input$removeLoops && input$viewNonRegulation) {
      edges <- filenames1()
      graph <- graph.data.frame(edges, directed = T)
      degree_value <- degree(graph, mode = "out")
      nodes1 <- unique(filenames1()$from)
      edges1 <- filter(filenames1(), to %in% nodes1)
      edges <-
        data.frame(
          from = edges1$from,
          to = edges1$to
        )
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      nodes3 <- unique(append(unique(edges2$from), unique(edges2$to)))
      nodes <- data.frame(id = nodes3, name = nodes3, group = NA, value = degree_value[match(nodes3, names(degree_value))])
    } else {
      nodes <- uniques()
    }
    
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] == TF) {
        nodes[i, 3] <- "regulatedTF"
      }
    }
    visNetworkProxy("network1") %>% visUpdateNodes(nodes)
  })
  
  
  #Show the Current Nodes
  output$shiny_return <- renderPrint({
    cat(nodesId())
  })
  
  output$shiny_return2 <- renderPrint({
    cat(nodesId())
  })
  
  #Show Genomes Name for TF Network
  
  
  genomesName <- reactive ({
    ID <- toString(genomes_df[match(input$selectedGenome, genomes_df$name), 1])
    cat(paste(paste("Genomes Name:", input$selectedGenome, sep = " "), paste("Genome ID: ", ID ,sep = "   "), sep = "\n" ))
  })
  
  output$genomesName1 <- renderPrint({
    genomesName()
  })
  
  #Genome Name for Model Genome
  output$Name_Genome <- renderPrint({
    cat(input$selectedGenome)
  })
  #Unselect Current Selected Nodes
  observe({
    input$gosel
    i <<- 1
    printouts <<- ""
    selected <<- assign("list", NULL, envir = .GlobalEnv)
    visNetworkProxy("network1") %>% visUnselectAll()
  })
  
  newEdges <- reactive({
    selectedss <- levels(as.factor(selectedId()))
    newGraph <- filenames1()
    newGraph <-
      filter(filenames1(), from %in% selectedss)
    edges <- data.frame(from = newGraph$from, to = newGraph$to)
    edges
  })
  
  newEdges2 <- reactive ({
    edgesT <- data.frame(from = newEdges()$from, to = newEdges()$to, color = "blue")
    if (input$colorSimDIFFTarget) {
      edgesT$color <- apply(edgesT, 1, function(x) 
        ifelse(any(x[1] == edges2()$from & x[2] == edges2()$to), 'grey','blue'))
    } 
    if (input$removeSimilarEdges) {
      edgesT$color <- apply(edgesT, 1, function(x) 
      ifelse(any(x[1] == edges2()$from & x[2] == edges2()$to), 'grey','blue'))
      edgesT <- filter(edgesT, edgesT$color == 'blue')
    }
      nodes1 <- levels(edgesT[, 2])
      nodes2 <- levels(edgesT[, 1])
      nodes3 <- append(nodes1, nodes2)
      nodes3 <- unique(nodes3)
      edges2 <- filter(edgesT, to %in% nodes3)
      print("EDGES")
      print(nodes3)
    edgesT
    })
  
  newNodes <- reactive({
    print(newEdges2())
    nodes1 <- levels(newEdges2()[, 2])
    nodes2 <- levels(newEdges2()[, 1])
    nodes3 <- append(nodes1, nodes2)
    nodes3 <- unique(nodes3)
    nodes <- data.frame(
      id = nodes3,
      label = nodes3,
      group = NA
    )
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% filenames1()[, 1]) {
        nodes[i, 3] <- "TF"
      } else {
        nodes[i, 3] <- "Gene"
      }
    }
    nodes
  })
  
  currentTFF <- reactive({
    nodes <- newNodes()
    nodes <- filter(nodes, nodes$group == "TF")
    currentTFs <- vector("list", 0)
    currentTFs <- nodes[,1]
  })
  
  baseNetwork <- reactive ({
    visNetwork(newNodes(),
               newEdges2(),
               width = "100%")  %>%
      visEdges(arrows = c("to")) %>%
      visIgraphLayout(layout = "layout_with_fr", randomSeed = 3)%>%
      visOptions(
        highlightNearest = list(
          enabled = T,
          algorithm = "hierarchical",
          hover = T
        ),
        collapse = T
      ) %>%
      visInteraction(multiselect = T) %>%
      visGroups(
        groupname = "Gene",
        color = list(background = "orange"),
        shape = "diamond"
      ) %>% visLegend()
  })
  output$network2 <- renderVisNetwork({
    baseNetwork()
  })
  
  output$network2a <- renderVisNetwork({
    baseNetwork()
  })
  
  edges2 <- reactive ({ 
    selectedSS <- levels(as.factor(selectedId()))
    name <- input$Selectiongenomes2
    ID <- toString(genomes_df[match(name, genomes_df$name), 1])
    edges <- edgesBase(selectedSS,ID)
    edgesnew <- data.frame(from = edges$from, to = edges$to, color = "blue", width = 1)
    if (input$colorSimDIFF) {
    edgesnew$color <- apply(edgesnew, 1, function(x) 
      ifelse(any(x[1] == newEdges()$from & x[2] == newEdges()$to), 'grey','blue'))}
    
    edgesnew
  })
  
  nodesId2 <- reactive ({ 
    dataframes <- data.frame(from = edges2()$from, to = edges2()$to)
    dataframes$to <- tolower(dataframes$to)
    dataframes$from <- tolower(dataframes$from)
    nodes1 <- levels(edges2()$from)
    nodes2 <- levels(edges2()$to)
    nodes3 <- append(nodes2, nodes1)
    nodes3 <- unique(nodes3)
    
    nodes <- data.frame(
      id = nodes3,
      label = nodes3,
      group = NA
    )
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% dataframes[, 1]) {
        nodes[i, 3] <- "TF"
      } else {
        nodes[i, 3] <- "Gene"
      }
    }
    nodes
    })

  output$network2b <- renderVisNetwork({
    visNetwork(nodesId2(),
               edges2(),
               width = "100%")  %>%
      visEdges(arrows = c("to")) %>%
      visIgraphLayout(layout = "layout_with_fr", randomSeed = 3)%>%
      visOptions(
        highlightNearest = list(
          enabled = T,
          algorithm = "hierarchical",
          hover = T
        ),
        collapse = T
      ) %>%
      visInteraction(multiselect = T) %>%
      visGroups(
        groupname = "Gene",
        color = list(background = "orange"),
        shape = "diamond"
      )
    
  })
})
