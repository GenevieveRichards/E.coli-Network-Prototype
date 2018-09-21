library(shinyWidgets)
library(shiny)
library("igraph")
library("ggplot2")
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)
library(tools)
library(readxl)
library(reshape2)
library(tidyr)
library (scales)
library(cowplot)
library(visNetwork)
library(tidyverse)
library(httr)
library(jsonlite)

#Globals
printouts <- ""
i <- 1
selected <- vector("list", 20)
regulatedTF <- vector("list", 0)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  h3("QCloud TRNDiff Regulon Browser", align = "center"),
  hr(),
  conditionalPanel( condition = "input.select == 0",
                    tags$div(
                      align = "center",
                      img(
                        src = 'Nav1.png',
                        align = "center",
                        width = "80%",
                        height = 80
                      )
                    ),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    tags$div(
                      align = "center",
                      uiOutput("genomeSelection"),
                      actionButton("select", "Submit"))),
  conditionalPanel(
    condition = "input.home == 0 && input.select > 0",
    fluidRow(  
      wellPanel(width = 12, 
                         
                         splitLayout(h5("Genome Name:"),
                                     h5("Genome Id: ")),
                         h5(
                           "Select the Transcription Factors or Regulatory pathways you are interested in:  "
                         ),
                         verbatimTextOutput("shiny_return"),
                         splitLayout(
                           actionButton("gosel", "Unselect nodes!"),
                           tags$div(align = "right",
                                    actionButton("home", "Submit"))
                         ),
                tags$div(align = "center",
                actionButton("Expand", "See More")),
                         conditionalPanel( condition = "input.Expand == 1",
                                           hr(),
                                           splitLayout(
                                             searchInput(
                                               inputId = "genesearch",
                                               label = "Search for a Gene you are interested in to highlight all the TFs that regulate it:",
                                               placeholder = "Search Gene name",
                                               btnSearch = icon("search"),
                                               btnReset = icon("remove"),
                                               width = "80%"
                                             ),
                                           splitLayout(
                                             materialSwitch(
                                               inputId = "viewNonRegulation",
                                               label = "Remove Unconnected Transcription Factors",
                                               status = "primary",
                                               right = TRUE,
                                               value = TRUE
                                             ),
                                             materialSwitch(
                                               "removeLoops",
                                               "Remove Self-Regulation",
                                               right = TRUE,
                                               value = TRUE,
                                               status = "primary"
                                             )))
                         )
    )),
    
    mainPanel( width = 12,
               tags$div(
                 align = "center",
                 visNetworkOutput(
                   "network1", width = "100%", height = "700"
                 )))
  ),
  conditionalPanel(
    condition = "input.home == 1",
    fluidRow(  
      wellPanel(width = 12, 
        selectInput(
          inputId = "gemoneSelect",
          label = "How many Genomes do you want to compare to: ",
          choices = c( 1, 2, 3, 4, 5, 6),
          selected = 1,
          multiple = FALSE,
          selectize = TRUE,
          width = "100%",
          size = NULL
        ),
        
        mainPanel( width = 12, 
                   visNetworkOutput(
                     "network2", width = "100%", height = 800
                   ))
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Genomes
  genomes_df <- read_excel("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/genomes.xlsx")
  output$genomeSelection <- renderUI({
    selectizeInput("selectedGenome", label = "Select your model genome", 
                   choices = as.list(genomes_df$name), options = list(create = TRUE))
  }) 
  
  dataframes <- read.csv("data.csv", header = T, as.is = T)
  dataframes$Gene <- tolower(dataframes$Gene)
  dataframes <- read.csv("data.csv", header = T, as.is = T)
  dataframes$Gene <- tolower(dataframes$Gene)
  dataframes$TFactor <- tolower(dataframes$TFactor)
  edges <-
    data.frame(from = dataframes$TFactor, to = dataframes$Gene)
  graph <- graph.data.frame(edges, directed = T)
  degree_value <- degree(graph, mode = "out")
  nodes1 <- unique(dataframes$TFactor)
  edges1 <- filter(dataframes, Gene %in% nodes1)
  nodes2 <- unique(edges1$TFactor)
  edges1 <- filter(dataframes, Gene %in% nodes2)
  edges <-
    data.frame(
      from = edges1$TFactor,
      to = edges1$Gene,
      value = edges1$Regulatory_effect
    )
  
  # filenames1 <- reactive({
  #   selectedId <- toString(genomes_df[match(input$selectedGenome, genomes_df$name), 1])
  #   print(selectedId)
  #   filename <- paste("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/",
  #                     paste(paste("nodes_G", selectedId, sep = ""), ".xlsx", sep = ""), sep="")
  #   print(filename)
  #   dataframes1 <- read_excel(filename)
  #   print(dataframes1)
  #   dataframes <- data.frame(from = dataframes1$From, to = dataframes1$To)
  #   print(dataframes)
  # })

  #To determine the degrees
  # graph <- graph.data.frame(edges, directed = T)
  # degree_value <- degree(graph, mode = "out")


  
  # nodes <- reactive({
  #   nodes1 <- unique(filenames1()$from)
  #   print(nodes1)
  #   edges1 <- filter(filenames1(), to %in% nodes1)
  #   nodes3 <- unique(edges1$from)
  #   print(nodes3)
  #   nodes2 <- data.frame(id = nodes3, name = nodes3)
  #   print(nodes2)
  #   nodes2
  # })
  # 
  # edges <- reactive({
  #   nodes1 <- unique(filenames1()$from)
  #   edges1 <- filter(filenames1(), to %in% nodes1)
  #   nodes3 <- unique(edges1$from)
  #   nodes2 <- data.frame(id = nodes3, name = nodes3)
  #   edges1 <- filter(filenames1(), to %in% nodes2)
  #   edges <- data.frame(from = edges1$from, to = edges1$to)
  #   edges
  # })
  
  #Function for removing lone TFs
  uniques <- reactive({
    if (!input$viewNonRegulation) {
      nodes <-
        data.frame(
          id = nodes1,
          label = nodes1,
          value = degree_value[match(nodes1, names(degree_value))],
          group = NA
        )
    } else if (input$removeLoops && input$viewNonRegulation) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      nodes1 <-
        unique(append(unique(edges2$from), unique(edges2$to)))
      nodes <- data.frame(
        id = nodes1,
        label = nodes1,
        value = degree_value[match(nodes1, names(degree_value))],
        group = NA
      )
    }
    else {
      nodes <-
        data.frame(
          id = nodes2,
          label = nodes2,
          value = degree_value[match(nodes2, names(degree_value))],
          group = NA
        )
    }
    nodes
  })


  loops <- reactive({
    if (!input$removeLoops) {
      vizualisation <-
        visNetwork(uniques(), edges, width = "100%") %>% visIgraphLayout(layout = "layout_nicely")
      
    } else if (input$removeLoops && input$viewNonRegulation) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      
      vizualisation <-
        visNetwork(uniques(), edges, width = "100%") %>%
        visHierarchicalLayout(
          direction = "LR",
          edgeMinimization = FALSE,
          levelSeparation = 120
        )
      # visIgraphLayout(layout = "layout_nicely")
      
    } else {
      net <-
        graph_from_data_frame(d = edges1,
                              vertices = uniques(),
                              directed = T)
      net <- simplify(net, remove.loops = T)
      data <- toVisNetworkData(net)
      vizualisation <-
        visNetwork(data$nodes, data$edges, width = "100%") %>% visIgraphLayout(layout = "layout_nicely")
    }
    vizualisation
  })
  
  output$network1 <- renderVisNetwork({
loops()%>%
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
        shape = "diamond"
      )
  })
  
  selectedId <- reactive({
    selected[i] <<- input$current_node_id
    print(selected)
    selected
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
    j = 1
    for (i in 1:nrow(dataframes)) {
      if (dataframes[i, 2] == inputgene) {
        regulatedTF[j] <<- dataframes[i, 1]
        j <- j + 1
      }
    }
    nodes <- uniques()
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% regulatedTF) {
        nodes[i, 4] <- "regulatedTF"
      }
    }
    visNetworkProxy("network1") %>% visUpdateNodes(nodes)
  })
  
  #Show the Current Nodes
  output$shiny_return <- renderPrint({
    nodesId()
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
    newGraph <- dataframes
    newGraph <-
      filter(dataframes, TFactor %in% selectedss)
    edges <- data.frame(from = newGraph$TFactor, to = newGraph$Gene)
    edges
  })

  newNodes <- reactive({
    nodes1 <- levels(newEdges()[, 2])
    nodes2 <- levels(newEdges()[, 1])
    nodes3 <- append(nodes1, nodes2)
    nodes3 <- unique(nodes3)
    nodes <- data.frame(
      id = nodes3,
      label = nodes3,
      value = degree_value[match(nodes3, names(degree_value))],
      group = NA
    )
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% dataframes[, 1]) {
        nodes[i, 4] <- "TF"
      } else {
        nodes[i, 4] <- "Gene"
      }
    }
    nodes
  })

  currentTFF <- reactive({
    nodes <- newNodes()
    nodes <- filter(nodes, nodes$group == "TF")
    print(nodes)
    currentTFs <- vector("list", 0)
    currentTFs <- nodes[,1]
    print(unique(currentTFs))
  })


  output$network2 <- renderVisNetwork({
    visNetwork(newNodes(),
               newEdges(),
               width = "100%")  %>%
      visEdges(arrows = c("to")) %>%
      visIgraphLayout(layout = "layout_nicely")%>%
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
}
# Run the application
shinyApp(ui = ui, server = server)
