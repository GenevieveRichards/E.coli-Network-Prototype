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
                                               value = FALSE,
                                               status = "primary"
                                             )))
                         )
    )),
    
    mainPanel( width = 12,
               tags$div(
                 align = "center",
                 visNetworkOutput(
                   "network1", width = "100%", height = "700"
                 )
                 ), 
               verbatimTextOutput("error_noEdges"))
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

selectedIds <- "601"
# Define server logic required to draw a histogram
server <- function(input, output) {
  #Genomes
  genomes_df <- read_excel("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/genomes.xlsx")
  
  #Selection Menu for Genomes
  output$genomeSelection <- renderUI({
    selectizeInput("selectedGenome", label = "Select your model genome", 
                   choices = as.list(genomes_df$name), options = list(create = TRUE), selected = "Acetobacter pasteurianus IFO 3283-01")
  }) 
  
#Get the file for the Chosen Genome
  filenames1 <- reactive({
    if(is.null(input$selectedGenome)) {
      selectedIds <<- "601"
    } else {
    selectedIds <<- toString(genomes_df[match(input$selectedGenome, genomes_df$name), 1])
    }
    filename <- paste("~/Documents/GitHub/E.coli-Network-Prototype/Capstone_regPrecise/",
                      paste(paste("nodes_G", selectedIds, sep = ""), ".xlsx", sep = ""), sep="")
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
    nodes2 <- unique(edges1$from)
    if (!input$viewNonRegulation) {
      nodes <-
        data.frame(
          id = nodes1,
          label = nodes1,
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
        group = NA
      )
    }
    else {
      nodes <-
        data.frame(
          id = nodes2,
          label = nodes2,
          group = NA
        )
    }
    nodes
  })

  loops <- reactive({
    #Initial Code
    edges <- filenames1()
    # graph <- graph.data.frame(edges, directed = T)
    # degree_value <- degree(graph, mode = "out")
    nodes1 <- unique(filenames1()$from)
    edges1 <- filter(filenames1(), to %in% nodes1)
    nodes2 <- unique(edges1$from)
    edges1 <- filter(filenames1(), to %in% nodes2)
    edges <-
      data.frame(
        from = edges1$from,
        to = edges1$to
      )
    #Conditionals for the edges
    if (!input$removeLoops) {
      vizualisation <-
        visNetwork(uniques(), edges, width = "100%") %>% visIgraphLayout(layout = "layout_nicely")
      
    } else if (input$removeLoops && input$viewNonRegulation) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      print(nrow(filter(temp, from != to)) == 0)
      if(nrow(filter(temp, from != to)) == 0) {
        vizualisation <- NULL
      } else {
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
  }
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
    if (!is.null(loops())) {
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
  }})
  errorMessage <- reactive ({
    if (is.null(loops())) {
      "No Transcription Factors Regulate Each Other. /n 
      To View all Transcription Factors set Remove Loops and Remove Self Regulation to False"
    }
  })
  output$error_noEdges <- renderPrint({
  errorMessage()
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
    j = 1
    for (i in 1:nrow(filenames1())) {
      if (filenames1()[i, 2] == inputgene) {
        regulatedTF[j] <<- filenames1()[i, 1]
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
    newGraph <- filenames1()
    newGraph <-
      filter(filenames1(), from %in% selectedss)
    edges <- data.frame(from = newGraph$from, to = newGraph$to)
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


  output$network2 <- renderVisNetwork({
    print(newNodes())
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
