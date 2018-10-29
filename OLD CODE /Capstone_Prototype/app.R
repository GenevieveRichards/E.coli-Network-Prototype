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

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  h1("QCloud TRNDiff Regulon Browser", align = "center"),
  hr(),
  conditionalPanel(
    condition = "input.home == 0 || input.back == 1",
    hr(),
    fluidRow(
      wellPanel(
        width = 12,
        verbatimTextOutput("genomesName1"),
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
        conditionalPanel(condition = "input.Expand == 1",
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
                               value = FALSE
                             ),
                             materialSwitch(
                               "removeLoops",
                               "Remove Self-Regulation",
                               right = TRUE,
                               value = FALSE,
                               status = "primary"
                             )
                           )
                         ))
      )
    ),
    
    mainPanel(
      width = 12,
      tags$div(
        align = "center",
        visNetworkOutput("network1", width = "100%", height = "500")
      )
    )
  ),
  conditionalPanel(
    condition = "input.home == 1",
    tags$div(
      align = "center",
      img(
        src = 'Nav2.png',
        align = "center",
        width = "60%",
        height = 150
      )
    ),
    hr(),
    br(),
    sidebarLayout(
      sidebarPanel(
        h5("Selected Full Network Overview"),
        actionButton("back", "Back")
      ),
      mainPanel(visNetworkOutput(
        "network2", width = "100%", height = 800
      ))
    )
  )
)
printouts <- ""
i <- 1
selected <- vector("list", 20)
# Define server logic required to draw a histogram
server <- function(input, output) {
  dataframes <- read.csv("data.csv", header = T, as.is = T)
  dataframes$Gene <- tolower(dataframes$Gene)
  dataframes$TFactor <- tolower(dataframes$TFactor)
  print(unique(append(unique(dataframes$Gene), unique(dataframes$TFactor))))
  edges <-
    data.frame(from = dataframes$TFactor, to = dataframes$Gene)
  graph <- graph.data.frame(edges, directed = T)
  degree_value <- (degree(graph, mode = "out") * 5)
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
  
  #Function for removing lone TFs
  uniques <- reactive({
    if (!input$viewNonRegulation) {
      nodes <-
        data.frame(id = nodes1,
                   label = nodes1,
                   value = degree_value[match(nodes1, names(degree_value))])
    } else {
      nodes <-
        data.frame(id = nodes2,
                   label = nodes2,
                   value = degree_value[match(nodes2, names(degree_value))])
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
      nodes1 <-
        unique(append(unique(edges2$from), unique(edges2$to)))
      edges <-
        data.frame(from = edges2$from,
                   to = edges2$to,
                   width = 1)
      nodes <- data.frame(id = nodes1, name = nodes1)
      
      vizualisation <- visNetwork(nodes, edges, width = "100%") %>%
        #visIgraphLayout(layout = "layout_nicely")
        visHierarchicalLayout(
          direction = "UD",
          edgeMinimization = FALSE,
          levelSeparation = 70
        )
      
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
    loops() %>%
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
                ;}")
})
  
  selectedId <- reactive({
    selected[i] <<- input$current_node_id
    print(selected)
    selected
  })
  nodesId <- reactive ({
    selected[i] <<- input$current_node_id
    if (i == 1) {printouts <<- paste(printouts, input$current_node_id, sep = "")} 
    else if (is.null(input$current_node_id)) {printouts}
    else {printouts <<- paste(printouts, input$current_node_id, sep = " -> ")}
    i <<- i + 1
    printouts
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
  
  newPoints <- reactive({
    newGraph <- dataframes
    newGraph <-
      filter(dataframes, TFactor %in% levels(as.factor(selectedId())))
    edges <- data.frame(from = newGraph$TFactor, to = newGraph$Gene)
    apply(df1, 1, function(x) 
      ifelse(any(x[1] == df2$pnr & x[2] == df2$drug), 'yes','no'))
    edges
  })
  
  
  output$network2 <- renderVisNetwork({
    nodes1 <- levels(newPoints()[, 2])
    nodes2 <- levels(newPoints()[, 1])
    df <- data.frame(id = nodes2, group = "TF")
    nodes3 <- append(nodes1, nodes2)
    nodes3 <- unique(nodes3)
    nodes <- data.frame(id = nodes3,
                        label = nodes3,
                        value = (degree_value[match(nodes3, names(degree_value))] * 5), 
                        group = NA)
    
    for (i in 1:nrow(nodes)) {
      if (nodes[i, 1] %in% nodes2) {
        nodes[i, 4] <- "TF"
        print(nodes[i, 4])
      } else {
        nodes[i, 4] <-"Gene"
      }
    }
    
    visNetwork(nodes,
               newPoints(),
               width = "100%")  %>%
      visEdges(arrows = c("to")) %>%
      # visHierarchicalLayout(
      #   direction = "UD",
      #   edgeMinimization = FALSE,
      #   levelSeparation = 70
      # ) %>%
      visIgraphLayout(layout = "layout_nicely") %>%
      visOptions(
        highlightNearest = list(
          enabled = T,
          algorithm = "hierarchical",
          hover = T
        ),
        collapse = F
      ) %>%
      visInteraction(multiselect = T) %>%
      visGroups(groupname = "TF", color = list(background = "orange"), shape = "diamond")
    
  })
}
# Run the application
shinyApp(ui = ui, server = server)
