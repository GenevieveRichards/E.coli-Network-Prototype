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
  conditionalPanel(condition = "input.home == 0 || input.back == 1",
                   tags$div(
                     align = "center",
                     img(
                       src = 'Nav1.png',
                       align = "center",
                       width = "60%",
                       height = 130
                     )
                   ),
                   hr(),
                   sidebarLayout(
                     sidebarPanel(
                       p("This network is a visualisation of all the Transcription Factors in your chosen Genome.  "),
                       hr(),
                       h4("Genome Name:"),
                       h4("Genome Id: "),
                       checkboxInput(
                         "viewNonRegulation",
                         "Remove Unconnected Transcription Factors",
                         value = TRUE
                       ),
                       checkboxInput("removeLoops", "Remove Self-Regulation",
                                     value = TRUE),
                       h5("Select the Transcription Factors or Regulatory pathways you are interested in:  "),
                       verbatimTextOutput("shiny_return"),
                       br(),
                       actionButton("gosel", "Unselect nodes!"),
                       br(),
                       actionButton("home", "Submit")
                     ),
                     
                     # Show a plot? of the generated distribution
                     mainPanel(visNetworkOutput(
                       "network1", width = "100%", height = 800
                     ))
                   )),
  conditionalPanel(condition = "input.home == 1",
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
                     sidebarPanel(h5("Selected Full Network Overview"), 
                                  actionButton("back", "Back")),
                     mainPanel(visNetworkOutput(
                       "network2", width = "100%", height = 800
                     ))
                   ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dataframes <- read.csv("data.csv", header = T, as.is = T)
  print(dataframes)
  dataframes$Gene <- tolower(dataframes$Gene)
  dataframes$TFactor <- tolower(dataframes$TFactor)
  edges <-
    data.frame(from = dataframes$TFactor, to = dataframes$Gene)
  graph <- graph.data.frame(edges, directed = T)
  degree_value <- degree(graph, mode = "out")
  print(degree_value)
  
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
      vizualisation <- visNetwork(uniques(), edges, width = "100%") %>% visIgraphLayout(layout = "layout_nicely") 
      
    } else if (input$removeLoops && input$viewNonRegulation) {
      temp <- edges
      i <- sapply(temp, is.factor)
      temp[i] <- lapply(temp[i], as.character)
      edges2 <- filter(temp, from != to)
      print("Edges 2 not equal")
      nodes <- unique(edges2$from)
      print(summary(nodes))
      edges2 <- filter(edges2, to %in% nodes)
      print("Edges 2 to in nodes")
      nodes <-
        unique(append(unique(edges2$from), unique(edges2$to)))
      edges <- data.frame(from = edges2$from, to = edges2$to)
      nodes1 <-
        data.frame(id = nodes,
                   label = nodes,
                   value = degree_value[match(nodes, names(degree_value))])
      
      vizualisation <- visNetwork(nodes1, edges, width = "100%") %>%
        # visIgraphLayout(layout = "layout_nicely")
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
        collapse = T, 
        nodesIdSelection = T
      ) %>%
      visInteraction(multiselect = T) %>%
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
})
  
  #Show the Current Nodes
  output$shiny_return <- renderPrint({
    input$current_node_id
  })
  
  #Unselect Current Selected Nodes
  observe({
    input$gosel
    visNetworkProxy("network1") %>% visUnselectAll()
  })
  
  newPoints <- reactive({
    print(summary(dataframes))
    newGraph <- dataframes
    newGraph <-
      filter(dataframes, TFactor %in% levels(as.factor(input$current_node_id)))
    edges <- data.frame(from = newGraph$TFactor, to = newGraph$Gene)
    edges
  })
  
  
  output$network2 <- renderVisNetwork({
    nodes1 <- levels(newPoints()[, 2])
    nodes2 <- levels(newPoints()[, 1])
    nodes3 <- append(nodes1, nodes2)
    nodes3 <- unique(nodes3)
    nodes <-data.frame(
      id = nodes3,
      label = nodes3,
      value = degree_value[match(nodes3, names(degree_value))]
    )
    visNetwork(nodes,
      newPoints(),
      width = "100%"
    )  %>%
      visEdges(arrows = c("to")) %>%
      visIgraphLayout(layout = "layout_nicely") %>%
      visOptions(
        highlightNearest = list(
          enabled = T,
          algorithm = "hierarchical",
          hover = T
        ),
        collapse = F
      ) %>%
      visInteraction(multiselect = T)
    
  })
}
# Run the application
shinyApp(ui = ui, server = server)
