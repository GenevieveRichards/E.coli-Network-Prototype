#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# library(network) 
# library(sna)
library(visNetwork)
# library(threejs)
# library(networkD3)
# library(ndtv)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("QCloud TRNDiff Regulon Browser"),
   br(),
   
   tabsetPanel(
     type = "tabs",
     id = "tabsetpanel",
     tabPanel(
       title = "Network Visualisation",
       br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      h4("Genome: E. coli"), 
      br(),
      checkboxInput("viewNonRegulation", "Remove Unconnected Transcription Factors",
                    value = TRUE), 
      br(), 
      checkboxInput("removeLoops", "Remove Self-Regulation",
                    value = TRUE), 
      br()
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        visNetworkOutput("network1", width = "100%", height = 750)
      )
   )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataframes <- read.csv("data.csv", header=T, as.is = T)
  dataframes$Gene <- tolower(dataframes$Gene)
  dataframes$TFactor <- tolower(dataframes$TFactor)
  edges <- data.frame(from=dataframes$TFactor, to=dataframes$Gene)
  graph <- graph.data.frame(edges, directed = T)
  degree_value <- degree(graph, mode = "out")
  print(degree_value)
  
  nodes1 <- unique(dataframes$TFactor)
  edges1 <- filter(dataframes, Gene %in% nodes1)
  nodes2 <- unique(edges1$TFactor)
  edges1 <- filter(dataframes, Gene %in% nodes2)
  edges <- data.frame(from=edges1$TFactor, to=edges1$Gene)
  
  #Function for removing lone TFs
  uniques <- reactive({
    if(!input$viewNonRegulation) {
      nodes <- data.frame(id = nodes1, label = nodes1, value = degree_value[match(nodes1, names(degree_value))] )
    } else {
      nodes <- data.frame(id = nodes2, label = nodes2, value = degree_value[match(nodes2, names(degree_value))])
    }
    nodes
  })
  
  loops <- reactive({
    if (!input$removeLoops) {
      vizualisation <- visNetwork(uniques(), edges, width="100%") %>%  visEdges(arrows = "from")
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
      nodes <- unique(append(unique(edges2$from), unique(edges2$to)))
      edges <- data.frame(from = edges2$from, to =edges2$to)
      nodes1 <- data.frame(id = nodes, label = nodes, value = degree_value[match(nodes, names(degree_value))])
      vizualisation <- visNetwork(nodes1, edges, width="100%") %>%  
        visEdges(arrows = "from") %>%
        visIgraphLayout(layout = "layout_nicely") %>%
      #visHierarchicalLayout(direction = "DU", edgeMinimization = FALSE, levelSeparation = 200) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), collapse = T) 
        
    } else {
      net <- graph_from_data_frame(d=edges1, vertices = uniques(), directed=T)
      net <- simplify(net, remove.loops = T)
      data <- toVisNetworkData(net)
      vizualisation <- visNetwork(data$nodes, data$edges, width="100%") %>%  visEdges(arrows = "from") %>% visIgraphLayout(layout = "layout_nicely")
    }
    vizualisation
  })
  
  output$network1 <- renderVisNetwork({
    # vizualisation <- visNetwork(unique(), edges, width="100%") %>%  visEdges(arrows = "from") %>% visIgraphLayout(layout = "layout_nicely")
    loops()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

