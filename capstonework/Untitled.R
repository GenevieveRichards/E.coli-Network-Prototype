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
dataframes <- read.csv("data.csv", header=T, as.is = T)

head(dataframes)

dataframes$Gene <- tolower(dataframes$Gene)
dataframes$TFactor <- tolower(dataframes$TFactor)

nodes1 <- unique(dataframes$TFactor)

edges1 <- filter(dataframes, Gene %in% nodes1)

nodes1 <- unique(edges1$TFactor)

nodes <- data.frame(id = nodes)
edges <- data.frame(from = edges1$TFactor, to = edges1$Gene)

deg <- degree(net, mode="all")
net <- graph_from_data_frame(d=edges1, vertices = nodes, directed=T)
net <- simplify(net, remove.loops=T, remove.multiple = T)

plot(net, layout=layout_nicely, edge.arrow.size = 0.1, edge.curved= 0.1, text = NA)

visNetwork(nodes, edges, width="100%")
