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

genomes_Edges <- function(genomeId) {
  urls = paste("http://regprecise.lbl.gov/Services/rest/regulons?genomeId=", genomeId, sep = "")
  regulons <- fromJSON(txt = urls)
  regulons <- regulons$regulon
  # print(regulons)
  
  #Filter so only TF regulons are kept
  regulons <- filter(regulons, "TF" == regulons$regulationType)
  # print(regulons)
  
  edges <-data.frame(matrix(vector(), 0, 2,
                            dimnames=list(c(), c("From", "To"))),
                     stringsAsFactors=T)
  row <- 1
  
  
  for(i in 1:nrow(regulons)) {
    if(!is.null(regulons[i, 9])){
      urls = paste("http://regprecise.lbl.gov/Services/rest/genes?regulonId=", regulons[i,]$regulonId, sep="")
      # print(urls)
      genes <- fromJSON(txt = urls)
      genes <- genes$gene
      if (is.null(nrow(genes))) {
        edges[row,1] <- regulons[i,]$regulonId
        edges[row,2] <- genes$vimssId
        row <- row + 1
      }
      else {
        for (j in 1:nrow(genes)) {
          edges[row,1] <- regulons[i,]$regulonId
          edges[row,2] <- genes[j,]$vimssId
          row <- row + 1
        }
      }
    }
  }
  return(edges)
}

genome_Nodes <- function(genomeId) {
  edges <- genomes_Edges(genomeId)
  
  nodes1 <- unique(edges$From) 
  nodes2 <- unique(edges$To)
  nodes <- append(nodes1, nodes2)
  nodes <- unique(nodes)
  nodes3 <- data.frame(id = nodes)
  return(nodes3)
  
}
  
