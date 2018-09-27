library("igraph")
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
library(xlsx)


genomes_Edges <- function(genomeId) {
  urls = paste("http://regprecise.lbl.gov/Services/rest/regulons?genomeId=", genomeId, sep = "")
  regulons <- fromJSON(txt = urls)
  regulons <- regulons$regulon
  edges <-data.frame(matrix(vector(), 0, 2,
                            dimnames=list(c(),c("from", "to"))),
                     stringsAsFactors=T)
  #Filter so only TF regulons are kept
  row <- 1
  if (is.null(nrow(regulons)) && regulons$regulationType == "RNA") {
    return(NULL)
  } else if (is.null(nrow(regulons)) && regulons$regulationType == "TF"){
    urls = paste("http://regprecise.lbl.gov/Services/rest/genes?regulonId=", regulons$regulonId, sep="")
    genes <- fromJSON(txt = urls)
    genes <- genes$gene
    for (j in 1:nrow(genes)) {
      edges[row,1] <- regulons$regulonId
      edges[row,2] <- genes[j,]$name
      row <- row + 1
    }} else if (nrow(regulons) == nrow(filter(regulons, "RNA" == regulons$regulationType))) {
      return(NULL)      
    }
  else {
  regulons <- filter(regulons, "TF" == regulons$regulationType)
  for(i in 1:nrow(regulons)) {
    if(!is.null(regulons[i, 9])){
    urls = paste("http://regprecise.lbl.gov/Services/rest/genes?regulonId=", regulons[i,]$regulonId, sep="")
    genes <- fromJSON(txt = urls)
    genes <- genes$gene
    if (is.null(nrow(genes))) {
      edges[row,1] <- regulons[i,]$regulatorName
      print(paste("regulon", regulons[i,]$regulonId))
      print(genes)
      if(is.na(genes$name) || is.null(genes$name)) {
        edges[row,2] <- genes$vimssId
      } else {
      edges[row,2] <- genes$name
      }
      row <- row + 1
      }
    else {
    for (j in 1:nrow(genes)) {
      edges[row,1] <- regulons[i,]$regulatorName
      if(is.na(genes[j,]$name) || is.null(genes$name)) {
        edges[row,2] <- genes[j,]$vimssId
      } else {
        edges[row,2] <- genes[j,]$name
      }
      row <- row + 1
    }
    }
    }
    }
  }
  return(edges)
  }

genomes_df <- fromJSON(txt = "http://regprecise.lbl.gov/Services/rest/genomes")
genomes_df <- genomes_df$genome
genomes <- list(52, 55, "54", "500", "334", "357", "497", "348", 
                "59", "60", "354", "63", "356", "61", "4", "7",
                "2", "1", "3", "5", "62", "6", "324", "98", "589", "57", 
                "57", "114", "556", "442", "10", "112", "242", 
                "16")
genomes_df <- filter(genomes_df, genomes_df$genomeId %in% genomes)
# print(genomes_df)
# write.xlsx(x = genomes_df, file = "Genomes.xlsx",
#            sheetName = "Genes", row.names = FALSE)
for (i in 9:30) {
  genomeId <- genomes_df[i,1]
  print(genomeId)
  filename <- paste(paste("nodes_G", genomeId, sep = ""), ".xlsx", sep = "")
  if(is.null(genomes_Edges(genomeId))) {
    next
  } else {
  write.xlsx(x = genomes_Edges(genomeId), file = filename,
             sheetName = "Nodes", row.names = FALSE)
  }
  print(paste("Done", genomeId))
 }
