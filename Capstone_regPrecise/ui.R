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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  h3("QCloud TRNDiff Regulon Browser", align = "center"),
  hr(),
  conditionalPanel(
    condition = "input.select == 0",
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
      actionButton("select", "Submit")
    )
  ),
  conditionalPanel(
    condition = "input.home == 0 && input.select > 0",
    fluidRow(
      wellPanel(
        width = 12,
        
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
        visNetworkOutput("network1", width = "100%", height = "700")
      ),
      verbatimTextOutput("error_noEdges")
    )
  ),
  conditionalPanel(
    condition = "input.home == 1",
    fluidRow(
      wellPanel(
        width = 12,
        h5("The pathway being analysed is: "),
        verbatimTextOutput("shiny_return2"),
        selectInput(
          inputId = "gemoneSelect",
          label = "How many Genomes do you want to compare to: ",
          choices = c(1, 2, 3, 4),
          selected = 2,
          multiple = FALSE,
          selectize = TRUE,
          width = "100%",
          size = NULL
        )
      ),
      conditionalPanel(condition = "input['gemoneSelect'] == 1 && input.home == 1",
                       hr(),
                       mainPanel(
                         width = 12,
                         visNetworkOutput("network2", width = "100%", height = 800)
                       )),
      conditionalPanel(condition = "input['gemoneSelect'] == 2",
                       mainPanel(width = 12,
                                 fluidRow (
                                   column(
                                     width = 6,
                                     wellPanel(verbatimTextOutput("Name_Genome"),
                                               br(),
                                               br()),
                                     visNetworkOutput("network2a", width = "100%", height = 400)
                                   ),
                                   column(
                                     width = 6,
                                     wellPanel(uiOutput("Selectiongenome2")),
                                     visNetworkOutput("network2b", width = "100%", height = 400)
                                   )
                                 )))
    )
  )
))
