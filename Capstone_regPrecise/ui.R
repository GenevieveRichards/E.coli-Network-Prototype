library(shinyWidgets)
library(shiny)
library("igraph")
library("ggplot2")
library(shinythemes)
library(readr)
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
  br(),
  conditionalPanel(
    condition = "input.select == 0",
    tags$div(
      align = "center",
      img(
        src = 'Nav1.png',
        align = "center",
        width = "50%",
        height = 90
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
      wellPanel(style = "background: #b4bdc1",
        width = 12,
        p("This view shows the transcription regulatory network without the genes. A node is a transcription factor (TF). 
           The edges show regulation that occurs between the two nodes with the direction of the arrow 
           identifying which TF regulates the other TF."),
        verbatimTextOutput("genomesName1"),
        h5(
          "Select the Transcription Factors or Regulatory pathways you are interested in:  "
        ),
        fluidRow(
        column(9,
        tags$div( align = "center",
        verbatimTextOutput("shiny_return", placeholder=TRUE))),
        column(3,
        splitLayout(
          actionButton("gosel", "Unselect nodes!"),
                   actionButton("home", "Submit"))
        )),
        tags$div(align = "center",
                 actionButton("Expand", "▼ Customise ▼")),
        conditionalPanel(condition = "input.Expand == 1",
                         hr(),
                         fluidRow(
                           column(4,
                                  searchInput(
                                    inputId = "TFsearch",
                                    label = "Find a TF you are interested in: ",
                                    placeholder = "Search Transcription Factor  name",
                                    btnSearch = icon("search"),
                                    btnReset = icon("remove"),
                                    width = "80%"
                                  ),
                           searchInput(
                             inputId = "genesearch",
                             label = "Search for a Gene you are interested in to highlight all the TFs that regulate it:",
                             placeholder = "Search Gene name",
                             btnSearch = icon("search"),
                             btnReset = icon("remove"),
                             width = "80%"
                           )
                           ), 
                           column(6,
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
                             ),
                           materialSwitch(
                             "removeEdges",
                             "Show Self-Regulated Edges Only",
                             right = TRUE,
                             value = FALSE,
                             status = "primary"
                         )
                         ), 
                         column(4, 
                                selectInput(
                                  inputId = "layoutSelect",
                                  label = "Choose your preferred layout option: ",
                                  choices = c("Layout Nicely", "Hierarchical", "Layout with Fruchterman-Reingold", 
                                              "Layout with Kamada Kawai", "Layout with LGL", "Layout with Davidson Harel",
                                              "Layout with Sugiyama"),
                                  selected = "Layout Nicely",
                                  multiple = FALSE,
                                  selectize = TRUE,
                                  width = "100%",
                                  size = NULL
                                ),
                                p("Here we will add filters to highlight aspects of the network to find greater insights based on from workshop"))))
      )
    ),
    
    mainPanel(
      width = 12,
      tags$div(
        align = "center",
        visNetworkOutput("network1", width = "100%", height = "500")
      ),
      verbatimTextOutput("error_noEdges")
    )
  ),
  conditionalPanel(
    condition = "input.home == 1",
    fluidRow(
      wellPanel( style = "background: #b4bdc1",
        width = 12,
        h5("The pathway being analysed is: "),
        verbatimTextOutput("shiny_return2"),
        selectInput(
          inputId = "gemoneSelect",
          label = "How many Genomes do you want to compare to: ",
          choices = c(1, 2),
          selected = 1,
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
                         visNetworkOutput("network2", width = "100%", height = 500)
                       )),
      conditionalPanel(condition = "input['gemoneSelect'] == 2",
                       mainPanel(width = 12,
                                 fluidRow (
                                   column(
                                     width = 6,
                                     wellPanel(style = "background: #b4bdc1", 
                                               h5("Model Genome: "),
                                               verbatimTextOutput("Name_Genome"),
                                               materialSwitch(
                                                 "colorSimDIFFTarget",
                                                 "Color edges present in the Selected Gene ",
                                                 right = TRUE,
                                                 value = FALSE,
                                                 status = "primary"
                                               ),
                                               materialSwitch(
                                                 "removeSimilarEdges",
                                                 "Remove Edges present in Selected Gene",
                                                 right = TRUE,
                                                 value = FALSE,
                                                 status = "primary"
                                               )
                                               ),
                                     visNetworkOutput("network2a", width = "100%", height = 400)
                                   ),
                                   column(
                                     width = 6,
                                     wellPanel(style = "background: #b4bdc1", uiOutput("Selectiongenome2"),
                                               materialSwitch(
                                                 "colorSimDIFF",
                                                 "Color edges present in the Target Gene ",
                                                 right = TRUE,
                                                 value = FALSE,
                                                 status = "primary"
                                               ), 
                                               materialSwitch(
                                                 "removeSimilarEdges2",
                                                 "Remove Edges present in Target Gene",
                                                 right = TRUE,
                                                 value = FALSE,
                                                 status = "primary"
                                               )),
                                     visNetworkOutput("network2b", width = "100%", height = 400)
                                   )
                                 )))
    )
  )
))
