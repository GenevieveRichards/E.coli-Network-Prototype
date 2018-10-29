# E.coli-Network-Prototype

Shiny app used for UI Prototyping purposes for the new visualisation conceptualisation for TRNDiff.org.

To Run this app you need to ensure that you have all the required packaged installed:

shinyWidgets
shiny
igraph
shiny
shinythemes
readr
dplyr
DT
tools
readxl
reshape2
tidyr
scales
visNetwork
tidyverse
httr
jsonlite

Once they are all installed, just hit Run App in rStudio to use this prototype.

The data used for this app has been take from RegPrecise and stored in excel files. We also have the e.coli network from RegulonDB in the old code folder. To obtain this data from the API the following Repo was created. https://github.com/GenevieveRichards/Regprecise-API-.

Note: The filter to remove similar edges in the compare pathway page is not fully functional and neither is the Show only self-regulation toggle as they were used for conceptualisation of changes from the UI workshop. 
