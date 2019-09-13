library(shiny)
library(argonR)
#devtools::install_github("RinteRface/argonDash")
library(argonDash)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(rgdal)
library(cleangeo)
library(ggplot2)
library(leaflet)
library(tmap)
library(htmltools)
library(spdep)
options(shiny.maxRequestSize=30*1024^2) 

# Template
source("sidebar.R")
source("header.R")
source("footer.R")

# UI elements
source("modules/data_ui.R")
source("modules/map_ui.R")
source("modules/weights_ui.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    title = "tobler",
    author = "Raphael Saldanha",
    description = "Spatial Econometrics with R",
    sidebar = argonSidebar,
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        data_tab,
        map_tab,
        weights_tab
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {
    
    source("modules/data_server.R", local = TRUE)
    source("modules/map_server.R", local = TRUE)
    source("modules/weights_server.R", local = TRUE)
    
  }
)