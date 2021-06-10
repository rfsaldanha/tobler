library(shiny)
library(argonR)
#devtools::install_github("RinteRface/argonDash")
#withr::with_libpaths(new = "/usr/local/lib/R/site-library", install_github("RinteRface/argonDash"))
library(argonDash)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(rgdal)
library(cleangeo)
library(leaflet)
library(highcharter)
library(tmap)
library(htmltools)
library(spdep)
library(spatialreg)
library(reshape2)
library(statquotes)
options(
  shiny.maxRequestSize=30*1024^2, 
  shiny.sanitize.errors = FALSE
) 

# Template
source("sidebar.R")
source("header.R")
source("footer.R")

# UI elements
source("modules/home_ui.R")
source("modules/data_ui.R")
source("modules/map_ui.R")
source("modules/weights_ui.R")
source("modules/autocor_ui.R")
source("modules/model_ui.R")
source("modules/pmodel_ui.R")

# App
shiny::shinyApp(
  ui = argonDashPage(
    tags$head(
      tags$style(HTML("
      .navbar-vertical.navbar-expand-md .navbar-brand-img
      {
        max-height: 6rem
      }
    "))
    ),
    title = "tobler",
    author = "Raphael Saldanha",
    description = "Spatial Econometrics with R",
    sidebar = argonSidebar,
    header = argonHeader,
    body = argonDashBody(
      argonTabItems(
        home_tab,
        data_tab,
        map_tab,
        weights_tab,
        autocor_tab,
        model_tab,
        pmodel_tab
      )
    ),
    footer = argonFooter
  ),
  server = function(input, output) {
    
    # Server elements
    source("modules/modal.R", local = TRUE)
    source("modules/data_server.R", local = TRUE)
    source("modules/map_server.R", local = TRUE)
    source("modules/weights_server.R", local = TRUE)
    source("modules/autocor_server.R", local = TRUE)
    source("modules/model_server.R", local = TRUE)
    source("modules/pmodel_server.R", local = TRUE)
    
  }
)