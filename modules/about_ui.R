about_tab <- argonTabItem(
  tabName = "about",
  argonRow(
    center = TRUE,
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "About",
        p("Tobler is an R Shiny app and relies on several packages to work."),
        tags$ul(
          tags$li("shiny"), 
          tags$li("argonR"), 
          tags$li("argonDash"),
          tags$li("shinycssloaders"),
          tags$li("tidyverse"),
          tags$li("glue"),
          tags$li("DT"),
          tags$li("rgdal"),
          tags$li("cleangeo"),
          tags$li("leaflet"),
          tags$li("highcharter"),
          tags$li("tmap"),
          tags$li("htmltools"),
          tags$li("spdep"),
          tags$li("spatialreg"),
          tags$li("plm"),
          tags$li("splm"),
          tags$li("statquotes")
        )
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "R Session Info",
        htmlOutput("session_info")
      )
    )
  )
)