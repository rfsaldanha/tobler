home_tab <- argonTabItem(
  tabName = "home",
  argonRow(
    center = TRUE,
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        src = NULL,
        #icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        #title = "About",
        argonImage(
          src = "tobleR.png",
          floating = TRUE,
          card_mode = FALSE
        ),
        p("Tobler is a web app that offers an intuitive step-by-step process for estimation of spatial econometric models, including a basic spatial visualization, creating of spatial weighting matrix and spatial correlation tests. Currently, it is possible to estimate several cross-section and panel models, with different estimators. Estimates of impacts are provided, when applicable."),
        p("This app was entirely created with R using the Shiny library. Spatial data is handled by the RGDAL, cleangeo and leaflet libraries. Spatial dependence tests and spatial models are provided by the spdep, spatialreg, sphet and splm library."),
        p('The name is a tribute to the geographer Waldo R. Tobler and his First Law of Geography: "Everything is related to everything else, but near things are more related than distant things."'),
        p("This is a prototype. All results must be checked and validated by the users."),
        p("Developers: Raphael Saldanha (Fiocruz); Eduardo Almeida (UFJF).")
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "General options",
        numericInput(inputId = "modal_time", label = "Additional time for quote window while a model is being estimated (seconds).", value = 3, min = 1, step = 1)
      )
    )
  )
)