autocor_tab <- argonTabItem(
  tabName = "autocor",
  argonRow(
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Spatial autocorrelation",
        argonRow(
          uiOutput("autocor_variable_UI")
        )
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Moran's I scatter plot",
        plotOutput("autocor_scatter")
      ),
      argonTabSet(
        id = "autocor-measures-tab",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "lg",
        width = 12,
        #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Moran's I",
          active = TRUE,
          verbatimTextOutput("autocor_moran"),
          verbatimTextOutput("autocor_moran_mc")
        ),
        argonTab(
          tabName = "Geary's C",
          active = FALSE,
          verbatimTextOutput("autocor_geary"),
          verbatimTextOutput("autocor_geary_mc")
        ),
        argonTab(
          tabName = "Getis-Ord's G",
          active = FALSE,
          verbatimTextOutput("autocor_getis")
        ),
        argonTab(
          tabName = "Getis-Ord's G Star",
          active = FALSE,
          verbatimTextOutput("autocor_getis_star")
        )
      )
    )
  )
)