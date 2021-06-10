map_tab <- argonTabItem(
  tabName = "map",
  argonH1(display = 3, "Map"),
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      #icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      #border_level = 2,
      hover_shadow = TRUE,
      #title = "Map",
      argonRow(
        leafletOutput("map_leaflet", height = 600)
      )
    ),
    argonCard(
      width = 12,
      src = NULL,
      #icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      #title = "Controls",
      argonRow(
        uiOutput("map_variable_UI")
      ),
      argonRow(
        selectInput("map_style", label = "Style", choices = c("Jenks (Natural breaks)" = "jenks", "Quantile" = "quantile", "Standard Deviation" = "sd", "Equal intervals" = "equal", "Pretty" = "pretty"))
      )
    )
  )
)