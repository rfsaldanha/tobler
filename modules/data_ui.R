data_tab <- argonTabItem(
  tabName = "data",
    width = 12,
    argonH1(display = 3, "Data"),
    argonCard(
      icon = argonIcon("atom"),
      title = "Upload your data",
      width = 12,
      src = NULL,
      status = "success",
      shadow = TRUE,
      hover_shadow = TRUE,
      p("Load here your spatial data file including variables. Up to 30MB total size."),
      radioButtons("data_file_type", label = "File type", choices = c("GML or Geojson" = "GML", "Shapefile")),
      checkboxInput("data_validate", "Validate and clean geometry (recommended).", value = TRUE),
      uiOutput("data_file_UI"),
      selectInput("data_type", label = "Data type", choices = c("Cross-section", "Panel")),
      uiOutput("data_panel"),
      uiOutput("data_table_UI")
    )
)