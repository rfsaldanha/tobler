data_tab <- argonTabItem(
  tabName = "data",
    width = 12,
    argonH1(display = 3, "Data"),
    argonCard(
      width = 12,
      src = NULL,
      status = "success",
      shadow = TRUE,
      hover_shadow = TRUE,
      p("Load here your spatial data file including variables. Currently, we suggest the GML format."),
      fileInput("data_file", label = "", multiple = FALSE),
      checkboxInput("data_validate", "Validate and clean geometry (recommended).", value = TRUE),
      selectInput("data_type", label = "Data type", choices = c("Cross-section", "Panel")),
      uiOutput("data_panel"),
      uiOutput("data_table_UI")
    )
)