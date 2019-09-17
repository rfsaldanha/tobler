data_tab <- argonTabItem(
  tabName = "data",
  argonColumn(
    width = 12,
    argonRow(
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Data upload",
        argonRow(
          "Load here your spatial data file including variables. Currently, we suggest the GML format.",
          fileInput("data_file", label = "", multiple = FALSE)
        ),
        argonRow(
          checkboxInput("data_validate", "Validate geometry", value = TRUE)
        )
      ) 
    ),
    argonColumn(
      width = 12,
      argonRow(
        uiOutput("data_table_UI")
      )
    )
  )
)