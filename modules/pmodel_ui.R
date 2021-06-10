pmodel_tab <- argonTabItem(
  tabName = "pmodel",
  argonRow(
    argonColumn(
      width = 12,
      argonH1(display = 3, "Spatial Panel Models"),
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Model specification",
        argonRow(
          uiOutput("pmodel_dependent_variable_UI")
        ),
        argonRow(
          uiOutput("pmodel_independent_variable_UI")
        )
      )
    )
  )
)