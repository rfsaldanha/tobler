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
      ),
      argonTabSet(
        id = "pmodel-tests-tab",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "lg",
        width = 12,
        #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Hausman Test", 
          active = TRUE,
          h3("Hausman test for panel models"), br(),br(),
          actionButton("pmodel_hausman_test_execute", label = "Execute"), br(),br(),
          verbatimTextOutput("pmodel_hausman_test_results")
        ),
        argonTab(
          tabName = "Pesaran Test",
          h3("Pesaran's Cross Section test for cross sectional dependence in panel models"),
          actionButton("pmodel_pesaran_test_execute", label = "Execute"),
          verbatimTextOutput("pmodel_pesaran_test_results")
        )
      )
    )
  )
)