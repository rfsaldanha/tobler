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
      argonH1(display = 4, "Tests"),
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
          h3("Hausman Test for panel models"), 
          actionButton("pmodel_hausman_test_execute", label = "Execute"), 
          verbatimTextOutput("pmodel_hausman_test_results")
        ),
        argonTab(
          tabName = "Pesaran Test",
          h3("Pesaran's Cross Section test for cross sectional dependence in panel models"),
          actionButton("pmodel_pesaran_test_execute", label = "Execute"),
          verbatimTextOutput("pmodel_pesaran_test_results")
        )
      ),
      br(),
      argonH1(display = 4, "Models"),
      argonTabSet(
        id = "pmodel-model-tab",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "lg",
        width = 12,
        #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "OLS", 
          active = TRUE,
          h3("OLS"),
          actionButton("pmodel_ols_estimate", label = "Estimate"),
          br(),br(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_ols_summary")
        ),
        argonTab(
          tabName = "SAR",
          h3("SAR"),
          radioButtons(
            inputId = "pmodel_sar_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random"), 
            selected = "within"
          ),
          actionButton("pmodel_sar_estimate", label = "Estimate"),
          br(),br(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sar_summary"),
          br(),br(),
          h4("Impacts"),
          verbatimTextOutput("pmodel_sar_impacts")
        ),
        argonTab(
          tabName = "SEM",
          h3("SEM"),
          radioButtons(
            inputId = "pmodel_sem_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random"), 
            selected = "within"
          ),
          actionButton("pmodel_sem_estimate", label = "Estimate"),
          br(),br(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sem_summary")
        ),
        argonTab(
          tabName = "SAC",
          h3("SAC"),
          radioButtons(
            inputId = "pmodel_sac_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random"), 
            selected = "within"
          ),
          actionButton("pmodel_sac_estimate", label = "Estimate"),
          br(),br(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sac_summary"),
          br(),br(),
          h4("Impacts"),
          verbatimTextOutput("pmodel_sac_impacts")
        )
      )
    )
  )
)