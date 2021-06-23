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
          hr(),
          verbatimTextOutput("pmodel_hausman_test_results"),
          hr(),
          textAreaInput(inputId = "pmodel_hausman_test_general_observations", label = "General observations for PDF report"),
          downloadButton("pmodel_hausman_test_download", "Generate report")
        ),
        argonTab(
          tabName = "Pesaran Test",
          h3("Pesaran's Cross Section test for cross sectional dependence in panel models"),
          actionButton("pmodel_pesaran_test_execute", label = "Execute"),
          hr(),
          verbatimTextOutput("pmodel_pesaran_test_results"),
          hr(),
          textAreaInput(inputId = "pmodel_pesaran_test_general_observations", label = "General observations for PDF report"),
          downloadButton("pmodel_pesaran_test_download", "Generate report")
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
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression."),
          actionButton("pmodel_ols_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_ols_summary"),
          hr(),
          textAreaInput(inputId = "pmodel_ols_general_observations", label = "General observations for PDF report"),
          downloadButton("pmodel_ols_download", "Generate report")
        ),
        argonTab(
          tabName = "SAR (ML)",
          h3("SAR (ML)"),
          p("Spatial Autoregressive (SAR) panel model with maximum likelihood (ML) estimator."),
          radioButtons(
            inputId = "pmodel_sar_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
            selected = "within"
          ),
          actionButton("pmodel_sar_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sar_summary"),
          hr(),
          h4("Impacts"),
          verbatimTextOutput("pmodel_sar_impacts")
        ),
        argonTab(
          tabName = "SEM (ML)",
          h3("SEM (ML)"),
          p("Spatial Error panel Model (SEM) model with maximum likelihood (ML) estimator."),
          radioButtons(
            inputId = "pmodel_sem_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
            selected = "within"
          ),
          uiOutput("pmodel_sem_error_type_UI"),
          actionButton("pmodel_sem_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sem_summary")
        ),
        argonTab(
          tabName = "SAC (ML)",
          h3("SAC (ML)"),
          p("Spatial Autocorrelation panel Model (SAC) with maximum likelihood (ML) estimator."),
          radioButtons(
            inputId = "pmodel_sac_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
            selected = "within"
          ),
          uiOutput("pmodel_sac_error_type_UI"),
          actionButton("pmodel_sac_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sac_summary"),
          hr(),
          h4("Impacts"),
          verbatimTextOutput("pmodel_sac_impacts")
        ),
        argonTab(
          tabName = "SDEM (ML)",
          h3("SDEM (ML)"),
          p("Spatial Durbin Error panel Model (SDEM) with maximum likelihood (ML) estimator."),
          radioButtons(
            inputId = "pmodel_sdem_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
            selected = "within"
          ),
          uiOutput("pmodel_sdem_error_type_UI"),
          actionButton("pmodel_sdem_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_sdem_summary")
        ),
        argonTab(
          tabName = "SLX (ML)",
          h3("SLX (ML)"),
          p("Spatial Lag X (SLX) panel model with maximum likelihood (ML) estimator."),
          radioButtons(
            inputId = "pmodel_slx_effects", 
            label = h3("Effects"),
            choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
            selected = "within"
          ),
          actionButton("pmodel_slx_estimate", label = "Estimate"),
          hr(),
          h4("Estimation"),
          verbatimTextOutput("pmodel_slx_summary")
        )
      )
    )
  )
)