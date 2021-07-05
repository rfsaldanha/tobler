pmodel_tab <- argonTabItem(
  tabName = "pmodel",
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
    fluidRow(
      column(
        width = 6,
        uiOutput("pmodel_dependent_variable_UI")
      ),
      column(
        width = 6,
        uiOutput("pmodel_independent_variable_UI")
      )
    ),
    fluidRow(
      column(
        width = 6,
        uiOutput("pmodel_endog_variable_UI")
      ),
      column(
        width = 6,
        uiOutput("pmodel_instruments_variable_UI")
      )
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
      tabName = "Pesaran's CD Test",
      h3("Pesaran's Cross Section test for cross sectional dependence in panel models"),
      radioButtons(
        inputId = "pmodel_pesaran_test_type", 
        label = h3("Type"),
        choices = c("Global", "Local"),
        selected = "Global"
      ),
      actionButton("pmodel_pesaran_test_execute", label = "Execute"),
      hr(),
      verbatimTextOutput("pmodel_pesaran_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_pesaran_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_pesaran_test_download", "Generate report")
    ),
    argonTab(
      tabName = "Baltagi, Song and Koh LM test",
      h3("Baltagi, Song and Koh LM test for spatial panels"),
      actionButton("pmodel_bsk_test_execute", label = "Execute"),
      hr(),
      verbatimTextOutput("pmodel_bsk_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_bsk_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_bsk_test_download", "Generate report")
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
      withMathJax("$$ y_t = \\alpha + X_t \\beta + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_ols_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
        selected = "within"
      ),
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
      p("Spatial Autoregressive (SAR) panel model with Maximum Likelihood (ML) estimator."),
      withMathJax("$$ y_t = \\lambda Wy_t + X_t \\beta + \\varepsilon_t $$"),
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
      verbatimTextOutput("pmodel_sar_impacts"),
      hr(),
      textAreaInput(inputId = "pmodel_sar_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sar_download", "Generate report")
    ),
    argonTab(
      tabName = "SAR (GM)",
      h3("SAR (GM)"),
      p("Spatial Autoregressive (SAR) panel model with Generalized Moments (GM) estimator."),
      withMathJax("$$ y_t = \\lambda Wy_t + X_t \\beta + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_sar_gm_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random"), 
        selected = "within"
      ),
      actionButton("pmodel_sar_gm_estimate", label = "Estimate"),
      hr(),
      h4("Estimation"),
      verbatimTextOutput("pmodel_sar_gm_summary"),
      hr(),
      h4("Impacts"),
      verbatimTextOutput("pmodel_sar_gm_impacts"),
      hr(),
      textAreaInput(inputId = "pmodel_sar_gm_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sar_gm_download", "Generate report")
    ),
    argonTab(
      tabName = "SEM (ML)",
      h3("SEM (ML)"),
      p("Spatial Error panel Model (SEM) model with Maximum Likelihood (ML) estimator."),
      withMathJax("$$ y_t = X_t \\beta + \\xi_t $$"),
      withMathJax("$$ \\xi_t = \\rho W \\xi_t + \\varepsilon_t $$"),
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
      verbatimTextOutput("pmodel_sem_summary"),
      hr(),
      textAreaInput(inputId = "pmodel_sem_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sem_download", "Generate report")
    ),
    argonTab(
      tabName = "SEM (GM)",
      h3("SEM (GM)"),
      p("Spatial Error panel Model (SEM) model with Generalized Moments (GM) estimator."),
      withMathJax("$$ y_t = X_t \\beta + \\xi_t $$"),
      withMathJax("$$ \\xi_t = \\rho W \\xi_t + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_sem_gm_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random"), 
        selected = "within"
      ),
      actionButton("pmodel_sem_gm_estimate", label = "Estimate"),
      hr(),
      h4("Estimation"),
      verbatimTextOutput("pmodel_sem_gm_summary"),
      hr(),
      textAreaInput(inputId = "pmodel_sem_gm_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sem_gm_download", "Generate report")
    ),
    argonTab(
      tabName = "SAC (ML)",
      h3("SAC (ML)"),
      p("Spatial Autocorrelation panel Model (SAC) with Maximum Likelihood (ML) estimator."),
      withMathJax("$$ y_t = \\rho Wy_t + X_t \\beta + \\xi_t $$"),
      withMathJax("$$ \\xi_t = \\rho W \\xi_t + \\varepsilon_t $$"),
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
      verbatimTextOutput("pmodel_sac_impacts"),
      hr(),
      textAreaInput(inputId = "pmodel_sac_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sac_download", "Generate report")
    ),
    argonTab(
      tabName = "SAC (GM)",
      h3("SAC (GM)"),
      p("Spatial Autocorrelation panel Model (SAC) with Generalized Moments (GM) estimator."),
      withMathJax("$$ y_t = \\lambda Wy_t + X_t \\beta + \\xi_t $$"),
      withMathJax("$$ \\xi_t = \\rho W \\xi_t + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_sac_gm_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random"), 
        selected = "within"
      ),
      actionButton("pmodel_sac_gm_estimate", label = "Estimate"),
      hr(),
      h4("Estimation"),
      verbatimTextOutput("pmodel_sac_gm_summary"),
      hr(),
      h4("Impacts"),
      verbatimTextOutput("pmodel_sac_gm_impacts"),
      hr(),
      textAreaInput(inputId = "pmodel_sac_gm_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sac_gm_download", "Generate report")
    ),
    argonTab(
      tabName = "SDEM (ML)",
      h3("SDEM (ML)"),
      p("Spatial Durbin Error panel Model (SDEM) with Maximum Likelihood (ML) estimator."),
      withMathJax("$$ y_t = X_t \\beta + WX_t \\theta + \\xi_t $$"),
      withMathJax("$$ \\xi_t = \\rho W \\xi_t + \\varepsilon_t $$"),
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
      verbatimTextOutput("pmodel_sdem_summary"),
      hr(),
      textAreaInput(inputId = "pmodel_sdem_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_sdem_download", "Generate report")
    ),
    argonTab(
      tabName = "SLX (ML)",
      h3("SLX (ML)"),
      p("Spatial Lag X (SLX) panel model with Maximum Likelihood (ML) estimator."),
      withMathJax("$$ y_t = X_t \\beta + WX_t \\theta + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_slx_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random", "Pooling" = "pooling"), 
        selected = "within"
      ),
      actionButton("pmodel_slx_estimate", label = "Estimate"),
      hr(),
      h4("Estimation"),
      verbatimTextOutput("pmodel_slx_summary"),
      hr(),
      textAreaInput(inputId = "pmodel_slx_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_slx_download", "Generate report")
    )
  )
)