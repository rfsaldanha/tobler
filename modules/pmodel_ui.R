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
      withMathJax("$$ H = NT(\\hat{\\theta}_\\text{FGLS} - \\hat{\\theta}_W)^\\top (\\hat{\\Sigma}_W - \\hat{\\Sigma}_\\text{FGLS})^1 (\\hat{\\theta}_\\text{FGLS} - \\hat{\\theta}_W) $$"),
      actionButton("pmodel_hausman_test_execute", label = "Execute"), 
      hr(),
      verbatimTextOutput("pmodel_hausman_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_hausman_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_hausman_test_download", "Generate report")
    ),
    argonTab(
      tabName = "Hausman Test for spatial panel models", 
      active = FALSE,
      h3("Hausman Test for spatial panel models"), 
      # withMathJax("$$ H = NT(\\hat{\\theta}_\\text{FGLS} - \\hat{\\theta}_W)^\\top (\\hat{\\Sigma}_W - \\hat{\\Sigma}_\\text{FGLS})^1 (\\hat{\\theta}_\\text{FGLS} - \\hat{\\theta}_W) $$"),
      p("SAC model with random effects and SAC model with fixed effects."),
      withMathJax("$$ H = NT \\left(\\hat{\\theta}^\\text{SAC}_\\text{FGLS} - \\hat{\\theta}^\\text{SAC}_\\text{within} \\right)^\\top \\left( \\hat{\\Sigma}^\\text{SAC}_\\text{within} - \\hat{\\Sigma}^\\text{SAC}_\\text{FGLS} \\right)^{-1} \\left( \\hat{\\theta}^\\text{SAC}_\\text{FGLS} - \\hat{\\theta}^\\text{SAC}_\\text{within} \\right)$$"),
      p("SAR model with random effects and SAR model with random effects."),
      withMathJax("$$ H = NT \\left(\\hat{\\theta}^\\text{SAR}_\\text{FGLS} - \\hat{\\theta}^\\text{SAR}_\\text{within} \\right)^\\top \\left( \\hat{\\Sigma}^\\text{SAR}_\\text{within} - \\hat{\\Sigma}^\\text{SAR}_\\text{FGLS} \\right)^{-1} \\left( \\hat{\\theta}^\\text{SAR}_\\text{FGLS} - \\hat{\\theta}^\\text{SAR}_\\text{within} \\right)$$"),
      p("SEM model with random effects and SEM model with fixed effects."),
      withMathJax("$$ H = NT \\left(\\hat{\\theta}^\\text{SEM}_\\text{FGLS} - \\hat{\\theta}^\\text{SEM}_\\text{within} \\right)^\\top \\left( \\hat{\\Sigma}^\\text{SEM}_\\text{within} - \\hat{\\Sigma}^\\text{SEM}_\\text{FGLS} \\right)^{-1} \\left( \\hat{\\theta}^\\text{SEM}_\\text{FGLS} - \\hat{\\theta}^\\text{SEM}_\\text{within} \\right)$$"),
      radioButtons(
        inputId = "pmodel_hausman_spatial_test_error_type", 
        label = h3("Error type"),
        choices = list("Baltagi" = "b", "Kapoor, Kelejian and Prucha" = "kkp"), 
        selected = "b"
      ),
      actionButton("pmodel_hausman_spatial_test_execute", label = "Execute"), 
      hr(),
      verbatimTextOutput("pmodel_hausman_spatial_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_hausman_spatial_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_hausman_spatial_test_download", "Generate report")
    ),
    argonTab(
      tabName = "Pesaran's CD Test",
      h3("Pesaran's Cross Section test for cross sectional dependence in panel models"),
      withMathJax("$$ H = \\text{ no global/local cross-sectional correlation} $$"),
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
      tabName = "Baltagi, Song and Koh LM tests",
      h3("Baltagi, Song and Koh LM tests for spatial panels"),
      tags$ul(
        tags$li(withMathJax("LM joint: One-sided joint LM test for \\( H^a_0 : \\lambda = \\sigma^2_\\mu = 0 \\)")), 
        tags$li(withMathJax("LM mu: Marginal LM test for \\( H^b_0 : \\sigma^2_\\mu = 0 \\) assuming \\( \\lambda = 0 \\)")), 
        tags$li(withMathJax("LM lambda: Marginal LM test for \\( H^C_0 : \\lambda = 0 \\) assuming \\( \\sigma^2_\\mu = 0 \\)")),
        tags$li(withMathJax("CLM mu: Conditional LM test for \\( H^e_0 : \\sigma^2_\\mu = 0 \\) assuming \\( \\lambda = 0  \\text{ or } \\lambda \\neq 0\\)")),
        tags$li(withMathJax("CLM lambda: Conditional LM test for \\( H^d_0 : \\lambda = 0  \\) assuming \\( \\sigma^2_\\mu \\geq 0 \\)"))
      ),
      tags$a(href="https://doi.org/10.1016/S0304-4076(03)00120-9", target="_blank", "Baltagi, Song and Koh (2003) paper link."),
      br(),br(),
      actionButton("pmodel_bsk_test_execute", label = "Execute"),
      hr(),
      verbatimTextOutput("pmodel_bsk_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_bsk_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_bsk_test_download", "Generate report")
    ),
    argonTab(
      tabName = "Baltagi, Song, Jung and Koh LM tests",
      h3("Baltagi, Song, Jung and Koh LM tests for spatial panels"),
      tags$ul(
        tags$li(withMathJax("C1: \\( H^h_0 : \\lambda = 0 \\) (allowing \\( \\rho \\neq 0 \\) and \\( \\sigma^2_\\mu > 0 \\) ), and the alternative is \\( H^h_1 : \\lambda \\neq 0 \\) (allowing \\( \\rho \\neq 0 \\) and \\( \\sigma^2_\\mu > 0 \\) ). This is a one-dimensional conditional test for no spatial error correlation allowing the presence of both serial correlation and random region effects.")), 
        tags$li(withMathJax("C2: \\( H^i_0 : \\rho = 0 \\) (allowing \\( \\lambda \\neq 0 \\) and \\( \\sigma^2_\\mu > 0 \\) ), and the alternative is \\( H^i_1 : \\rho \\neq 0  \\) (allowing \\( \\lambda \\neq 0 \\) and \\(\\sigma^2_\\mu > 0 \\) ). This is a one-dimensional conditional test for no serial correlation allowing the presence of both spatial error correlation and random region effects.")),
        tags$li(withMathJax("C3: \\( H^j_0 : \\sigma^2_\\mu = 0 \\) (allowing \\( \\rho \\neq 0 \\) and \\( \\lambda \\neq 0 \\) ), and the alternative is \\( H^j_1 : \\sigma^2_\\mu > 0 \\) (allowing \\( \\rho \\neq 0 \\) and \\( \\lambda \\neq 0 \\) ). This is a one-dimensional conditional test for zero random region effects allowing the presence of both serial and spatial error correlation.")),
        tags$li(withMathJax("J: \\(H^a_0 : \\lambda = \\rho = \\sigma^2_\\mu = 0 \\) , this is the joint hypothesis that there is no spatial or serial error correlation and no random region effects. The alternative \\( H^a_1 \\) is that at least one component is not zero, so that there may be serial or spatial error correlation or random region effects."))
      ),
      tags$a(href="https://doi.org/10.1016/j.jeconom.2006.09.001", target="_blank", "Baltagi, Song, Jung and Koh (2007) paper link."),
      br(),br(),
      actionButton("pmodel_bsjk_test_execute", label = "Execute"),
      hr(),
      verbatimTextOutput("pmodel_bsjk_test_results"),
      hr(),
      textAreaInput(inputId = "pmodel_bsjk_test_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_bsjk_test_download", "Generate report")
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
      checkboxGroupInput(inputId = "pmodel_sar_gm_options", label = "Options", choices = c("Lag external instrument variables" = "lag_instruments")),
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
      checkboxGroupInput(inputId = "pmodel_sem_gm_options", label = "Options", choices = c("Lag external instrument variables" = "lag_instruments")),
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
      withMathJax("$$ y_t = \\lambda Wy_t + X_t \\beta + \\xi_t $$"),
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
      checkboxGroupInput(inputId = "pmodel_sac_gm_options", label = "Options", choices = c("Lag external instrument variables" = "lag_instruments")),
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
      uiOutput("pmodel_sdem_durbin_var_UI"),
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
    ),
    argonTab(
      tabName = "SLX (GM)",
      h3("SLX (GM)"),
      p("Spatial Lag X (GM) panel model with Generalized Moments (GM) estimator."),
      withMathJax("$$ y_t = X_t \\beta + WX_t \\theta + \\varepsilon_t $$"),
      radioButtons(
        inputId = "pmodel_slx_gm_effects", 
        label = h3("Effects"),
        choices = list("Fixed (within)" = "within", "Random" = "random"), 
        selected = "within"
      ),
      checkboxGroupInput(inputId = "pmodel_slx_gm_options", label = "Options", choices = c("Lag external instrument variables" = "lag_instruments")),
      actionButton("pmodel_slx_gm_estimate", label = "Estimate"),
      hr(),
      h4("Estimation"),
      verbatimTextOutput("pmodel_slx_gm_summary"),
      hr(),
      textAreaInput(inputId = "pmodel_slx_gm_general_observations", label = "General observations for PDF report"),
      downloadButton("pmodel_slx_gm_download", "Generate report")
    )
  )
)