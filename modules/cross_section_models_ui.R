cross_section_models_tab <- argonTabItem(
  tabName = "cross_section_models",
  argonRow(
    argonColumn(
      width = 12,
      argonTabSet(
        id = "cross_section_models_tab",
        card_wrapper = TRUE,
        horizontal = FALSE,
        circle = FALSE,
        size = "lg",
        width = 12,
        #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Ordinary Least Squares estimator for linear regression",
          active = TRUE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression."),
          withMathJax(helpText('$$3^2+4^2=5^2$$'))
        ),
        argonTab(
          tabName = "Spatial Autoregressive (SAR) model",
          active = FALSE,
          h3("Spatial Autoregressive (SAR) model"),
          withMathJax(helpText('$$3^2+4^2=5^2$$')),
          radioButtons(
            "sar_estimator",
            label = h4("Estimator"),
            choices = list("Maximum Likelihood (ML) estimator" = "ml", "Spatial Two Stage Least Squares (STSLS) estimator" = "stsls"), 
            selected = "ml"
          ),
          uiOutput("cross_section_model_sar_consistent_UI"),
          uiOutput("cross_section_model_sar_variance_UI"),
          h4("Model specification"),
          uiOutput("cross_section_model_dependent_variable_UI"),
          uiOutput("cross_section_model_independent_variable_UI"),
          uiOutput("cross_section_model_endogenous_variable_UI"),
          uiOutput("cross_section_model_instrument_variable_UI"),
          actionButton("cross_section_model_estimate_sar", "Estimate", icon = icon("math"), status = "primary"),
          br(),br(),
          h4("Summary"),
          verbatimTextOutput("model_sar_summary")
        ),
        argonTab(
          tabName = "Spatial Error Model (SEM) model",
          active = FALSE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression.")
        ),
        argonTab(
          tabName = "Spatial Autocorrelation Model (SAC)",
          active = FALSE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression.")
        ),
        argonTab(
          tabName = "Spatial Lag X (SLX) model",
          active = FALSE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression.")
        ),
        argonTab(
          tabName = "Spatial Durbin Model (SDM)",
          active = FALSE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression.")
        ),
        argonTab(
          tabName = "Spatial Durbin Error Model (SDM)",
          active = FALSE,
          h3("OLS model"),
          p("Ordinary Least Squares estimator for linear regression.")
        )
      )
    )
  )
)