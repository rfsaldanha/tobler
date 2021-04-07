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
          radioButtons(
            "sar_consistent",
            label = h4("Spatial heteroskedasticity and autocorrelation consistent (HAC)"),
            choices = list("HAC applied" = "hac", "HAC not applied" = "not_hac"), 
            selected = "not_hac"
          ),
          radioButtons(
            "sar_variance",
            label = h4("Variance"),
            choices = list("Homoskedasticity" = "homo", "Heteroskedasticity (robust)" = "hete"), 
            selected = "homo"
          )
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