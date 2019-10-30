model_tab <- argonTabItem(
  tabName = "model",
  argonRow(
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Spatial models",
        argonRow(
          uiOutput("model_dependent_variable_UI")
        ),
        argonRow(
          uiOutput("model_independent_variable_UI")
        )
      ),
      argonTabSet(
        id = "models-tab",
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
          p("Ordinary Least Squares estimator."),
          actionButton("model_estimate_ols", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_ols_summary"),
          h4("Moran's I for error term"),
          verbatimTextOutput("model_ols_error"),
          h4("Residual map"),
          leafletOutput("model_ols_map", height = 600),
          h4("Lagrange multiplier"),
          verbatimTextOutput("model_ols_lagrange")
        ),
        argonTab(
          tabName = "SAR (ML)",
          active = FALSE,
          h3("SAR (ML)"),
          p("SAR model with ML estimator."),
          actionButton("model_estimate_sar_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sar_mv_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_sar_mv_impacts"),
          h4("Residual map"),
          leafletOutput("model_sar_mv_map", height = 600)
        ),
        argonTab(
          tabName = "SAR (STSLS)",
          active = FALSE,
          h3("SAR (STSLS)"),
          p("SAR model with STSLS estimator."),
          actionButton("model_estimate_sar_stsls", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sar_mq2e_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_sar_mq2e_impacts"),
          h4("Residual map"),
          leafletOutput("model_sar_mq2e_map", height = 600)
        ),
        argonTab(
          tabName = "SEM (ML)",
          active = FALSE,
          h3("SEM (ML)"),
          p("SAR model with ML estimator."),
          actionButton("model_estimate_sem_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sem_mv_summary"),
          h4("Residual map"),
          leafletOutput("model_sem_mv_map", height = 600)
        ),
        argonTab(
          tabName = "SEM (STSLS)",
          active = FALSE,
          h3("SEM (STSLS)"),
          p("SAR model with STSLS estimator."),
          actionButton("model_estimate_sem_stsls", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sem_mq2e_summary"),
          h4("Residual map"),
          leafletOutput("model_sem_mq2e_map", height = 600)
        ),
        argonTab(
          tabName = "SAC (ML)",
          active = FALSE,
          h3("SAC (ML)"),
          p("SAC model with ML estimator."),
          actionButton("model_estimate_sac_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sac_mv_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_sac_mv_impacts"),
          h4("Residual map"),
          leafletOutput("model_sac_mv_map", height = 600)
        ),
        argonTab(
          tabName = "SAC (STSLS)",
          active = FALSE,
          h3("SAC (STSLS)"),
          p("SAC model with STSLS estimator."),
          actionButton("model_estimate_sac_stsls", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sac_mq2e_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_sac_mq2e_impacts"),
          h4("Residual map"),
          leafletOutput("model_sac_mq2e_map", height = 600)
        ),
        argonTab(
          tabName = "SLX (ML)",
          active = FALSE,
          h3("SLX (ML)"),
          p("SLX model with ML estimator."),
          actionButton("model_estimate_slx_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_slx_mv_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_slx_mv_impacts"),
          h4("Residual map"),
          leafletOutput("model_slx_mv_map", height = 600)
        ),
        argonTab(
          tabName = "SDM (ML)",
          active = FALSE,
          h3("SDM (ML)"),
          p("SDM model with ML estimator."),
          actionButton("model_estimate_sdm_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sdm_mv_summary"),
          h4("Impacts"),
          verbatimTextOutput("model_sdm_mv_impacts"),
          h4("Residual map"),
          leafletOutput("model_sdm_mv_map", height = 600)
        ),
        argonTab(
          tabName = "SDEM (ML)",
          active = FALSE,
          h3("SDEM (ML)"),
          p("SDEM model with ML estimator."),
          actionButton("model_estimate_sdem_ml", "Estimate", icon = icon("math"), status = "primary"),
          h4("Estimation"),
          verbatimTextOutput("model_sdem_mv_summary"),
          h4("Residual map"),
          leafletOutput("model_sdem_mv_map", height = 600)
        )
      )
    )
  )
)