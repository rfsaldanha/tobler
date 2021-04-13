cross_section_models_tab <- argonTabItem(
  tabName = "cross_section_models",
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
        title = "Model type",
        radioButtons(
          "cross_section_model_type", 
          label = "Model type", 
          choices = list(
            "Ordinary Least Squares standard model" = "ols_std",
            "Spatial Autoregressive (SAR) model" = "sar",
            "Spatial Error Model (SEM) model" = "sem",
            "Spatial Autocorrelation Model (SAC)" = "sac",
            "Spatial Lag X (SLX) model" = "slx",
            "Spatial Durbin Model (SDM)" = "sdm",
            "Spatial Durbin Error Model (SDEM)" = "sdem"
          ), 
          selected = "ols_std"
        )
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Model specification",
        uiOutput("cross_section_model_dependent_variable_UI"),
        uiOutput("cross_section_model_independent_variable_UI"),
        uiOutput("cross_section_model_endogenous_variable_UI"),
        uiOutput("cross_section_model_instrument_variable_UI")
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Model estimator and options",
        uiOutput("cross_section_model_estimator_UI"),
        uiOutput("cross_section_model_variance_UI"),
        actionButton("cross_section_model_estimate", "Estimate", icon = icon("math"), status = "primary")
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = icon("cogs"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Model output",
        verbatimTextOutput("cross_section_model_summary")
      )
    )
  )
)