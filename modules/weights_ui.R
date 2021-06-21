weights_tab <- argonTabItem(
  tabName = "weights",
  argonH1(display = 3, "Spatial Weights Matrix"),
  argonRow(
    argonColumn(
      width = 12,
      argonTabSet(
        id = "weights-tab",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "lg",
        width = 12,
        #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
        argonTab(
          tabName = "Spatial Contiguity Matrix",
          active = TRUE,
          h3("Spatial Contiguity Matrix"),
          radioButtons(inputId = "weights_contiguity_radio", 
                       label = "Contiguity matrix options",
                       choices = list("Rook" = 1, "Queen" = 2),
                       selected = 1),
          numericInput(inputId = "weights_contiguity_order", label = "Order", min = 1, value = 1),
          radioButtons(inputId = "weights_contiguity_style", 
                       label = "Coding scheme style",
                       choices = list("Binary coding" = "B",
                                      "Row standardised" = "W", 
                                      "Globally standardised" = "C"), 
                       selected = "W"
                       ),
          actionButton(inputId = "weights_contiguity_create", label = "Create and use as primary matrix"),
          actionButton(inputId = "weights_contiguity_create_secondary", label = "Create and use as secondary matrix"),
        ),
        argonTab(
          tabName = "Inverse Distance Matrix",
          active = FALSE,
          h3("Inverse Distance Matrix"),
          numericInput(inputId = "weights_inverse_distance_lower_bound", label = "Lower Distance bound (km)", min = 0, value = 0),
          numericInput(inputId = "weights_inverse_distance_upper_bound", label = "Upper Distance bound (km)", min = 1, value = 10000),
          numericInput(inputId = "weights_inverse_distance_power", label = "Power", min = 1, max = 2, value = 1),
          radioButtons(inputId = "weights_inverse_distance_style", 
                       label = "Coding scheme style",
                       choices = list("Binary coding" = "B",
                                      "Row standardised" = "W", 
                                      "Globally standardised" = "C"),
                       selected = "W"
          ),
          actionButton(inputId = "weights_inverse_distance_create", label = "Create and use as primary matrix"),
          actionButton(inputId = "weights_inverse_distance_create_secondary", label = "Create and use as secondary matrix")
        ),
        argonTab(
          tabName = "K-Nearest Neighbors Matrix",
          active = FALSE,
          h3("K-Nearest Neighbors Matrix"),
          numericInput(inputId = "weights_k_nearest_k", label = "k", min = 1, value = 1),
          radioButtons(inputId = "weights_k_nearest_style", 
                       label = "Coding scheme style",
                       choices = list("Binary coding" = "B",
                                      "Row standardised" = "W", 
                                      "Globally standardised" = "C"),
                       selected = "W"
          ),
          actionButton(inputId = "weights_k_nearest_create", label = "Create and use as primary matrix"),
          actionButton(inputId = "weights_k_nearest_create_secondary", label = "Create and use as secondary matrix")
        ),
        argonTab(
          tabName = "Baumont (2004) procedure",
          active = FALSE,
          h3("Baumont (2004) procedure"),
          p("This procedure will regress a model by OLS and test the residuals for spatial autocorrelation (Moran's Test) using several K-Nearest neighbors matrixes, varing k from 1 to the selected maximum k. The matrix with the higher Moran's I will be selected."),
          tags$a(href="https://hal.archives-ouvertes.fr/hal-01525664/document", target="_blank", "Baumont (2004) link."),
          uiOutput("weights_baumont_dependent_variable_UI"),
          uiOutput("weights_baumont_idependent_variable_UI"),
          numericInput(inputId = "weights_baumont_max_k", label = "Max k", min = 5, value = 20),
          radioButtons(inputId = "weights_baumont_style", 
                       label = "Coding scheme style",
                       choices = list("Binary coding" = "B",
                                      "Row standardised" = "W", 
                                      "Globally standardised" = "C"),
                       selected = "W"
          ),
          actionButton(inputId = "weights_baumont_create", label = "Create and use as primary matrix"),
          actionButton(inputId = "weights_baumont_create_secondary", label = "Create and use as secondary matrix")
        ),
        argonTab(
          tabName = "Stakhovych-Bijmolt (2009) adapted procedure",
          p("This adapted procedure will create several spatial weights matrixes (Queen, Rook, Inverse Distance and K-Nearest Neighbors with k = 1, 5, 10, 15 and 20) and estimate several spatial models (SAR, SEM, SAC, SLX, SDM, SDEM) with those matrixes. The final matrix will be selected observing the maximum AIC obtained among all models."),
          tags$a(href="https://doi.org/10.1111/j.1435-5957.2008.00213.x", target="_blank", "Stakhovych-Bijmolt (2009) link."),
          uiOutput("weights_stakhovych_dependent_variable_UI"),
          uiOutput("weights_stakhovych_idependent_variable_UI"),
          radioButtons(inputId = "weights_stakhovych_style", 
                       label = "Coding scheme style",
                       choices = list("Binary coding" = "B",
                                      "Row standardised" = "W", 
                                      "Globally standardised" = "C"),
                       selected = "W"
          ),
          active = FALSE,
          actionButton(inputId = "weights_stakhovych_create", label = "Create and use as primary matrix"),
          actionButton(inputId = "weights_stakhovych_create_secondary", label = "Create and use as secondary matrix")
        )
      )
    )
  ),
  br(),br(),
  argonRow(
    h3("Current spatial matrix configuration"),
    argonColumn(
      width = 12,
      argonTabSet(
        id = "weights-results-tab",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "lg",
        width = 12,
        argonTab(
          tabName = "Primary weights matrix",
          active = TRUE,
          h3("Primary weights matrix"),
          uiOutput("matrix_info1_UI", ),
          uiOutput("matrix_info2_UI", ),
          uiOutput("matrix_info3_UI"),
          uiOutput("matrix_plot_UI")
        ),
        argonTab(
          tabName = "Secondary weights matrix",
          active = TRUE,
          h3("Secondary weights matrix"),
          uiOutput("matrix_secondary_info1_UI", ),
          uiOutput("matrix_secondary_info2_UI", ),
          uiOutput("matrix_secondary_info3_UI"),
          uiOutput("matrix_secondary_plot_UI")
        )
      )
    )
  )
)