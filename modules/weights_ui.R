weights_tab <- argonTabItem(
  tabName = "weights",
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
          radioButtons(inputId = "weights_contiguity_radio", 
                       label = "Contiguity matrix options",
                       choices = list("Rook" = 1, "Queen" = 2),
                       selected = 1),
          numericInput(inputId = "weights_contiguity_order", label = "Order", min = 1, value = 1),
          radioButtons(inputId = "weights_contiguity_style", 
                       label = "Coding scheme style",
                       choices = list("Row standardised" = "W", "Globally standardised" = "C")),
          actionButton(inputId = "weights_contiguity_create", label = "Create matrix"),
          uiOutput("teste")
        ),
        argonTab(
          tabName = "Inverse Distance Matrix",
          active = FALSE,
          numericInput(inputId = "weights_inverse_distance_lower_bound", label = "Lower Distance bound (km)", min = 0, value = 0),
          numericInput(inputId = "weights_inverse_distance_upper_bound", label = "Upper Distance bound (km)", min = 1, value = 10000),
          numericInput(inputId = "weights_inverse_distance_power", label = "Power", min = 1, max = 2, value = 1),
          radioButtons(inputId = "weights_inverse_distance_style", 
                       label = "Coding scheme style",
                       choices = list("Row standardised" = "W", "Globally standardised" = "C")),
          actionButton(inputId = "weights_inverse_distance_create", label = "Create matrix")
        ),
        argonTab(
          tabName = "K-Nearest Neighbors Matrix",
          active = FALSE,
          numericInput(inputId = "weights_k_nearest_k", label = "k", min = 1, value = 1),
          radioButtons(inputId = "weights_k_nearest_style", 
                       label = "Coding scheme style",
                       choices = list("Row standardised" = "W", "Globally standardised" = "C")),
          actionButton(inputId = "weights_k_nearest_create", label = "Create matrix")
        ),
        argonTab(
          tabName = "Baumont (2004) procedure",
          active = FALSE,
          uiOutput("weights_baumont_dependent_variable_UI"),
          uiOutput("weights_baumont_idependent_variable_UI"),
          numericInput(inputId = "weights_baumont_max_k", label = "Max k", min = 5, value = 20),
          radioButtons(inputId = "weights_baumont_style", 
                       label = "Coding scheme style",
                       choices = list("Row standardised" = "W", "Globally standardised" = "C")),
          actionButton(inputId = "weights_baumont_create", label = "Create matrix")
        ),
        argonTab(
          tabName = "Stakhovych-Bijmolt(2009) adapted procedure",
          active = FALSE,
          "bla4"
        )
      )
    )
  ),
  argonRow(
    argonColumn(
      width = 12,
      uiOutput("matrix_info_UI", ),
      uiOutput("matrix_info2_UI"),
      uiOutput("matrix_plot_UI")
    )
  )
)