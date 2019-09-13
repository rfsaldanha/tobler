weights_tab <- argonTabItem(
  tabName = "weights",
  argonTabSet(
    id = "weights-tab",
    card_wrapper = TRUE,
    horizontal = TRUE,
    circle = FALSE,
    size = "md",
    width = 12,
    #iconList = lapply(X = 1:3, FUN = argonIcon, name = "atom"),
    argonTab(
      tabName = "Contiguity",
      active = TRUE,
      radioButtons(inputId = "weights_contiguity_radio", 
                   label = "Contiguity matrix options",
                   choices = list("Rook" = 1, "Queen" = 2),
                   selected = 1),
      numericInput(inputId = "weights_contiguity_order", label = "Order", min = 1, value = 1),
      radioButtons(inputId = "weights_contiguity_style", 
                   label = "Coding scheme style",
                   choices = list("Row standardised" = "W", "Globally standardised" = "C")),
      actionButton(inputId = "weights_contiguity_create", label = "Create matrix")
    ),
    argonTab(
      tabName = "Inverse Distance",
      active = FALSE,
      numericInput(inputId = "weights_inverse_distance_power", label = "Power", min = 1, max = 2, value = 1),
      radioButtons(inputId = "weights_inverse_distance_style", 
                   label = "Coding scheme style",
                   choices = list("Row standardised" = "W", "Globally standardised" = "C")),
      actionButton(inputId = "weights_inverse_distance_create", label = "Create matrix")
    ),
    argonTab(
      tabName = "K Neighbors",
      active = FALSE,
      "bla3"
    ),
    argonTab(
      tabName = "Automatic procedures",
      active = FALSE,
      "bla3"
    )
  ),
  argonRow(
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "Matrix info",
      argonRow(
        verbatimTextOutput("matrix_info")
      )
    )
  )
)