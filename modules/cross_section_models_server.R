output$cross_section_model_dependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("cross_section_model_dependent_variable", label = "Dependent variable", choices = variables)
})

output$cross_section_model_independent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("cross_section_model_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

output$cross_section_model_sar_consistent_UI <- renderUI({
  if(input$sar_estimator == "ml"){
    NULL
  } else {
    radioButtons(
      "sar_consistent",
      label = h4("Spatial heteroskedasticity and autocorrelation consistent (HAC) estimator"),
      choices = list("HAC applied" = "hac", "HAC not applied" = "not_hac"), 
      selected = "not_hac"
    )
  }
})

output$cross_section_model_sar_variance_UI <- renderUI({
  if(input$sar_estimator == "ml"){
    NULL
  } else {
    radioButtons(
      "sar_variance",
      label = h4("Variance"),
      choices = list("Homoskedasticity" = "homo", "Heteroskedasticity (robust)" = "hete"), 
      selected = "homo"
    )
  }
})

output$cross_section_model_endogenous_variable_UI <- renderUI({
  variables <- names(geodata()@data)

  if(input$sar_estimator == "ml"){
    NULL
  } else {
    selectInput("cross_section_model_endogenous_variable", label = "Endogenous variables (optional)", choices = variables, multiple = TRUE)
  }
})

output$cross_section_model_instrument_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  
  if(input$sar_estimator == "ml"){
    NULL
  } else {
    selectInput("cross_section_model_instrument_variable", label = "Instrument variables (optional)", choices = variables, multiple = TRUE)
  }
})

# Model specification
cross_section_model_esp <- reactive({
  paste0(as.character(input$cross_section_model_dependent_variable), " ~ ", paste0(input$cross_section_model_independent_variable, collapse = " + "))
})

# Endogenous variables
cross_section_model_endogenous <- reactive({
  paste0(" ~ ", paste0(input$cross_section_model_endogenous_variable, collapse = " + "))
})

# Instrument variables
cross_section_model_instrument <- reactive({
  paste0(" ~ ", paste0(input$cross_section_model_instrument_variable, collapse = " + "))
})

# SAR Model
model_sar <- eventReactive(input$cross_section_model_estimate_sar, {
  show_modal()
  
  if(input$sar_estimator == "ml"){
    lagsarlm(formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw)
  } else {
    endogenous <- if(cross_section_model_endogenous() == " ~ "){
      NULL
    } else {
      cross_section_model_endogenous()
    }
    instrument <- if(cross_section_model_instrument() == " ~ "){
      NULL
    } else {
      cross_section_model_instrument()
    }
    hac <- ifelse(input$sar_consistent == "hac", TRUE, FALSE)
    het <- ifelse(input$sar_variance == "hete", TRUE, FALSE)
    model <- ifelse(input$sar_consistent == "hac", "ivhac", "lag")
    
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      endog = endogenous,
      instruments = instrument,
      HAC = hac,
      het = het,
      model = model
    )
  }
})

observeEvent(model_sar(), removeModal())

output$model_sar_summary <- renderPrint({
  summary(model_sar())
})


