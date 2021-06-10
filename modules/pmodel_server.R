output$pmodel_dependent_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  selectInput("pmodel_dependent_variable", label = "Dependent variable", choices = variables)
})

output$pmodel_independent_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  selectInput("pmodel_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

# Panel model specification
pesp <- reactive({
  paste0(as.character(input$pmodel_dependent_variable), " ~ ", paste0(input$pmodel_independent_variable, collapse = " + "))
})

# Hausman Test

pmodel_hausman_test <- eventReactive(input$pmodel_hausman_test_execute, {
  # Fixed effects panel (non spatial)
  fe <- plm(formula = formula(pesp()), data = geodata()@data)
  
  # Random effects panel (non spatial)
  re <- plm(formula = formula(pesp()), data = geodata()@data, model="random")
  
  # Hausman test
  phtest(fe, re)
})

output$pmodel_hausman_test_results <- renderPrint({
  print(pmodel_hausman_test())
})

# Pesaran test

pmodel_pesaran_test <- eventReactive(input$pmodel_pesaran_test_execute, {
  pcdtest(formula(pesp()), data = geodata()@data)
})

output$pmodel_pesaran_test_results <- renderPrint({
  print(pmodel_pesaran_test())
})




# OLS model

pmodel_ols <- eventReactive(input$pmodel_ols_estimate, {
  plm(formula = formula(pesp()), data = geodata()@data)
})

output$pmodel_ols_summary <- renderPrint({
  summary(pmodel_ols())
})

# SAR model

pmodel_sar <- eventReactive(input$pmodel_sar_estimate, {
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, model = "within", effect = "individual", spatial.error = "none")
})

output$pmodel_sar_summary <- renderPrint({
  summary(pmodel_sar())
})

output$pmodel_sar_impacts <- renderPrint({
  res <- splm:::impacts.splm(pmodel_sar(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
  summary(res, zstats=TRUE, short=TRUE)
})
