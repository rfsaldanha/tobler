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
  effects <- input$pmodel_sar_effects
  
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, model = effects, effect = "individual", spatial.error = "none")
})

output$pmodel_sar_summary <- renderPrint({
  summary(pmodel_sar())
})

output$pmodel_sar_impacts <- renderPrint({
  res <- splm:::impacts.splm(pmodel_sar(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
  summary(res, zstats=TRUE, short=TRUE)
})


# SEM model

pmodel_sem <- eventReactive(input$pmodel_sem_estimate, {
  effects <- input$pmodel_sem_effects
  
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = "b")
})

output$pmodel_sem_summary <- renderPrint({
  summary(pmodel_sem())
})

# SAC model

pmodel_sac <- eventReactive(input$pmodel_sac_estimate, {
  effects <- input$pmodel_sac_effects
  
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag=TRUE, model = effects, effect = "individual", spatial.error = "b")
})

output$pmodel_sac_summary <- renderPrint({
  summary(pmodel_sac())
})

output$pmodel_sac_impacts <- renderPrint({
  res <- splm:::impacts.splm(pmodel_sac(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
  summary(res, zstats=TRUE, short=TRUE)
})

# Function to lag independent variables
lag_independent_variables <- function(x){
  lag.listw(x = w_matrix$listw, var = x)
}

# SDM model

pmodel_sdm <- eventReactive(input$pmodel_sdm_estimate, {
  effects <- input$pmodel_sdm_effects
  
  independent_variables <- input$pmodel_independent_variable
  
  lagged_data <- geodata_original()@data %>%
    mutate(across(starts_with(independent_variables), lag_independent_variables)) %>%
    select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
    pivot_longer(
      cols = 2:last_col()
    ) %>%
    mutate(
      variable = as.character(gsub("[[:digit:]]", "", name)),
      time = as.character(gsub("[[:alpha:]]", "", name))
    ) %>%
    select(1, variable, time, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(1, time)
  
  spml(formula(pesp()), data = lagged_data, listw = w_matrix$listw, lag=TRUE, model = effects, effect = "individual", spatial.error = "none")
})

output$pmodel_sdm_summary <- renderPrint({
  summary(pmodel_sdm())
})

output$pmodel_sdm_impacts <- renderPrint({
  res <- splm:::impacts.splm(pmodel_sdm(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
  summary(res, zstats=TRUE, short=TRUE)
})


# SDEM model

pmodel_sdem <- eventReactive(input$pmodel_sdem_estimate, {
  effects <- input$pmodel_sdem_effects
  
  independent_variables <- input$pmodel_independent_variable
  
  lagged_data <- geodata_original()@data %>%
    mutate(across(starts_with(independent_variables), lag_independent_variables)) %>%
    select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
    pivot_longer(
      cols = 2:last_col()
    ) %>%
    mutate(
      variable = as.character(gsub("[[:digit:]]", "", name)),
      time = as.character(gsub("[[:alpha:]]", "", name))
    ) %>%
    select(1, variable, time, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(1, time)
  
  spml(formula(pesp()), data = lagged_data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = "b")
})

output$pmodel_sdem_summary <- renderPrint({
  summary(pmodel_sdem())
})


# SLX model

pmodel_slx <- eventReactive(input$pmodel_slx_estimate, {
  effects <- input$pmodel_slx_effects
  
  independent_variables <- input$pmodel_independent_variable
  
  lagged_data <- geodata_original()@data %>%
    mutate(across(starts_with(independent_variables), lag_independent_variables)) %>%
    select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
    pivot_longer(
      cols = 2:last_col()
    ) %>%
    mutate(
      variable = as.character(gsub("[[:digit:]]", "", name)),
      time = as.character(gsub("[[:alpha:]]", "", name))
    ) %>%
    select(1, variable, time, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(1, time)
  
  plm(formula(pesp()), data = lagged_data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = "none")
})

output$pmodel_slx_summary <- renderPrint({
  summary(pmodel_slx())
})