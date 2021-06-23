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

# Error type options for SEM

output$pmodel_sem_error_type_UI <- renderUI({
  if(input$pmodel_sem_effects == "random"){
    radioButtons(
      inputId = "pmodel_sem_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b", "Kapoor, Kelejian and Prucha" = "kkp"), 
      selected = "b"
    )
  } else {
    radioButtons(
      inputId = "pmodel_sem_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b"), 
      selected = "b"
    )
  }
})

output$pmodel_sac_error_type_UI <- renderUI({
  if(input$pmodel_sac_effects == "random"){
    radioButtons(
      inputId = "pmodel_sac_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b", "Kapoor, Kelejian and Prucha" = "kkp"), 
      selected = "b"
    )
  } else {
    radioButtons(
      inputId = "pmodel_sac_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b"), 
      selected = "b"
    )
  }
})

output$pmodel_sdem_error_type_UI <- renderUI({
  if(input$pmodel_sdem_effects == "random"){
    radioButtons(
      inputId = "pmodel_sdem_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b", "Kapoor, Kelejian and Prucha" = "kkp"), 
      selected = "b"
    )
  } else {
    radioButtons(
      inputId = "pmodel_sdem_error_type", 
      label = h3("Error type"),
      choices = list("Baltagi" = "b"), 
      selected = "b"
    )
  }
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

output$pmodel_hausman_test_download <- downloadHandler(
  
  filename = paste0("tobler_pmodel_hausman_test_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_hausman_test_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_hausman_test_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    params <- list(
      general_observations = input$pmodel_hausman_test_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      test_summary = pmodel_hausman_test()
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# Pesaran test

pmodel_pesaran_test <- eventReactive(input$pmodel_pesaran_test_execute, {
  pcdtest(formula(pesp()), data = geodata()@data)
})

output$pmodel_pesaran_test_results <- renderPrint({
  print(pmodel_pesaran_test())
})




# OLS model

pmodel_ols <- eventReactive(input$pmodel_ols_estimate, {
  show_modal()
  
  plm(formula = formula(pesp()), data = geodata()@data)
})

output$pmodel_ols_summary <- renderPrint({
  summary(pmodel_ols())
})

observeEvent(pmodel_ols(), removeModal())

# SAR model

pmodel_sar <- eventReactive(input$pmodel_sar_estimate, {
  show_modal()
  
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

observeEvent(pmodel_sar(), removeModal())

# SEM model

pmodel_sem <- eventReactive(input$pmodel_sem_estimate, {
  show_modal()
  
  effects <- input$pmodel_sem_effects
  error_type <- input$pmodel_sem_error_type
  
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = error_type)
})

output$pmodel_sem_summary <- renderPrint({
  summary(pmodel_sem())
})

observeEvent(pmodel_sem(), removeModal())

# SAC model

pmodel_sac <- eventReactive(input$pmodel_sac_estimate, {
  show_modal()
  
  effects <- input$pmodel_sac_effects
  error_type <- input$pmodel_sac_error_type
  
  spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag=TRUE, model = effects, effect = "individual", spatial.error = error_type)
})

output$pmodel_sac_summary <- renderPrint({
  summary(pmodel_sac())
})

output$pmodel_sac_impacts <- renderPrint({
  res <- splm:::impacts.splm(pmodel_sac(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
  summary(res, zstats=TRUE, short=TRUE)
})

observeEvent(pmodel_sac(), removeModal())

# Function to lag independent variables
lag_independent_variables <- function(x){
  lag.listw(x = w_matrix$listw, var = x)
}

# SDEM model

pmodel_sdem <- eventReactive(input$pmodel_sdem_estimate, {
  show_modal()
  
  effects <- input$pmodel_sdem_effects
  error_type <- input$pmodel_sdem_error_type
  
  independent_variables <- input$pmodel_independent_variable
  
  lagged_term <- geodata_original()@data %>%
    select(starts_with(independent_variables)) %>%
    rename(setNames(names(.), paste0('W_', names(.)))) %>%
    mutate(
      across(everything(), lag_independent_variables)
    )
  
  lagged_data <- geodata_original()@data %>%
    select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
    bind_cols(lagged_term) %>%
    pivot_longer(
      cols = 2:last_col()
    ) %>%
    mutate(
      variable = as.character(gsub("[[:digit:]]", "", name)),
      time = as.character(gsub("[[:alpha:],_]", "", name))
    ) %>%
    select(1, variable, time, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(1, time)
  
  esp <- paste0(pesp(), " + ", paste0("W_", input$pmodel_independent_variable, collapse = " + "))
  
  spml(formula(esp), data = lagged_data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = error_type)
})

output$pmodel_sdem_summary <- renderPrint({
  summary(pmodel_sdem())
})

observeEvent(pmodel_sdem(), removeModal())

# SLX model

pmodel_slx <- eventReactive(input$pmodel_slx_estimate, {
  show_modal()
  
  effects <- input$pmodel_slx_effects
  
  independent_variables <- input$pmodel_independent_variable
  
  lagged_term <- geodata_original()@data %>%
    select(starts_with(independent_variables)) %>%
    rename(setNames(names(.), paste0('W_', names(.)))) %>%
    mutate(
      across(everything(), lag_independent_variables)
    )
  
  lagged_data <- geodata_original()@data %>%
    select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
    bind_cols(lagged_term) %>%
    pivot_longer(
      cols = 2:last_col()
    ) %>%
    mutate(
      variable = as.character(gsub("[[:digit:]]", "", name)),
      time = as.character(gsub("[[:alpha:],_]", "", name))
    ) %>%
    select(1, variable, time, value) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(1, time)
  
  esp <- paste0(pesp(), " + ", paste0("W_", input$pmodel_independent_variable, collapse = " + "))
  
  plm(formula(esp), data = lagged_data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = "none")
})

output$pmodel_slx_summary <- renderPrint({
  summary(pmodel_slx())
})

observeEvent(pmodel_slx(), removeModal())