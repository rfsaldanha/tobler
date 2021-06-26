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

output$pmodel_endog_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  selectInput("pmodel_endog_variable", label = "Additional endogenous variables", choices = variables, multiple = TRUE)
})

output$pmodel_instruments_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  selectInput("pmodel_instruments_variable", label = "External instrument variables", choices = variables, multiple = TRUE)
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

output$pmodel_pesaran_test_download <- downloadHandler(
  
  filename = paste0("tobler_pmodel_pesaran_test_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_pesaran_test_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_pesaran_test_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    params <- list(
      general_observations = input$pmodel_pesaran_test_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      test_summary = pmodel_pesaran_test()
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)





# OLS model

pmodel_ols <- eventReactive(input$pmodel_ols_estimate, {
  show_modal()
  
  plm(formula = formula(pesp()), data = geodata()@data)
})

output$pmodel_ols_summary <- renderPrint({
  summary(pmodel_ols())
})

observeEvent(pmodel_ols(), removeModal())

output$pmodel_ols_download <- downloadHandler(
  
  filename = paste0("tobler_panel_ols_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_ols_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_ols_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_ols_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      model_specification = pesp(),
      model_summary = summary(pmodel_ols())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



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

output$pmodel_sar_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sar_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sar_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sar_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_sar_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_effects = input$pmodel_sar_effects,
      model_summary = summary(pmodel_sar()),
      model_impacts = summary(splm:::impacts.splm(pmodel_sar(), listw = w_matrix$listw, time = length(unique(geodata()@data$time))), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SAR GM model

pmodel_sar_gm <- eventReactive(input$pmodel_sar_gm_estimate, {
  show_modal()
  
  effects <- input$pmodel_sar_gm_effects
  
  if(length(input$pmodel_endog_variable) > 0){
    endog <- paste0(" ~ ", paste0(input$pmodel_endog_variable, collapse = " + "))
  } else (
    endog = NULL
  )
  
  if(length(input$pmodel_instruments_variable) > 0){
    instruments <- paste0(" ~ ", paste0(input$pmodel_instruments_variable, collapse = " + "))
  } else {
    instruments <- NULL
  }
  
  if(effects == "within"){
    spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = FALSE, model = effects, endog = endog, instruments = instruments)
  } else if(effects == "random"){
    spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = FALSE, model = effects, method = "ec2sls", endog = endog, instruments = instruments)
  }
})

output$pmodel_sar_gm_summary <- renderPrint({
  summary(pmodel_sar_gm())
})

output$pmodel_sar_gm_impacts <- renderPrint({
  req(pmodel_sar_gm())
  
  if(length(input$pmodel_endog_variable) == 0 & length(input$pmodel_instruments_variable) == 0){
    res <- splm:::impacts.splm(pmodel_sar_gm(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
    summary(res, zstats=TRUE, short=TRUE)
  } else {
    cat("No impacts estimates when endogenous variables are present in the system.")
  }
})

observeEvent(pmodel_sar_gm(), removeModal())

output$pmodel_sar_gm_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sar_gm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sar_gm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sar_gm_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    if(length(input$pmodel_endog_variable) > 0){
      endog <- paste0(" ~ ", paste0(input$pmodel_endog_variable, collapse = " + "))
    } else (
      endog = "None"
    )
    
    if(length(input$pmodel_instruments_variable) > 0){
      instruments <- paste0(" ~ ", paste0(input$pmodel_instruments_variable, collapse = " + "))
    } else {
      instruments <- "None"
    }
    
    if(length(input$pmodel_endog_variable) == 0 & length(input$pmodel_instruments_variable) == 0){
      res <- splm:::impacts.splm(pmodel_sar_gm(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
      impacts <- summary(res, zstats=TRUE, short=TRUE)
    } else {
      impacts <- "No impacts estimates when endogenous variables are present in the system."
    }
    
    params <- list(
      general_observations = input$pmodel_sar_gm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_endog = endog,
      model_instruments = instruments,
      model_effects = input$pmodel_sar_gm_effects,
      model_summary = summary(pmodel_sar_gm()),
      model_impacts = impacts
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)





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

output$pmodel_sem_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sem_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sem_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sem_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_sem_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_error_type = input$pmodel_sem_error_type,
      model_effects = input$pmodel_sem_effects,
      model_summary = summary(pmodel_sem())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)




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

output$pmodel_sac_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sac_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sac_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sac_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_sac_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_effects = input$pmodel_sac_effects,
      model_error_type = input$pmodel_sac_error_type,
      model_summary = summary(pmodel_sac()),
      model_impacts = summary(splm:::impacts.splm(pmodel_sac(), listw = w_matrix$listw, time = length(unique(geodata()@data$time))), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)






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

output$pmodel_sdem_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sdem_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sdem_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sdem_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_sdem_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_error_type = input$pmodel_sdem_error_type,
      model_effects = input$pmodel_sdem_effects,
      model_summary = summary(pmodel_sdem())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



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

output$pmodel_slx_download <- downloadHandler(
  
  filename = paste0("tobler_panel_slx_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_slx_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_slx_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$pmodel_slx_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_effects = input$pmodel_slx_effects,
      model_summary = summary(pmodel_slx())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)