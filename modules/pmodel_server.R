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
  
  multiInput("pmodel_independent_variable", label = "Independent variables", choices = variables)
})

output$pmodel_endog_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  multiInput("pmodel_endog_variable", label = "Additional endogenous variables (optional, for GM estimator)", choices = variables)
})

output$pmodel_instruments_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(3:last_col()) %>%
    names()
  
  multiInput("pmodel_instruments_variable", label = "External instrument variables (optional, for GM estimator)", choices = variables)
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
  tryCatch({
    # Fixed effects panel (non spatial)
    fe <- plm(formula = formula(pesp()), data = geodata()@data)
    
    # Random effects panel (non spatial)
    re <- plm(formula = formula(pesp()), data = geodata()@data, model="random")
    
    # Hausman test
    phtest(fe, re)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
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


# Spatial Hausman Test
pmodel_hausman_spatial_test <- eventReactive(input$pmodel_hausman_spatial_test_execute, {
  tryCatch({
    error_type <- input$pmodel_hausman_spatial_test_error_type
    
    sar_random <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                       lag = TRUE, model = "random", effect = "individual", spatial.error = "none")
    
    sar_fixed <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                      lag = TRUE, model = "within", effect = "individual", spatial.error = "none")
    
    sem_random <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                       lag = FALSE, model = "random", effect = "individual", spatial.error = error_type)
    
    sem_fixed <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                      lag = FALSE, model = "within", effect = "individual", spatial.error = error_type)
    
    sac_random <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                       lag = TRUE, model = "random", effect = "individual", spatial.error = error_type)
    
    sac_fixed <- spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw,
                      lag = TRUE, model = "within", effect = "individual", spatial.error = error_type)
    
    test_sar <- sphtest(sar_random, sar_fixed)
    test_sem <- sphtest(sem_random, sem_fixed)
    test_sac <- sphtest(sac_random, sac_fixed)
    
    res <- cbind(
      c(test_sar$statistic, test_sar$p.value),
      c(test_sem$statistic, test_sem$p.value),
      c(test_sac$statistic, test_sac$p.value)
    )
    
    dimnames(res) <- list(c("test", "p-value"), c("SAR","SEM","SAC"))
    round(x = res, digits = 5)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
})

output$pmodel_hausman_spatial_test_results <- renderPrint({
  print(pmodel_hausman_spatial_test())
})

output$pmodel_hausman_spatial_test_download <- downloadHandler(
  
  filename = paste0("tobler_pmodel_hausman_spatial_test_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_hausman_spatial_test_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_hausman_spatial_test_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    params <- list(
      general_observations = input$pmodel_hausman_spatial_test_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      test_error_type = input$pmodel_hausman_spatial_test_error_type,
      test_summary = pmodel_hausman_spatial_test()
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# Pesaran test
pmodel_pesaran_test <- eventReactive(input$pmodel_pesaran_test_execute, {
  tryCatch({
    test_type <- input$pmodel_pesaran_test_type
    
    if(test_type == "Global"){
      res_pooling <- pcdtest(formula(pesp()), data = geodata()@data, test = "cd", model = "pooling")
      res_within <- pcdtest(formula(pesp()), data = geodata()@data, test = "cd", model = "within")
      res_random <- pcdtest(formula(pesp()), data = geodata()@data, test = "cd", model = "random")
    } else if(test_type == "Local"){
      res_pooling <- pcdtest(formula(pesp()), data = geodata()@data, w = listw2mat(w_matrix$listw), test = "cd", model = "pooling")
      res_within <- pcdtest(formula(pesp()), data = geodata()@data, w = listw2mat(w_matrix$listw), test = "cd", model = "within")
      res_random <- pcdtest(formula(pesp()), data = geodata()@data, w = listw2mat(w_matrix$listw), test = "cd", model = "random")
    }
    
    res <- cbind(
      c(res_pooling$statistic, res_pooling$p.value),
      c(res_within$statistic, res_within$p.value),
      c(res_random$statistic, res_random$p.value)
    )
    
    dimnames(res) <- list(c("test", "p-value"), c("Pooled","Fixed","Random"))
    round(x = res, digits = 5)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
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
      test_type = input$pmodel_pesaran_test_type,
      test_summary = pmodel_pesaran_test()
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)





# BSK test
pmodel_bsk_test <- eventReactive(input$pmodel_bsk_test_execute, {
  tryCatch({
    res_lmh <- bsktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "LMH")
    res_lm1 <- bsktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "LM1")
    res_lm2 <- bsktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "LM2")
    res_clm_mu <- bsktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "CLMmu")
    res_clm_lambda <- bsktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "CLMlambda")
    
    res <- cbind(
      c(res_lmh$statistic, res_lmh$p.value),
      c(res_lm1$statistic, res_lm1$p.value),
      c(res_lm2$statistic, res_lm2$p.value),
      c(res_clm_mu$statistic, res_clm_mu$p.value),
      c(res_clm_lambda$statistic, res_clm_lambda$p.value)
    )
    
    dimnames(res) <- list(c("test", "p-value"), c("LM joint","LM mu","LM lambda", "CLM mu", "CLM lambda"))
    round(x = res, digits = 5)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
})

output$pmodel_bsk_test_results <- renderPrint({
  print(pmodel_bsk_test())
})

output$pmodel_bsk_test_download <- downloadHandler(
  
  filename = paste0("tobler_pmodel_bsk_test_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_bsk_test_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_bsk_test_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    params <- list(
      general_observations = input$pmodel_bsk_test_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      test_summary = pmodel_bsk_test()
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# BSJK test
pmodel_bsjk_test <- eventReactive(input$pmodel_bsjk_test_execute, {
  tryCatch({
    res_c1 <- bsjktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "C.1")
    res_c2 <- bsjktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "C.2")
    res_c3 <- bsjktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "C.3")
    res_j <- bsjktest(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, test = "J")
    
    res <- cbind(
      c(res_c1$statistic, res_c1$p.value),
      c(res_c2$statistic, res_c2$p.value),
      c(res_c3$statistic, res_c3$p.value),
      c(res_j$statistic, res_j$p.value)
    )
    
    dimnames(res) <- list(c("test", "p-value"), c("C.1","C.2","C.3", "J"))
    round(x = res, digits = 5)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
})

output$pmodel_bsjk_test_results <- renderPrint({
  print(pmodel_bsjk_test())
})

output$pmodel_bsjk_test_download <- downloadHandler(
  
  filename = paste0("tobler_pmodel_bsjk_test_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_bsjk_test_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_bsjk_test_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    params <- list(
      general_observations = input$pmodel_bsjk_test_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      test_summary = pmodel_bsjk_test()
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
  
  effects <- input$pmodel_ols_effects
  
  tryCatch({
    plm(formula = formula(pesp()), data = geodata()@data, model = effects)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
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
      model_effects = input$pmodel_ols_effects,
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
  
  tryCatch({
    spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, model = effects, effect = "individual", spatial.error = "none")
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
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
  lag_instruments <- if_else("lag_instruments" %in% input$pmodel_sar_gm_options, TRUE, FALSE)
  
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
    tryCatch({
      spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = FALSE, model = effects, moments = "weights", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
    },
    warning = function(warn){
      showNotification(paste0(warn), type = "warning", duration = NULL)
    },
    error = function(err){
      showNotification(paste0(err), type = "err", duration = NULL)
    })
  } else if(effects == "random"){
    tryCatch({
      spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = FALSE, model = effects, moments = "weights", method = "ec2sls", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
    },
    warning = function(warn){
      showNotification(paste0(warn), type = "warning", duration = NULL)
    },
    error = function(err){
      showNotification(paste0(err), type = "err", duration = NULL)
    })
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
      model_options = input$pmodel_sar_gm_options,
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
  
  tryCatch({
    spml(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = error_type)
  },
  warning = function(warn){
    showNotification(paste0(warn), type = "warning", duration = NULL)
  },
  error = function(err){
    showNotification(paste0(err), type = "err", duration = NULL)
  })
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


# SEM GM model

pmodel_sem_gm <- eventReactive(input$pmodel_sem_gm_estimate, {
  show_modal()
  
  effects <- input$pmodel_sem_gm_effects
  lag_instruments <- if_else("lag_instruments" %in% input$pmodel_sem_gm_options, TRUE, FALSE)
  
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
    tryCatch({
      spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = FALSE, spatial.error = TRUE, model = effects, moments = "weights", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
    },
    warning = function(warn){
      showNotification(paste0(warn), type = "warning", duration = NULL)
    },
    error = function(err){
      showNotification(paste0(err), type = "err", duration = NULL)
    })
  } else if(effects == "random"){
    tryCatch({
      spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = FALSE, spatial.error = TRUE, model = effects, moments = "weights", method = "ec2sls", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
    },
    warning = function(warn){
      showNotification(paste0(warn), type = "warning", duration = NULL)
    },
    error = function(err){
      showNotification(paste0(err), type = "err", duration = NULL)
    })
  }
})

output$pmodel_sem_gm_summary <- renderPrint({
  summary(pmodel_sem_gm())
})

observeEvent(pmodel_sem_gm(), removeModal())

output$pmodel_sem_gm_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sem_gm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sem_gm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sem_gm_report.Rmd", tempReport, overwrite = TRUE)
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
    
    params <- list(
      general_observations = input$pmodel_sem_gm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_endog = endog,
      model_instruments = instruments,
      model_effects = input$pmodel_sem_gm_effects,
      model_options = input$pmodel_sem_gm_options,
      model_summary = summary(pmodel_sem_gm())
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


# SAC GM model

pmodel_sac_gm <- eventReactive(input$pmodel_sac_gm_estimate, {
  show_modal()
  
  effects <- input$pmodel_sac_gm_effects
  lag_instruments <- if_else("lag_instruments" %in% input$pmodel_sac_gm_options, TRUE, FALSE)
  
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
    spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = TRUE, model = effects, moments = "weights", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
  } else if(effects == "random"){
    spgm(formula(pesp()), data = geodata()@data, listw = w_matrix$listw, lag = TRUE, spatial.error = TRUE, model = effects, moments = "weights", method = "ec2sls", endog = endog, instruments = instruments, lag.instruments = lag_instruments)
  }
})

output$pmodel_sac_gm_summary <- renderPrint({
  summary(pmodel_sac_gm())
})

output$pmodel_sac_gm_impacts <- renderPrint({
  req(pmodel_sac_gm())
  
  if(length(input$pmodel_endog_variable) == 0 & length(input$pmodel_instruments_variable) == 0){
    res <- splm:::impacts.splm(pmodel_sac_gm(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
    summary(res, zstats=TRUE, short=TRUE)
  } else {
    cat("No impacts estimates when endogenous variables are present in the system.")
  }
})

observeEvent(pmodel_sac_gm(), removeModal())

output$pmodel_sac_gm_download <- downloadHandler(
  
  filename = paste0("tobler_panel_sac_gm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_sac_gm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_sac_gm_report.Rmd", tempReport, overwrite = TRUE)
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
      res <- splm:::impacts.splm(pmodel_sac_gm(), listw = w_matrix$listw, time = length(unique(geodata()@data$time)))
      impacts <- summary(res, zstats=TRUE, short=TRUE)
    } else {
      impacts <- "No impacts estimates when endogenous variables are present in the system."
    }
    
    params <- list(
      general_observations = input$pmodel_sac_gm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_endog = endog,
      model_instruments = instruments,
      model_effects = input$pmodel_sac_gm_effects,
      model_options = input$pmodel_sar_gm_options,
      model_summary = summary(pmodel_sac_gm()),
      model_impacts = impacts
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

output$pmodel_sdem_durbin_var_UI <- renderUI({
  req(input$pmodel_independent_variable)
  multiInput("pmodel_sdem_durbin_var", label = "Select explanatory variables to lag (leave empty for all)", choices = input$pmodel_independent_variable)
})

pmodel_sdem <- eventReactive(input$pmodel_sdem_estimate, {
  show_modal()
  
  effects <- input$pmodel_sdem_effects
  error_type <- input$pmodel_sdem_error_type
  
  id_variable <- input$pdata_id_variable
  other_variables <- input$pdata_variables
  
  dependent_variables <- input$pmodel_dependent_variable
  dependent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(dependent_variables, collapse = "|"))]
  
  independent_variables <- input$pmodel_independent_variable
  independent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(independent_variables, collapse = "|"))]
  
  if(length(input$pmodel_sdem_durbin_var) > 0){
    durbin_variables <- input$pmodel_sdem_durbin_var
  } else {
    durbin_variables <- independent_variables
  }
  durbin_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(durbin_variables, collapse = "|"))]
  
  lagged_term <- geodata_original()@data %>%
    select(all_of(durbin_full_name)) %>%
    rename(setNames(names(.), paste0('W_', names(.)))) %>%
    mutate(
      across(everything(), lag_independent_variables)
    )
  
  lagged_data <- geodata_original()@data %>%
    select(all_of(id_variable), all_of(dependent_variables_full_name), all_of(independent_variables_full_name)) %>%
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
  
  esp <- paste0(pesp(), " + ", paste0("W_", durbin_variables, collapse = " + "))
  
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
    
    if(length(input$pmodel_sdem_durbin_var) > 0){
      durbin_var <- paste0(" ~ ", paste0(input$pmodel_sdem_durbin_var, collapse = " + "))
    } else {
      durbin_var = "All"
    }
    
    params <- list(
      general_observations = input$pmodel_sdem_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_durbin_var = durbin_var,
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

output$pmodel_slx_durbin_var_UI <- renderUI({
  req(input$pmodel_independent_variable)
  multiInput("pmodel_slx_durbin_var", label = "Select explanatory variables to lag (leave empty for all)", choices = input$pmodel_independent_variable)
})

pmodel_slx <- eventReactive(input$pmodel_slx_estimate, {
  show_modal()
  
  effects <- input$pmodel_slx_effects
  
  id_variable <- input$pdata_id_variable
  other_variables <- input$pdata_variables
  
  dependent_variables <- input$pmodel_dependent_variable
  dependent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(dependent_variables, collapse = "|"))]
  
  independent_variables <- input$pmodel_independent_variable
  independent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(independent_variables, collapse = "|"))]
  
  if(length(input$pmodel_slx_durbin_var) > 0){
    durbin_variables <- input$pmodel_slx_durbin_var
  } else {
    durbin_variables <- independent_variables
  }
  durbin_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(durbin_variables, collapse = "|"))]
  
  lagged_term <- geodata_original()@data %>%
    select(all_of(durbin_full_name)) %>%
    rename(setNames(names(.), paste0('W_', names(.)))) %>%
    mutate(
      across(everything(), lag_independent_variables)
    )
  
  lagged_data <- geodata_original()@data %>%
    select(all_of(id_variable), all_of(dependent_variables_full_name), all_of(independent_variables_full_name)) %>%
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
  
  esp <- paste0(pesp(), " + ", paste0("W_", durbin_variables, collapse = " + "))
  
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
    
    if(length(input$pmodel_slx_durbin_var) > 0){
      durbin_var <- paste0(" ~ ", paste0(input$pmodel_slx_durbin_var, collapse = " + "))
    } else {
      durbin_var = "All"
    }
    
    params <- list(
      general_observations = input$pmodel_slx_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_durbin_var = durbin_var,
      model_effects = input$pmodel_slx_effects,
      model_summary = summary(pmodel_slx())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# SLX GM model

output$pmodel_slx_gm_durbin_var_UI <- renderUI({
  req(input$pmodel_independent_variable)
  multiInput("pmodel_slx_gm_durbin_var", label = "Select explanatory variables to lag (leave empty for all)", choices = input$pmodel_independent_variable)
})

pmodel_slx_gm <- eventReactive(input$pmodel_slx_gm_estimate, {
  show_modal()
  
  effects <- input$pmodel_slx_gm_effects
  lag_instruments <- if_else("lag_instruments" %in% input$pmodel_slx_gm_options, TRUE, FALSE)
  
  id_variable <- input$pdata_id_variable
  other_variables <- input$pdata_variables
  
  dependent_variables <- input$pmodel_dependent_variable
  dependent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(dependent_variables, collapse = "|"))]
  
  independent_variables <- input$pmodel_independent_variable
  independent_variables_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(independent_variables, collapse = "|"))]
  
  if(length(input$pmodel_slx_gm_durbin_var) > 0){
    durbin_variables <- input$pmodel_slx_gm_durbin_var
  } else {
    durbin_variables <- independent_variables
  }
  durbin_full_name <- other_variables[str_starts(string = other_variables, pattern = paste(durbin_variables, collapse = "|"))]
  
  lagged_term <- geodata_original()@data %>%
    select(all_of(durbin_full_name)) %>%
    rename(setNames(names(.), paste0('W_', names(.)))) %>%
    mutate(
      across(everything(), lag_independent_variables)
    )
  
  lagged_data <- geodata_original()@data %>%
    select(all_of(id_variable), all_of(dependent_variables_full_name), all_of(independent_variables_full_name)) %>%
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
  
  esp <- paste0(pesp(), " + ", paste0("W_", durbin_variables, collapse = " + "))
  
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
  
  if(is.null(endog) | is.null(instruments)){
    plm(formula(esp), data = lagged_data, listw = w_matrix$listw, lag=FALSE, model = effects, effect = "individual", spatial.error = "none")
  } else {
    spgm(formula(esp), data = lagged_data, listw = w_matrix$listw, lag = FALSE, spatial.error = FALSE, model = effects, endog = endog, instruments = instruments, lag.instruments = lag_instruments)
  }
  
  
  
  
})

output$pmodel_slx_gm_summary <- renderPrint({
  summary(pmodel_slx_gm())
})

observeEvent(pmodel_slx_gm(), removeModal())

output$pmodel_slx_gm_download <- downloadHandler(
  
  filename = paste0("tobler_panel_slx_gm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "pmodel_slx_gm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/pmodel_slx_gm_report.Rmd", tempReport, overwrite = TRUE)
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
    
    if(length(input$pmodel_slx_gm_durbin_var) > 0){
      durbin_var <- paste0(" ~ ", paste0(input$pmodel_slx_gm_durbin_var, collapse = " + "))
    } else {
      durbin_var = "All"
    }
    
    params <- list(
      general_observations = input$pmodel_slx_gm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      spatial_weights_matrix = w_matrix$name,
      model_specification = pesp(),
      model_durbin_var = durbin_var,
      model_endog = endog,
      model_instruments = instruments,
      model_effects = input$pmodel_slx_gm_effects,
      model_options = input$pmodel_slx_gm_options,
      model_summary = summary(pmodel_slx_gm())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)