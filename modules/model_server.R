output$model_dependent_variable_UI <- renderUI({
  variables <- names(geodata_original()@data)
  selectInput("model_dependent_variable", label = "Dependent variable", choices = variables)
})

output$model_independent_variable_UI <- renderUI({
  variables <- names(geodata_original()@data)
  multiInput("model_independent_variable", label = "Independent variables", choices = variables)
})

output$model_endog_variable_UI <- renderUI({
  variables <- names(geodata_original()@data)
  multiInput("model_endog_variable", label = "Additional endogenous variables (optional, for STSLS and GMM estimators)", choices = variables)
})

output$model_instruments_variable_UI <- renderUI({
  variables <- names(geodata_original()@data)
  multiInput("model_instruments_variable", label = "External instrument variables (optional, for STSLS and GMM estimators)", choices = variables)
})

# Model specification
esp <- reactive({
  paste0(as.character(input$model_dependent_variable), " ~ ", paste0(input$model_independent_variable, collapse = " + "))
})

# OLS Model
model_ols <- eventReactive(input$model_estimate_ols, {
  
  show_modal()
  
  lm(formula = formula(esp()), data = geodata_original()@data)
})

output$model_ols_summary <- renderPrint({
  summary(model_ols())
})

output$model_ols_error <- renderPrint({
  lm.morantest(model_ols(), w_matrix$listw)
})

output$model_ols_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_ols())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

output$model_ols_lagrange <- renderPrint({
  lm.LMtests(model = model_ols(), listw = w_matrix$listw,
             test = c("LMerr","RLMerr","LMlag","RLMlag"))
})

observeEvent(model_ols(), removeModal())

output$model_ols_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_ols_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_ols_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_ols_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_ols_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_ols()),
      model_error = lm.morantest(model_ols(), w_matrix$listw),
      model_lagrange = lm.LMtests(model = model_ols(), listw = w_matrix$listw,
                                  test = c("LMerr","RLMerr","LMlag","RLMlag"))
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SAR ML
model_sar_ml <- eventReactive(input$model_estimate_sar_ml, {
  show_modal()
  
  lagsarlm(formula(esp()), data = geodata_original()@data, listw = w_matrix$listw)
})

output$model_sar_ml_summary <- renderPrint({
  summary(model_sar_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sar_ml_impacts <- renderPrint({
  summary(impacts(model_sar_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sar_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sar_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sar_ml(), removeModal())

output$model_sar_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sar_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sar_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sar_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sar_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_sar_ml()),
      model_impacts = summary(impacts(model_sar_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SAR STSLS
model_sar_stsls <- eventReactive(input$model_estimate_sar_stsls, {
  show_modal()
  
  robust_option <- if_else("is_robust" %in% input$model_sar_stsls_options, TRUE, FALSE)
  
  if(length(input$model_endog_variable) > 0){
    endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
  } else (
    endog = NULL
  )
  
  if(length(input$model_instruments_variable) > 0){
    instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
  } else {
    instruments <- NULL
  }
  
  spreg(
    formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw,
    model = "lag", het = robust_option, endog = endog, instruments = instruments
  )
  
})

output$model_sar_stsls_summary <- renderPrint({
  summary(model_sar_stsls())
})

output$model_sar_stsls_impacts <- renderPrint({
  summary(impacts(model_sar_stsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sar_stsls_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sar_stsls())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sar_stsls(), removeModal())

output$model_sar_stsls_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sar_stsls_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sar_stsls_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sar_stsls_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    if(length(input$model_endog_variable) > 0){
      endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
    } else (
      endog = "None"
    )
    
    if(length(input$model_instruments_variable) > 0){
      instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
    } else {
      instruments <- "None"
    }
    
    params <- list(
      general_observations = input$model_sar_stsls_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_endog = endog,
      model_instruments = instruments,
      model_options = input$model_sar_stsls_options,
      model_summary = summary(model_sar_stsls()),
      model_impacts = summary(impacts(model_sar_stsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SEM (ML)
model_sem_ml <- eventReactive(input$model_estimate_sem_ml, {
  show_modal()
  
  errorsarlm(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw)
})

output$model_sem_ml_summary <- renderPrint({
  summary(model_sem_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sem_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sem_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sem_ml(), removeModal())

output$model_sem_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sem_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sem_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sem_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sem_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_sem_ml())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SEM (GMM)

model_sem_gmm <- eventReactive(input$model_estimate_sem_gmm, {
  robust_option <- if_else("is_robust" %in% input$model_sem_stsls_options, TRUE, FALSE)
  
  if(length(input$model_endog_variable) > 0){
    endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
  } else (
    endog = NULL
  )
  
  if(length(input$model_instruments_variable) > 0){
    instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
  } else {
    instruments <- NULL
  }
  
  spreg(
    formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw,
    model = "error", step1.c = TRUE, het = robust_option, endog = endog, instruments = instruments
  )
})

output$model_sem_gmm_summary <- renderPrint({
  summary(model_sem_gmm(), Hausman = TRUE)
})

output$model_sem_gmm_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sem_gmm())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sem_gmm(), removeModal())

output$model_sem_gmm_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sem_gmm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sem_gmm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sem_gmm_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    if(length(input$model_endog_variable) > 0){
      endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
    } else (
      endog = "None"
    )
    
    if(length(input$model_instruments_variable) > 0){
      instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
    } else {
      instruments <- "None"
    }
    
    params <- list(
      general_observations = input$model_sem_gmm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_endog = endog,
      model_instruments = instruments,
      model_summary = summary(model_sem_gmm())
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SAC ML
model_sac_ml <- eventReactive(input$model_estimate_sac_ml, {
  show_modal()
  
  if("use_secondary_w_matrix" %in% input$model_estimate_sac_ml_options){
    sacsarlm(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw, listw2 = w_matrix_secondary$listw) 
  } else {
    sacsarlm(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw) 
  }
  
  
})

output$model_sac_ml_summary <- renderPrint({
  summary(model_sac_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sac_ml_impacts <- renderPrint({
  summary(impacts(model_sac_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sac_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sac_ml(), removeModal())

output$model_sac_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sac_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sac_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sac_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sac_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_options = input$model_sac_ml_options,
      model_summary = summary(model_sac_ml()),
      model_impacts = summary(impacts(model_sac_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

# SAC GSTSLS
model_sac_gstsls <- eventReactive(input$model_estimate_sac_gstsls, {
  show_modal()
  
  robust_option <- if_else("is_robust" %in% input$model_sac_gstsls_options, TRUE, FALSE)
  
  if(length(input$model_endog_variable) > 0){
    endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
  } else (
    endog = NULL
  )
  
  if(length(input$model_instruments_variable) > 0){
    instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
  } else {
    instruments <- NULL
  }
  
  if("use_secondary_w_matrix" %in% input$model_sac_gstsls_options){
    spreg(
      formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw,
      model = "sarar",  listw2 = w_matrix_secondary$listw, step1.c = TRUE, het = robust_option, endog = endog, instruments = instruments
    )
  } else {
    spreg(
      formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw,
      model = "sarar", step1.c = TRUE, het = robust_option, endog = endog, instruments = instruments
    )
  }
  
})

output$model_sac_gstsls_summary <- renderPrint({
  summary(model_sac_gstsls())
})

output$model_sac_gstsls_impacts <- renderPrint({
  summary(impacts(model_sac_gstsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_gstsls_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sac_gstsls())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sac_gstsls(), removeModal())

output$model_sac_gstsls_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sac_gstsls_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sac_gstsls_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sac_gstsls_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    if(length(input$model_endog_variable) > 0){
      endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
    } else (
      endog = "None"
    )
    
    if(length(input$model_instruments_variable) > 0){
      instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
    } else {
      instruments <- "None"
    }
    
    params <- list(
      general_observations = input$model_sac_gstsls_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_endog = endog,
      model_instruments = instruments,
      model_options = input$model_sac_gstsls_options,
      model_summary = summary(model_sac_gstsls()),
      model_impacts = summary(impacts(model_sac_gstsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


# SLX (ML)
model_slx_ml <- eventReactive(input$model_estimate_slx_ml, {
  show_modal()
  
  lmSLX(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw)
})

output$model_slx_ml_summary <- renderPrint({
  summary(model_slx_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_slx_ml_impacts <- renderPrint({
  summary(impacts(model_slx_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_slx_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_slx_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_slx_ml(), removeModal())

output$model_slx_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_slx_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_slx_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_slx_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_slx_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_slx_ml()),
      model_impacts = summary(impacts(model_slx_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)


# SDM (ML)
model_sdm_ml <- eventReactive(input$model_estimate_sdm_ml, {
  show_modal()
  
  lagsarlm(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdm_ml_summary <- renderPrint({
  summary(model_sdm_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdm_ml_impacts <- renderPrint({
  summary(impacts(model_sdm_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sdm_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sdm_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sdm_ml(), removeModal())

output$model_sdm_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sdm_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sdm_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sdm_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sdm_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_sdm_ml()),
      model_impacts = summary(impacts(model_sdm_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# SDM (GMM)
model_sdm_gmm <- eventReactive(input$model_estimate_sdm_gmm, {
  show_modal()
  
  robust_option <- if_else("is_robust" %in% input$model_sdm_gmm_options, TRUE, FALSE)
  
  if(length(input$model_endog_variable) > 0){
    endog <- paste0(" ~ ", paste0(input$model_endog_variable, collapse = " + "))
  } else (
    endog = NULL
  )
  
  if(length(input$model_instruments_variable) > 0){
    instruments <- paste0(" ~ ", paste0(input$model_instruments_variable, collapse = " + "))
  } else {
    instruments <- NULL
  }
  
  spreg(
    formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw,
    model = "lag", Durbin = TRUE, het = robust_option, endog = endog, instruments = instruments
  )
})

output$model_sdm_gmm_summary <- renderPrint({
  summary(model_sdm_gmm(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdm_gmm_impacts <- renderPrint({
  summary(impacts(model_sdm_gmm(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sdm_gmm_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sdm_gmm())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sdm_gmm(), removeModal())

output$model_sdm_gmm_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sdm_gmm_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sdm_gmm_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sdm_gmm_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sdm_gmm_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_sdm_gmm()),
      model_impacts = summary(impacts(model_sdm_gmm(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)



# SDEM (ML)
model_sdem_ml <- eventReactive(input$model_estimate_sdem_ml, {
  show_modal()
  
  errorsarlm(formula = formula(esp()), data = geodata_original()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdem_ml_summary <- renderPrint({
  summary(model_sdem_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdem_ml_impacts <- renderPrint({
  summary(impacts(model_sdem_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sdem_ml_map <- renderLeaflet({
  geodata_res <- geodata_original()
  geodata_res@data$residuals <- resid(model_sdem_ml())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sdem_ml(), removeModal())


output$model_sdem_ml_download <- downloadHandler(
  
  filename = paste0("tobler_cross_section_sdem_ml_model_report_", format(Sys.time(), "%Y.%m.%d_%H.%M.%S"), ".pdf"),
  content = function(file) {
    
    tempDir <- tempdir()
    tempReport <- file.path(tempDir, "model_sdem_ml_report.Rmd")
    tempLogo <- file.path(tempDir, "tobleR.png")
    file.copy("reports_rmd/model_sdem_ml_report.Rmd", tempReport, overwrite = TRUE)
    file.copy("www/tobleR.png", tempLogo, overwrite = TRUE)
    
    
    params <- list(
      general_observations = input$model_sdem_ml_general_observations,
      data_file = input$data_file[1],
      data_type = input$data_type,
      original_data = geodata_original()@data,
      spatial_weights_matrix = w_matrix$name,
      model_specification = esp(),
      model_summary = summary(model_sdem_ml()),
      model_impacts = summary(impacts(model_sdem_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
    )
    
    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)

