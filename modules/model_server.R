output$model_dependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("model_dependent_variable", label = "Dependent variable", choices = variables)
})

output$model_independent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("model_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

# Model specification
esp <- reactive({
  paste0(as.character(input$model_dependent_variable), " ~ ", paste0(input$model_independent_variable, collapse = " + "))
})

# OLS Model
model_ols <- eventReactive(input$model_estimate_ols, {
  
  show_modal()
  
  lm(formula = formula(esp()), data = geodata()@data)
})

output$model_ols_summary <- renderPrint({
  summary(model_ols())
})

output$model_ols_error <- renderPrint({
  lm.morantest(model_ols(), w_matrix$listw)
})

output$model_ols_map <- renderLeaflet({
  geodata_res <- geodata()
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


# SAR ML
model_sar_ml <- eventReactive(input$model_estimate_sar_ml, {
  show_modal()
  
  lagsarlm(formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sar_ml_summary <- renderPrint({
  summary(model_sar_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sar_ml_impacts <- renderPrint({
  summary(impacts(model_sar_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sar_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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

# SAR STSLS
model_sar_stsls <- eventReactive(input$model_estimate_sar_stsls, {
  show_modal()
  
  robust_option <- if_else("is_robust" %in% input$model_sar_stsls_options, TRUE, FALSE)
  not_w2x_option <- if_else("not_w2x" %in% input$model_sar_stsls_options, FALSE, TRUE)
  
  stsls(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw,
        robust = robust_option, W2X = not_w2x_option)
})

output$model_sar_stsls_summary <- renderPrint({
  summary(model_sar_stsls())
})

output$model_sar_stsls_impacts <- renderPrint({
  summary(impacts(model_sar_stsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sar_stsls_map <- renderLeaflet({
  geodata_res <- geodata()
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

# SEM (ML)
model_sem_ml <- eventReactive(input$model_estimate_sem_ml, {
  show_modal()
  
  errorsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sem_ml_summary <- renderPrint({
  summary(model_sem_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sem_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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

# SEM (GMM)

model_sem_gmm <- eventReactive(input$model_estimate_sem_gmm, {
  GMerrorsar(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sem_gmm_summary <- renderPrint({
  summary(model_sem_gmm(), Hausman = TRUE)
})

output$model_sem_gmm_map <- renderLeaflet({
  geodata_res <- geodata()
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


# SAC ML
model_sac_ml <- eventReactive(input$model_estimate_sac_ml, {
  show_modal()
  
  sacsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw) 
})

output$model_sac_ml_summary <- renderPrint({
  summary(model_sac_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sac_ml_impacts <- renderPrint({
  summary(impacts(model_sac_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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

# SAC GSTSLS
model_sac_gstsls <- eventReactive(input$model_estimate_sac_gstsls, {
  show_modal()
  
  robust_option <- if_else("is_robust" %in% input$model_sac_gstsls_options, TRUE, FALSE)
  not_w2x_option <- if_else("not_w2x" %in% input$model_sar_stsls_options, FALSE, TRUE)
  
  gstsls(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw,
         robust = robust_option, W2X = not_w2x_option)
})

output$model_sac_gstsls_summary <- renderPrint({
  summary(model_sac_gstsls())
})

output$model_sac_gstsls_impacts <- renderPrint({
  summary(impacts(model_sac_gstsls(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_gstsls_map <- renderLeaflet({
  geodata_res <- geodata()
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


# SLX (ML)
model_slx_ml <- eventReactive(input$model_estimate_slx_ml, {
  show_modal()
  
  lmSLX(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_slx_ml_summary <- renderPrint({
  summary(model_slx_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_slx_ml_impacts <- renderPrint({
  summary(impacts(model_slx_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_slx_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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


# SDM (ML)
model_sdm_ml <- eventReactive(input$model_estimate_sdm_ml, {
  show_modal()
  
  lagsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdm_ml_summary <- renderPrint({
  summary(model_sdm_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdm_ml_impacts <- renderPrint({
  summary(impacts(model_sdm_ml(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sdm_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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



# SDEM (ML)
model_sdem_ml <- eventReactive(input$model_estimate_sdem_ml, {
  show_modal()
  
  errorsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdem_ml_summary <- renderPrint({
  summary(model_sdem_ml(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdem_ml_map <- renderLeaflet({
  geodata_res <- geodata()
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


