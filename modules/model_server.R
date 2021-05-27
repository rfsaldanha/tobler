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
  
  stsls(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
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

# SEM ML
model_sem_mv <- eventReactive(input$model_estimate_sem_ml, {
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


# SEM STSLS

model_sem_mq2e <- eventReactive(input$models_estimate, {
  GMerrorsar(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sem_mq2e_summary <- renderPrint({
  summary(model_sem_mq2e())
})

output$model_sem_mq2e_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sem_mq2e())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sem_mv(), removeModal())

# SEM STSLS
model_sem_mq2e <- eventReactive(input$model_estimate_sem_stsls, {
  show_modal()
  
  GMerrorsar(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sem_mq2e_summary <- renderPrint({
  summary(model_sem_mq2e())
})

output$model_sem_mq2e_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sem_mq2e())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sem_mq2e(), removeModal())


# SAC ML
model_sac_mv <- eventReactive(input$model_estimate_sac_ml, {
  show_modal()
  
  sacsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw) 
})

output$model_sac_mv_summary <- renderPrint({
  summary(model_sac_mv(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sac_mv_impacts <- renderPrint({
  summary(impacts(model_sac_mv(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_mv_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sac_mv())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sac_mv(), removeModal())

# SAC STSLS
model_sac_mq2e <- eventReactive(input$model_estimate_sac_stsls, {
  show_modal()
  
  gstsls(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_sac_mq2e_summary <- renderPrint({
  summary(model_sac_mq2e())
})

output$model_sac_mq2e_impacts <- renderPrint({
  summary(impacts(model_sac_mq2e(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sac_mq2e_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sac_mq2e())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sac_mq2e(), removeModal())


# SLX (ML)
model_slx_mv <- eventReactive(input$model_estimate_slx_ml, {
  show_modal()
  
  lmSLX(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw)
})

output$model_slx_mv_summary <- renderPrint({
  summary(model_slx_mv(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_slx_mv_impacts <- renderPrint({
  summary(impacts(model_slx_mv(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_slx_mv_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_slx_mv())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_slx_mv(), removeModal())


# SDM (ML)
model_sdm_mv <- eventReactive(input$model_estimate_sdm_ml, {
  show_modal()
  
  lagsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdm_mv_summary <- renderPrint({
  summary(model_sdm_mv(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdm_mv_impacts <- renderPrint({
  summary(impacts(model_sdm_mv(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
})

output$model_sdm_mv_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sdm_mv())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sdm_mv(), removeModal())



# SDEM (ML)
model_sdem_mv <- eventReactive(input$model_estimate_sdem_ml, {
  show_modal()
  
  errorsarlm(formula = formula(esp()), data = geodata()@data, listw = w_matrix$listw, Durbin = TRUE)
})

output$model_sdem_mv_summary <- renderPrint({
  summary(model_sdem_mv(), Nagelkerke = TRUE, Hausman = TRUE)
})

output$model_sdem_mv_map <- renderLeaflet({
  geodata_res <- geodata()
  geodata_res@data$residuals <- resid(model_sdem_mv())
  
  map <- tm_shape(geodata_res) +
    tm_fill(col = "residuals",
            palette = "-RdBu",
            alpha = 0.7,
            midpoint = 0,
            title = "Residuals") +
    tm_borders()
  tmap_leaflet(map)
})

observeEvent(model_sdem_mv(), removeModal())


