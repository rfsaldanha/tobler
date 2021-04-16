output$cross_section_model_dependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("cross_section_model_dependent_variable", label = "Dependent variable", choices = variables)
})

output$cross_section_model_independent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("cross_section_model_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

output$cross_section_model_endogenous_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  
  if(input$cross_section_model_type == "ols_std"){
    NULL
  } else {
    selectInput("cross_section_model_endogenous_variable", label = "Endogenous variables (optional)", choices = variables, multiple = TRUE)
  }
})

output$cross_section_model_instrument_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  
  if(input$cross_section_model_type == "ols_std"){
    NULL
  } else {
    selectInput("cross_section_model_instrument_variable", label = "Instrument variables (optional)", choices = variables, multiple = TRUE)
  }
})

output$cross_section_model_estimator_UI <- renderUI({
  variables <- names(geodata()@data)
  
  if(input$cross_section_model_type == "ols_std"){
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Ordinary Least Squares" = "ols_std"
      ), 
      selected = "ols_std"
    )
  } else if(input$cross_section_model_type == "sar" & !length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml",
        "Spatial Two Stage Least Squares (STSLS)" = "stsls"
      ), 
      selected = "ml"
    )
  } else if(input$cross_section_model_type == "sar" & length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Spatial Two Stage Least Squares (STSLS)" = "stsls"
      ), 
      selected = "stsls"
    )
  } else if(input$cross_section_model_type == "sem" & !length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml",
        "GMM estimator" = "gmm"
      ), 
      selected = "ml"
    )
  } else if(input$cross_section_model_type == "sem" & length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "GMM estimator" = "gmm"
      ), 
      selected = "gmm"
    )
  } else if(input$cross_section_model_type == "sac" & !length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml",
        "General Spatial Two Stage Least Squares (GSTSLS)" = "gstsls"
      ), 
      selected = "ml"
    )
  } else if(input$cross_section_model_type == "sac" & length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "General Spatial Two Stage Least Squares (GSTSLS)" = "gstsls"
      ), 
      selected = "gstsls"
    )
  } else if(input$cross_section_model_type == "slx" & !length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml",
        "Ordinary Least Squares (OLS)" = "ols"
      ), 
      selected = "ml"
    )
  } else if(input$cross_section_model_type == "slx" & length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Ordinary Least Squares (OLS)" = "ols"
      ), 
      selected = "ols"
    )
  } else if(input$cross_section_model_type == "sdem" & !length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml",
        "GMM estimator" = "gmm"
      ), 
      selected = "ml"
    )
  } else if(input$cross_section_model_type == "sdem" & length(input$cross_section_model_instrument_variable) > 0) {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "GMM estimator" = "gmm"
      ), 
      selected = "gmm"
    )
  } else if(input$cross_section_model_type == "sdm") {
    radioButtons(
      "cross_section_model_estimator", 
      label = "Estimator", 
      choices = list(
        "Maximum Likelihood (ML)" = "ml"
      ), 
      selected = "ml"
    )
  } 
})

output$cross_section_model_variance_UI <- renderUI({
  req(input$cross_section_model_estimator)
  
  if(input$cross_section_model_estimator %in% c("ols_std", "ml")){
    NULL
  } else {
    radioButtons(
      "cross_section_model_variance",
      label = h4("Variance"),
      choices = list("Homoskedasticity" = "homo", "Heteroskedasticity (robust)" = "hete"), 
      selected = "homo"
    )
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

# Estimate model
cross_section_model <- eventReactive(input$cross_section_model_estimate, {
  req(input$cross_section_model_type)
  req(input$cross_section_model_estimator)
  
  show_modal()
  
  model_type <- input$cross_section_model_type
  model_estimator <- input$cross_section_model_estimator
  model_variance <- input$cross_section_model_variance
  have_instruments <- if_else(length(input$cross_section_model_instrument_variable) > 0, TRUE, FALSE)
  endogenous <- if(cross_section_model_endogenous() == " ~ "){
    NULL
  } else {
    cross_section_model_endogenous()
  }
  instruments <- if(cross_section_model_instrument() == " ~ "){
    NULL
  } else {
    cross_section_model_instrument()
  }
  
  if(model_type == "ols_std" & model_estimator == "ols_std"){
    lm(formula = formula(cross_section_model_esp()), data = geodata()@data)
  } else if(model_type == "sar" & model_estimator == "ml" & !have_instruments){
    lagsarlm(formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw)
  } else if(model_type == "sar" & model_estimator == "stsls" & !have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "lag"
    )
  } else if(model_type == "sar" & model_estimator == "stsls" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw,
      het = TRUE, model = "lag"
    )
  } else if(model_type == "sar" & model_estimator == "stsls" & have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "lag",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sar" & model_estimator == "stsls" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "lag",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sem" & model_estimator == "ml" & !have_instruments){
    errorsarlm(formula = formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw)
  } else if(model_type == "sem" & model_estimator == "gmm" & !have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "error", 
      step1.c = TRUE
    )
  } else if(model_type == "sem" & model_estimator == "gmm" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "error", 
      step1.c = TRUE
    )
  } else if(model_type == "sem" & model_estimator == "gmm" & have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "error", 
      step1.c = TRUE,
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sem" & model_estimator == "gmm" & have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "error", 
      step1.c = TRUE,
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sac" & model_estimator == "ml" & !have_instruments){
    sacsarlm(formula = formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw) 
  } else if(model_type == "sac" & model_estimator == "gstsls" & !have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "sarar"
    )
  } else if(model_type == "sac" & model_estimator == "gstsls" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "sarar"
    )
  } else if(model_type == "sac" & model_estimator == "gstsls" & have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "sarar",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sac" & model_estimator == "gstsls" & have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "sarar",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "slx" & model_estimator == "ml" & !have_instruments){
    lmSLX(formula = formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw)
  } else if(model_type == "slx" & model_estimator == "ols" & !have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "ols"
    )
  } else if(model_type == "slx" & model_estimator == "ols" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "ols"
    )
  } else if(model_type == "slx" & model_estimator == "ols" & have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "ols",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "slx" & model_estimator == "ols" & have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "ols",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sdem" & model_estimator == "ml" & !have_instruments){
    lagsarlm(formula = formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw, type = "emixed")
  } else if(model_type == "sdem" & model_estimator == "gmm" & !have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "error"
    )
  } else if(model_type == "sdem" & model_estimator == "gmm" & !have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "error"
    )
  } else if(model_type == "sdem" & model_estimator == "gmm" & have_instruments & model_variance == "homo"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = FALSE, model = "error",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sdem" & model_estimator == "gmm" & have_instruments & model_variance == "hete"){
    spreg(
      formula = formula(cross_section_model_esp()), 
      data = geodata()@data, 
      listw = w_matrix$listw, 
      het = TRUE, model = "error",
      endog = endogenous,
      instruments = instruments
    )
  } else if(model_type == "sdm" & model_estimator == "ml" & !have_instruments){
    lagsarlm(formula = formula(cross_section_model_esp()), data = geodata()@data, listw = w_matrix$listw, type = "mixed")
  }
  
  
})


output$cross_section_model_summary <- renderPrint({
  summary(cross_section_model())
})

output$cross_section_model_analysis_UI <- renderUI({
  req(input$cross_section_model_type)
  req(input$cross_section_model_estimator)
  
  model_type <- input$cross_section_model_type
  model_estimator <- input$cross_section_model_estimator
  
  if(model_type == "ols_std"){
    tagList(
      h3("Moran's I for error term"),
      renderPrint({
        lm.morantest(cross_section_model(), w_matrix$listw)
      }),
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      }),
      h3("Lagrange multiplier"),
      renderPrint({
        lm.LMtests(model = cross_section_model(), listw = w_matrix$listw,
                   test = c("LMerr","RLMerr","LMlag","RLMlag"))
      })
    )
  } else if(model_type == "sar" & model_estimator == "ml"){
    tagList(
      h3("Impacts"),
      renderPrint({
        summary(impacts(cross_section_model(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
      }),
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "sar" & model_estimator == "stsls"){
    tagList(
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "sem"){
    tagList(
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "sac" & model_estimator == "ml"){
    tagList(
      h3("Impacts"),
      renderPrint({
        summary(impacts(cross_section_model(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
      }),
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "sac" & model_estimator == "gstsls"){
    tagList(
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "slx" & model_estimator == "ml"){
    tagList(
      h3("Impacts"),
      renderPrint({
        summary(impacts(cross_section_model(), tr=w_matrix$tr, R=1000), zstats=TRUE, short=TRUE)
      }),
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  } else if(model_type == "slx" & model_estimator == "ols"){
    tagList(
      h3("Residual map"),
      renderLeaflet({
        geodata_res <- geodata()
        geodata_res@data$residuals <- resid(cross_section_model())
        
        map <- tm_shape(geodata_res) +
          tm_fill(col = "residuals",
                  palette = "-RdBu",
                  alpha = 0.7,
                  midpoint = 0,
                  title = "Residuals") +
          tm_borders()
        tmap_leaflet(map)
      })
    )
  }
  
  
  
})

observeEvent(cross_section_model(), removeModal())

