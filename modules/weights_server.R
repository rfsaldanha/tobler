w_matrix <- reactiveValues(listw = NULL, nb = NULL, name = NULL)

observeEvent(input$weights_contiguity_create, {
  if(input$weights_contiguity_radio == 1){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- paste0("Rook matrix")
      showNotification(ui = "Rook matrix created!", type = "message")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- paste0("Rook matrix")
      showNotification(ui = "Rook matrix created!", type = "message")
    }
  } else if(input$weights_contiguity_radio == 2){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- paste0("Queen matrix")
      showNotification(ui = "Queen matrix created!", type = "message")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- paste0("Queen matrix")
      showNotification(ui = "Queen matrix created!", type = "message")
    }
  }
    
})


observeEvent(input$weights_inverse_distance_create, {
  
  power <- input$weights_inverse_distance_power
  coords <- coordinates(geodata())
  w_nb <- dnearneigh(coords, input$weights_inverse_distance_lower_bound, input$weights_inverse_distance_upper_bound, longlat = TRUE)
  dlist <- nbdists(w_nb, coords)
  dlist <- lapply(dlist, function(x) 1/x^power)
  w_matrix$nb <- w_nb
  w_matrix$listw <- nb2listw(w_nb, glist=dlist, style = input$weights_inverse_distance_style)
  w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
  w_matrix$name <- paste0("Inverse distance matrix")
  showNotification(ui = "Inverse distance matrix created!", type = "message")
  
})


observeEvent(input$weights_k_nearest_create, {
  
  IDs <- row.names(geodata()@data)
  coords <- coordinates(geodata())
  
  w_knear <- knearneigh(coords, k = input$weights_k_nearest_k)
  w_nb <-knn2nb(w_knear, row.names=IDs)
  w_matrix$nb <- w_nb
  w_matrix$listw <- nb2listw(w_nb, style = input$weights_k_nearest_style)
  w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
  w_matrix$name <- paste0("K-Nearest Neighbors")
  showNotification(ui = "K-Nearest Neighbors matrix created!", type = "message")
  
})

output$weights_baumont_dependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("weights_baumont_dependent_variable", label = "Dependent variable", choices = variables)
})

output$weights_baumont_idependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("weights_baumont_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})



observeEvent(input$weights_baumont_create, {
  
  withProgress(message = "Processing", detail = "Baumont procedure", value = 0,{
    max_k <- input$weights_baumont_max_k
    IDs <- row.names(geodata()@data)
    coords <- coordinates(geodata())
    res.pesos <- data.frame(k=numeric(),i=numeric(),valorp=numeric())
    
    esp <- paste0(as.character(input$weights_baumont_dependent_variable), " ~ ", paste0(input$weights_baumont_independent_variable, collapse = " + "))
    
    ols_model <- lm(formula = formula(esp), data = geodata()@data)
    
    for(k in 1:max_k)
    {
      # Armazenando número k atual
      res.pesos[k,1] <- k
      
      incProgress(amount = 1/max_k, detail = paste("k =",k))
      
      # Calculando o I e significância para o k atual
      w_knear <- knearneigh(coords, k = k)
      w_nb <-knn2nb(w_knear, row.names=IDs)
      matrix <- nb2listw(w_nb, style = input$weights_baumont_style)
      residual_test <- lm.morantest(ols_model, matrix)
      
      # Armazenando o valor I para o k atual
      res.pesos[k,2] <- residual_test$statistic
      
      # Armazenando o p-value para o k atual
      res.pesos[k,3] <- residual_test$p.value
    }
    
    maxi <- which.max(res.pesos[,2])
    
    showNotification(ui = paste0("Maximum Moran's I of ", round(res.pesos[maxi,2],4), " obtained with k = ", maxi), type = "message", duration = NULL)
    
    w_knear <- knearneigh(coords, k = maxi)
    w_nb <-knn2nb(w_knear, row.names=IDs)
    w_matrix$nb <- w_nb
    w_matrix$listw <- nb2listw(w_nb, style = input$weights_baumont_style)
    w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
    w_matrix$name <- paste0("Baumont (2004) procedure, k=", maxi, " selected")
    showNotification(ui = "Baumont procedure executed!", type = "message")
  })
  
})


output$weights_stakhovych_dependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("weights_stakhovych_dependent_variable", label = "Dependent variable", choices = variables)
})

output$weights_stakhovych_idependent_variable_UI <- renderUI({
  variables <- names(geodata()@data)
  selectInput("weights_stakhovych_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

observeEvent(input$weights_stakhovych_create, {
  
  withProgress(message = "Processing", detail = "Stakhovych-Bijmolt procedure", value = 0,{
    
    # 7 matrizes
    # 6 modelos: 42
    # Compilando valores
    # Computando matriz final
    
    const_passos <- 1/16
    
    shape <- geodata()
    esp <- paste0(as.character(input$weights_stakhovych_dependent_variable), " ~ ", paste0(input$weights_stakhovych_independent_variable, collapse = " + "))
    
    # Matriz queen
    incProgress(amount = const_passos, detail = "Creating queen matrix")
    w1 <- nb2listw(poly2nb(shape, queen = TRUE))
    
    # Matriz rook
    incProgress(amount = const_passos, detail = "Creating rook matrix")
    w2 <- nb2listw(poly2nb(shape, queen = FALSE))
    
    # Distância inversa
    incProgress(amount = const_passos, detail = "Creating inverse distance matrix")
    coords <- coordinates(shape)
    nb <- dnearneigh(coords, 0, 1000, longlat = TRUE)
    dlist <- nbdists(nb, coords)
    dlist <- lapply(dlist, function(x) 1/x)
    w3 <- nb2listw(nb, glist=dlist)
    
    # K5 10 15 20
    incProgress(amount = const_passos, detail = "Creating k=1 matrix")
    w4 <- nb2listw(knn2nb(knearneigh(coords, k=1)))
    incProgress(amount = const_passos, detail = "Creating k=5 matrix")
    w5 <- nb2listw(knn2nb(knearneigh(coords, k=5)))
    incProgress(amount = const_passos, detail = "Creating k=10 matrix")
    w6 <- nb2listw(knn2nb(knearneigh(coords, k=10)))
    incProgress(amount = const_passos, detail = "Creating k=15 matrix")
    w7 <- nb2listw(knn2nb(knearneigh(coords, k=15)))
    incProgress(amount = const_passos, detail = "Creating k=20 matrix")
    w8 <- nb2listw(knn2nb(knearneigh(coords, k=20)))
    
    # SAR
    incProgress(amount = const_passos, detail = "Estimating SAR model with all matrixes")
    sar.w1 <- lagsarlm(formula = esp, data = shape@data, listw = w1)
    sar.w2 <- lagsarlm(formula = esp, data = shape@data, listw = w2)
    sar.w3 <- lagsarlm(formula = esp, data = shape@data, listw = w3)
    sar.w4 <- lagsarlm(formula = esp, data = shape@data, listw = w4)
    sar.w5 <- lagsarlm(formula = esp, data = shape@data, listw = w5)
    sar.w6 <- lagsarlm(formula = esp, data = shape@data, listw = w6)
    sar.w7 <- lagsarlm(formula = esp, data = shape@data, listw = w7)
    sar.w8 <- lagsarlm(formula = esp, data = shape@data, listw = w8)
    
    # SEM
    incProgress(amount = const_passos, detail = "Estimating SEM model with all matrixes")
    sem.w1 <- errorsarlm(formula = esp, data = shape@data, listw = w1)
    sem.w2 <- errorsarlm(formula = esp, data = shape@data, listw = w2)
    sem.w3 <- errorsarlm(formula = esp, data = shape@data, listw = w3)
    sem.w4 <- errorsarlm(formula = esp, data = shape@data, listw = w4)
    sem.w5 <- errorsarlm(formula = esp, data = shape@data, listw = w5)
    sem.w6 <- errorsarlm(formula = esp, data = shape@data, listw = w6)
    sem.w7 <- errorsarlm(formula = esp, data = shape@data, listw = w7)
    sem.w8 <- errorsarlm(formula = esp, data = shape@data, listw = w8)
    
    # SAC
    incProgress(amount = const_passos, detail = "Estimating SAC model with all matrixes")
    sac.w1 <- sacsarlm(formula = esp, data = shape@data, listw = w1)
    sac.w2 <- sacsarlm(formula = esp, data = shape@data, listw = w2)
    sac.w3 <- sacsarlm(formula = esp, data = shape@data, listw = w3)
    sac.w4 <- sacsarlm(formula = esp, data = shape@data, listw = w4)
    sac.w5 <- sacsarlm(formula = esp, data = shape@data, listw = w5)
    sac.w6 <- sacsarlm(formula = esp, data = shape@data, listw = w6)
    sac.w7 <- sacsarlm(formula = esp, data = shape@data, listw = w7)
    sac.w8 <- sacsarlm(formula = esp, data = shape@data, listw = w8)
    
    # SLX
    incProgress(amount = const_passos, detail = "Estimating SLX model with all matrixes")
    slx.w1 <- lmSLX(formula = esp, data = shape@data, listw = w1)
    slx.w2 <- lmSLX(formula = esp, data = shape@data, listw = w2)
    slx.w3 <- lmSLX(formula = esp, data = shape@data, listw = w3)
    slx.w4 <- lmSLX(formula = esp, data = shape@data, listw = w4)
    slx.w5 <- lmSLX(formula = esp, data = shape@data, listw = w5)
    slx.w6 <- lmSLX(formula = esp, data = shape@data, listw = w6)
    slx.w7 <- lmSLX(formula = esp, data = shape@data, listw = w7)
    slx.w8 <- lmSLX(formula = esp, data = shape@data, listw = w8)
    
    # SDM
    incProgress(amount = const_passos, detail = "Estimating SDM model with all matrixes")
    sdm.w1 <- lagsarlm(formula = esp, data = shape@data, listw = w1, type = "mixed")
    sdm.w2 <- lagsarlm(formula = esp, data = shape@data, listw = w2, type = "mixed")
    sdm.w3 <- lagsarlm(formula = esp, data = shape@data, listw = w3, type = "mixed")
    sdm.w4 <- lagsarlm(formula = esp, data = shape@data, listw = w4, type = "mixed")
    sdm.w5 <- lagsarlm(formula = esp, data = shape@data, listw = w5, type = "mixed")
    sdm.w6 <- lagsarlm(formula = esp, data = shape@data, listw = w6, type = "mixed")
    sdm.w7 <- lagsarlm(formula = esp, data = shape@data, listw = w7, type = "mixed")
    sdm.w8 <- lagsarlm(formula = esp, data = shape@data, listw = w8, type = "mixed")
    
    
    # SDEM
    incProgress(amount = const_passos, detail = "Estimating SDEM model with all matrixes")
    sdem.w1 <- errorsarlm(formula = esp, data = shape@data, listw = w1, etype = "emixed")
    sdem.w2 <- errorsarlm(formula = esp, data = shape@data, listw = w2, etype = "emixed")
    sdem.w3 <- errorsarlm(formula = esp, data = shape@data, listw = w3, etype = "emixed")
    sdem.w4 <- errorsarlm(formula = esp, data = shape@data, listw = w4, etype = "emixed")
    sdem.w5 <- errorsarlm(formula = esp, data = shape@data, listw = w5, etype = "emixed")
    sdem.w6 <- errorsarlm(formula = esp, data = shape@data, listw = w6, etype = "emixed")
    sdem.w7 <- errorsarlm(formula = esp, data = shape@data, listw = w7, etype = "emixed")
    sdem.w8 <- errorsarlm(formula = esp, data = shape@data, listw = w8, etype = "emixed")
    
    incProgress(amount = const_passos, detail = "Computing results")
    df <- data.frame(modelo = c("SAR", "SEM", "SAC", "SLX", "SDM", "SDEM"),
                     W_Queen = c(AIC(sar.w1), AIC(sem.w1), AIC(sac.w1), AIC(slx.w1), AIC(sdm.w1), AIC(sdem.w1)),
                     W_Rook = c(AIC(sar.w2), AIC(sem.w2), AIC(sac.w2), AIC(slx.w2), AIC(sdm.w2), AIC(sdem.w2)),
                     W_dist_inv = c(AIC(sar.w3), AIC(sem.w3), AIC(sac.w3), AIC(slx.w3), AIC(sdm.w3), AIC(sdem.w3)),
                     W_k1 = c(AIC(sar.w4), AIC(sem.w4), AIC(sac.w4), AIC(slx.w4), AIC(sdm.w4), AIC(sdem.w4)),
                     W_k5 = c(AIC(sar.w5), AIC(sem.w5), AIC(sac.w5), AIC(slx.w5), AIC(sdm.w5), AIC(sdem.w5)),
                     W_k10 = c(AIC(sar.w6), AIC(sem.w6), AIC(sac.w6), AIC(slx.w6), AIC(sdm.w6), AIC(sdem.w6)),
                     W_k15 = c(AIC(sar.w7), AIC(sem.w7), AIC(sac.w7), AIC(slx.w7), AIC(sdm.w7), AIC(sdem.w7)),
                     W_k20 = c(AIC(sar.w8), AIC(sem.w8), AIC(sac.w8), AIC(slx.w8), AIC(sdm.w8), AIC(sdem.w8)))
    
    df_melt <- reshape2::melt(df, id = "modelo")
    df_melt <- df_melt[order(-df_melt$value),]
    names(df_melt) <- c("Modelo", "Matriz", "AIC")
    row.names(df_melt) <- NULL
    
    df_top <- df_melt %>% 
      arrange(-AIC) %>%
      top_n(1)
    
    showNotification(ui = paste0("Maximum AIC value of ", round(df_top$AIC, 4)," obtained with ", df_top$Modelo, " model and ", df_top$Matriz), type = "message", duration = NULL)
    
    incProgress(amount = const_passos, detail = "Computing final matrix")
    if(df_top$Matriz == "W_Queen"){
      
      w_matrix$nb <- poly2nb(shape, queen = TRUE)
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_stakhovych_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "Queen matrix"
      
    } else if(df_top$Matriz == "W_Rook"){
      
      w_matrix$nb <- poly2nb(shape, queen = FALSE)
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_stakhovych_style)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "Queen matrix"
      
    } else if(df_top$Matriz == "W_dist_inv"){
      
      coords <- coordinates(shape)
      w_matrix$nb <- dnearneigh(coords, 0, 1000, longlat = TRUE)
      dlist <- nbdists(nb, coords)
      dlist <- lapply(dlist, function(x) 1/x)
      w_matrix$listw <- nb2listw(w_matrix$nb, glist=dlist)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "Inverse distance matrix"
      
    } else if(df_top$Matriz == "W_k1"){
      w_matrix$nb <- knn2nb(knearneigh(coords, k=1))
      w_matrix$listw <- nb2listw(w_matrix$nb)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "K-Nearest Neighbors with k=1"
      
    } else if(df_top$Matriz == "W_k5"){
      
      w_matrix$nb <- knn2nb(knearneigh(coords, k=5))
      w_matrix$listw <- nb2listw(w_matrix$nb)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "K-Nearest Neighbors with k=5"
      
    } else if(df_top$Matriz == "W_k10"){
      
      w_matrix$nb <- knn2nb(knearneigh(coords, k=10))
      w_matrix$listw <- nb2listw(w_matrix$nb)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "K-Nearest Neighbors with k=10"
      
    } else if(df_top$Matriz == "W_k15"){
      
      w_matrix$nb <- knn2nb(knearneigh(coords, k=15))
      w_matrix$listw <- nb2listw(w_matrix$nb)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "K-Nearest Neighbors with k=15"
      
    } else if(df_top$Matriz == "W_k20"){
      
      w_matrix$nb <- knn2nb(knearneigh(coords, k=20))
      w_matrix$listw <- nb2listw(w_matrix$nb)
      w_matrix$tr <- trW(as(w_matrix$listw, "CsparseMatrix"), type="mult")
      w_matrix$name <- "K-Nearest Neighbors with k=20"
      
    } 
    
  })
  
})





observeEvent(w_matrix, {
  
  output$matrix_info1_UI <- renderUI({
    req(w_matrix$listw)
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = paste(w_matrix$name, "info"),
      argonRow(
        renderPrint(summary(w_matrix$listw))
      )
    )
  })
  
  output$matrix_plot_UI <- renderUI({
    req(w_matrix$nb)
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = paste(w_matrix$name, "plot"),
      argonRow(
        renderPlot(plot(w_matrix$nb, coordinates(geodata())))
      )
    )
  })  
})




