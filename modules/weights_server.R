w_matrix <- reactiveValues(listw = NULL, nb = NULL, name = NULL)

observeEvent(input$weights_contiguity_create, {
  if(input$weights_contiguity_radio == 1){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Rook matrix")
      showNotification(ui = "Rook matrix created!", type = "message")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Rook matrix")
      showNotification(ui = "Rook matrix created!", type = "message")
    }
  } else if(input$weights_contiguity_radio == 2){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Queen matrix")
      showNotification(ui = "Queen matrix created!", type = "message")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
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
    w_knear <- knearneigh(coords, k = maxi)
    w_nb <-knn2nb(w_knear, row.names=IDs)
    w_matrix$nb <- w_nb
    w_matrix$listw <- nb2listw(w_nb, style = input$weights_baumont_style)
    w_matrix$name <- paste0("Baumont (2004) procedure, k=", maxi, " selected")
    showNotification(ui = "Baumont procedure executed!", type = "message")
  })
  
})

observeEvent(w_matrix, {
  output$matrix_info_UI <- renderUI({
    req(w_matrix$listw)
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = w_matrix$name,
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
      title = "Matrix plot",
      argonRow(
        renderPlot(plot(w_matrix$nb, coordinates(geodata())))
      )
    )
  })  
})




