w_matrix <- reactiveValues(listw = NULL, nb = NULL, name = NULL)

observeEvent(input$weights_contiguity_create, {
  if(input$weights_contiguity_radio == 1){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Rook matrix")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Rook matrix")
    }
  } else if(input$weights_contiguity_radio == 2){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_matrix$nb <- w_nb
      w_matrix$listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Queen matrix")
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_matrix$nb <- w_nblag
      w_matrix$listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
      w_matrix$name <- paste0("Queen matrix")
    }
  }
    
})


observeEvent(input$weights_inverse_distance_create, {
  
  power <- input$weights_inverse_distance_power
  coords <- coordinates(geodata())
  w_nb <- dnearneigh(coords, 0, 1000)
  dlist <- nbdists(w_nb, coords)
  dlist <- lapply(dlist, function(x) 1/x^power)
  w_matrix$nb <- w_nb
  w_matrix$listw <- nb2listw(w_nb, glist=dlist, style = input$weights_inverse_distance_style)
  w_matrix$name <- paste0("Inverse distance matrix")
  
})


observeEvent(input$weights_k_nearest_create, {
  
  IDs <- row.names(geodata()@data)
  coords <- coordinates(geodata())
  
  w_knear <- knearneigh(coords, k = input$weights_k_nearest_k)
  w_nb <-knn2nb(w_knear, row.names=IDs)
  w_matrix$nb <- w_nb
  w_matrix$listw <- nb2listw(w_nb, style = input$weights_k_nearest_style)
  w_matrix$name <- paste0("K-Nearest Neighbors")
  
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
    
    message(k)
    
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
  
})

output$matrix_info <- renderPrint({
  req(w_matrix$listw)
  print(w_matrix$name, quote = FALSE)
  summary(w_matrix$listw)
})

output$matrix_plot <- renderPlot({
  req(w_matrix$nb)
  plot(w_matrix$nb, coordinates(geodata()))
})