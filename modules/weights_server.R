w_matrix <- eventReactive(input$weights_contiguity_create, {
  if(input$weights_contiguity_radio == 1){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = FALSE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
    }
  } else if(input$weights_contiguity_radio == 2){
    if(input$weights_contiguity_order == 1){
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_listw <- nb2listw(w_nb, style = input$weights_contiguity_style)
    } else {
      w_nb <- poly2nb(pl = geodata(), queen = TRUE)
      w_nblag <- nblag_cumul(nblag(w_nb, maxlag = input$weights_contiguity_order))
      w_listw <- nb2listw(w_nblag, style = input$weights_contiguity_style)
    }
  }
  
  w_listw
  
})

w_matrix <- eventReactive(input$weights_inverse_distance_create, {
  power <- input$weights_inverse_distance_power
  coords <- coordinates(geodata())
  w_nb <- dnearneigh(coords, 0, 1000)
  dlist <- nbdists(w_nb, coords)
  dlist <- lapply(dlist, function(x) 1/x^power)
  w_listw <- nb2listw(w_nb, glist=dlist, style = input$weights_inverse_distance_style)
})

output$matrix_info <- renderPrint({
  summary(w_matrix())
})