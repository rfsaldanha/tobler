output$autocor_variable_UI <- renderUI({
  req(geodata())
  sub <- geodata()@data %>%
    select_if(is.numeric)
  varSelectInput("autocor_variable", label = "Interest variable", data = sub)
})

output$autocor_scatter <- renderPlot({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  moran.plot(sub$var, listw = w_matrix$listw, xlab = as.character(input$autocor_variable), ylab = paste("Spatially lagged", as.character(input$autocor_variable)))
})

output$autocor_moran <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  moran.test(sub$var, listw = w_matrix$listw)
})

output$autocor_moran_mc <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  moran.mc(sub$var, listw = w_matrix$listw, nsim = 999)
})

output$autocor_geary <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  geary.test(sub$var, listw = w_matrix$listw)
})

output$autocor_geary_mc <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  geary.mc(sub$var, listw = w_matrix$listw, nsim = 999)
})

output$autocor_getis <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  globalG.test(as.vector(scale(sub$var, center = FALSE)), listw = w_matrix$listw, B1correct = TRUE)
})

output$autocor_getis_star <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  
  matrix <- include.self(w_matrix$nb)
  matrix <- nb2listw(matrix, style="W", zero.policy = TRUE)
  
  globalG.test(as.vector(scale(sub$var, center = FALSE)), listw = matrix, B1correct = TRUE)
})
  
  
  
  
  
  
  
  
  
  