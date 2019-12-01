output$autocor_variable_UI <- renderUI({
  req(geodata())
  sub <- geodata()@data %>%
    select_if(is.numeric)
  varSelectInput("autocor_variable", label = "Interest variable", data = sub)
})

output$autocor_name_variable_UI <- renderUI({
  req(geodata())
  sub <- geodata()@data %>%
    select_if(is.factor)
  varSelectInput("autocor_name_variable", label = "Name variable", data = sub)
})

output$autocor_scatter <- renderHighchart({
  req(geodata())
  req(input$autocor_variable)
  req(input$autocor_name_variable)
  req(w_matrix$listw)
  
  #sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  
  sub <- geodata()@data %>%
    select(!!input$autocor_variable, !!input$autocor_name_variable)
  
  names(sub) <- c("var", "name")
  
  sub$svar <- scale(sub$var)
  sub$lag_svar <- lag.listw(w_matrix$listw, sub$svar, zero.policy = TRUE)
  
  #plot(x = sub$svar, y = sub$lag_svar)
  
  x <- c("Name", input$autocor_variable, paste("Lagged", input$autocor_variable))
  y <- c("{point.name}", "{point.svar:.2f}", "{point.lag_svar:.2f}")
  tltip <- tooltip_table(x, y)
  
  hchart(sub, "scatter", hcaes(x = svar, y = lag_svar, name = name)) %>%
    hc_xAxis(title = list(text = paste(input$autocor_variable)),
             plotLines = list(
               list(color = "#000000",
                    width = 1.5,
                    value = 0
               )
             )
     ) %>% 
    hc_yAxis(title = list(text = paste("Lagged", input$autocor_variable)),
             plotLines = list(
               list(color = "#000000",
                    width = 1.5,
                    value = 0
               )
             )
    ) %>%
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip) %>%
    hc_exporting(
      enabled = TRUE
    )
  
})

output$autocor_moran <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  moran.test(sub$var, listw = w_matrix$listw, zero.policy = TRUE)
})

output$autocor_moran_mc <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  moran.mc(sub$var, listw = w_matrix$listw, nsim = 999, zero.policy = TRUE)
})

output$autocor_geary <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  geary.test(sub$var, listw = w_matrix$listw, zero.policy = TRUE)
})

output$autocor_geary_mc <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  geary.mc(sub$var, listw = w_matrix$listw, nsim = 999, zero.policy = TRUE)
})

output$autocor_getis <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  globalG.test(as.vector(scale(sub$var, center = FALSE)), listw = w_matrix$listw, B1correct = TRUE, zero.policy = TRUE)
})

output$autocor_getis_star <- renderPrint({
  req(geodata())
  req(input$autocor_variable)
  req(w_matrix$listw)
  sub <- subset(geodata()@data, select = as.character(input$autocor_variable))
  names(sub) <- "var"
  
  matrix <- include.self(w_matrix$nb)
  matrix <- nb2listw(matrix, style="W", zero.policy = TRUE)
  
  globalG.test(as.vector(scale(sub$var, center = FALSE)), listw = matrix, B1correct = TRUE, zero.policy = TRUE)
})
  
  
  
  
  
  
  
  
  
  