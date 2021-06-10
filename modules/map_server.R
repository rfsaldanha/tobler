output$map_variable_UI <- renderUI({
  req(geodata())
  sub <- geodata_tmp()@data %>%
    select_if(is.numeric)
  varSelectInput("map_variable", label = "Variable", data = sub)
})


output$map_leaflet <- renderLeaflet({
  req(geodata_tmp())
  req(input$map_variable)
  
  map_var <- as.character(input$map_variable)
  map <- tm_shape(geodata_tmp()) +
    tm_fill(col = map_var,
            style = as.character(input$map_style),
            alpha = 0.7,
            title = map_var) +
    tm_borders()
  tmap_leaflet(map)
})