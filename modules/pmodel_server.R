output$pmodel_dependent_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(variable) %>%
    distinct() %>%
    pull(variable)
  
  selectInput("pmodel_dependent_variable", label = "Dependent variable", choices = variables)
})

output$pmodel_independent_variable_UI <- renderUI({
  variables <- geodata()@data %>%
    select(variable) %>%
    distinct() %>%
    pull(variable)
  
  selectInput("pmodel_independent_variable", label = "Independent variables", choices = variables, multiple = TRUE)
})

# Model specification
pesp <- reactive({
  paste0(as.character(input$pmodel_dependent_variable), " ~ ", paste0(input$pmodel_independent_variable, collapse = " + "))
})