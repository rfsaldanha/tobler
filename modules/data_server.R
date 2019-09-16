geodata <- reactive({
  req(input$data_file$datapath)
  
  withProgress(message = "Processing", detail = "Loading data", value = 0,{
    # Load data
    shape <- readOGR(input$data_file$datapath)
    incProgress(amount = 0.3, detail = "Data loaded")
    
    # Validate and clean data
    if(input$data_validate == TRUE){
      incProgress(amount = 0.3, detail = "Validating data")
      if(!clgeo_IsValid(shape)){
        incProgress(amount = 0.3, detail = "Cleaning geometry")
        shape <- clgeo_Clean(shape)
      }
      incProgress(amount = 0.1, detail = "Done!")
      showNotification(ui = "Data loaded!", type = "message")
    } else {
      incProgress(amount = 0.7, detail = "Done!")
      showNotification(ui = "Data loaded!", type = "message")
    } 
  })

  # Return data
  shape
})

observeEvent(geodata(), {
  output$data_table_UI <- renderUI({
    argonCard(
      width = 12,
      src = NULL,
      icon = icon("cogs"),
      status = "success",
      shadow = TRUE,
      border_level = 2,
      hover_shadow = TRUE,
      title = "Data table",
      argonRow(
        renderDT(geodata()@data, options = list(scrollX = TRUE))
      )
    )
  })
})