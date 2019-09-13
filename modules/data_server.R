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
    } else {
      incProgress(amount = 0.7, detail = "Done!")
    } 
  })

  # Return data
  shape
})

output$data_information <- renderPrint({
  geodata()
})

output$data_table <- renderDT(geodata()@data, options = list(scrollX = TRUE))