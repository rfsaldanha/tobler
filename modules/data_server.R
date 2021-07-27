output$data_file_UI <- renderUI({
  if(input$data_file_type == "GML"){
    fileInput("data_file", label = "", multiple = FALSE, accept = c(".gml", ".geojson", ".json"))
  } else if(input$data_file_type == "Shapefile") {
    fileInput("data_file", label = "", multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))
  }
})

output$data_panel <- renderUI({
  if(input$data_type == "Panel"){
    variables <- names(geodata_original()@data)
    tagList(
      selectInput("pdata_id_variable", label = "ID variable", choices = variables),
      multiInput("pdata_variables", label = "Measure/time variables", choices = variables),
      p("* Try to use a variable name pattern like INCOME1990, CRIME2010, UNEMPL2010, or INCOME90, CRIME10, UNEMPL10.")
    )
  }
})

geodata_original <- reactive({
  req(input$data_file$datapath)
  
  withProgress(message = "Processing", detail = "Loading data", value = 0,{
    # Load data
    if(input$data_file_type == "GML"){
      # Read GML
      tryCatch({
        shape <- readOGR(input$data_file$datapath)
      },
      error = function(err){
        showNotification(paste0(err), type = "err", duration = NULL)
      })
    } else if(input$data_file_type == "Shapefile") {
      # Name of the temporary directory where files are uploaded
      tmp_dir_name <- dirname(input$data_file$datapath[1])
      
      # Rename files
      for (i in 1:nrow(input$data_file)) {
        file.rename(
          input$data_file$datapath[i],
          paste0(tmp_dir_name, "/", input$data_file$name[i])
        )
      }
      
      # Read shapefile
      tryCatch({
        shape <- readOGR(paste(tmp_dir_name,
                               input$data_file$name[grep(pattern = "*.shp$", input$data_file$name)],
                               sep = "/"
        ))
      },
      error = function(err){
        showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
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


geodata <- reactive({
  
  if(input$data_type == "Panel"){
    req(input$pdata_id_variable)
    req(input$pdata_variables)
    
    shape <- geodata_original()
    
    res <- shape@data %>%
      select(!!!input$pdata_id_variable, !!!input$pdata_variables) %>%
      pivot_longer(
        cols = 2:last_col()
      ) %>%
      mutate(
        variable = as.character(gsub("[[:digit:]]", "", name)),
        time = as.character(gsub("[[:alpha:]]", "", name))
      ) %>%
      select(1, variable, time, value) %>%
      pivot_wider(
        names_from = variable,
        values_from = value
      ) %>%
      arrange(1, time)
    
    shape@data <- res
    
    shape
  } else {
    geodata_original()
  }
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
