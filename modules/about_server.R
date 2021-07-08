# about_server

output$session_info <- renderPrint({
  sessionInfo()
})

packages_names <- c("base", "argonR", "argonDash", "shinycssloaders", "tidyverse", "glue", "DT", "rgdal", "cleangeo", "leaflet", "highcharter", "tmap", "htmltools", "spdep", "spatialreg", "sphet", "plm", "splm", "statquotes")
citation_ui_list <- list()

for (i in 1:length(packages_names)){
  citation_ui_list[[i]] <- argonCard(
    width = 12,
    src = NULL,
    icon = argonIcon("atom"),
    status = "success",
    shadow = TRUE,
    border_level = 2,
    hover_shadow = TRUE,
    title = packages_names[i],
    HTML(capture.output(cat(format(citation(packages_names[i]), "text"), sep = "<br><br>")))
  )
}

output$citation_UI <- renderUI(citation_ui_list)