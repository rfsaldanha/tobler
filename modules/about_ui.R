about_tab <- argonTabItem(
  tabName = "about",
  argonRow(
    center = TRUE,
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "About",
        p("Tobler is an R Shiny app and relies on several packages to work."),
        uiOutput("citation_UI")
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "R Session Info",
        htmlOutput("session_info")
      )
    )
  )
)