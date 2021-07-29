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
        title = "Source code and issues",
        p("The source code of this project is open and hosted at GitHub:"),
        a("https://github.com/rfsaldanha/tobler", href="https://github.com/rfsaldanha/tobler", target="_blank"),
        br(), br(),
        p("Issues can be posted here:"),
        a("https://github.com/rfsaldanha/tobler/issues", href="https://github.com/rfsaldanha/tobler/issues", target="_blank"),
      ),
      argonCard(
        width = 12,
        src = NULL,
        icon = argonIcon("atom"),
        status = "success",
        shadow = TRUE,
        border_level = 2,
        hover_shadow = TRUE,
        title = "Packages",
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