argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "md",
  side = "left",
  id = "my_sidebar",
  #brand_url = "http://www.google.com",
  brand_logo = "tobleR.png",
  #brand_logo = "https://demos.creative-tim.com/argon-design-system/assets/img/brand/blue.png",
  #argonSidebarHeader(title = "Main Menu"),
  argonSidebarMenu(
    argonSidebarItem(
      tabName = "home",
      icon = argonIcon(name = "world", color = "info"),
      "Home"
    ),
    argonSidebarItem(
      tabName = "data",
      icon = argonIcon(name = "folder-17", color = "info"),
      "Data"
    ),
    #argonSidebarDivider(),
    #argonSidebarHeader(title = "Exploratory data analysis"),
    argonSidebarItem(
      tabName = "map",
      icon = argonIcon(name = "map-big", color = "info"),
      "Map"
    ),
    argonSidebarItem(
      tabName = "weights",
      icon = argonIcon(name = "world-2", color = "info"),
      "Spatial Weights Matrix"
    ),
    argonSidebarItem(
      tabName = "autocor",
      icon = argonIcon(name = "compass-04", color = "info"),
      "Spatial Autocorrelation"
    ),
    argonSidebarItem(
      tabName = "model",
      icon = argonIcon(name = "single-copy-04", color = "info"),
      "Spatial Cross Section Models"
    ),
    argonSidebarItem(
      tabName = "pmodel",
      icon = argonIcon(name = "ungroup", color = "info"),
      "Spatial Panel Models"
    )
  )
)