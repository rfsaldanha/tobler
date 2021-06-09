# Show modal function
show_modal <- function(time = 3){
  fortune <- statquote()
  showModal(session = getDefaultReactiveDomain(),
            modalDialog(
              title = "Please wait",
              "Your model is being estimated...",
              br(),br(),
              em(fortune$text),br(),
              em(paste("---", fortune$source)),
              footer = NULL
            )
  )
  Sys.sleep(time)
}