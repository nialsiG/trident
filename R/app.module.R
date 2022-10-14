myUI <- function(id) {
  ns <- NS(id)
  textOutput(NS(id, "feedback"))
}

myServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$feedback <- shiny::renderText("This is a test")
  })
}
