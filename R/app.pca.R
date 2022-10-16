################################################################################
PCAUI <- function(id) {
  ns <- NS(id)
  tagList(

    textOutput(ns("feedback"))
  )
}

################################################################################
PCAServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$feedback <- shiny::renderText("This is a test")

  })
}
