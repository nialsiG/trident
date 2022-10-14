variablesUI <- function(id) {
  ns <- NS(id)
  tagList(
  actionButton(ns("addFactor"), "Add Factor"),
  actionButton(ns("multiCheck"), "Multi-Check"),
  actionButton(ns("rankBy"), "Rank"),
  actionButton(ns("top3"), "Top-3"),
  hr(),
  textOutput(ns("feedback"))
  )
}

variablesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #addFactor button----
    observeEvent(input$addFactor, {})

    #multiCheck button----
    observeEvent(input$multiCheck, {})

    #rankBy button----
    observeEvent(input$rankBy, {})

    #top3 button----
    observeEvent(input$top3, {})

    output$feedback <- renderText("This is a test")
  })
}
