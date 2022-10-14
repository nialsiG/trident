graphicsUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("biplot"), "Bivariate"),
    actionButton(ns("boxplot"), "Box-Plot"),
    actionButton(ns("pca"), "PCA"),
    hr(),
    textOutput(ns("feedback"))
)
}

graphicsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #multiCheck button----
    observeEvent(input$biplot, {})

    #rankBy button----
    observeEvent(input$boxplot, {})

    #pca button----
    observeEvent(input$pca, {})

    output$feedback <- shiny::renderText("This is a test")

  })
}
