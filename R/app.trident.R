#' @title trident.app
#' @description Open the Trident app using Shiny.
#' @export
trident.app <- function()
{
  library(shiny)
  library(data.table)
  library(DT)

  ui <- navbarPage(
    "Trident",
    tabPanel("Batch Analysis", batchAnalysisUI("tab0")),
    tabPanel("Dataset", datasetUI("tab1")),
    tabPanel("Variables", variablesUI("tab2")),
    tabPanel("Graphics", graphicsUI("tab3"))
  )


  server <- function(input, output, session) {
    batchAnalysisServer("tab0")
    datasetServer("tab1")
    variablesServer("tab2")
    graphicsServer("tab3")
  }


  if (interactive()) {
    shinyApp(ui, server)
  }
}
