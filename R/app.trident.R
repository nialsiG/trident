#' @title trident.app
#' @description Open the Trident app using Shiny.
#' @export
trident.app <- function()
{
  library(shiny)
  library(data.table)
  library(DT)
  library(shinyjs)
  #library(glue)

  ##############################################################################
  ui <- navbarPage(
    "Trident",
    tabPanel("Batch Analysis", batchAnalysisUI("tab0")),
    tabPanel("Dataset", datasetUI("tab1")),
    tabPanel("Variables", variablesUI("tab2")),
    tabPanel("Graphics", graphicsUI("tab3")),
    tabPanel("PCA", PCAUI("tab4"))
  )

  ##############################################################################
  server <- function(input, output, session) {
    batchAnalysisServer("tab0")
    dataTrident <- datasetServer("tab1")
    variablesServer("tab2", data = dataTrident)
    graphicsServer("tab3")
    PCAServer("tab3")
  }

  if (interactive()) {
    shinyApp(ui, server)
  }
}
