#' @title trident.app
#' @description Open the Trident app using Shiny.
#' @export
trident.app <- function()
{
  library(data.table)
  library(DT)
  library(shinyjs)
  library(shiny)
  library(shinyFiles)
  library(foreach) #needed for the %dopar% operator

  .onAttach <- function(libname, pkgname) {
    shiny::addResourcePath(libname,
                           system.file(libname,
                                       package = pkgname))
  }

  .onAttach('www', 'trident')

  # Increase the limit of file size to be uploaded
  options(shiny.maxRequestSize=10000*1024^2)
  options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

  ##############################################################################
  ui <- navbarPage(
    title=div(img(src="www/trident.gif", ), "Trident"),
    theme = "www/style.css",
    # Tabs----
    tabPanel("Batch Analysis", batchAnalysisUI("tab0")),
    tabPanel("Dataset", datasetUI("tab1")),
    tabPanel("Variables", variablesUI("tab2")),
    navbarMenu("Graphics",
               tabPanel("Univariate", univariateUI("tab3")),
               tabPanel("Multivariate", multivariateUI("tab4")))
  )

  ##############################################################################
  server <- function(input, output, session) {
    batchAnalysisServer("tab0")
    dataTrident <- datasetServer("tab1")
    varTrident <- variablesServer("tab2", data = dataTrident)
    univariateServer("tab3", data = dataTrident, variables = varTrident)
    multivariateServer("tab4", data = dataTrident, variables = varTrident)
  }

  if (interactive()) {
    shinyApp(ui, server)
  }
}
