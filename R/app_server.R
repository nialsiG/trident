#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_BatchAnalysis_server("BatchAnalysis_1")
  dataTrident <- mod_DataSet_server("DataSet_1")
  varTrident  <- mod_Variables_server("Variables_1", dataTrident)
  mod_Univariate_server("Univariate_1", data = dataTrident, variables = varTrident)
  mod_Multivariate_server("Multivariate_1", data = dataTrident, variables = varTrident)
}
