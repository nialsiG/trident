#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

library(shinyjs)
library(shinyFiles)

app_ui <- function(request) {
    tagList(
        golem_add_external_resources(),
        fluidPage(
            shinyjs::useShinyjs(),
            navbarPage(
                title = div(img(src = "www/trident.gif"), "Trident"),
                theme = "www/style.css",
                tabPanel("Batch Analysis", mod_BatchAnalysis_ui("BatchAnalysis_1")),
                tabPanel("Dataset", mod_DataSet_ui("DataSet_1")),
                tabPanel("Variables", mod_Variables_ui("Variables_1")),
                navbarMenu("Graphics",
                           tabPanel("Univariate", mod_Univariate_ui("Univariate_1")),
                           tabPanel("Multivariate", mod_Multivariate_ui("Multivariate_1")))
            )
        )
    )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "trident"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
