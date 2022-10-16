################################################################################
batchAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file2"), "Please select surface files:"),

    radioButtons(ns("peakRemoval"), "Peak removal:",
                 choiceNames = list(
                   "none", "2nd polynome", "8th polynome"                 ),
                 choiceValues = list(
                   "0", "2", "8"
                 )),

    h5("Parameters:"),

    fluidRow(
      column(3,
             checkboxInput(ns("complexity"), "Complexity", TRUE),
             checkboxInput(ns("height"), "Height parameters", TRUE),
             checkboxInput(ns("spatial"), "Spatial parameters", TRUE),
             checkboxInput(ns("topology"), "Topology", TRUE)
      )
    ),

    fluidRow(
      column(3,
             checkboxInput(ns("hComplexity"), "Complexity's heterogenity", FALSE),
             checkboxInput(ns("hHeight"), "Height's heterogeneity", FALSE),
             checkboxInput(ns("hSpatial"), "Spatial heterogeneity", FALSE),
             checkboxInput(ns("hTopology"), "Topology's heterogeneity", FALSE)
      )
    ),

    br(),

    sliderInput(ns("xSize"), "x-y size:",
                min = 64, max = 256,
                value = 256),
    sliderInput(ns("nCell"), "Heterogeneity - Number of grid cells:",
                min = 1, max = 256,
                value = 64),

    hr(),

    actionButton(ns("launchBatch"), "Launch Batch Analysis"),

    textOutput(ns("txt")),
    tableOutput(ns("values")),
  )
}

################################################################################
batchAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #Launcg batch analysis button----
    observeEvent(input$launchBatch, {


    })

    #DEBUG LOG----
    output$txt <- renderText({
      paste("DEBUG LOG", input$peakRemoval)
    })
    sliderValues <- reactive({
      data.frame(
        Name = c("x-size",
                 "n cells"),
        Value = as.character(c(input$xSize,
                               input$nCell),
        stringsAsFactors = FALSE))
    })
    output$values <- renderTable({
      sliderValues()
    })


  })
}
