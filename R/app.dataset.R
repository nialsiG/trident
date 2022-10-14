datasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("saveTxt"), "Save as .txt"),
    actionButton(ns("exportR"), "Export to R"),

    hr(),

    fileInput(ns("file1"), "Choose .txt File",
              accept = c(
                "text/txt",
                "text/plain",
                ".txt")),
    checkboxInput(ns("header"), "Header", TRUE),

    DTOutput(ns("dataset"))
  )
}


datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    dataTrident <- NULL

    #dataTable rendering----
    output$dataset <- renderDT({
      inFile <- input$file1
        if (is.null(inFile))
          return(NULL)
      dataTrident <<- read.delim(inFile$datapath, header = input$header)
      })

    #dowload dataset as txt----
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        write.table(dataTrident, file, quote = FALSE, sep = "\t")
      }
    )

    #export to R button----
    observeEvent(input$exportR, {
      assign(paste0(testName <- "HelloWorld"), dataTrident, envir = .GlobalEnv)
    })


  })
}

