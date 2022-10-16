################################################################################
datasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    verbatimTextOutput(ns('text')),

    hr(),

    fileInput(ns("file1"), "Choose .txt File",
              accept = c(
                "text/txt",
                "text/plain",
                ".txt")),

    hr(),

    disabled(downloadButton(ns("saveTxt"), "Save as .txt")),
    disabled(actionButton(ns("exportR"), "Export to R")),
    disabled(actionButton(ns("boxCoxTransform"), "Box-Cox Transform")),
    disabled(actionButton(ns("logTransform"), "Log10 Transform")),

    DTOutput(ns("dataset"))
  )
}

################################################################################
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      if (!is.null(input$file1)) {
        if (is.null(input$dataset_columns_selected)) return('You must select a variable as factor')
        if (!is.null(input$dataset_columns_selected)){
          if (!is.factor(Mydf[, input$dataset_columns_selected + 1])) return(paste0('Column ', input$dataset_columns_selected + 1,': Selected variable is not a factor'))
          if (is.factor(Mydf[, input$dataset_columns_selected + 1])) return(paste0('Column ', input$dataset_columns_selected + 1, ' selected as factor'))
        }
      }
      else return('Please select a dataset')
    })


    #Upload file----
    #Set data as a reactive object
    dataTrident <- eventReactive(input$file1, {
      Mydf <- read.delim(input$file1$datapath, header = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factors  <- dplyr::select_if(Mydf, is.character)
      data.frame(Factors, Numerics)
    })

    #Render datatable----
    output$dataset <- renderDT({
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      DT::datatable(Mydf,
                    rownames = FALSE,
                    extensions = c("FixedColumns"),
                    selection = list(target = "column"),
                    options = list(
                      fixedColumns = list(leftColumns = length(Factors))
                      )
                    )
    })


    #Buttons----
    #dowload dataset as txt----
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        Mydf <- dataTrident()
        Mydf <- unclass(Mydf)
        write.table(Mydf, file, quote = FALSE, sep = "\t")
      }
    )

    #export to R button----
    observeEvent(input$exportR, {
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      assign(paste0(testName <- "HelloWorld"), Mydf, envir = .GlobalEnv)
    })

    #boxCoxTranform button----
    observeEvent(input$boxCoxTransform, {
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)

      myBoxcox <- trident::trident.boxcox(df = Numerics, y = Mydf[, input$dataset_columns_selected + 1])
      colnames(myBoxcox$boxcox) <- paste(colnames(myBoxcox$boxcox), "boxcox", sep = ".")
      data.frame(Factors, myBoxcox$boxcox)
    })

    #logTransform button----
    observeEvent(input$logTransform, {
      dataTrident()
    })


    #Enable/Disable buttons----
    observe({
      if (!is.null(input$file1)){
        enable("saveTxt")
        enable("exportR")
        enable("logTransform")

        Mydf <- dataTrident()
        Mydf <- unclass(Mydf)
        Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)

        if (!is.null(input$dataset_columns_selected) && is.factor(Mydf[, input$dataset_columns_selected + 1]))
        {
          #DEBUG disabled until boxcox function is fixed
          #enable("boxCoxTransform")
        }
        else disable("boxCoxTransform")
      }
    })

    #export the dataset----
    #CRUCIAL FOR THE OTHER MODULES
    return(dataTrident)
  })
}

