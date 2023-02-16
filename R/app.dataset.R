#UI----
datasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    #Help in a verbatim text output
    verbatimTextOutput(ns('text')),
    #Choose dataset
    fileInput(ns("file1"), "Choose .txt File",
              accept = c(
                "text/txt",
                "text/plain",
                ".txt")),
    #Radiobuttons----

    #...to select separators
    em("Separator:"),
    radioButtons(ns("txtSeparator"), NULL,
                 choiceNames = list("tab", "space"),
                 choiceValues = list("0", "1")),
    hr(),
    #Buttons----
    #...to save dataset as text
    disabled(downloadButton(ns("saveTxt"), "Save as .txt")),
    #...to export dataset into R
    disabled(tags$button(
      id = ns("exportR"),
      div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
      class = "btn action-button")),
    #...to log transform data
    disabled(tags$button(
      id = ns("logTransform"),
      div(tags$img(src = "www/log10.png", height = "40px"), "Log10 Transform "),
      class = "btn action-button")),
    #...to perform BoxCox transform on data
    disabled(tags$button(
      id = ns("boxCoxTransform"),
      div(tags$img(src = "www/boxcox.png", height = "40px"), "Box-Cox Transform"),
      class = "btn action-button")),
    #Table displaying the dataset
    DTOutput(ns("dataset"))
  )
}

#SERVER----
datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident$value
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

    #Reactive data----
    #...dataset
    dataTrident <- reactiveValues(value = NULL)
    #...logicals
    isLogtransform <- reactiveValues(value = FALSE)
    isBoxCoxtransform <- reactiveValues(value = FALSE)

    #Buttons----
    #...for uploading dataset
    observeEvent(input$file1, {
        if(input$txtSeparator == 0) txtSeparator = "\t"
        else if (input$txtSeparator == 1) txtSeparator = " "

        Mydf <- read.delim(input$file1$datapath, header = TRUE, sep = txtSeparator)
        Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
        Mydf <- stats::na.omit(Mydf)
        Numerics <- dplyr::select_if(Mydf, is.numeric)
        Factors  <- dplyr::select_if(Mydf, is.character)
        #
        isLogtransform$value <- FALSE
        #
        dataTrident$value <- data.frame(Factors, Numerics)
      })

    #...for dowloading dataset as txt
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        Mydf <- dataTrident$value
        Mydf <- unclass(Mydf)
        write.table(Mydf, file, quote = FALSE, sep = "\t", row.names = FALSE)
      }
    )

    #...for exporting to R button----
    observeEvent(input$exportR, {
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      assign(paste0(testName <- "datasetTrident"), Mydf, envir = .GlobalEnv)
    })

    #...for BoxCox transformation
    observeEvent(input$boxCoxTransform, {
      Mydf <- dataTrident$value
      Factors <- dplyr::select_if(Mydf, is.character)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      #
      myBoxcox <- trident::trident.boxcox(df = Numerics, y = factor(Mydf[, input$dataset_columns_selected + 1]))
      colnames(myBoxcox$boxcox) <- paste(colnames(myBoxcox$boxcox), "boxcox", sep = ".")
      dataTrident$value <- data.frame(Factors, myBoxcox$boxcox)
    })

    #...for log transformation
    observeEvent(input$logTransform, {
      Mydf <- dataTrident$value
      Factors <- dplyr::select_if(Mydf, is.character)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      #
      isLogtransform$value <- TRUE
      #
      for (i in length(colnames(Numerics))) {
        colMin = min(Numerics[, i])
        Numerics[, i] = Numerics[, i] + colMin + 1
      }
      #
      Numerics <- log10(Numerics)
      #
      colnames(Numerics) <- paste(colnames(Numerics), "log10", sep = ".")
      dataTrident$value <- data.frame(Factors, Numerics)
    })

    #Table----
    output$dataset <- renderDT({
      #no display if no data
      req(!is.null(dataTrident$value))
      #convert data
      Mydf <- dataTrident$value
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

    #Enable/Disable buttons----
    observe({
      if (!is.null(input$file1)){
        enable("saveTxt")
        enable("exportR")
        enable("logTransform")
        Mydf <- dataTrident$value
        Mydf <- unclass(Mydf)
        Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
        if (!isLogtransform$value)
        {
          #DEBUG disabled until boxcox function is fixed
          enable("logTransform")
        }
        else disable("logTransform")
        if (!is.null(input$dataset_columns_selected) && is.factor(Mydf[, input$dataset_columns_selected + 1]))
        {
          #DEBUG disabled until boxcox function is fixed
          enable("boxCoxTransform")
        }
        else disable("boxCoxTransform")
      }
    })

    #export the dataset----
    return(dataTrident)     #CRUCIAL FOR THE OTHER MODULES
  })
}

