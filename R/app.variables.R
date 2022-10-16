################################################################################
variablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns('text')),
    DTOutput(ns("factorTable")),
    hr(),
    actionButton(ns("multiCheck"), "Multi-Check"),
    actionButton(ns("rankBy"), "Rank"),
    actionButton(ns("top3"), "Top-3"),
    hr(),
    DTOutput(ns("numericTable"))
  )
}

################################################################################
variablesServer <- function(id, data) {
  moduleServer(id, function(input, output, session, dataTrident = data) {

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident()
      if (!is.null(Mydf)) {
        if (is.null(input$factorTable_rows_selected)) return('You must select a variable as factor')
        if (!is.null(input$factorTable_rows_selected)) return(paste0('Row ', input$factorTable_rows_selected, ' selected as factor'))
      }
      else return('Please select a dataset')
    })

    #Set variables dataframe----
    dataNumericTrident <- reactive({
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      #Factors  <- dplyr::select_if(Mydf, is.character)
      data.frame(Variables = colnames(Numerics), Type = sapply(Numerics,typeof))
    })
    dataFactorTrident <- reactive({
      Mydf <- dataTrident()
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      #Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      lengthLevels <- function(x) {return(length(levels(x)))}
      data.frame(Variables = colnames(Factors), Levels = sapply(Factors,lengthLevels))
    })


    #multiCheck button----
    observeEvent(input$multiCheck, {

    })

    #rankBy button----
    observeEvent(input$rankBy, {})

    #top3 button----
    observeEvent(input$top3, {})

    output$feedback <- renderText("This is a test")

    #dataTable rendering----
    output$factorTable <- renderDT({
      Mydf <- dataFactorTrident()
      DT::datatable(Mydf,
                    rownames = FALSE,
                    selection = list(target = "row")
      )
    })
    output$numericTable <- renderDT({
      Mydf <- dataNumericTrident()
      DT::datatable(Mydf,
                    rownames = FALSE,
                    selection = list(target = "row")
      )
    })


  })
}
