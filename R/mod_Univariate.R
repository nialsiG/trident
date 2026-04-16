# =============================================================================
# Module: Univariate
# Description: Shiny module for exploring the distribution of a numeric variable
#              across levels of a selected factor. Allows users to visualize data
#              using boxplots or violin plots and export the resulting graphic to
#              the R environment.
# Author:      Arthur Francisco
# Date:        2026-04-13
# Version:     1.0.0
# =============================================================================

#' Univariate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Univariate_ui <- function(id) {
  ns <- NS(id)
  tagList(
      #Help in a verbatim text output
      verbatimTextOutput(ns('text')),
      hr(),
      fluidRow(
          column(
              3,
              #select variables in table
              DT::DTOutput(ns("numericTable"))
          ),
          column(
              3,
              #select factors in table
              DT::DTOutput(ns("factorTable")),
          ),
          #Buttons to choose the type of graph output
          column(
              6,
              #...choose boxplot
              disabled(tags$button(
                  id = ns("boxplot"),
                  div(tags$img(src = "www/boxplot.png", height = "40px"), "Box Plot"),
                  class = "btn action-button")),
              #...choose violin plot
              disabled(tags$button(
                  id = ns("violin"),
                  div(tags$img(src = "www/violin.png", height = "40px"), "Violin Plot"),
                  class = "btn action-button")),
              #...export table into R
              disabled(tags$button(
                  id = ns("exportR"),
                  div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
                  class = "btn action-button")),
              hr(),
              #Graphical output
              plotOutput(ns("plot"), height = 400, width = 400)
          )
      ),
      hr(),

      textOutput(ns("feedback"))
  )
}

#' Univariate Server Functions
#'
#' @noRd
mod_Univariate_server <- function(id, data, variables){
  moduleServer(id, function(input, output, session, dataTrident = data, varTrident = variables){
    ns <- session$ns

    #Help in a verbatim text----
    output$text <- renderText({
        #
        #
        shiny::validate(
            need(dataTrident$value, "Please select a dataset"),
            need(length(input$factorTable_rows_selected)  == 1, "You must select 1 variable as factor"),
            need(length(input$numericTable_rows_selected) == 1, "You must also select 1 variable")
        )

        #
        Mydf <- dataTrident$value

        #
        FactorsCols  <- colnames(dplyr::select(Mydf, where(is.factor)))

        #
        if (!is.null(varTrident$variables)) {
            NumericsCols <- varTrident$variables
        } else {
            NumericsCols <- colnames(dplyr::select(Mydf, where(is.numeric)))
        }

        #
        selectedFactor  <- FactorsCols[input$factorTable_rows_selected]
        selectedNumeric <- NumericsCols[input$numericTable_rows_selected]

        #
        paste0(
            'Row "', input$factorTable_rows_selected,  '" (', selectedFactor,  ') selected as factor\n\n',
            'Row "', input$numericTable_rows_selected, '" (', selectedNumeric, ') selected as variable'
        )
    })

    #Reactive data----
    #...Reactive data to select variables
    dataNumericTrident <- reactive({
        #
        req(dataTrident$value)

        #
        if (!is.null(varTrident$variables)) {
            return(data.frame(variable = as.character(varTrident$variables), stringsAsFactors = FALSE))
        }

        #
        numerics_df <- dataTrident$value %>%
            as.data.frame() %>%
            dplyr::select(where(is.numeric)) %>%
            dplyr::filter(dplyr::if_all(everything(), ~ !is.infinite(.))) %>%
            tidyr::drop_na()

        #
        data.frame(variable = colnames(numerics_df), stringsAsFactors = FALSE)
    })

    #...Reactive data to select factors
    dataFactorTrident <- reactive({
        #
        req(dataTrident$value)

        #
        df_clean <- dataTrident$value %>%
            as.data.frame(stringsAsFactors = TRUE) %>%
            dplyr::filter(dplyr::if_all(where(is.numeric), ~ is.finite(.))) %>%
            tidyr::drop_na()
        #
        Factors <- dplyr::select(df_clean, where(is.factor))

        #
        data.frame(
            variable = colnames(Factors),
            levels   = vapply(Factors, function(x) length(levels(x)), integer(1)),
            stringsAsFactors = FALSE
        )

    })

    #...Reactive values for choosing the type of graph output
    currentPlot <- reactiveValues(plotType = "boxplot", plot = NULL)

    #Tables----
    #...For selecting variables
    output$factorTable <- DT::renderDT({
        req(!is.null(dataTrident$value))
        Mydf <- dataFactorTrident()
        DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...For selecting factors
    output$numericTable <- DT::renderDT({
        req(!is.null(dataTrident$value))
        Mydf <- dataNumericTrident()
        DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #Buttons----
    #...for changing to violin plot
    observeEvent(input$violin, {
        currentPlot$plotType <- "violin"
    })

    #...for changing to boxplot
    observeEvent(input$boxplot, {
        currentPlot$plotType <- "boxplot"
    })

    #Plot----
    output$plot <- renderPlot({

        #no display if no data
        req(!is.null(dataTrident$value))

        #prepare dataset
        Mydf <- dataTrident$value

        #Select only numeric / factor variables
        Numerics <- dplyr::select_if(Mydf, is.numeric)
        Factors  <- dplyr::select_if(Mydf, is.factor)

        #Order variables
        if(!is.null(varTrident$variables))
        {
            Numerics <- Numerics[, varTrident$variables]
        }

        #
        selected_f <- input$factorTable_rows_selected
        selected_n <- input$numericTable_rows_selected

        if (currentPlot$plotType %in% c("violin", "boxplot") & length(selected_f) == 1 & length(selected_n) == 1) {

            #
            factorName <- colnames(Factors)[selected_f]
            varName    <- colnames(Numerics)[selected_n]

            #
            plotData <- data.frame(Factors[, selected_f, drop=F], Numerics[, selected_n, drop=F])
            colnames(plotData) <- c(factorName, varName)

            #
            currentPlot$plot <- renderDistributionPlot(
                df    = plotData,
                x_var = factorName,
                y_var = varName,
                type  = currentPlot$plotType
            )

            currentPlot$plot
        }
    })

    #...for exporting to R button----
    exportName <- reactiveValues()
    observeEvent(input$exportR, {
        # display a modal dialog with a header, textinput and action buttons
        showModal(modalDialog(
            tags$h3('Please enter the name of exported object'),
            textInput(ns('name'), 'Name'),
            footer = tagList(
                actionButton(ns('submit'), 'Submit'),
                modalButton('cancel')
            )
        ))
    })

    # only store the information if the user clicks submit
    observeEvent(input$submit, {
        removeModal()
        myPlot <- currentPlot$plot
        exportName$name <- input$name
        assign(paste0(exportName$name), myPlot, envir = .GlobalEnv)
    })

    #Enable / Disable buttons----
    observe({
        req(dataTrident$value)
        myValue <- dataTrident$value
        if (!is.null(myValue)) {
            if (!is.null(input$factorTable_rows_selected) && !is.null(input$numericTable_rows_selected)) {
                enable("violin")
                enable("boxplot")
            }
            if (is.null(input$factorTable_rows_selected) || is.null(input$numericTable_rows_selected)) {
                disable("violin")
                disable("boxplot")
            }
        }
    })
    observe({
        req(currentPlot$plot)
        if(!is.null(currentPlot$plot))
        {
            enable("exportR")
        }
        else if(is.null(currentPlot$plot))
        {
            disable("exportR")
        }
    })

  })
}

## To be copied in the UI
# mod_Univariate_ui("Univariate_1")

## To be copied in the server
# mod_Univariate_server("Univariate_1")
