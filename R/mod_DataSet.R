# =============================================================================
# Module: DataSet
# Description: A Shiny module designed to explore and evaluate variables from a
#              dataset. It allows users to perform correlation analysis, identify
#              discriminant variables, apply multiple ranking methods, and
#              extract the top variables based on statistical criteria.
# Author:      Arthur Francisco
# Date:        2026-04-13
# Version:     1.0.0
# =============================================================================

#' DataSet UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#@importFrom shiny NS tagList
#' @import shiny shinyjs shinyFiles dplyr
#'
mod_DataSet_ui <- function(id) {
  ns <- NS(id)
  tagList(

      #Help in a verbatim text output
      verbatimTextOutput(ns('text')),

      #Choose dataset
      fileInput(ns("file1"), "Choose .txt File",
                accept = c(
                    "text/txt",
                    "text/plain",
                    ".txt")),
      #Remove NAs?
      checkboxInput(ns("removeNa"), "Remove variables with NA values?", TRUE),

      #Radiobuttons----

      #...to select separators
      em("Separator:"),
      radioButtons(ns("txtSeparator"), NULL,
                   choiceNames  = list("tab", "space"),
                   choiceValues = list("\t", " ")),
      hr(),

      #Buttons----

      #...to save dataset as text
      disabled(downloadButton(ns("saveTxt"), "Save as .txt")),

      #...to export dataset into R
      disabled(tags$button(
          id = ns("exportR"),
          div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
          class = "btn action-button"
      )),

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
      DT::DTOutput(ns("dataset"))
  )
}

#' DataSet Server Functions
#'
#' @noRd
mod_DataSet_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    EXP_DT      <- exp_form()

    #Help in a verbatim text----
    output$text <- renderText({

        if(v$isComputing) return('Wait, computation in progress...')

        if (is.null(input$file1)) {
            return("Please select a dataset")
        }

        if (is.null(input$dataset_columns_selected)) {
            return("You must select a variable as factor")
        }

        Mydf <- data.frame(unclass(dataTrident$value), stringsAsFactors = TRUE)

        col_id <- input$dataset_columns_selected + 1

        if (is.factor(Mydf[, col_id])) {
            paste0("Column ", col_id, " selected as factor")
        } else {
            paste0("Column ", col_id, ": Selected variable is not a factor")
        }

    })

    #Reactive data----
    #...dataset
    dataTrident       <- reactiveValues(value = NULL)
    #...logicals
    isLogtransform    <- reactiveValues(value = FALSE)
    isBoxCoxtransform <- reactiveValues(value = FALSE)

    #Buttons----
    #...for uploading dataset
    observeEvent(input$file1, {

        req(input$file1)

        Mydf <- utils::read.delim(input$file1$datapath,
                                  header = TRUE,
                                  sep = input$txtSeparator,
                                  stringsAsFactors = TRUE) %>%
                           dplyr::filter(!is.infinite(rowSums(dplyr::select(., where(is.numeric))))) # Remove rows with infinite values in numeric columns


        Numerics <- dplyr::select(Mydf, where(is.numeric))
        Factors  <- dplyr::select(Mydf, where(is.factor))

        isLogtransform$value <- FALSE

        if (isTRUE(input$removeNa)) {
            Numerics <- dplyr::select(Numerics, where(~ all(!is.na(.)))) # ~: This starts an anonymous function (or "formula" style function).
        }                                                                # where(~ all(!is.na(.))) checks each column (.) in the Numerics data frame to see if it contains any NA values.
                                                                         # If a column has no NA values, it is retained; otherwise, it is removed.
        dataTrident$value <- dplyr::bind_cols(Factors, Numerics)

    })

    #...for dowloading dataset as txt
    output$saveTxt <- downloadHandler(
        filename = function() {
            paste0("untitled.txt")
        },
        content = function(file) {
            Mydf <- dataTrident$value

            utils::write.table(Mydf, file, quote = FALSE, sep = "\t", row.names = FALSE)
        }
    )

    #...for exporting to R button----
    exportName <- reactiveValues()

    observeEvent(input$exportR, {
        showModal(modalDialog(
            tags$h3("Please enter the name of the exported object"),
            textInput(ns("name"), "Name"),
            footer = tagList(
                actionButton(ns("submit"), "Submit"),
                modalButton("Cancel")
            )
        ))
    })

    observeEvent(input$submit, {
        removeModal()
        assign(input$name, data.frame(v$data), envir = .GlobalEnv)
        exportName$name <- input$name
    })

    #...Reactive value for computing state
    v <- reactiveValues(isComputing = FALSE)

    #...for BoxCox transformation
    observeEvent(input$boxCoxTransform, {

        v$isComputing <- TRUE
        Mydf          <- dataTrident$value
        Factors       <- dplyr::select_if(Mydf, is.factor)
        Numerics      <- dplyr::select_if(Mydf, is.numeric)

        #BoxCox transform
        myBoxcox <- trident.boxcox(df = Numerics, y = factor(Mydf[, input$dataset_columns_selected + 1]))

        colnames(myBoxcox$boxcox) <- paste(colnames(myBoxcox$boxcox), "bc", sep = ".")

        dataTrident$value <- data.frame(Factors, myBoxcox$boxcox)

        #End
        v$isComputing <- FALSE
    })


    observeEvent(input$logTransform, {

        req(dataTrident$value)

        v$isComputing <- TRUE

        # separate factors and numerics, and apply log transformation only to numeric columns
        new_df <- dataTrident$value %>%
            dplyr::mutate(dplyr::across(where(is.numeric), ~ {

                val <- . - min(., na.rm = TRUE) + 1 # x - min + 1
                log10(val)

            })) %>%
            dplyr::rename_with(~ paste0(., ".log10"), where(is.numeric))

        dataTrident$value    <- new_df
        isLogtransform$value <- TRUE
        v$isComputing        <- FALSE

    })


    #Table----
    output$dataset <- DT::renderDT({

        #no display if no data
        req(dataTrident$value)

        #convert data
        Mydf     <- dataTrident$value
        Factors  <- dplyr::select_if(Mydf, is.factor)

        DT::datatable(Mydf,
                      rownames   = FALSE,
                      extensions = c("FixedColumns"),
                      selection  = list(target = "column",
                                        mode   = "single"),
                      options    = list(fixedColumns = list(leftColumns = length(Factors)),
                                        rowCallback  = DT::JS(EXP_DT))
                      )
    })

    #Enable/Disable buttons----
    observe({

        if (!is.null(input$file1)){

            enable("saveTxt")
            enable("exportR")
            enable("logTransform")

            Mydf <- dataTrident$value

            is_factor_selected <- !is.null(input$dataset_columns_selected) &&
                is.factor(Mydf[[input$dataset_columns_selected + 1]])

            toggleState("boxCoxTransform", condition = !isLogtransform$value && is_factor_selected)

        }

    })

    #export the dataset----
    return(dataTrident)     #CRUCIAL FOR THE OTHER MODULES


  })
}

## To be copied in the UI
# mod_DataSet_ui("DataSet_1")

## To be copied in the server
# mod_DataSet_server("DataSet_1")
