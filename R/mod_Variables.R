# =============================================================================
# Module: Variables
# Description: This module provides an interactive interface to explore and
#              analyze variables from a dataset. Users can select a factor
#              variable and perform multiple statistical analyses on numeric
#              variables, including correlation analysis, discriminant
#              assessment (multicheck), and variable ranking based on several
#              statistical criteria (e.g., ANOVA, Kruskal-Wallis, HSD, LSD).
#
#              The module also allows identification of the top 3 variables
#              most associated with the selected factor, using a combination
#              of statistical metrics. Additional options enable filtering of
#              non-discriminant variables, ranking by number of discriminant
#              groups, and computation of geometric means.
#
#              Results can be visualized in interactive tables, exported as
#              text files, or saved directly into the R environment.
# Author:      Arthur Francisco
# Date:        2026-04-13
# Version:     1.0.0
# =============================================================================

#' Variables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Variables_ui <- function(id) {
  ns <- NS(id)
  tagList(

      verbatimTextOutput(ns("text")),
      DT::DTOutput(ns("factorTable")),
      hr(),
      fluidRow(
          column(4,
                 disabled(downloadButton(ns("saveTxt"), "Save as .txt")),
                 disabled(tags$button(
                     id = ns("exportR"),
                     div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
                     class = "btn action-button"
                 )),
                 disabled(tags$button(
                     id = ns("correlation"),
                     div(tags$img(src = "www/correlation.png", height = "40px"), "Correlation"),
                     class = "btn action-button"
                 )),
                 disabled(tags$button(
                     id = ns("multiCheck"),
                     div(tags$img(src = "www/multicheck.png", height = "40px"), "Multi-Check"),
                     class = "btn action-button"
                 )),
                 disabled(tags$button(
                     id = ns("top3"),
                     div(tags$img(src = "www/top3.png", height = "40px"), "Top-3"),
                     class = "btn action-button"
                 )),
                 em("*Top3 automatically performs a Box-Cox transformation and should be reserved for untransformed data."),
                 em("**Consider saving the results of your Top3, as it will not affect variable order in other tabs.")
          ),
          column(4,
                 disabled(tags$button(
                     id = ns("rankBy"),
                     div(tags$img(src = "www/rankby.png", height = "40px"), "Rank by..."),
                     class = "btn action-button"
                 )),
                 radioButtons(
                     ns("rankBySelect"), NULL,
                     choiceNames  = list("P-value (ANOVA)", "P-value (Kruskall)", "Average HSD",
                                         "Pairwise HSD", "Average LSD", "Pairwise LSD"),
                     choiceValues = list("0", "1", "2", "3", "4", "5")
                 )
          ),
          column(4,
                 checkboxInput(ns("byNGroups"),      "Rank by number of discriminant groups?", TRUE),
                 checkboxInput(ns("geomean"),        "Compute geometric mean?",                FALSE),
                 checkboxInput(ns("removeNonDisc"),  "Remove non-discriminant variables?",     FALSE),
                 uiOutput(ns("pairsInput"))
          )
      ),
      DT::DTOutput(ns("varTable")),
      hr(),
      textOutput(ns("feedback"))

  )
}


#' Variables Server Functions
#'
#' @noRd
mod_Variables_server <- function(id, data){
  moduleServer(id, function(input, output, session, dataTrident = data){
    ns <- session$ns

    EXP_DT    <- exp_form()

    # retrieve and clean data
    cleanData <- reactive({

        req(dataTrident$value)

        dataTrident$value %>%
            dplyr::mutate(across(where(is.character), as.factor)) %>%
            dplyr::filter(if_all(where(is.numeric), is.finite)) %>%
            tidyr::drop_na()
    })

    # -------------------------------------------------------------------------
    # Mapping rankBySelect (correction 8 : table au lieu de if/else if)
    # -------------------------------------------------------------------------
    rankMethods <- c(
        "0" = "aov.p.value",      "1" = "k.p.value",
        "2" = "hsd.mean.p.value", "3" = "hsd.p.value",
        "4" = "lsd.mean.p.value", "5" = "lsd.p.value"
    )

    pairwiseMethods <- c("3", "5")

    # -------------------------------------------------------------------------
    # Reactive values
    # -------------------------------------------------------------------------
    v           <- reactiveValues(data = NULL, isComputing = FALSE)
    varTrident  <- reactiveValues(variables = NULL)

    # -------------------------------------------------------------------------
    # Help in a verbatim text
    # -------------------------------------------------------------------------
    output$text <- renderText({
        df <- dataTrident$value
        if (v$isComputing)                            return("Wait, computation in progress...")
        if (is.null(df))                              return("Please select a dataset")
        if (is.null(input$factorTable_rows_selected)) return("You must select a variable as factor")
        paste0("Row ", input$factorTable_rows_selected, " selected as factor")
    })

    # -------------------------------------------------------------------------
    # Reactive data tables for numeric and factor variables
    # -------------------------------------------------------------------------
    dataNumericTrident <- reactive({
        df <- cleanData()
        data.frame(variable = colnames(getNumeric(df)))
    })

    dataFactorTrident <- reactive({
        df      <- cleanData()
        Factors <- dplyr::select_if(df, is.factor)
        data.frame(
            variable = colnames(Factors),
            levels   = sapply(Factors, function(x) length(levels(x)))
        )
    })

    # -------------------------------------------------------------------------
    # Dynamic UI for pairwise comparison methods
    # -------------------------------------------------------------------------
    output$pairsInput <- renderUI({

        req(input$rankBySelect %in% pairwiseMethods)
        req(input$factorTable_rows_selected)

        df     <- cleanData()
        Factor <- getFactor(df, input$factorTable_rows_selected)
        pairs  <- utils::combn(levels(as.factor(Factor)), 2,
                               paste, collapse = " vs. ", simplify = TRUE)
        choices <- stats::setNames(as.list(seq_along(pairs)), pairs)

        selectInput(ns("pairsInput"), label = NULL, choices = choices, selected = 1)

    })

    # -------------------------------------------------------------------------
    # Correlation table
    # -------------------------------------------------------------------------
    observeEvent(input$correlation, {
        safeCompute({

            df <- cleanData()

            ct <- df %>%
                getNumeric() %>%
                cleanNumerics(margin = 2) %>%
                dplyr::select(where(~ sd(.x, na.rm = TRUE) > 0)) %>%
                picante::cor.table(cor.method = "pearson")

            ct$r <- round(ct$r, 4)

            sig  <- function(p_thr, mark) {
                ct$r[ct$P < p_thr] <<- paste0(ct$r[ct$P < p_thr], mark)
            }

            ct$r[ct$P >= 0.05] <- "NS" # Non-significant results are labeled "NS"

            sig(0.05,  "*") # Significant results at the 0.05 level are labeled with one asterisk (*)
            sig(0.01,  "*") # Significant results at the 0.01 level are labeled with two asterisks (**)
            sig(0.001, "*") # Significant results at the 0.001 level are labeled with three asterisks (***)

            v$data <- data.frame(Variable = colnames(ct$r), ct$r)

        }, status = v$isComputing)
    })

    # -------------------------------------------------------------------------
    # Multicheck performed on all numeric variables
    # -------------------------------------------------------------------------
    observeEvent(input$multiCheck, {

        req(input$factorTable_rows_selected)

        safeCompute({
            df       <- cleanData()
            Numerics <- getNumeric(df)
            Factor   <- getFactor(df, input$factorTable_rows_selected)

            cols     <- Numerics %>%
                dplyr::select(dplyr::where(\(x) stats::var(x, na.rm = TRUE) > 0)) %>%
                names() # exclude 0 variance columns

            Numerics <- Numerics[cols]

            v$data   <- data.frame(multicheck(df = Numerics, y = Factor))
        }, status = v$isComputing)

        v$data <- cleanNumerics(v$data, margin = 1)

    })

    # -------------------------------------------------------------------------
    # Rank variables by selected method
    # -------------------------------------------------------------------------
    observeEvent(input$rankBy, {

        req(input$factorTable_rows_selected)

        safeCompute({

            df       <- cleanData()
            Numerics <- getNumeric(df)
            Factor   <- getFactor(df, input$factorTable_rows_selected)

            cols     <- Numerics %>%
                dplyr::select(dplyr::where(\(x) stats::var(x, na.rm = TRUE) > 0)) %>%
                names() # exclude 0 variance columns

            Numerics <- Numerics[cols]

            if (input$removeNonDisc) {

                mc        <- multicheck(df = Numerics, y = Factor)
                disc_idx  <- which(as.logical(mc$is.discriminant))

                if (length(disc_idx) == 0) {
                    showModal(modalDialog(
                        title = "Ranking procedure has been stopped",
                        "No discriminant variable found; please uncheck 'Remove non-discriminant variables' or try a new dataset."
                    ))
                    return()
                }

                if (length(disc_idx) < 3) {
                    showModal(modalDialog(
                        title = "Ranking procedure has been stopped",
                        "Less than 3 discriminant variables found; please uncheck 'Remove non-discriminant variables' or try a new dataset."
                    ))
                    return()
                }

                Numerics <- Numerics[, disc_idx, drop = FALSE]
            }

            rankByMethod <- rankMethods[input$rankBySelect]

            # Priorité des groupes
            gpPriority <- seq_len(length(levels(Factor)))

            if (input$rankBySelect %in% pairwiseMethods) {
                allPairs       <- utils::combn(levels(as.factor(Factor)), 2,
                                               paste, collapse = " vs. ", simplify = TRUE)
                currentPair    <- unlist(strsplit(allPairs[as.numeric(input$pairsInput)], " vs. "))
                myLevels       <- levels(as.factor(Factor))
                gpPriority     <- c(which(myLevels %in% currentPair),
                                    which(!(myLevels %in% currentPair)))
            }

            RankBy <- trident.arrange(
                df          = Numerics,
                y           = Factor,
                by          = rankByMethod,
                byngr       = input$byNGroups,
                geomean     = input$geomean,
                gp.priority = gpPriority
            )

            v$data               <- data.frame(variable = rownames(RankBy), RankBy)
            varTrident$variables <- v$data$variable

        }, status = v$isComputing)

    })

    # -------------------------------------------------------------------------
    # Top 3 variables most associated with the factor, based on a combination of multiple criteria
    # -------------------------------------------------------------------------
    observeEvent(input$top3, {

        req(input$factorTable_rows_selected)

        safeCompute({
            df       <- cleanData()
            Numerics <- getNumeric(df)
            Factor   <- getFactor(df, input$factorTable_rows_selected)

            cols     <- Numerics %>%
                dplyr::select(dplyr::where(\(x) stats::var(x, na.rm = TRUE) > 0)) %>%
                names() # exclude 0 variance columns

            Numerics <- Numerics[cols]

            Mytop    <- trident.top3(df = Numerics, y = Factor)

            if (is.null(Mytop)) {
                showModal(modalDialog(
                    title = "Top 3 procedure has been stopped",
                    "No discriminant variable found; please try a new dataset."
                ))
                return()
            }

            ranked   <- Mytop$ranked
            v$data   <- data.frame(variable = rownames(ranked), ranked)

        }, status = v$isComputing)
    })

    # -------------------------------------------------------------------------
    # Tables
    # -------------------------------------------------------------------------
    output$factorTable <- DT::renderDT({
        DT::datatable(dataFactorTrident(), rownames = FALSE,
                      selection = list(target = "row"))
    })

    output$varTable <- DT::renderDT({

        req(!is.null(v$data))

        df <- data.frame(v$data)

        dt <- DT::datatable(
            df,
            rownames   = FALSE,
            extensions = "FixedColumns",
            options    = list(fixedColumns = list(leftColumns = 1),
                              rowCallback  = DT::JS(EXP_DT))
        )

        # discriminant variables are highlighted in green, non-discriminant in pink
        if ("is.discriminant" %in% colnames(df)) {
            dt <- dt %>%
                DT::formatStyle(
                    "is.discriminant",
                    backgroundColor = DT::styleEqual(
                        c("FALSE", "TRUE"), c("lightpink", "lightgreen")
                    )
                )
        }

        dt <- dt %>%
            DT::formatSignif(
                columns    = names(df)[sapply(df, is.numeric)],
                digits     = 4
            )
        dt
    })

    # -------------------------------------------------------------------------
    # Sauvegarde / Export
    # -------------------------------------------------------------------------
    output$saveTxt <- downloadHandler(
        filename = function() "untitled.txt",
        content  = function(file) {
            utils::write.table(data.frame(v$data), file,
                        quote = FALSE, row.names = FALSE, sep = "\t")
        }
    )

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

    # -------------------------------------------------------------------------
    # Enable / Disable
    # -------------------------------------------------------------------------
    observe({
        req(!is.null(dataTrident$value))
        hasFactor <- !is.null(input$factorTable_rows_selected)

        enable("saveTxt")
        enable("exportR")
        enable("correlation")

        toggleState("multiCheck", hasFactor)
        toggleState("top3",       hasFactor)
        toggleState("rankBy",     hasFactor)
    })

    # -------------------------------------------------------------------------
    # Export inter-modules
    # -------------------------------------------------------------------------
    return(varTrident)

  })
}

## To be copied in the UI
# mod_Variables_ui("Variables_1")

## To be copied in the server
# mod_Variables_server("Variables_1")
