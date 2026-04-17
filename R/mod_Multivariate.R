# =============================================================================
# Module: MultiVariate
# Description: Shiny module for performing Principal Component Analysis (PCA)
#              on selected numeric variables. Allows visualization through scree
#              plots, correlation circles, and bivariate projections, with support
#              for supplementary variables and individuals.
# Author:      Arthur Francisco
# Date:        2026-04-13
# Version:     1.0.0
# =============================================================================


#' Multivariate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyjs enable disabled disable toggleState
#' @importFrom shiny NS tagList
#'
mod_Multivariate_ui <- function(id) {
  ns <- NS(id)

  tagList(
      #Help in a verbatim text output
      verbatimTextOutput(ns('text')),
      hr(),
      fluidRow(
          column(
              3,
              #select variables in table
              em("Select variables:"),
              DT::DTOutput(ns("numericTable")),
              #select supplementary variables in table
              em("Select supplementary variables:"),
              DT::DTOutput(ns("numericTable2"))
          ),

          column(
              3,
              #select factors in table
              em("Select factor:"),
              DT::DTOutput(ns("factorTable")),
              #select PCs in dropdown lists
              em("Select PCs:"),
              uiOutput(ns("selectPCX")),
              uiOutput(ns("selectPCY")),

              #save PCA in a txt file
              disabled(downloadButton(ns("savePCA"), "Save PCA data"))
          ),
          #Buttons to choose the type of graph output
          column(
              6,
              fluidRow(
                  column(
                      6,
                      #...choose screeplot
                      disabled(tags$button(
                          id = ns("screenplot"),
                          div(tags$img(src = "www/screenPlot.png", height = "40px"), "    Screen Plot     "),
                          class = "btn action-button")),
                      #...choose correlation circle
                      disabled(tags$button(
                          id = ns("corcircle"),
                          div(tags$img(src = "www/corCircle.png", height = "40px"), " Correlation Circle"),
                          class = "btn action-button")),
                      #...choose bivariate plot
                      disabled(tags$button(
                          id = ns("bivariate"),
                          div(tags$img(src = "www/pca.png", height = "40px"), " Bivariate Graph"),
                          class = "btn action-button")),
                      #...to export table into R
                      disabled(tags$button(
                          id = ns("exportR"),
                          div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
                          class = "btn action-button")),
                  ),
                  column(
                      6,
                      #A button to add supplementary individuals
                      fileInput(ns("addIndividuals"), " Add individuals",
                                accept = c(
                                    "text/txt",
                                    "text/plain",
                                    ".txt")),
                      #...to select separators
                      em("Separator:"),
                      radioButtons(ns("txtSeparator"), NULL,
                                   choiceNames  = list("tab", "space"),
                                   choiceValues = list("\t", " ")),
                  )
              ),
              hr(),
              #Graph output
              plotOutput(ns("plot"), height = 400)#, width = 400)
          )
      ),

      #####
      hr(),
      textOutput(ns("feedback"))

  )
}

#' Multivariate Server Functions
#'
#' @noRd
mod_Multivariate_server <- function(id, data, variables){
  moduleServer(id, function(input, output, session, dataTrident = data, varTrident = variables){
    ns <- session$ns

    output$text <- renderText({
        # 1. Gestion des validations (Arrête l'exécution si les conditions ne sont pas remplies)
        shiny::validate(
            need(dataTrident$value, "Please select a dataset"),
            need(length(input$factorTable_rows_selected)  == 1, "You must select 1 variable as factor"),
            need(length(input$numericTable_rows_selected) >= 2, "You must also select at least 2 variables")
        )

        # 2. Préparation des noms de colonnes (sans charger tout le dataframe)
        Mydf <- dataTrident$value

        # Récupération des noms pour les facteurs
        factorCols <- colnames(dplyr::select(Mydf, where(is.factor)))

        # Récupération des noms pour les numériques (logique varTrident respectée)
        if (!is.null(varTrident$variables)) {
            numericCols <- varTrident$variables
        } else {
            numericCols <- colnames(dplyr::select(Mydf, where(is.numeric)))
        }

        # 3. Extraction des éléments sélectionnés
        selectedFactorName <- factorCols[input$factorTable_rows_selected]

        # On récupère tous les noms numériques sélectionnés d'un coup
        selectedNumericNames <- numericCols[input$numericTable_rows_selected]
        selectedNumericRows  <- input$numericTable_rows_selected

        # 4. Construction du texte final
        # Header
        header <- paste0('Row "', input$factorTable_rows_selected, '" (', selectedFactorName, ') selected as factor\n')

        # Corps : On utilise paste() sur les vecteurs pour éviter la boucle for
        body <- paste0(
            'Row "', selectedNumericRows, '" (', selectedNumericNames, ') selected as variable',
            collapse = "\n"
        )

        return(paste0(header, "\n", body))
    })

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

    #...Reactive values for supp individuals
    suppIndividuals <- reactiveValues(value = NULL)

    #...Reactive values ACP
    acpResults <- reactiveValues(data = NULL)

    #...Reactive values for choosing the type of graph output
    currentPlot <- reactiveValues(plotType = "screenplot", plot = NULL)

    #Tables----
    #...Table to select variables
    output$numericTable <- DT::renderDT({
        req(dataTrident$value)
        Mydf <- dataNumericTrident()
        DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...Table to select supplementary variables
    output$numericTable2 <- DT::renderDT({
        req(dataTrident$value)
        Mydf <- dataNumericTrident()
        DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...Table to select factor
    output$factorTable <- DT::renderDT({
        req(dataTrident$value)
        Mydf <- dataFactorTrident()
        DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #Buttons----
    #...for changing to screenplot
    observeEvent(input$screenplot, {
        currentPlot$plotType <- "screenplot"
    })

    #...for changing to correlation circle
    observeEvent(input$corcircle, {
        currentPlot$plotType <- "corcircle"
    })

    #...for changing to bivariate graph
    observeEvent(input$bivariate, {
        currentPlot$plotType <- "bivariate"
    })

    #...for adding individuals
    observeEvent(input$addIndividuals, {

        suppIndividuals$value <- utils::read.delim(input$addIndividuals$datapath,
                                                   header = TRUE,
                                                   sep = input$txtSeparator,
                                                   stringsAsFactors = TRUE) %>%
            dplyr::filter(!is.infinite(rowSums(dplyr::select(., where(is.numeric))))) # Remove rows with infinite values in numeric columns
    })

    create_pc_selector <- function(id, selected) {

        renderUI({

            req(acpResults$data)

            ncp <- ncol(acpResults$data$ind$coord)

            choices <- stats::setNames(seq_len(ncp), paste0("PC", seq_len(ncp)))

            shiny::selectInput(
                id,
                label = NULL,
                choices = choices,
                selected = selected
            )
        })
    }

    output$selectPCX <- create_pc_selector(ns("selectPCX"), 1)
    output$selectPCY <- create_pc_selector(ns("selectPCY"), 2)

    output$plot <- shiny::renderPlot({
        shiny::req(
            !is.null(dataTrident$value),
            length(input$numericTable_rows_selected) > 1,
            length(input$factorTable_rows_selected) == 1
        )

        # ---
        prepared    <- prepare_pca_data(dataTrident$value, varTrident$variables)
        numerics    <- prepared$numerics
        factors     <- prepared$factors

        active_cols <- colnames(numerics)[input$numericTable_rows_selected]
        supp_cols   <- colnames(numerics)[input$numericTable2_rows_selected]
        factor_col  <- colnames(factors)[input$factorTable_rows_selected]

        # ---
        supp_inds <- if (!is.null(suppIndividuals$value)) {
            supp_prepared <- prepare_pca_data(suppIndividuals$value, varTrident$variables)
            supp_num      <- supp_prepared$numerics
            common_cols   <- intersect(active_cols, colnames(supp_num))
            supp_num[, common_cols, drop = FALSE]
        } else {
            NULL
        }

        # --- PCA ---
        acpResults$data <- run_pca(numerics, active_cols = active_cols,
                                             supp_vars   = supp_cols,
                                             supp_inds   = supp_inds)

        pc_x <- as.integer(input$selectPCX)
        pc_y <- as.integer(input$selectPCY)

        # --- Plot ---
        plot_ok <- length(input$factorTable_rows_selected) == 1 &&
                   length(input$numericTable_rows_selected) > 1

        p <- switch(
            currentPlot$plotType,
            screenplot = if (plot_ok) plot_screeplot(acpResults$data),
            bivariate  = if (plot_ok) plot_bivariate(acpResults$data, factors, factor_col, pc_x, pc_y),
            corcircle  = if (plot_ok) plot_corcircle(acpResults$data, pc_x, pc_y)
        )

        currentPlot$plot <- p
        p
    })

    #Download----
    #...PCA Data
    output$savePCA <- downloadHandler(
        filename = function() {paste0("PCA_Data.txt")},
        content = function(file) {
            sink(file)
            cat("Eigenvalues\n")
            print(factoextra::get_eigenvalue(acpResults$data), quote = FALSE)
            cat("\n")
            cat("\n--------------------------------------------------\n")
            cat("\nIndividuals\n")
            cat("Coordinates for the individuals\n")
            print(factoextra::get_pca_ind(acpResults$data)$coord, quote = FALSE)
            cat("Cos2 for the individuals\n")
            print(factoextra::get_pca_ind(acpResults$data)$cos2, quote = FALSE)
            cat("\n")
            cat("Contributions of the individuals\n")
            print(factoextra::get_pca_ind(acpResults$data)$contrib, quote = FALSE)
            cat("\n")
            cat("\n--------------------------------------------------\n")
            cat("\nVariables\n")
            cat("Coordinates for the variables\n")
            print(factoextra::get_pca_var(acpResults$data)$coord, quote = FALSE)
            cat("\n")
            cat("Correlation between variables and dimensions\n")
            print(factoextra::get_pca_var(acpResults$data)$cor, quote = FALSE)
            cat("\n")
            cat("Cos2 for the variables\n")
            print(factoextra::get_pca_var(acpResults$data)$cos2, quote = FALSE)
            cat("\n")
            cat("Contributions of the variables\n")
            print(factoextra::get_pca_var(acpResults$data)$contrib, quote = FALSE)
            cat("\n")
            cat("\n--------------------------------------------------\n")
            cat("\nSupplementary variables\n")
            cat("Coordinates for the variables\n")
            print(acpResults$data$quanti.sup$coord, quote = FALSE)
            cat("Correlation between the variables and the dimensions\n")
            cat("\n")
            print(acpResults$data$quanti.sup$cor, quote = FALSE)
            cat("\n")
            cat("Cos2 for the variables\n")
            print(acpResults$data$quanti.sup$cos2, quote = FALSE)
            cat("\n")
            cat("\n--------------------------------------------------\n")
            cat("\nSupplementary individuals\n")
            cat("Coordinates for the individuals\n")
            print(acpResults$data$ind.sup$coord, quote = FALSE)
            cat("\n")
            cat("Cos2 for the individuals\n")
            print(acpResults$data$ind.sup$cos2, quote = FALSE)
            sink()
        },
        contentType = "text/txt"
    )

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

    observe({

        req(acpResults$data)

        ready <- !is.null(input$factorTable_rows_selected) &&
                 !is.null(input$numericTable_rows_selected)

        ids <- c("screenplot","bivariate","corcircle","addIndividuals","savePCA")

        if (ready) {
            lapply(ids, function(id) enable(id))
        } else {
            lapply(ids, function(id) disable(id))
        }
    })

    observe({
        req(currentPlot$plot)
        if(!is.null(currentPlot$plot))
        {
            enable("exportR")
        }
        else
        {
            disable("exportR")
        }
    })

  })

}

## To be copied in the UI
# mod_Multivariate_ui("Multivariate_1")

## To be copied in the server
# mod_Multivariate_server("Multivariate_1")
