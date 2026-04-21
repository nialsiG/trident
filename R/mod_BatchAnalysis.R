# =============================================================================
# Module: BatchAnalysis
# Description: Shiny module for batch processing of .SUR surface files.
#              Computes surface texture parameters (complexity, height, spatial,
#              topology) with optional heterogeneity metrics and parallelization.
# Author:      Arthur Francisco
# Date:        2026-04-13
# Version:     1.0.0
# =============================================================================

#' BatchAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs enable disabled disable toggleState
#'
mod_BatchAnalysis_ui <- function(id) {
  ns <- NS(id)
  tagList(

    #Help in a verbatim text output
    verbatimTextOutput(ns('console')),

    #Buttons
    #...to select the SUR files for the batch analysis
    shinyFiles::shinyFilesButton(ns("surFiles"), "Choose .SUR files" ,
                                  title = "Please select .SUR files:",
                                  multiple = TRUE),

    hr(),

    fluidRow(
      #Radiobuttons for selection of peak removal method
      column(2,
             em("Peak removal:"),
             radioButtons(ns("peakRemoval"), NULL,
                          choiceNames  = list("none", "2nd order polynomial", "8th order polynomial"),
                          choiceValues = list("0", "2", "8")
             ),
      ),
      #Checkboxes for parameters selection (1/2)
      column(3,
             em("Select parameters:"),
             checkboxInput(ns("complexity"), "Complexity (Asfc2)", TRUE),
             checkboxInput(ns("height"),     "Height parameters",  TRUE),
             checkboxInput(ns("spatial"),    "Spatial parameters", TRUE),
             checkboxInput(ns("topology"),   "Topology",           TRUE),
             checkboxInput(ns("doStd"),      "compute Std? *",     FALSE)
      ),
      #Checkboxes for parameters selection (2/2)
      column(3,
             em("Heterogeneity:"),
             checkboxInput(ns("hComplexity"), "Complexity's heterogenity", FALSE),
             checkboxInput(ns("hHeight"),     "Height's heterogeneity",    FALSE),
             checkboxInput(ns("hSpatial"),    "Spatial heterogeneity",     FALSE),
             checkboxInput(ns("hTopology"),   "Topology's heterogeneity",  FALSE)
      )
    ),

    #Notes (1/2)
    em("* When computing Std, make sure that surfaces have been\nscanned using a standardized direction"),

    hr(),

    fluidRow(
      #Slider inputs
      #...for surface size in pixels
      column(5,
             em("Size in pixels\n(same size along x-axis and y-axis)"),
             sliderInput(ns("xSize"), NULL, min = 64, max = 256, value = 256),
      ),
      #...for number of heterogeneiry cells
      column(5,
             em("Heterogeneity \n(number of grid cells along x-axis and y-axis)"),
             sliderInput(ns("nCell"), NULL, min = 2, max = 16, value = 8)
      )
    ),

    br(),

    #Adding factors
    em("Add factors from file name:"),
    numericInput(ns("nFactors"), "Number of factors", value = 0, min = 0, max = 5),

    br(),

    fluidRow(
      #Placeholders
      #...for factor names
      column(2, uiOutput(ns("nameFactor"))),

      #...for separators
      column(2, uiOutput(ns("separatorFactor"))),

      #...for position in name
      column(2, uiOutput(ns("positionFactor"))),

      #...for prefixes
      column(2, uiOutput(ns("prefixFactor"))),

      #...for suffixes
      column(2, uiOutput(ns("suffixFactor"))),

    ),

    #Buttons (2/2)
    fluidRow(
      #...for launching the batch analysis
      column(2,
        actionButton(
          inputId = ns("launchBatch"),
          label   = tagList(
                        tags$img(src = "www/batch.png", height = "40px"),
                        " Launch"
                           ),
          class = "btn"
        )
      ),

      #...to export dataset into R
      column(2, disabled(tags$button(
                            id = ns("exportR"),
                            div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
                            class = "btn action-button"))),

        #...for saving data as txt
        column(2, disabled(downloadButton(ns("saveTxt"), "Save as .txt"))),

    ),

    #Table output for the results of the batch analysis
    DT::DTOutput(ns("dataset")),

    hr(),

    textOutput(ns("feedback"))

  )

}

#' BatchAnalysis Server Functions
#'
#' @noRd
mod_BatchAnalysis_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Check for fortran executable presence
    OSSYSTEM  <- Sys.info()["sysname"]

    if (OSSYSTEM == "Windows"){

      PrgPath <- system.file("extdata", "structure", "prg", package = "trident")

      if (PrgPath == "") {
        PrgPath <- "inst/extdata/structure/prg/prg.exe"
      } else {
        PrgPath <- paste0(PrgPath, "/prg.exe")
      }

      if ( !file.exists(PrgPath) ) {

        utils::download.file(url = "https://github.com/TRIBO-Pprime/TRIDENT_V1/raw/refs/heads/master/prg.exe", destfile = PrgPath, mode = "wb")

      }

      run <- "prg.exe"

    } else if (OSSYSTEM == "Linux") {

      PrgPath <- system.file("extdata", "structure", "prg", package = "trident")

      if (PrgPath == "") {
        PrgPath <- "inst/extdata/structure/prg/main"
      } else {
        PrgPath <- paste0(PrgPath, "/main")
      }

      if ( !file.exists(PrgPath) ) {

        utils::download.file(url = "https://github.com/TRIBO-Pprime/TRIDENT_V1/raw/refs/heads/master/main", destfile = PrgPath, mode = "wb")
        Sys.chmod(PrgPath, mode = "0755")

      }

      run <- "main"

    }

    # ShinyFiles setup
    volumes = getVolumes()
    shinyFiles::shinyFileChoose(input, "surFiles", roots = volumes,
                                session = session, filetypes = c("sur", "SUR"))

    EXP_DT <- exp_form()

    #Help in the text console----
    output$console <- renderText({textConsole$text})

    #Reactive values----
    batchData        <- reactiveValues(data = NULL)
    textConsole      <- reactiveValues(text = "Please select .sur files")
    selectedSurFiles <- reactiveValues(datapath = NULL)


    #Placeholders renderers----

    #...for factor names
    output$nameFactor <- renderUI({

      n <- input$nFactors

      if (n > 0) h5("Factor name")

      if (is.null(n) || n == 0) return(NULL)

      tagList(
        lapply(1:n, function(i) {
          textInput(
            ns(paste0("nameFactor", i)),
            label = NULL,
            value = paste0("Factor", i)
          )
        })
      )
    })

    #...for separators
    output$separatorFactor <- renderUI({

      n <- input$nFactors

      if (n > 0) h5("Separator")

      if (is.null(n) || n == 0) return(NULL)

      tagList(
        lapply(1:n, function(i) {
          textInput(
            ns(paste0("separatorFactor", i)),
            label = NULL,
            value = "_"
          )
        })
      )
    })

    #...for position
    output$positionFactor <- renderUI({

      n <- input$nFactors

      if (n > 0) h5("Position")

      if (is.null(n) || n == 0) return(NULL)

      tagList(
        lapply(1:n, function(i) {
          numericInput(
            ns(paste0("positionFactor", i)),
            label = NULL,
            value = i,
            min   = 1,
            max   = n
          )
        })
      )
    })

    #...for prefixes
    output$prefixFactor <- renderUI({

      n <- input$nFactors

      if (n > 0) h5("Prefix")

      if (is.null(n) || n == 0) return(NULL)

      tagList(
        lapply(1:n, function(i) {
          textInput(
            ns(paste0("prefixFactor", i)),
            label = NULL,
            value = ""
          )
        })
      )
    })

    #...for suffixes
    output$suffixFactor <- renderUI({

      n <- input$nFactors

      if (n > 0) h5("Suffix")

      if (is.null(n) || n == 0) return(NULL)

      tagList(
        lapply(1:n, function(i) {
          textInput(
            ns(paste0("suffixFactor", i)),
            label = NULL,
            value = ""
          )
        })
      )
    })

    #Render data table----
    output$dataset <- DT::renderDT({
      req(batchData$data)

      Mydf <- batchData$data
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      DT::datatable(Mydf,
                    rownames = FALSE,
                    extensions = c("FixedColumns"),
                    selection = list(target = "column"),
                    options = list(
                        fixedColumns = list(leftColumns = length(Factors)),
                        rowCallback  = DT::JS(EXP_DT)
                        )
                    )
    })

    #Buttons----
    #...For getting surface files----
    observeEvent(input$surFiles,{

      # textConsole$text <-  "Please select .sur files"


      # req(input$surFiles)

      files <- shinyFiles::parseFilePaths(volumes, input$surFiles)

      # Check if any files were selected
      req(nrow(files) > 0)

      selectedSurFiles$datapath <- files$datapath

      textConsole$text <- paste(basename(selectedSurFiles$datapath), collapse = "\n")

    })

    #...For launching batch analysis----
    observeEvent(input$launchBatch, {

      req(length(selectedSurFiles$datapath) > 0) # Ensure that at least one file is selected before proceeding

      sur           <- selectedSurFiles$datapath
      size          <- input$xSize
      nCell         <- input$nCell^2 # We need the total number of cells, which is nCell^2, since nCell is the number of divisions along one axis
      peakRemoval   <- input$peakRemoval
      doComplexity  <- input$complexity
      doHeight      <- input$height
      doSpatial     <- input$spatial
      doStd         <- input$doStd
      doTopology    <- input$topology
      doHComplexity <- input$hComplexity
      doHHeight     <- input$hHeight
      doHSpatial    <- input$hSpatial
      doHTopology   <- input$hTopology

      #Preparation for factor name construction
      nameFactor      <- NULL
      separatorFactor <- NULL
      positionFactor  <- NULL
      prefixFactor    <- NULL
      suffixFactor    <- NULL

      if (input$nFactors > 0){
         for (i in 1:input$nFactors)
         {
           nameFactor[i]      <- input[[paste0("nameFactor",      i)]]
           separatorFactor[i] <- input[[paste0("separatorFactor", i)]]
           positionFactor[i]  <- input[[paste0("positionFactor",  i)]]
           prefixFactor[i]    <- input[[paste0("prefixFactor",    i)]]
           suffixFactor[i]    <- input[[paste0("suffixFactor",    i)]]
         }
       }

      # Parallel loop

      proj_path <- getwd()

      cl  <- snow::makeSOCKcluster(parallel::detectCores() - 1)
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::clusterExport(cl, "proj_path", envir = environment())

      parallel::clusterEvalQ(cl, {
          if (requireNamespace("pkgload", quietly = TRUE) &&
              file.exists(file.path(proj_path, "DESCRIPTION"))) {
              pkgload::load_all(proj_path, quiet = TRUE)
          } else {
              library(trident)
          }
      })

      doSNOW::registerDoSNOW(cl)

      withProgress(message = 'Batch analysis in progress...', value = 0, {

        n_iterations <- length(sur)

        # Callback function to update the progress bar
        # Note : this function will be called from the worker processes, so it needs to be defined in the main process and passed as an option to doSNOW
        progress_callback <- function(n) {
          incProgress(1 / n_iterations, detail = paste("Processing file ", n))
        }

        # Options spécifiques à doSNOW pour la progression
        opts <- list(progress = progress_callback)

        batchAnalysisTable <- foreach::foreach(
          i = 1:n_iterations,
          .packages = c("dplyr"),
          .combine = dplyr::bind_rows,
          .options.snow = opts
        ) %dopar% {

          tryCatch({
            Batch.fun(
              sur           = sur[i],
              size          = size,
              nCell         = nCell,
              peakRemoval   = peakRemoval,
              doComplexity  = doComplexity,
              doHeight      = doHeight,
              doSpatial     = doSpatial,
              doStd         = doStd,
              doTopology    = doTopology,
              doHComplexity = doHComplexity,
              doHHeight     = doHHeight,
              doHSpatial    = doHSpatial,
              doHTopology   = doHTopology
            )
          }, error = function(e) {
                        message("Error on file ", sur[i], " : ", conditionMessage(e))
                        data.frame(File   = basename(sur[i]),
                                   Status = paste("Error: ", conditionMessage(e)))
          })

        }

      })

      # here it is necessary to manually unlist the numerical variables
      if (nrow(batchAnalysisTable) > 1) {

        Numerics <- data.frame(apply(batchAnalysisTable[, -1], 2, unlist))
        Factors  <- data.frame(File = batchAnalysisTable[, 1])

      } else {

        Numerics <- data.frame(File = batchAnalysisTable[, -1])
        Factors  <- data.frame(File = batchAnalysisTable[,  1])
      }

      #A for loop for generating factors, depending on the length of input$nFactors
      if (input$nFactors > 0)
      {
        for (i in 1:input$nFactors)
        {
          Factors <- data.frame(Factors, newFactor = FactorFromFileName(x = Factors$File,
                                                                        separator = separatorFactor[i],
                                                                        position = positionFactor[i],
                                                                        prefix = prefixFactor[i],
                                                                        suffix = suffixFactor[i]))
          colnames(Factors)[which(colnames(Factors) == "newFactor")] <- nameFactor[i]
        }
      }

      # Display dataset
      if (nrow(Factors) == 0 || nrow(Numerics) == 0) {

        batchData$data <- data.frame(File = basename(sur), Status = "Empty Factors or Numerics")

      }else{

        batchData$data   <- data.frame(Factors, Numerics)
      }

    })

    #...for dowloading dataset as txt
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        Mydf <- batchData$data
        Mydf <- unclass(Mydf)
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
        assign(input$name, data.frame(batchData$data), envir = .GlobalEnv)
        exportName$name <- input$name
    })

    #Enable/Disable objects----
    observe({
      if (typeof(input$surFiles) == "list") {
        enable("launchBatch")
      }
      else {
        disable("launchBatch")
      }
    })

    observe({
      if (!is.null(batchData$data)) {
        enable("saveTxt")
        enable("exportR")
      }
      else {
        disable("saveTxt")
        disable("exportR")
      }
    })

  })

}

## To be copied in the UI
# mod_BatchAnalysis_ui("BatchAnalysis_1")

## To be copied in the server
# mod_BatchAnalysis_server("BatchAnalysis_1")
