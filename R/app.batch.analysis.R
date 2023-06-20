#UI----
batchAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    #Help in a verbatim text output
    verbatimTextOutput(ns('console')),
    #Buttons
    #...to select the SUR files for the batch analysis
    shinyFilesButton(ns("surFiles"), "Choose .SUR files" ,
                              title = "Please select .SUR files:",
                              multiple = TRUE),
    hr(),
    fluidRow(
      #Radiobuttons for selection of peak removal method
      column(2,
             em("Peak removal:"),
             radioButtons(ns("peakRemoval"), NULL,
                          choiceNames = list("none", "2nd order polynomial", "8th order polynomial"),
                          choiceValues = list("0", "2", "8")
             ),
      ),
      #Checkboxes for parameters selection (1/2)
      column(3,
             em("Select parameters:"),
             checkboxInput(ns("complexity"), "Complexity (Asfc2)", TRUE),
             checkboxInput(ns("height"), "Height parameters", TRUE),
             checkboxInput(ns("spatial"), "Spatial parameters", TRUE),
             checkboxInput(ns("topology"), "Topology", TRUE),
             checkboxInput(ns("doStd"), "compute Std? *", FALSE)
      ),
      #Checkboxes for parameters selection (2/2)
      column(3,
             em("Heterogeneity:"),
             checkboxInput(ns("hComplexity"), "Complexity's heterogenity", FALSE),
             checkboxInput(ns("hHeight"), "Height's heterogeneity", FALSE),
             checkboxInput(ns("hSpatial"), "Spatial heterogeneity", FALSE),
             checkboxInput(ns("hTopology"), "Topology's heterogeneity", FALSE)
             #checkboxInput(ns("doMin"), "compute min? **", FALSE),
             #checkboxInput(ns("doMax"), "compute max? **", FALSE)
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
      column(2,
             uiOutput(ns("nameFactor")),
             uiOutput(ns("nameFactor1")),
             uiOutput(ns("nameFactor2")),
             uiOutput(ns("nameFactor3")),
             uiOutput(ns("nameFactor4")),
             uiOutput(ns("nameFactor5"))
      ),
      #...for separators
      column(1,
             uiOutput(ns("separatorFactor")),
             uiOutput(ns("separatorFactor1")),
             uiOutput(ns("separatorFactor2")),
             uiOutput(ns("separatorFactor3")),
             uiOutput(ns("separatorFactor4")),
             uiOutput(ns("separatorFactor5"))
      ),
      #...for position in name
      column(2,
             uiOutput(ns("positionFactor")),
             uiOutput(ns("positionFactor1")),
             uiOutput(ns("positionFactor2")),
             uiOutput(ns("positionFactor3")),
             uiOutput(ns("positionFactor4")),
             uiOutput(ns("positionFactor5")),
      ),
      #...for prefixes
      column(2,
             uiOutput(ns("prefixFactor")),
             uiOutput(ns("prefixFactor1")),
             uiOutput(ns("prefixFactor2")),
             uiOutput(ns("prefixFactor3")),
             uiOutput(ns("prefixFactor4")),
             uiOutput(ns("prefixFactor5")),
      ),
      #...for suffixes
      column(2,
             uiOutput(ns("suffixFactor")),
             uiOutput(ns("suffixFactor1")),
             uiOutput(ns("suffixFactor2")),
             uiOutput(ns("suffixFactor3")),
             uiOutput(ns("suffixFactor4")),
             uiOutput(ns("suffixFactor5")),
      )
    ),
    #Buttons (2/2)
    fluidRow(
      #...for launching the batch analysis
      column(2,
             tags$button(
               id = ns("launchBatch"),
               div(tags$img(src = "www/batch.png", height = "40px"), " Launch"),
               class = "btn action-button")
      ),
      #...for saving data as txt
      column(2, disabled(downloadButton(ns("saveTxt"), "Save as .txt"))),
      #...for exporting data to R
      column(2, disabled(actionButton(ns("exportR"), "Export to R"))),
    ),
    #Table output for the results of the batch analysis
    DTOutput(ns("dataset")),

    #####
    hr(),
    textOutput(ns("feedback"))

  )
}

#SERVER----
batchAnalysisServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #Help in the text console----
    output$console <- renderText({
      textConsole$text})

    #Reactive values----
    batchData <- reactiveValues(data = NULL)
    textConsole <- reactiveValues(text = NULL)
    selectedSurFiles <- reactiveValues(datapath = NULL)


    #Placeholders renderers----

    #...for factor names
    output$nameFactor <- renderUI({
      if (input$nFactors > 0) h5("Factor name")
    })
    output$nameFactor1 <- renderUI({
      if (input$nFactors > 0) textInput(ns("nameFactor1"), label = NULL, value = "Factor1")
    })
    output$nameFactor2 <- renderUI({
      if (input$nFactors > 1) textInput(ns("nameFactor2"), label = NULL, value = "Factor2")
    })
    output$nameFactor3 <- renderUI({
      if (input$nFactors > 2) textInput(ns("nameFactor3"), label = NULL, value = "Factor3")
    })
    output$nameFactor4 <- renderUI({
      if (input$nFactors > 3) textInput(ns("nameFactor4"), label = NULL, value = "Factor4")
    })
    output$nameFactor5 <- renderUI({
      if (input$nFactors > 4) textInput(ns("nameFactor5"), label = NULL, value = "Factor5")
    })

    #...for separators
    output$separatorFactor <- renderUI({
      if (input$nFactors > 0) h5("Separator")
    })
    output$separatorFactor1 <- renderUI({
      if (input$nFactors > 0) textInput(ns("separatorFactor1"), label = NULL, "_")
    })
    output$separatorFactor2 <- renderUI({
      if (input$nFactors > 1) textInput(ns("separatorFactor2"), label = NULL, "_")
    })
    output$separatorFactor3 <- renderUI({
      if (input$nFactors > 2) textInput(ns("separatorFactor3"), label = NULL, "_")
    })
    output$separatorFactor4 <- renderUI({
      if (input$nFactors > 3) textInput(ns("separatorFactor4"), label = NULL, "_")
    })
    output$separatorFactor5 <- renderUI({
      if (input$nFactors > 4) textInput(ns("separatorFactor5"), label = NULL, "_")
    })

    #...for position
    output$positionFactor <- renderUI({
      if (input$nFactors > 0) h5("Position")
    })
    output$positionFactor1 <- renderUI({
      if (input$nFactors > 0) numericInput(ns("positionFactor1"), label = NULL, 1, min = 1, max = 100)
    })
    output$positionFactor2 <- renderUI({
      if (input$nFactors > 1) numericInput(ns("positionFactor2"), label = NULL, 1, min = 1, max = 100)
    })
    output$positionFactor3 <- renderUI({
      if (input$nFactors > 2) numericInput(ns("positionFactor3"), label = NULL, 1, min = 1, max = 100)
    })
    output$positionFactor4 <- renderUI({
      if (input$nFactors > 3) numericInput(ns("positionFactor4"), label = NULL, 1, min = 1, max = 100)
    })
    output$positionFactor5 <- renderUI({
      if (input$nFactors > 4) numericInput(ns("positionFactor5"), label = NULL, 1, min = 1, max = 100)
    })

    #...for prefixes
    output$prefixFactor <- renderUI({
      if (input$nFactors > 0) h5("Prefix")
    })
    output$prefixFactor1 <- renderUI({
      if (input$nFactors > 0) textInput(ns("prefixFactor1"), label = NULL, "")
    })
    output$prefixFactor2 <- renderUI({
      if (input$nFactors > 1) textInput(ns("prefixFactor2"), label = NULL, "")
    })
    output$prefixFactor3 <- renderUI({
      if (input$nFactors > 2) textInput(ns("prefixFactor3"), label = NULL, "")
    })
    output$prefixFactor4 <- renderUI({
      if (input$nFactors > 3) textInput(ns("prefixFactor4"), label = NULL, "")
    })
    output$prefixFactor5 <- renderUI({
      if (input$nFactors > 4) textInput(ns("prefixFactor5"), label = NULL, "")
    })

    #...for suffixes
    output$suffixFactor <- renderUI({
      if (input$nFactors > 0) h5("Suffix")
    })
    output$suffixFactor1 <- renderUI({
      if (input$nFactors > 0) textInput(ns("suffixFactor1"), label = NULL, "")
    })
    output$suffixFactor2 <- renderUI({
      if (input$nFactors > 1) textInput(ns("suffixFactor2"), label = NULL, "")
    })
    output$suffixFactor3 <- renderUI({
      if (input$nFactors > 2) textInput(ns("suffixFactor3"), label = NULL, "")
    })
    output$suffixFactor4 <- renderUI({
      if (input$nFactors > 3) textInput(ns("suffixFactor4"), label = NULL, "")
    })
    output$suffixFactor5 <- renderUI({
      if (input$nFactors > 4) textInput(ns("suffixFactor5"), label = NULL, "")
    })

    #Render data table----
    output$dataset <- renderDT({
      Mydf <- batchData$data
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
    #...For getting surface files----
    volumes = getVolumes()
    observe({
      shinyFileChoose(input, "surFiles", roots = volumes, session = session)
      selectedSurFiles$datapath <- parseFilePaths(volumes, input$surFiles)$datapath
      textConsole$text <-  "Please select .sur files"
      output$txt_file <- renderText(as.character(selectedSurFiles$datapath))
    })

    #...For launching batch analysis----
    observeEvent(input$launchBatch, {
      textConsole$text <- ""

      sur <- selectedSurFiles$datapath
      size <- input$xSize
      nCell <- input$nCell^2
      peakRemoval <- input$peakRemoval
      doComplexity <- input$complexity
      doHeight <- input$height
      doSpatial <- input$spatial
      doStd <- input$doStd
      #doMin <- input$doMin
      #doMax <- input$doMax
      doTopology <- input$topology
      doHComplexity <- input$hComplexity
      doHHeight <- input$hHeight
      doHSpatial <- input$hSpatial
      doHTopology <- input$hTopology

      #Preparation for factor name construction
      nameFactor <- NULL
      separatorFactor <- NULL
      positionFactor <- NULL
      prefixFactor <- NULL
      suffixFactor <- NULL

      if (input$nFactors > 0)
      {
        nameFactor[1] <- input$nameFactor1
        separatorFactor[1] <- input$separatorFactor1
        positionFactor[1] <- input$positionFactor1
        prefixFactor[1] <- input$prefixFactor1
        suffixFactor[1] <- input$suffixFactor1

        if (input$nFactors > 1)
        {
          nameFactor[2] <- input$nameFactor2
          separatorFactor[2] <- input$separatorFactor2
          positionFactor[2] <- input$positionFactor2
          prefixFactor[2] <- input$prefixFactor2
          suffixFactor[2] <- input$suffixFactor2

          if (input$nFactors > 2)
          {
            nameFactor[3] <- input$nameFactor3
            separatorFactor[3] <- input$separatorFactor3
            positionFactor[3] <- input$positionFactor3
            prefixFactor[3] <- input$prefixFactor3
            suffixFactor[3] <- input$suffixFactor3

            if (input$nFactors > 3)
            {
              nameFactor[4] <- input$nameFactor4
              separatorFactor[4] <- input$separatorFactor4
              positionFactor[4] <- input$positionFactor4
              prefixFactor[4] <- input$prefixFactor4
              suffixFactor[4] <- input$suffixFactor4

              if (input$nFactors > 4)
              {
                nameFactor[5] <- input$nameFactor5
                separatorFactor[5] <- input$separatorFactor5
                positionFactor[5] <- input$positionFactor5
                prefixFactor[5] <- input$prefixFactor5
                suffixFactor[5] <- input$suffixFactor5
              }
            }
          }
        }
      }

      # Local function for batch analysis----
      Batch.fun <- function(sur, size, nCell, peakRemoval,
                            #doMin, doMax,
                            doStd,
                            doComplexity, doHeight, doSpatial, doTopology,
                            doHComplexity, doHHeight, doHSpatial, doHTopology) {
        # Peak removal
        if (peakRemoval == "0") {
          sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), ".sur")), "\\\\", "/")
        }
        if (peakRemoval == "2") {
          trident::polynom.sur(sur, deg = 2)
          sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), "_X1_002_Y1_002.sur")), "\\\\", "/")
        }
        if (peakRemoval == "8") {
          trident::polynom.sur(sur, deg = 8)
          sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), "_X1_008_Y1_008.sur")), "\\\\", "/")
        }

        # Prepare data frame
        Mytable <- data.frame(File = basename(sur), stringsAsFactors = TRUE)

        # DMTA Computation:
        if (doComplexity) {
          Complexity <- trident::dmta.asfc(sur = sur, type = "single", size.x = size, size.y = size)
          Mytable$Asfc2 <- Complexity[3]
          Mytable$R2adj <- Complexity[4]
        }
        if (doHeight) {
          Elev <- trident::dmta.height(sur = sur, type = "single", size.x = size, size.y = size)
          Mytable$Sa <- Elev[3]
          Mytable$Sp <- Elev[4]
          Mytable$Sq <- Elev[5]
          Mytable$Sv <- Elev[6]
          Mytable$Ssk <- Elev[7]
          Mytable$Sku <- Elev[8]
          Mytable$Sdar <- Elev[9]
          Mytable$Sm <- Elev[10]
          Mytable$Smd <- Elev[11]
        }
        if (doSpatial) {
          Spat <- trident::dmta.spatial(sur = sur, type = "single", size.x = size, size.y = size)
          Mytable$Rmax <- Spat[3]
          Mytable$Sal <- Spat[4]
          Mytable$Stri <- Spat[5]
          if (doStd) Mytable$Std <- Spat[6]
          Mytable$b.sl <- Spat[7]
          Mytable$r.sl <- Spat[8]
          Mytable$s.sl <- Spat[9]
        }
        if (doTopology) {
          Topo <- trident::dmta.topology(sur = sur, type = "single", size.x = size, size.y = size)
          Mytable$Sk1 <- Topo[3]
          Mytable$Sk2 <- Topo[4]
          Mytable$Smc1 <- Topo[5]
          Mytable$Smc2 <- Topo[6]
          Mytable$Snb1 <- Topo[7]
          Mytable$Snb2 <- Topo[8]
          Mytable$Sh <- Topo[9]
        }
        # Heterogeneity
        substrRight <- function(x, n){
          substr(x, nchar(x)-n+1, nchar(x))
        }

        if (doHComplexity) {
          HComplexity <- trident::dmta.asfc(sur = sur, type = "multi", size.x = size, size.y = size, size.n = nCell)
          hTable <- c(plyr::colwise(trident::trident.hetero)(data.frame(HComplexity[3])))

          #if (!doMin) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".min")]
          #}
          #if (!doMax) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".max")]
          #}
          Mytable <- data.frame(Mytable, hTable)
        }

        if (doHHeight) {
          HElev <- trident::dmta.height(sur = sur, type = "multi", size.x = size, size.y = size, size.n = nCell)
          hTable <- c(plyr::colwise(trident::trident.hetero)(data.frame(HElev[3:11])))

          #if (!doMin) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".min")]
          #}
          #if (!doMax) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".max")]
          #}
          Mytable <- data.frame(Mytable, hTable)
        }

        if (doHSpatial) {
          HSpat <- trident::dmta.spatial(sur = sur, type = "multi", size.x = size, size.y = size, size.n = nCell)
          if (doStd)  {hTable <- c(plyr::colwise(trident::trident.hetero)(data.frame(HSpat[3:9])))}
          if (!doStd) {hTable <- c(plyr::colwise(trident::trident.hetero)(data.frame(HSpat[c(3,4,5,7,8,9)])))}

          #if (!doMin) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".min")]
          #}
          #if (!doMax) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".max")]
          #}
          Mytable <- data.frame(Mytable, hTable)
        }

        if (doHTopology) {
          HTopo <- trident::dmta.topology(sur = sur, type = "multi", size.x = size, size.y = size, size.n = nCell)
          hTable <- c(plyr::colwise(trident::trident.hetero)(data.frame(HTopo[3:9])))

          #if (!doMin) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".min")]
          #}
          #if (!doMax) {
          #  hTable <- hTable[, which(substrRight(colnames(hTable), 4) != ".max")]
          # }
          Mytable <- data.frame(Mytable, hTable)
        }

        # Return dataset
        return(Mytable)
      }

      # PROGRESSBAR
      Mypb <- utils::winProgressBar(title = "Preparation - this might take some time", min = 1, max = length(sur), width = 300)
      progress <- function(n) setWinProgressBar(Mypb, n, title = paste(round(n/length(sur)*100, 0), "% done"))
      opts <- list(progress = progress)

      # Parallel loop
      doSNOW::registerDoSNOW(snow::makeSOCKcluster(parallel::detectCores() - 1))
      batchAnalysisTable <- foreach::foreach(i = 1:length(sur),
                                             .options.snow = opts,
                                             .combine = "rbind") %dopar% Batch.fun(sur = sur[i],
                                                                                   size = size,
                                                                                   nCell = nCell,
                                                                                   peakRemoval = peakRemoval,
                                                                                   doComplexity = doComplexity,
                                                                                   doHeight = doHeight,
                                                                                   doSpatial = doSpatial,
                                                                                   doStd = doStd,
                                                                                   #doMin = doMin,
                                                                                   #doMax = doMax,
                                                                                   doTopology = doTopology,
                                                                                   doHComplexity = doHComplexity,
                                                                                   doHHeight = doHHeight,
                                                                                   doHSpatial = doHSpatial,
                                                                                   doHTopology = doHTopology)

      # Close progress bar
      close(Mypb)

      # here it is necessary to manually unlist the numerical variables
      Numerics <- data.frame(apply(batchAnalysisTable[, -1], 2, unlist))
      Factors <- data.frame(File = batchAnalysisTable[, 1])

      FactorFromFileName <- function(x, separator, position, suffix, prefix) {
        Strings <- strsplit(as.character(x), as.character(separator))
        Dataframe <- data.frame(matrix(unlist(Strings), nrow = length(Strings[[1]])))
        return(paste(prefix, unlist(Dataframe[position, ]), suffix, sep = ""))
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
      batchData$data <- data.frame(Factors, Numerics)
      textConsole$text <- "Done! Do not forget to save the results."

    })

    #...for dowloading dataset as txt
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        Mydf <- batchData$data
        Mydf <- unclass(Mydf)
        write.table(Mydf, file, quote = FALSE, sep = "\t", row.names = FALSE)
      }
    )

    #...For exporting to R----
    observeEvent(input$exportR, {
      Mydf <- batchData$data
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      assign(paste0(testName <- "batchDataTrident"), Mydf, envir = .GlobalEnv)
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

    #####
    #output$feedback <- shiny::renderText(paste("Debug"))
  })
}
