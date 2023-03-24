#UI----
variablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    #Help in a verbatim text output
    verbatimTextOutput(ns('text')),
    #Table to select a factor
    DTOutput(ns("factorTable")),
    hr(),

    #Buttons
    fluidRow(
      column(4,
             #...to save table as txt
             disabled(downloadButton(ns("saveTxt"), "Save as .txt")),
             #...to export table into R
             disabled(tags$button(
               id = ns("exportR"),
               div(tags$img(src = "www/export.png", height = "40px"), "Export to R"),
               class = "btn action-button")),
             #...to launch correlation
             disabled(tags$button(
               id = ns("correlation"),
               div(tags$img(src = "www/correlation.png", height = "40px"), "Correlation"),
               class = "btn action-button")),
             #...to launch multicheck
             disabled(tags$button(
               id = ns("multiCheck"),
               div(tags$img(src = "www/multicheck.png", height = "40px"), "Multi-Check"),
               class = "btn action-button")),
             #...to launch Top3
             disabled(tags$button(
               id = ns("top3"),
               div(tags$img(src = "www/top3.png", height = "40px"), "Top-3"),
               class = "btn action-button")),
             em("*Top3 automatically performs a Box-Cox transformation and should be reserved for untransformed data."),
             em("**Consider saving the results of your Top3, as it will not affect variable order in other tabs.")
      ),
      #...to rank by
      column(4,
             disabled(tags$button(
               id = ns("rankBy"),
               div(tags$img(src = "www/rankby.png", height = "40px"), "Rank by..."),
               class = "btn action-button")),
             #Radiobuttons----

             #...to select ranking factors
             radioButtons(ns("rankBySelect"), NULL,
                          choiceNames = list("P-value (ANOVA)", "P-value (Kruskall)", "Average HSD", "Pairwise HSD", "Average LSD", "Pairwise LSD"),
                          choiceValues = list("0", "1", "2", "3", "4", "5"))
      ),

      #Checkboxes----
      column(4,
             checkboxInput(ns("byNGroups"), "Rank by number of discriminant groups?", TRUE),
             checkboxInput(ns("geomean"), "Compute geometric mean?", FALSE),
             checkboxInput(ns("removeNonDisc"), "Remove non-discriminant variables?", FALSE),
             #UI output for selecting pairs----
             uiOutput(ns("pairsInput")),
      )
    ),
    #Table----
    #...for main display of classified variables
    DTOutput(ns("varTable")),

    #####
    hr(),
    textOutput(ns("feedback"))
  )
}

#Server----
variablesServer <- function(id, data) {
  moduleServer(id, function(input, output, session, dataTrident = data) {
    ns <- session$ns

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident$value
      if(v$isComputing) return('Wait, computation in progress...')
      else if (!is.null(Mydf)) {
        if (is.null(input$factorTable_rows_selected)) return('You must select a variable as factor')
        if (!is.null(input$factorTable_rows_selected)) return(paste0('Row ', input$factorTable_rows_selected, ' selected as factor'))
      }
      else return('Please select a dataset')
    })

    #Reactive data----
    #...Reactive data to select variables
    dataNumericTrident <- reactive({
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      data.frame(variable = colnames(Numerics))
    })

    #...Reactive data to select factors
    dataFactorTrident <- reactive({
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      lengthLevels <- function(x) {return(length(levels(x)))}
      data.frame(variable = colnames(Factors), levels = sapply(Factors,lengthLevels))
    })

    #...Reactive value for data
    v <- reactiveValues(data = NULL, isComputing = FALSE)

    #...Reactive value for ordered variables
    varTrident <- reactiveValues(variables = NULL)

    #Input list----
    #...for pairs
    output$pairsInput <- renderUI({
      if (input$rankBySelect == 3 || input$rankBySelect == 5)
      {
        Mydf <- dataTrident$value
        Mydf <- unclass(Mydf)
        Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
        Factor  <- dplyr::select_if(Mydf, is.factor)[, input$factorTable_rows_selected]
        myNames <- utils::combn(levels(as.factor(Factor)), 2, paste, collapse = ' vs. ', simplify = TRUE)
        myList <- list()
        for (i in 1:length(myNames))
        {
          myList[[i]] = i
        }
        names(myList) <- myNames
        selectInput(ns("pairsInput"), label = NULL,
                    choices = myList,
                    selected = 1)
      }
    })

    #Buttons----
    #...for calculating correlation between variables
    dataCorrelation <- observeEvent(input$correlation, {
      v$isComputing <- TRUE
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)

      Mycortable <- picante::cor.table(Numerics, cor.method = "pearson")
      Mycortable$r <- round(Mycortable$r, 4)
      Mycortable$r[Mycortable$P >= 0.05] <- "NS"
      Mycortable$r[Mycortable$P < 0.05] <- paste(Mycortable$r[Mycortable$P < 0.05], "*", sep = "")
      Mycortable$r[Mycortable$P < 0.01] <- paste(Mycortable$r[Mycortable$P < 0.01], "*", sep = "")
      Mycortable$r[Mycortable$P < 0.001] <- paste(Mycortable$r[Mycortable$P < 0.001], "*", sep = "")

      v$isComputing <- FALSE
      v$data <- data.frame(Variable = colnames(Mycortable$r), Mycortable$r)
    })

    #...for performing multiCheck
    dataMulticheck <- observeEvent(input$multiCheck, {
      v$isComputing <- TRUE
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factor  <- dplyr::select_if(Mydf, is.factor)[, input$factorTable_rows_selected]
      Multicheck <- trident::multicheck(df = Numerics, y = Factor)
      v$isComputing <- FALSE
      v$data <- data.frame(Multicheck)
    })

    #...for ranking by
    dataRankBy <- observeEvent(input$rankBy, {
      v$isComputing <- TRUE
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factor  <- dplyr::select_if(Mydf, is.factor)[, input$factorTable_rows_selected]

      Multicheck <- NULL

      #If only discriminant variables are to be ranked
      if (input$removeNonDisc)
      {
        Multicheck <- trident::multicheck(df = Numerics, y = Factor)
        if(!any(as.logical(Multicheck$is.discriminant)))
        {
          warning("No discriminant variable found; please uncheck the box 'Remove non-discriminant variables' or try with a new dataset")
        }
        else if (length(Multicheck$is.discriminant[which(Multicheck$is.discriminant == TRUE)]) < 3)
        {
          warning("There are less than 3 discriminant variables; please uncheck the box 'Remove non-discriminant variables' or try with a new dataset")
        }
        else Numerics <- as.data.frame(Numerics[, which(Multicheck$is.discriminant == TRUE)])
      }
      #The different modes of rank by
      rankByMethod <- "k.p.value"
      gpPriority <- c(1:length(levels(Factor)))
      if (input$rankBySelect == 0) rankByMethod = "aov.p.value"
      else if (input$rankBySelect == 1) rankByMethod = "k.p.value"
      else if (input$rankBySelect == 2) rankByMethod = "hsd.mean.p.value"
      else if (input$rankBySelect == 3)
      {
        rankByMethod = "hsd.p.value"
      }
      else if (input$rankBySelect == 4) rankByMethod = "lsd.mean.p.value"
      else if (input$rankBySelect == 5)
      {
        rankByMethod = "lsd.p.value"
      }
      if(input$rankBySelect == 3 || input$rankBySelect == 5)
      {
        Mypairs <- utils::combn(levels(as.factor(Factor)), 2, paste, collapse = ' vs. ', simplify = TRUE)
        Mycurrentpair <- unlist(strsplit(Mypairs[as.numeric(input$pairsInput)], " vs. "))
        myLevels <- levels(as.factor(Factor))
        gpPriority <- c(which(myLevels %in% Mycurrentpair), which(!(myLevels %in% Mycurrentpair)))
      }
      else
      {
        gpPriority = c(1:length(levels(Factor)))
      }

      #The actual variable arrangement
      RankBy <- trident::trident.arrange(
        df = Numerics,
        y = Factor,
        by = rankByMethod,
        byngr = input$byNGroups,
        geomean = input$geomean,
        gp.priority = gpPriority)

      #Extracting the results
      if (input$removeNonDisc && !any(as.logical(Multicheck$is.discriminant)))
      {
        showModal(modalDialog(
          title = "Ranking procedure has been stopped",
          "No discriminant variable could be found; please uncheck the box 'Remove non-discriminant variables' or try with a new dataset"
        ))
      }
      else if (input$removeNonDisc && length(Multicheck$is.discriminant[which(Multicheck$is.discriminant == TRUE)]) < 3)
      {
        showModal(modalDialog(
          title = "Ranking procedure has been stopped",
          "There are less than 3 discriminant variables; please uncheck the box 'Remove non-discriminant variables' or try with a new dataset"
        ))
      }
      else
      {
        v$data <- data.frame(variable = rownames(RankBy), RankBy)
        varTrident$variables <- v$data$variable
      }
      #End
      v$isComputing <- FALSE
    })

    #...for top3
    dataTop3 <- observeEvent(input$top3, {
      req((!is.null(input$factorTable_rows_selected)))
      v$isComputing <- TRUE
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factor  <- dplyr::select_if(Mydf, is.factor)[, input$factorTable_rows_selected]
      #TOP3
      Mytop <- trident::trident.top3(df = Numerics, y = Factor)
      if (is.null(Mytop))
      {
        showModal(modalDialog(
          title = "Top 3 procedure has been stopped",
          "No discriminant variable could be found; please try with a new dataset"
        ))
      }
      else
      {
        Mytop <- Mytop$ranked
        Mytop <- data.frame(variable = rownames(Mytop), Mytop)
        v$data <- Mytop
      }
      v$isComputing <- FALSE
    })

    #Table----
    #...selecting factors
    output$factorTable <- renderDT({
      Mydf <- dataFactorTrident()
      DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...main dataset
    output$varTable <- renderDT({
      #No render if no data
      req(!is.null(v$data))
      #Prepare data
      Mydf <- data.frame(v$data)
      my_vals = Mydf$variable
      my_colors = ifelse(my_vals=='Sa','green','pink')
      #Only use the formatStyle option when there are 2 levels in is.discriminant
      if(length(levels(as.factor(Mydf$is.discriminant))) == 2)
      {
        tableData <- DT::datatable(
          Mydf,
          rownames = FALSE,
          extensions = c("FixedColumns"),
          options = list(fixedColumns = list(leftColumns = 1))
        ) %>%
          formatStyle(
            "is.discriminant",
            backgroundColor = styleEqual(
              unique(Mydf$is.discriminant), c('lightpink', 'lightgreen')
            )
          )
      }
      else
      {
        tableData <- DT::datatable(
          Mydf,
          rownames = FALSE,
          extensions = c("FixedColumns"),
          options = list(fixedColumns = list(leftColumns = 1))
        )
      }

    })

    #Buttons----
    #...for dowloading dataset as txt
    output$saveTxt <- downloadHandler(
      filename = function() {
        paste0("untitled.txt")
      },
      content = function(file) {
        Mydf <- data.frame(v$data)
        write.table(Mydf, file, quote = FALSE, row.names = FALSE, sep = "\t")
      }
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
      myDf <- data.frame(v$data)
      exportName$name <- input$name
      assign(paste0(exportName$name), myDf, envir = .GlobalEnv)
    })

    #Enable / Disable buttons----
    observe({
      req(dataTrident)
      Mydf <- dataTrident$value
      if (!is.null(Mydf))
      {
        enable("saveTxt")
        enable("exportR")
        enable("correlation")
        if (!is.null(input$factorTable_rows_selected))
        {
          enable("multiCheck")
          enable("top3")
          enable("rankBy")
        }
        if (is.null(input$factorTable_rows_selected))
        {
          disable("multiCheck")
          disable("top3")
          disable("rankBy")
        }
      }
    })

    #####
    #output$feedback <- shiny::renderText(paste("Debug"))

    #export the dataset----
    return(varTrident)     #CRUCIAL FOR THE OTHER MODULES
  })
}
