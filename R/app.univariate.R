#UI----
univariateUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    #Help in a verbatim text output
    verbatimTextOutput(ns('text')),
    hr(),
    fluidRow(
      column(
        3,
        #select variables in table
        DTOutput(ns("numericTable"))
      ),
      column(
        3,
        #select factors in table
        DTOutput(ns("factorTable")),
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

    #####
    textOutput(ns("feedback"))
  )
}

#SERVER----
univariateServer <- function(id, data, variables) {
  moduleServer(id, function(input, output, session, dataTrident = data, varTrident = variables) {
    ns <- session$ns

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident$value
      if (!is.null(Mydf)) {
        if (length(input$factorTable_rows_selected) != 1) return('You must select 1 variable as factor')
        if (length(input$factorTable_rows_selected) == 1) {
          if (length(input$numericTable_rows_selected) != 1) return('You must also select 1 variable')
          if (length(input$numericTable_rows_selected) == 1) {
            Mydf <- dataTrident$value
            Factors  <- dplyr::select_if(Mydf, is.character)
            Numerics <- dplyr::select_if(Mydf, is.numeric)
            if(!is.null(varTrident$variables)) Numerics <- Numerics[, varTrident$variables]
            myText <- paste0('Row "', input$factorTable_rows_selected, '" (', colnames(Factors)[input$factorTable_rows_selected], ') selected as factor\n')
            myText <- paste0(myText, "\n", 'Row "', input$numericTable_rows_selected, '" (', colnames(Numerics)[input$numericTable_rows_selected], ') selected as variable')
            return(myText)
          }
        }
      }
      else return('Please select a dataset')
    })

    #Reactive data----
    #...Reactive data to select variables
    dataNumericTrident <- reactive({
      req(!is.null(dataTrident$value))
      if(!is.null(varTrident$variables))
      {
        Myvar <- varTrident$variables
        Myvar <- unclass(Myvar)
        Temp <- data.frame(variable = Myvar)
      }
      else
      {
        Mydf <- dataTrident$value
        Mydf <- unclass(Mydf)
        Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
        Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
        Mydf <- stats::na.omit(Mydf)
        Numerics <- dplyr::select_if(Mydf, is.numeric)
        Temp <- data.frame(variable = colnames(Numerics))
      }
      Temp
    })

    #...Reactive data to select factors
    dataFactorTrident <- reactive({
      req(!is.null(dataTrident$value))
      Mydf <- dataTrident$value
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      lengthLevels <- function(x) {return(length(levels(x)))}
      data.frame(variable = colnames(Factors), levels = sapply(Factors,lengthLevels))
    })
    #...Reactive values for choosing the type of graph output
    currentPlot <- reactiveValues(plotType = "boxplot", plot = NULL)

    #Tables----
    #...For selecting variables
    output$factorTable <- renderDT({
      req(!is.null(dataTrident$value))
      Mydf <- dataFactorTrident()
      DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })
    #...For selecting factors
    output$numericTable <- renderDT({
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
      Factors  <- dplyr::select_if(Mydf, is.character)

      #Order variables
      if(!is.null(varTrident$variables))
      {
        Numerics <- Numerics[, varTrident$variables]
      }

      PLOT.COLORS = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3) #24 levels max

      if(currentPlot$plotType == "violin" & length(input$factorTable_rows_selected) == 1 & length(input$numericTable_rows_selected) == 1) {
        testdf <- data.frame(Factors[, input$factorTable_rows_selected], Numerics[, input$numericTable_rows_selected])
        factorName <- colnames(Factors)[input$factorTable_rows_selected]
        varName <- colnames(Numerics)[input$numericTable_rows_selected]
        colnames(testdf) <- c(factorName, varName)

        currentPlot$plot <- ggplot2::ggplot(data = testdf, ggplot2::aes(x = "", y = testdf[, varName], group = testdf[, factorName])) +
          ggplot2::labs(x = "", y = varName) +
          ggplot2::guides(size = "none") +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                         legend.position = "right", legend.title = ggplot2::element_text(size = 14),
                         axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.x = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.ontop = FALSE,
                         axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                         axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2))) +
          ggplot2::scale_fill_manual(name = factorName, labels = levels(as.factor(testdf[, factorName])), values = PLOT.COLORS) +
          ggplot2::geom_violin(ggplot2::aes(fill = testdf[, factorName]), scale = "area", size = 0.5) +
          ggplot2::theme(axis.text.x = ggplot2::element_blank())
        currentPlot$plot
              }

      else if(currentPlot$plotType == "boxplot" & length(input$factorTable_rows_selected) == 1 & length(input$numericTable_rows_selected) == 1) {
        testdf <- data.frame(Factors[, input$factorTable_rows_selected], Numerics[, input$numericTable_rows_selected])
        factorName <- colnames(Factors)[input$factorTable_rows_selected]
        varName <- colnames(Numerics)[input$numericTable_rows_selected]
        colnames(testdf) <- c(factorName, varName)

        currentPlot$plot <- ggplot2::ggplot(data = testdf, ggplot2::aes(x = testdf[, factorName], y = testdf[, varName], fill = testdf[, factorName])) +
          ggplot2::labs(x = factorName, y = varName) +
          ggplot2::guides(size = "none") +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                         legend.position = "right", legend.title = ggplot2::element_text(size = 14),
                         axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.x = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.ontop = FALSE,
                         axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                         axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2))) +
          ggplot2::scale_color_manual(name = factorName, labels = levels(as.factor(testdf[, factorName])), values = PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = factorName, labels = levels(as.factor(testdf[, factorName])), values = colorspace::lighten(PLOT.COLORS, amount = 0.5)) +
          ggplot2::geom_boxplot() +
          ggplot2::geom_jitter(ggplot2::aes(col = testdf[, factorName]), position = ggplot2::position_jitterdodge(jitter.width = 0.5)) +
          ggplot2::theme(axis.text.x = ggplot2::element_blank())
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


    #####
    #output$feedback <- shiny::renderText(paste("Debug"))
  })
}
