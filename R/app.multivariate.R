#UI----
multivariateUI <- function(id) {
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
        DTOutput(ns("numericTable")),
        #select supplementary variables in table
        em("Select supplementary variables:"),
        DTOutput(ns("numericTable2"))
      ),

      column(
        3,
        #select factors in table
        em("Select factor:"),
        DTOutput(ns("factorTable")),
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
            #...Checkboxes
            #NOTE: currently the behavior of ellipses and convex hulls is unstable, it was deactivated
            #checkboxInput(ns("IsEllipse"), "Add ellipses?", FALSE),
            #checkboxInput(ns("IsCHull"), "Add convex hulls?", FALSE)
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
                         choiceNames = list("tab", "space"),
                         choiceValues = list("0", "1")),
          )
        ),
        hr(),
        #Graph output
        plotOutput(ns("plot"), height = 400, width = 400)
      )
    ),

    #####
    hr(),
    textOutput(ns("feedback"))
  )
}

#SERVER----
multivariateServer <- function(id, data, variables) {
  moduleServer(id, function(input, output, session, dataTrident = data, varTrident = variables) {
    ns <- session$ns

    #Help in a verbatim text----
    output$text <- renderText({
      Mydf <- dataTrident$value
      if (!is.null(Mydf)) {
        if (length(input$factorTable_rows_selected) != 1) return('You must select 1 variable as factor')
        if (length(input$factorTable_rows_selected) == 1) {
          if (length(input$numericTable_rows_selected) < 2) return('You must also select at least 2 variables')
          if (length(input$numericTable_rows_selected) >= 2) {
            Mydf <- dataTrident$value
            Factors  <- dplyr::select_if(Mydf, is.character)
            Numerics <- dplyr::select_if(Mydf, is.numeric)
            if(!is.null(varTrident$variables)) Numerics <- Numerics[, varTrident$variables]
            myText <- paste0('Row "', input$factorTable_rows_selected, '" (', colnames(Factors)[input$factorTable_rows_selected], ') selected as factor\n')
            for (i in 1:length(input$numericTable_rows_selected))
            {
              currentRow <- input$numericTable_rows_selected[i]
              myText <- paste0(myText, "\n", 'Row "', currentRow, '" (', colnames(Numerics)[currentRow], ') selected as variable')
            }
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
        Mydf <- Mydf[!is.infinite(base::rowSums(dplyr::select_if(Mydf, is.numeric))), ]
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
      Mydf <- Mydf[!is.infinite(base::rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      Factors  <- dplyr::select_if(Mydf, is.factor)
      lengthLevels <- function(x) {return(length(levels(x)))}
      data.frame(variable = colnames(Factors), levels = sapply(Factors,lengthLevels))
    })

    #...Reactive values for supp individuals
    suppIndividuals <- reactiveValues(value = NULL)

    #...Reactive values ACP
    acpResults <- reactiveValues(data = NULL)

    #...Reactive values for choosing the type of graph output
    v <- reactiveValues(plot = "screenplot")

    #Tables----
    #...Table to select variables
    output$numericTable <- renderDT({
      req(!is.null(dataTrident$value))
      Mydf <- dataNumericTrident()
      DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...Table to select supplementary variables
    output$numericTable2 <- renderDT({
      req(!is.null(dataTrident$value))
      Mydf <- dataNumericTrident()
      DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #...Table to select factor
    output$factorTable <- renderDT({
      req(!is.null(dataTrident$value))
      Mydf <- dataFactorTrident()
      DT::datatable(Mydf, rownames = FALSE, selection = list(target = "row"))
    })

    #Buttons----
    #...for changing to screenplot
    observeEvent(input$screenplot, {
      v$plot <- "screenplot"
    })

    #...for changing to correlation circle
    observeEvent(input$corcircle, {
      v$plot <- "corcircle"
    })

    #...for changing to bivariate graph
    observeEvent(input$bivariate, {
      v$plot <- "bivariate"
    })

    #...for adding individuals
    observeEvent(input$addIndividuals, {
      if(input$txtSeparator == 0) txtSeparator = "\t"
      else if (input$txtSeparator == 1) txtSeparator = " "
      Mydf <- read.delim(input$addIndividuals$datapath, header = TRUE, sep = txtSeparator)
      Mydf <- unclass(Mydf)
      Mydf <- data.frame(Mydf, stringsAsFactors = TRUE)
      Mydf <- Mydf[!is.infinite(base::rowSums(dplyr::select_if(Mydf, is.numeric))), ]
      Mydf <- stats::na.omit(Mydf)
      #Numerics <- dplyr::select_if(Mydf, is.numeric)
      #Factors  <- dplyr::select_if(Mydf, is.character)
      suppIndividuals$value <- data.frame(Mydf)
    })

    #UI outputs----
    #...for PCX
    output$selectPCX <- renderUI({
      if(!is.null(acpResults$data))
      {
        Ncp <- length(acpResults$data$ind$coord[1, ])
        myList <- list()
        myNames <- NULL
        for (i in 1:Ncp)
        {
          myList[[i]] = i
          myNames[i] = paste0("PC", i)
        }
        names(myList) <- myNames

        selectInput(ns("selectPCX"), label = NULL,
                    choices = myList,
                    selected = 1)
      }
    })
    #...for PCY
    output$selectPCY <- renderUI({
      if(!is.null(acpResults$data))
      {
        Ncp <- length(acpResults$data$ind$coord[1, ])
        myList <- list()
        myNames <- NULL
        for (i in 1:Ncp)
        {
          myList[[i]] = i
          myNames[i] = paste0("PC", i)
        }
        names(myList) <- myNames

        selectInput(ns("selectPCY"), label = NULL,
                    choices = myList,
                    selected = 2)
      }
    })

    #Plot----
    output$plot <- renderPlot({
      req(!is.null(dataTrident$value) &&
            length(input$numericTable_rows_selected) > 1 &&
            length(input$factorTable_rows_selected) == 1)

      #prepare dataset
      Mydf <- dataTrident$value
      Numerics <- dplyr::select_if(Mydf, is.numeric)
      Factors  <- dplyr::select_if(Mydf, is.character)
      #order variables
      if(!is.null(varTrident$variables))
      {
        Numerics <- Numerics[, varTrident$variables]
      }

      if (!is.null(suppIndividuals$value))
      {
        MySupp <- suppIndividuals$value
        NumSupp <- dplyr::select_if(MySupp, is.numeric)
        #order variables
        if(!is.null(varTrident$variables))
        {
          myVarSelection <- intersect(varTrident$variables, colnames(NumSupp))
          NumSupp <- NumSupp[, myVarSelection]
        }
      }

      myVars <- colnames(Numerics)[input$numericTable_rows_selected]
      mySuppVars <- colnames(Numerics)[input$numericTable2_rows_selected]
      myFactor <- colnames(Factors)[input$factorTable_rows_selected]

      # PCA
      # case 1.A: no supp var, no supp ind
      if (length(input$numericTable2_rows_selected) == 0 && is.null(suppIndividuals$value))
      {
        myPcaData <- Numerics[, myVars]
        acpResults$data <- FactoMineR::PCA(
          myPcaData,
          graph = FALSE,
          ncp = ncol(length(myVars)))
      }
      # case 2.A: supp var, no supp ind
      if (length(input$numericTable2_rows_selected) > 0 && is.null(suppIndividuals$value))
      {
        myPcaData <- dplyr::bind_cols(
          Numerics[, myVars],
          Numerics[, mySuppVars])

        acpResults$data <- FactoMineR::PCA(
          myPcaData,
          graph = FALSE,
          quanti.sup = c((length(myVars) + 1):(length(myVars) + length(mySuppVars))),
          ncp = ncol(length(myVars)))
      }
      # case 1.B: no supp var, supp ind
      if (length(input$numericTable2_rows_selected) == 0 && !is.null(suppIndividuals$value))
      {
        myPcaData <- Numerics[, myVars]
        myPcaSuppData <- NumSupp[, myVars]

        Mynewpcadata <- dplyr::union(myPcaData, myPcaSuppData)

        acpResults$data <- FactoMineR::PCA(
          Mynewpcadata,
          graph = FALSE,
          ind.sup = c((length(Numerics[, 1]) + 1):(length(Numerics[, 1]) + length(myPcaSuppData[, 1]))),
          ncp = ncol(length(myVars)))
      }
      # case 2.B: supp var, supp ind
      if (length(input$numericTable2_rows_selected) > 0 && !is.null(suppIndividuals$value))
      {
        myPcaData <- dplyr::bind_cols(
          Numerics[, myVars],
          Numerics[, mySuppVars])
        myPcaSuppData <- dplyr::bind_cols(
          NumSupp[, myVars],
          NumSupp[, mySuppVars])

        Mynewpcadata <- dplyr::union(myPcaData, myPcaSuppData)

        acpResults$data <- FactoMineR::PCA(
          Mynewpcadata,
          graph = FALSE,
          ind.sup = c((length(Numerics[, 1]) + 1):(length(Numerics[, 1]) + length(NumSupp[, 1]))),
          quanti.sup = c((length(myVars) + 1):(length(myVars) + length(mySuppVars))),
          ncp = ncol(length(myVars)))
      }
      #Select PCs
      pcX <- as.numeric(input$selectPCX)
      pcY <- as.numeric(input$selectPCY)

      # Plot
      PLOT.COLORS = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3) #24 levels max
      PLOT.PCH = c(1, 15:18, 9:10, 18:15, 10:8, 11:14, 2:7)

      if(v$plot == "screenplot" & length(input$factorTable_rows_selected) == 1 & length(input$numericTable_rows_selected) > 1) {
        factoextra::fviz_eig(acpResults$data, addlabels = TRUE, ylim = c(0, max(acpResults$data$eig[, 2])), barfill = "lightgoldenrod", barcolor = "lightgoldenrod3")
      }

      else if(v$plot == "bivariate" & length(input$factorTable_rows_selected) == 1 & length(input$numericTable_rows_selected) > 1) {
        factoextra::fviz_pca_ind(
          acpResults$data, axes = c(pcX, pcY),
          geom = c("point", "text"),
          repel = FALSE,
          col.ind = "transparent",
          col.ind.sup = "hotpink",
          pointsize = 3,
          pointshape = 8
        ) +
          ggplot2::labs(x = paste(colnames(acpResults$data$ind$coord)[pcX], " (", round(acpResults$data$eig[pcX, 2], 1), " %)", sep = ""),
                        y = paste(colnames(acpResults$data$ind$coord)[pcY], " (", round(acpResults$data$eig[pcY, 2], 1), " %)", sep = "")) +
          ggplot2::guides(size = "none") +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                         legend.position = "bottom", legend.title = ggplot2::element_text(size = 14),
                         axis.text.x = ggplot2::element_text(size = 12, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.ontop = FALSE,
                         axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                         axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
          ggplot2::geom_point(ggplot2::aes(shape = Factors[, myFactor], color = Factors[, myFactor], fill = Factors[, myFactor]), size = 3) +
          ggplot2::scale_color_manual(name = myFactor, labels = levels(as.factor(Factors[, myFactor])), values = PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = myFactor, labels = levels(as.factor(Factors[, myFactor])), values = PLOT.COLORS) +
          ggplot2::scale_shape_manual(name = myFactor, labels = levels(as.factor(Factors[, myFactor])), values = PLOT.PCH) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2))) +
          ggplot2::labs(title = NULL)
      }

      else if(v$plot == "corcircle" & length(input$factorTable_rows_selected) == 1 & length(input$numericTable_rows_selected) > 1) {
        factoextra::fviz_pca_var(acpResults$data,
                                 axes = c(pcX, pcY),
                                 repel = TRUE,
                                 col.var = "steelblue",
                                 col.quanti.sup = "hotpink",
                                 arrowsize = 1,
                                 labelsize = 6) +
          ggplot2::labs(x = paste(colnames(acpResults$data$ind$coord)[pcX], " (", round(acpResults$data$eig[pcX, 2], 1), " %)", sep = ""),
                        y = paste(colnames(acpResults$data$ind$coord)[pcY], " (", round(acpResults$data$eig[pcY, 2], 1), " %)", sep = "")) +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                         axis.text.x = ggplot2::element_text(size = 12, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.ontop = FALSE,
                         subtitle = NULL,
                         axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                         axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
          ggplot2::labs(title = NULL)
      }
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

    #Enable / Disable buttons----
    observe({
      req(acpResults)
      myPCA <- acpResults$data
      if (!is.null(myPCA)) {
        enable("savePCA")
        if (!is.null(input$factorTable_rows_selected) && !is.null(input$numericTable_rows_selected)) {
          enable("screenplot")
          enable("bivariate")
          enable("corcircle")
          enable("addIndividuals")
          enable("savePCA")
        }
        if (is.null(input$factorTable_rows_selected) || is.null(input$numericTable_rows_selected)) {
          disable("screenplot")
          disable("bivariate")
          disable("corcircle")
          disable("addIndividuals")
          disable("savePCA")
        }
      }
    })

    #####
    #output$feedback <- shiny::renderText("")
  })
}
