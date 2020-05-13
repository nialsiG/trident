# Graphical User Interface----
#' @title trident
#' @description Open the trident graphical user interface.
#' @export
trident <- function(){
  # Tcltk Objects----
  METADATA <- list(VERSION = '0.1.0', DESCRIPTION = "Dusty Sandbox")
  MyFile <- NULL
  MyTable <- NULL
  MyLambda <- NULL
  # Tcltk Commands: table.cmd----
  table.cmd <- function (x, win) {
    MyWin <<- win

    MyWin$TABLE <<- tcltk::tkframe(MyWin)
    MyWin$SAVE <- tcltk::tkframe(MyWin)
    SAVEBUTTON <- tcltk::tkbutton(MyWin$SAVE, text = "SAVE", justify = "left", width = 10, command = function(){
      utils::write.table(x, file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN2, title = "Save table as...", initialfile = METADATA$FILENAME, defaultextension = ".txt")),
                         append = TRUE, quote = FALSE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
      tcltk::tkdestroy(MyWin)
    })
    tcltk::tkpack(MyWin$TABLE, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkpack(MyWin$SAVE, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkgrid(SAVEBUTTON, padx = 5, pady = 5)
    # Create table for tcltk widget
    matrix <- rbind(colnames(x), x)
    matrix1 <- cbind(row.names(matrix), matrix)
    matrix1 <- t(t(matrix1))
    # Define a Tcl array and initialize it to that matrix :
    tclArray1 <- tcltk::tclArray()
    for (i in (1:length(matrix1[, 1]))) {
      for (j in (1:length(matrix1[1, ]))) {
        tclArray1[[i-1, j-1]] <- matrix1[i, j]
      }
    }
    displayInTable <- function(tclarray, height = -1, width = -1, nrow = -1, ncol = -1){
      # Create table widget and scrollbar widgets
      tcltk::tclRequire("Tktable")
      Table <- tcltk::tkwidget(MyWin$TABLE, "table", rows = nrow, cols = ncol, titlerows = 1, titlecols = 1, height = height+1, width = width+3,
                               xscrollcommand = function(...) tcltk::tkset(xscr, ...), yscrollcommand = function(...) tcltk::tkset(yscr, ...))
      xscr <- tcltk::tkscrollbar(MyWin$TABLE, orient = "horizontal", command = function(...) tcltk::tkxview(Table, ...))
      yscr <- tcltk::tkscrollbar(MyWin$TABLE, command = function(...) tcltk::tkyview(Table, ...))
      # Grid the table and scrollbars
      tcltk::tkgrid(Table, yscr)
      tcltk::tkgrid.configure(yscr, sticky = "nsw")
      tcltk::tkgrid(xscr, sticky = "new")
      tcltk::tkconfigure(Table, variable = tclarray, background = "papayawhip", selectmode = "extended")
      tcltk::tkconfigure(Table, selectmode = "extended", rowseparator = "\"\n\"", colseparator = "\"\t\"")
      # Configuration of table tags
      tcltk::tktag.configure(Table, "title", background = "tan")
      # To control whether rows and/or columns can be resized
      tcltk::tkconfigure(Table, resizeborders = "both")
    }
    Table <- displayInTable(tclArray1, nrow = nrow(matrix1), ncol = ncol(matrix1), height = 30, width = 20)
  }

  # Tcltk Commands: dataset.cmd----
  dataset.cmd <- function() {
    if (is.null(MyTable) == TRUE) stop("ERREUR: PAS DE MyTable")
    if (is.null(WIN$FRAME) == FALSE) tcltk::tkdestroy(WIN$FRAME)
    WIN$FRAME <<- tcltk2::tk2frame(WIN)
    table.cmd(MyTable, WIN$FRAME)
    tcltk::tkpack(WIN$FRAME, side = "bottom")
  }
  # Tkgui Main Window----
  WIN <<- tcltk::tktoplevel()
  tcltk::tkconfigure(WIN, borderwidth = 10, bg = "tan")
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkwm.title(WIN, paste("trident", METADATA$VERSION))
  tcltk2::tk2ico.setFromFile(WIN, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
  # Basic layout:
  WIN$MENU <- tcltk2::tk2menu(WIN)
  tcltk::tkconfigure(WIN, menu = WIN$MENU)
  WIN$NOTEBOOK <- tcltk2::tk2notebook(WIN, height = 90, tabs = c("Microwear", "Data", "Organize", "Visualize", "Batch analysis"))
  tcltk::tkpack(WIN$NOTEBOOK, side = "top", fill = "both" , expand = TRUE)
  WIN$FRAME <- NULL

  # Notetab 'Microwear'----
  WIN$NOTEBOOK$COL <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Microwear")


  # Notetab 'Data'----
  WIN$NOTEBOOK$DAT <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Data")
  # Create buttons
  OPEN.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), width = 50, height = 50,
                              command = function(){
                                File.tmp <- tcltk::tk_choose.files(filters = matrix(c("Calc", "R file", "Text", "All files", ".csv", ".txt", ".R", "*"), ncol = 2))
                                if (tools::file_ext(File.tmp) == "txt") MyFile <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL))
                                if (tools::file_ext(File.tmp) == "csv") MyFile <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL))
                                if (is.null(MyTable) == FALSE) {
                                  WIN3 <- tcltk::tktoplevel()
                                  YES.BTN <- tcltk::tkbutton(WIN3, text = "Yes", command = function() {
                                    MyTable <<- MyFile
                                    tcltk::tkdestroy(WIN3)
                                  })
                                  NO.BTN <- tcltk::tkbutton(WIN3, text = "No", command = function() {tcltk::tkdestroy(WIN3)})
                                  tcltk::tkgrid(tcltk::tklabel(WIN3, text = "Do you want to replace current dataset?"))
                                  tcltk::tkgrid(YES.BTN, NO.BTN, padx = 5, pady = 5, sticky = "ns")
                                }
                                if (is.null(MyTable) == TRUE) MyTable <<- MyFile
                                dataset.cmd()
                              })
  BUILD.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","build.gif", package = "trident")), width = 50, height = 50,
                               command = function() {
                                 dataset.cmd()
                               })
  IMPORT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","import.gif", package = "trident")), width = 50, height = 50, command = function(){})
  REMOVE.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","rm.gif", package = "trident")), width = 50, height = 50, command = function(){})

  # Grid all
  tcltk::tkgrid(OPEN.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$DAT, orientation = "vertical"),
                BUILD.BTN, IMPORT.BTN, REMOVE.BTN,
                padx = 5, pady = 5, sticky = "ns")
  # Tooltips
  tcltk2::tk2tip(OPEN.BTN, "Open...")
  tcltk2::tk2tip(BUILD.BTN, "Build new dataset...")
  tcltk2::tk2tip(IMPORT.BTN, "Import variable...")
  tcltk2::tk2tip(REMOVE.BTN, "Remove variable...")



  # Notetab 'Organize'----
  WIN$NOTEBOOK$ORG <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Organize")
  # Create buttons
  NORM.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","norm.gif", package = "trident")), width = 50, height = 50, command = function(){})
  VARHOMO.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","varhomo.gif", package = "trident")), width = 50, height = 50, command = function(){})
  MULTI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","multicheck.gif", package = "trident")), width = 50, height = 50, command = function(){})
  BOXCOX.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","boxcox.gif", package = "trident")), width = 50, height = 50,
                                command = function(){
                                  MyBoxCox <- trident::trident.boxcox(MyTable, y)
                                  MyTable <- MyBoxCox$boxcox
                                  MyLambda <- MyBoxCox$lambda
                                })
  ASSIGN.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  OUTLIERS.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  PVAL.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  DISCRI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), width = 50, height = 50, command = function(){})
  NONDISCRI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), width = 50, height = 50, command = function(){})
  AUTOTOP.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), width = 50, height = 50, command = function(){})
  CLASSBY.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  # Grid all
  tcltk::tkgrid(NORM.BTN, VARHOMO.BTN, MULTI.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                BOXCOX.BTN, OUTLIERS.BTN, ASSIGN.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                PVAL.BTN, DISCRI.BTN, NONDISCRI.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                AUTOTOP.BTN, CLASSBY.BTN,
                padx = 5, pady = 5, sticky = "ns")
  # Tooltips
  tcltk2::tk2tip(NORM.BTN, "Normality?")
  tcltk2::tk2tip(VARHOMO.BTN, "Homoscedasticity?")
  tcltk2::tk2tip(MULTI.BTN, "Multicheck...")
  tcltk2::tk2tip(BOXCOX.BTN, "Boxcox transformation...")
  tcltk2::tk2tip(ASSIGN.BTN, "Assign to families...")
  tcltk2::tk2tip(OUTLIERS.BTN, "Remove outliers...")
  tcltk2::tk2tip(PVAL.BTN, "Get p-values...")
  tcltk2::tk2tip(DISCRI.BTN, "Get discriminating variables...")
  tcltk2::tk2tip(NONDISCRI.BTN, "Get non-discriminating variables...")
  tcltk2::tk2tip(AUTOTOP.BTN, "Top 3 auto...")
  tcltk2::tk2tip(CLASSBY.BTN, "Classify variables by...")


  # Notetab 'Visualize'----
  WIN$NOTEBOOK$VIS <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Visualize")
  # Create buttons
  BIPLOT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  BOXPLOT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  VIOLIN.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  PCA.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  DFA.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})


  # Grid all
  #tcltk::tkgrid(tcltk::tklabel(WIN$NOTEBOOK$VIS, text = "Plots..."), padx = 5, pady = 5, columnspan = 6)
  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$VIS, orientation = "vertical"),
                PCA.BTN, DFA.BTN,
                padx = 5, pady = 5, sticky = "ns")
  # Tooltips
  tcltk2::tk2tip(BIPLOT.BTN, "Biplot...")
  tcltk2::tk2tip(BOXPLOT.BTN, "Boxplot...")
  tcltk2::tk2tip(VIOLIN.BTN, "Violin plot...")
  tcltk2::tk2tip(PCA.BTN, "PCA...")
  tcltk2::tk2tip(DFA.BTN, "DFA...")

  # Notetab 'Batch analysis'----
  WIN$NOTEBOOK$BATCH <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Batch analysis")
  # Create buttons
  # Grid all
  # Tooltips



  # Menu 'File'----
  WIN$FILE <- tcltk::tkmenu(WIN$MENU, tearoff = FALSE)
  # Open
  tcltk::tkadd(WIN$FILE, "command", label = "Open file...", command = function(){})
  # Save
  tcltk::tkadd(WIN$FILE, "separator")
  WIN$FILE$SAVE <- tcltk::tkmenu(WIN$FILE, tearoff = FALSE)
  tcltk::tkadd(WIN$FILE$SAVE, "command", label = "Save project...", command = function(){})
  tcltk::tkadd(WIN$FILE, "cascade", label = "Save", menu = WIN$FILE$SAVE)
  # Exit
  tcltk::tkadd(WIN$FILE, "separator")
  tcltk::tkadd(WIN$FILE, "command", label = "Quit", command = function() tcltk::tkdestroy(WIN))
  # Add File to menu
  tcltk::tkadd(WIN$MENU, "cascade", label = "File", menu = WIN$FILE)

  # Menu 'Edit'----
  # Menu 'Options'----



  # Wrapping----
  tcltk::tcl("wm", "attributes", WIN, topmost = TRUE)
  tcltk::tcl("wm", "attributes", WIN, topmost = FALSE)
}
