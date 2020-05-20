#' @title trident.gui
#' @description Open the trident graphical user interface.
#' @export
trident.gui <- function(){
  #################################################################
  # TCLTK OBJECTS----
  METADATA <- list(VERSION = '0.1.0', DESCRIPTION = "Dusty Sandbox")
  PROJECT <- NULL
  PROJECT <- list(MYFILE = NULL,
                  MYDATASET = NULL)
  #################################################################

  #################################################################
  # TCLTK COMMANDS
  # build.table.cmd----
  build.table.cmd <- function (x, win){
    WIN2 <<- win
    WIN2$TABLE <<- tcltk::tkframe(WIN2)
    WIN2$SAVE <- tcltk::tkframe(WIN2)
    SAVEBUTTON <- tcltk::tkbutton(WIN2$SAVE, text = "SAVE", justify = "left", width = 10, command = function(){
      utils::write.table(x, file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN2, title = "Save table as...", initialfile = METADATA$FILENAME, defaultextension = ".txt")),
                         append = TRUE, quote = FALSE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
      tcltk::tkdestroy(WIN2)
    })
    tcltk::tkpack(WIN2$TABLE, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkpack(WIN2$SAVE, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkgrid(SAVEBUTTON, padx = 5, pady = 5)
    # ...Create table for tcltk widget
    matrix <- rbind(colnames(x), x)
    matrix1 <- cbind(row.names(matrix), matrix)
    matrix1 <- t(t(matrix1))
    # ...Define a Tcl array and initialize it to that matrix :
    tclArray1 <- tcltk::tclArray()
    for (i in (1:length(matrix1[, 1]))) {
      for (j in (1:length(matrix1[1, ]))) {
        tclArray1[[i-1, j-1]] <- matrix1[i, j]
      }
    }
    displayInTable <- function(tclarray, height = -1, width = -1, nrow = -1, ncol = -1){
      # ...Create table widget and scrollbar widgets
      tcltk::tclRequire("Tktable")
      Table <- tcltk::tkwidget(WIN2$TABLE, "table", rows = nrow, cols = ncol, titlerows = 1, titlecols = 1, height = height +1, width = width +3,
                               xscrollcommand = function(...) tcltk::tkset(xscr, ...), yscrollcommand = function(...) tcltk::tkset(yscr, ...))
      xscr <- tcltk::tkscrollbar(WIN2$TABLE, orient = "horizontal", command = function(...) tcltk::tkxview(Table, ...))
      yscr <- tcltk::tkscrollbar(WIN2$TABLE, command = function(...) tcltk::tkyview(Table, ...))
      # ...Grid the table and scrollbars
      tcltk::tkgrid(Table, yscr)
      tcltk::tkgrid.configure(yscr, sticky = "nsw")
      tcltk::tkgrid(xscr, sticky = "new")
      tcltk::tkconfigure(Table, variable = tclarray, background = "papayawhip", selectmode = "extended")
      tcltk::tkconfigure(Table, selectmode = "extended", rowseparator = "\"\n\"", colseparator = "\"\t\"")
      # ...Configuration of table tags
      tcltk::tktag.configure(Table, "title", background = "tan")
      # ...To control whether rows and/or columns can be resized
      tcltk::tkconfigure(Table, resizeborders = "both")
    }
    Table <- displayInTable(tclArray1, nrow = nrow(matrix1), ncol = ncol(matrix1), height = 30, width = 20)
  }
  #################################################################

  #################################################################
  # TKGUI - MAIN WINDOW
  # Window----
  WIN <<- tcltk::tktoplevel()
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkconfigure(WIN, borderwidth = 10, bg = "tan")
  tcltk::tkwm.title(WIN, paste("trident", METADATA$VERSION, "-", METADATA$DESCRIPTION))
  tcltk2::tk2ico.setFromFile(WIN, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
  #################################################################

  #################################################################
  # TKGUI - MENU
  # Menu----
  MENU <- tcltk2::tk2menu(WIN)
  tcltk::tkconfigure(WIN, menu = MENU)
  # Menu 'File'----
  MENU$FILE <- tcltk::tkmenu(MENU, tearoff = FALSE)
  #
  # ...New project
  tcltk::tkadd(MENU$FILE, "command", label = "New project...      (Ctrl+N)", command = function(){})
  #
  # ...Open project
  tcltk::tkadd(MENU$FILE, "command", label = "Open project...     (Ctrl+O)", command = function(){})
  #
  # ...Save project
  MENU$FILE$SAVE <- tcltk::tkmenu(MENU$FILE, tearoff = FALSE)
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save...        (Ctrl+S)", command = function(){})
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save as...", command = function(){})
  tcltk::tkadd(MENU$FILE, "cascade", label = "Save project", menu = MENU$FILE$SAVE)
  #
  # ...Exit
  tcltk::tkadd(MENU$FILE, "separator")
  tcltk::tkadd(MENU$FILE, "command", label = "Quit...      (Ctrl+Q)", command = function() tcltk::tkdestroy(WIN))
  #
  # ...Add File to menu
  tcltk::tkadd(MENU, "cascade", label = "File", menu = MENU$FILE)
  #
  # ...Shortcuts
  tcltk::tkbind(WIN,"<Control-n>", function() tcltk::tkmessageBox(message = 'New project'))
  tcltk::tkbind(WIN,"<Control-o>", function() tcltk::tkmessageBox(message = 'Open project'))
  tcltk::tkbind(WIN,"<Control-s>", function() tcltk::tkmessageBox(message = 'Save'))
  tcltk::tkbind(WIN,"<Control-q>", function() tcltk::tkmessageBox(message = 'Quit'))

  # Menu 'Edit'----
  MENU$EDIT <- tcltk::tkmenu(MENU, tearoff = FALSE)
  #
  # ...Undo
  tcltk::tkadd(MENU$EDIT, "command", label = "Undo...     (Ctrl+Z)", command = function(){})
  #
  # ...Redo
  tcltk::tkadd(MENU$EDIT, "command", label = "Redo...     (Ctrl+Y)", command = function(){})
  #
  # Add Edit to menu
  tcltk::tkadd(MENU, "cascade", label = "Edit", menu = MENU$EDIT)
  #
  # ...Shortcuts
  tcltk::tkbind(WIN,"<Control-z>", function() tcltk::tkmessageBox(message = 'Undo'))
  tcltk::tkbind(WIN,"<Control-y>", function() tcltk::tkmessageBox(message = 'Redo'))

  # Menu 'About'----
  MENU$ABOUT <- tcltk::tkmenu(MENU, tearoff = FALSE)
  # ...About
  tcltk::tkadd(MENU$ABOUT, "command", label = "About...", command = function(){})
  # Add About to menu
  tcltk::tkadd(MENU, "cascade", label = "About", menu = MENU$ABOUT)
  #################################################################

  #################################################################
  #TKGUI - NOTEBOOK
  # Notebook----
  NOTEBOOK <- tcltk2::tk2notebook(WIN, height = 100, tabs = c("Data", "Statistics", "Variables", "Microwear", "Plots", "Batch analysis"))
  tcltk::tkpack(NOTEBOOK, side = "top", fill = "both" , expand = FALSE)
  # Notetab 'Data'----
  NOTEBOOK$DATA <- tcltk2::tk2notetab(NOTEBOOK, "Data")
  # ...Create buttons
  OPEN.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Open", compound = "top", command = function(){
                                # ...Open the file and read it with 'tibble'
                                File.tmp <- tcltk::tk_choose.files(filters = matrix(c("Calc", "R file", "Text", "All files", ".csv", ".txt", ".R", "*"), ncol = 2))
                                n <- length(PROJECT$MYFILE) +1
                                # REMARQUE: Utiliser les fonctions du package 'readr' à la place de 'read.table' ?
                                if (tools::file_ext(File.tmp) == "txt") PROJECT$MYFILE[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL))
                                if (tools::file_ext(File.tmp) == "csv") PROJECT$MYFILE[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL))
                                if (is.null(PROJECT$MYDATASET) == FALSE) {
                                  # ...a window to choose whether the current dataset must be replaced
                                  WIN3 <- tcltk::tktoplevel()
                                  # ...Yes
                                  YES.BTN <- tcltk::tkbutton(WIN3, text = "Yes", command = function() {
                                    PROJECT$MYDATASET <<- PROJECT$MYFILE[[n]]
                                    tcltk::tkdestroy(WIN3)
                                  })
                                  # ...No
                                  NO.BTN <- tcltk::tkbutton(WIN3, text = "No", command = function() {tcltk::tkdestroy(WIN3)})
                                  tcltk::tkgrid(tcltk::tklabel(WIN3, text = "Do you want to replace current dataset?"))
                                  tcltk::tkgrid(YES.BTN, NO.BTN, padx = 5, pady = 5, sticky = "ns")
                                }
                                # ...Display dataset in the adequate frame
                                if (is.null(PROJECT$MYDATASET) == TRUE) PROJECT$MYDATASET <<- PROJECT$MYFILE[[n]]
                                if (is.null(WIN$TABLE1) == FALSE) tcltk::tkdestroy(WIN$TABLE1)
                                WIN$TABLE1 <<- tcltk2::tk2frame(WIN)
                                build.table.cmd(PROJECT$MYDATASET, WIN$TABLE1)
                                tcltk::tkpack(WIN$TABLE1, side = "top", expand = TRUE)
                              })
  BUILD.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","build.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Build", compound = "top", command = function(){})
  COMBINE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","combine.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Combine", compound = "top", command = function(){})
  IMPORT.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","import.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Import", compound = "top", command = function(){})
  REMOVE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","rm.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Remove", compound = "top", command = function(){})
  # ...Create menubuttons
  # .......Transformations:
  TRANS.MBTN <- tcltk::tkmenubutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","transform.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Transform", compound = "top")
  TRANS.MENU <- tcltk::tkmenu(TRANS.MBTN)
  tcltk::tkconfigure(TRANS.MBTN, menu = TRANS.MENU)
  tcltk::tkadd(TRANS.MENU, "command", label = "Boxcox transformation...", command = function(){})
  tcltk::tkadd(TRANS.MENU, "command", label = "Remove outliers...", command = function(){})
  # ...Grid all
  tcltk::tkgrid(OPEN.BTN, COMBINE.BTN, BUILD.BTN, IMPORT.BTN, REMOVE.BTN, TRANS.MBTN, padx = 5, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(OPEN.BTN, "Open data...")
  tcltk2::tk2tip(BUILD.BTN, "Build new dataset...")
  tcltk2::tk2tip(COMBINE.BTN, "Combine two or more datasets...")
  tcltk2::tk2tip(IMPORT.BTN, "Import variable...")
  tcltk2::tk2tip(REMOVE.BTN, "Remove variable...")

  # Notetab 'Statistics'----
  NOTEBOOK$STATS <- tcltk2::tk2notetab(NOTEBOOK, "Statistics")
  # ...Create buttons
  NORM.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","norm.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Normality", compound = "top", command = function(){})
  HOMO.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","varhomo.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Var.Homosc.", compound = "top", command = function(){})
  SUM.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","summary.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Summary", compound = "top", command = function(){})
  PVAL.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","pval.gif", package = "trident")), height = 50, relief = "flat",
                              text = "P-value", compound = "top", command = function(){})
  DISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Discriminant", compound = "top", command = function(){})
  NONDISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Non-discriminant", compound = "top", command = function(){})
  MULTI.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","multicheck.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Multicheck", compound = "top", command = function(){
                                # ...a window to select the variables x and the factor y
                                WIN2 <- tcltk::tktoplevel()
                                NumVar <- NULL
                                FacVar <- NULL
                                LABELS <- colnames(PROJECT$MYDATASET)
                                #MyList <- tcltk::tk_select.list(choices = LABELS, preselect = NULL, multiple = TRUE, title = "Choose variables")
                                NUMVARLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "multiple", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                FACVARLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...buttons
                                OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function(){
                                  NumVar <<- tcltk2::selection(NUMVARLIST)
                                  FacVar <<- tcltk2::selection(FACVARLIST)

                                  tcltk::tkdestroy(WIN2)
                                  tcltk::tkmessageBox(message = c("Numeric variables selected:\n", paste(LABELS[NumVar], "\n"), "Factorial variable selected:\n", paste(LABELS[FacVar], "\n")))
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2, text = "Cancel", command = function() tcltk::tkdestroy(WIN2))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN2, text = "Choose numeric variables"), tcltk::tklabel(WIN2, text = "Choose factor"))
                                tcltk::tkgrid(NUMVARLIST, FACVARLIST)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)

                              })
  # ...Grid all
  tcltk::tkgrid(NORM.BTN, HOMO.BTN, SUM.BTN, PVAL.BTN, DISC.BTN, NONDISC.BTN, MULTI.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips

  # Notetab 'Variables'----
  NOTEBOOK$VARIA <- tcltk2::tk2notetab(NOTEBOOK, "Variables")
  # ...Create menubuttons
  # .......Arrange by:
  ARRNG.MBTN <- tcltk::tkmenubutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","arrange.gif", package = "trident")), height = 50, relief = "flat",
                                   text = "Arrange by", compound = "top")
  ARRNG.MENU <- tcltk::tkmenu(ARRNG.MBTN)
  tcltk::tkconfigure(ARRNG.MBTN, menu = ARRNG.MENU)
  tcltk::tkadd(ARRNG.MENU, "command", label = "F-stat...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "ANOVA P-value...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "Kruskall P-value...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "Mean HSD P-value...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "Mean LSD P-value...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "HSD P-value by group priority...", command = function(){})
  tcltk::tkadd(ARRNG.MENU, "command", label = "LSD P-value by group priority...", command = function(){})
  # ...Create buttons
  TAG.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","tag.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Tag", compound = "top", command = function(){})
  TOP3.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Top 3", compound = "top", command = function(){})
  # ...Grid all
  tcltk::tkgrid(TAG.BTN, ARRNG.MBTN, TOP3.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips

  # Notetab 'Microwear'----
  NOTEBOOK$MICRO <- tcltk2::tk2notetab(NOTEBOOK, "Microwear")
  # ...Create buttons
  LOAD.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Load", compound = "top", command = function(){})
  CLEAN.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Clean", compound = "top", command = function(){})
  ABBOTT.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                   text = "Abbott", compound = "top")
  ELLI.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Elli.acv", compound = "top")
  FACET.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Facettes", compound = "top")
  INDFRAC.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Ind.Frac.", compound = "top")
  COMPLEX.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Complexity", compound = "top")
  TOPO.BTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Topology", compound = "top")
  # ...Grid all
  tcltk::tkgrid(LOAD.BTN, CLEAN.BTN, ABBOTT.BTN, ELLI.BTN, FACET.BTN, INDFRAC.BTN, COMPLEX.BTN, TOPO.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")

  # ...Tooltips

  # Notetab 'Plots'----
  NOTEBOOK$PLOTS <- tcltk2::tk2notetab(NOTEBOOK, "Plots")
  # ...Create buttons
  BIPLOT.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","biplot.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Biplot", compound = "top", command = function(){
                                # ...a window to select x, y and the factor
                                WIN2 <- tcltk::tktoplevel()
                                LABELS <- colnames(PROJECT$MYDATASET)
                                Myx <- NULL
                                Myy <- NULL
                                Myfactor <- NULL
                                XLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                YLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                FACTORLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function(){
                                  Myx <<- tcltk2::selection(XLIST)
                                  Myy <<- tcltk2::selection(YLIST)
                                  Myfactor <<- tcltk2::selection(FACTORLIST)
                                  tcltk::tkdestroy(WIN2)
                                  # ...Biplot window
                                  WIN3 <<- tcltk::tktoplevel()
                                  tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                  tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                  WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                  WIN3$SAVE <- tcltk::tkframe(WIN3)
                                  # ...Create plot for tcltk widget
                                  TKPLOT <- NULL
                                  Plot <- ggplot2::ggplot(data = PROJECT$MYDATASET, ggplot2::aes(x = PROJECT$MYDATASET[, Myx], y = PROJECT$MYDATASET[, Myy], group = PROJECT$MYDATASET[, Myfactor])) +
                                    ggplot2::labs(x = LABELS[Myx], y = LABELS[Myy]) +
                                    ggplot2::guides(size = FALSE) +
                                    ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                                                   legend.position = "right", legend.title = ggplot2::element_text(size = 12),
                                                   axis.text.x = ggplot2::element_text(size = 9, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                   axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                   panel.background = ggplot2::element_rect(fill = NA, colour = "#000000", linetype = "dashed"),
                                                   panel.grid.major = ggplot2::element_line(colour = "#A0A0A0"),
                                                   panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0"),
                                                   panel.ontop = FALSE,
                                                   axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                                                   axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
                                    ggplot2::geom_point(ggplot2::aes(shape = PROJECT$MYDATASET[, Myfactor], color = PROJECT$MYDATASET[, Myfactor]), size = 2) +
                                    ggplot2::scale_color_manual(name = LABELS[Myfactor], labels = levels(PROJECT$MYDATASET[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                                    ggplot2::scale_shape_manual(name = LABELS[Myfactor], labels = levels(PROJECT$MYDATASET[, Myfactor]), values = c(16, 17, 15, 1, 2, 5, 7, 12)) +
                                    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

                                  TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function (){graphics::plot(Plot)})
                                  # ...Create buttons
                                  SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function(){
                                    ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(LABELS[Myx], "_vs_", LABELS[Myy], sep =), defaultextension = ".png")), plot = Plot)
                                    tcltk::tkdestroy(WIN3)
                                  })
                                  # Grid all
                                  tcltk::tkpack(WIN3$PLOT, side = "top", fill = "both" , expand = TRUE)
                                  tcltk::tkpack(WIN3$SAVE, side = "top", fill = "both" , expand = TRUE)
                                  tcltk::tkgrid(SAVEBUTTON, padx = 5, pady = 5)
                                  tcltk::tkgrid(TKPLOT)


                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2, text = "Cancel", command = function() tcltk::tkdestroy(WIN2))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN2, text = "Choose x-axis variable"),  tcltk::tklabel(WIN2, text = "Choose y-axis variable"), tcltk::tklabel(WIN2, text = "Choose factor"))
                                tcltk::tkgrid(XLIST, YLIST, FACTORLIST)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)

                              })
  BOXPLOT.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","boxplot.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Boxplot", compound = "top", command = function(){

                                  # ...a window to select x and the factor
                                  WIN2 <- tcltk::tktoplevel()
                                  LABELS <- colnames(PROJECT$MYDATASET)
                                  Myy <- NULL
                                  Myfactor <- NULL
                                  YLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  FACTORLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  # ...OK button
                                  OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function(){
                                    Myy <<- tcltk2::selection(YLIST)
                                    Myfactor <<- tcltk2::selection(FACTORLIST)
                                    tcltk::tkdestroy(WIN2)
                                    # ...Biplot window
                                    WIN3 <<- tcltk::tktoplevel()
                                    tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                    tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                    WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                    WIN3$SAVE <- tcltk::tkframe(WIN3)
                                    # ...Create plot for tcltk widget
                                    TKPLOT <- NULL
                                    Plot <- ggplot2::ggplot(data = PROJECT$MYDATASET, ggplot2::aes(y = PROJECT$MYDATASET[, Myy], group = PROJECT$MYDATASET[, Myfactor])) +
                                      ggplot2::labs(y = LABELS[Myy]) +
                                      ggplot2::guides(size = FALSE) +
                                      ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                                                     legend.position = "right", legend.title = ggplot2::element_text(size = 12),
                                                     axis.text.x = ggplot2::element_text(size = 9, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                     axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                     panel.background = ggplot2::element_rect(fill = NA, colour = "#000000", linetype = "dashed"),
                                                     panel.grid.major = ggplot2::element_line(colour = "#A0A0A0"),
                                                     panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0"),
                                                     panel.ontop = FALSE,
                                                     axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                                                     axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
                                      ggplot2::geom_boxplot(ggplot2::aes(fill = PROJECT$MYDATASET[, Myfactor]), size = 1) +
                                      #ggplot2::scale_color_manual(name = LABELS[Myfactor], labels = levels(PROJECT$MYDATASET[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                                      ggplot2::scale_fill_manual(name = LABELS[Myfactor], labels = levels(PROJECT$MYDATASET[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                                      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

                                    TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function (){graphics::plot(Plot)})
                                    # ...Create buttons
                                    SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function(){
                                      ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(LABELS[Myx], "_vs_", LABELS[Myy], sep =), defaultextension = ".png")), plot = Plot)
                                      tcltk::tkdestroy(WIN3)
                                    })
                                    # Grid all
                                    tcltk::tkpack(WIN3$PLOT, side = "top", fill = "both" , expand = TRUE)
                                    tcltk::tkpack(WIN3$SAVE, side = "top", fill = "both" , expand = TRUE)
                                    tcltk::tkgrid(SAVEBUTTON, padx = 5, pady = 5)
                                    tcltk::tkgrid(TKPLOT)


                                  })
                                  CANCEL.BTN <- tcltk2::tk2button(WIN2, text = "Cancel", command = function() tcltk::tkdestroy(WIN2))
                                  # ...grid all
                                  tcltk::tkgrid(tcltk::tklabel(WIN2, text = "Choose y-axis variable"), tcltk::tklabel(WIN2, text = "Choose factor"))
                                  tcltk::tkgrid(YLIST, FACTORLIST)
                                  tcltk::tkgrid(OK.BTN, CANCEL.BTN)

                                })
  VIOLIN.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","violin.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Violin", compound = "top", command = function(){

                                  # ...a window to select x and the factor
                                  WIN2 <- tcltk::tktoplevel()
                                  LABELS <- colnames(PROJECT$MYDATASET)
                                  Myy <- NULL
                                  Myfactor <- NULL
                                  YLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  FACTORLIST <- tcltk2::tk2listbox(WIN2, values = LABELS, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  # ...OK button
                                  OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function(){
                                    Myy <<- tcltk2::selection(YLIST)
                                    Myfactor <<- tcltk2::selection(FACTORLIST)
                                    tcltk::tkdestroy(WIN2)
                                    # ...Biplot window
                                    WIN3 <<- tcltk::tktoplevel()
                                    tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                    tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                    WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                    WIN3$SAVE <- tcltk::tkframe(WIN3)
                                    # ...Create plot for tcltk widget
                                    TKPLOT <- NULL
                                    Plot <- ggplot2::ggplot(data = PROJECT$MYDATASET, ggplot2::aes(x = "", y = PROJECT$MYDATASET[, Myy], group = PROJECT$MYDATASET[, Myfactor])) +
                                      ggplot2::labs(y = LABELS[Myy], x = "") +
                                      ggplot2::guides(size = FALSE) +
                                      ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                                                     legend.position = "right", legend.title = ggplot2::element_text(size = 12),
                                                     axis.text.x = ggplot2::element_text(size = 9, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                     axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                                                     panel.background = ggplot2::element_rect(fill = NA, colour = "#000000", linetype = "dashed"),
                                                     panel.grid.major = ggplot2::element_line(colour = "#A0A0A0"),
                                                     panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0"),
                                                     panel.ontop = FALSE,
                                                     axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                                                     axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
                                      ggplot2::geom_violin(ggplot2::aes(fill = PROJECT$MYDATASET[, Myfactor]), scale = "area", size = 0.5) +
                                      ggplot2::scale_fill_manual(name = LABELS[Myfactor], labels = levels(PROJECT$MYDATASET[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
                                      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

                                    TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function (){graphics::plot(Plot)})
                                    # ...Create buttons
                                    SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function(){
                                      ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(LABELS[Myx], "_vs_", LABELS[Myy], sep =), defaultextension = ".png")), plot = Plot)
                                      tcltk::tkdestroy(WIN3)
                                    })
                                    # Grid all
                                    tcltk::tkpack(WIN3$PLOT, side = "top", fill = "both" , expand = TRUE)
                                    tcltk::tkpack(WIN3$SAVE, side = "top", fill = "both" , expand = TRUE)
                                    tcltk::tkgrid(SAVEBUTTON, padx = 5, pady = 5)
                                    tcltk::tkgrid(TKPLOT)


                                  })
                                  CANCEL.BTN <- tcltk2::tk2button(WIN2, text = "Cancel", command = function() tcltk::tkdestroy(WIN2))
                                  # ...grid all
                                  tcltk::tkgrid(tcltk::tklabel(WIN2, text = "Choose y-axis variable"), tcltk::tklabel(WIN2, text = "Choose factor"))
                                  tcltk::tkgrid(YLIST, FACTORLIST)
                                  tcltk::tkgrid(OK.BTN, CANCEL.BTN)

                                })
  PCA.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","pca.gif", package = "trident")), height = 50, relief = "flat",
                                text = "PCA", compound = "top", command = function(){})
  DFA.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","dfa.gif", package = "trident")), height = 50, relief = "flat",
                                text = "DFA", compound = "top", command = function(){})
  # ...Grid all
  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN, DFA.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips

  # Notetab 'Batch analysis'----
  NOTEBOOK$BATCH <- tcltk2::tk2notetab(NOTEBOOK, "Batch analysis")
  # ...Create buttons
  DIR.BTN <- tcltk::tkbutton(NOTEBOOK$BATCH, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","directory.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Directory", compound = "top", command = function(){})
  LAUNCH.BTN <- tcltk::tkbutton(NOTEBOOK$BATCH, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","batch.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Launch", compound = "top", command = function(){})
  # ...Grid all
  tcltk::tkgrid(DIR.BTN, LAUNCH.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  #################################################################

  #################################################################
  # TKGUI - WRAPPER----
  tcltk::tcl("wm", "attributes", WIN, topmost = TRUE)
  tcltk::tcl("wm", "attributes", WIN, topmost = FALSE)
  tcltk::tkbind(OPEN.BTN,"<Destroy>", function() tcltk::tkmessageBox(message = 'Hello world'))
  #################################################################
}
