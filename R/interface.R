#' @title trident.gui
#' @description Open the trident graphical user interface.
#' @export
trident.gui <- function() {
  # TCLTK OBJECTS----
  METADATA <- list(VERSION = '0.3.0', DESCRIPTION = "(Pre)release the Kraken!")
  PROJECT <- NULL
  PROJECT <- list(FILES = list(),
                  DATASET = NULL,
                  VARIABLES = NULL,
                  NAMES = NULL,
                  OPTIONS = NULL)
  PROJECT$NAMES <- list(EXPORT = tcltk::tclVar(paste("My_export")))
  PROJECT$OPTIONS <- list(JIGGER.VALUE = tcltk::tclVar("0"),
                          BOXCOX.VALUE = tcltk::tclVar("0"),
                          DIXON.VALUE = tcltk::tclVar("0"),
                          DISC.ONLY.VALUE = tcltk::tclVar("1"),
                          GEOMEAN.VALUE = tcltk::tclVar("0"),
                          BYNGR.VALUE = tcltk::tclVar("0"),
                          BY.ANOVA.VALUE = "P-value",
                          BY.KTEST.VALUE = "P-value",
                          BY.MEANPOSTHOC.VALUE = "Tukey's HSD",
                          BY.GRPOSTHOC.VALUE = "Tukey's HSD",
                          PLOT.COLORS = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                          PLOT.DPI = 300,
                          PLOT.HEIGHT = 150,
                          PLOT.WIDTH = 140,
                          PLOT.UNITS = "mm")

  # TCLTK COMMANDS
  # --build.table.cmd----
  # a command to build tables in any widget
  build.table.cmd <- function(x, widget, bg.table = "ivory", bg.title = "dimgray", height = 27, width = 12) {
    # ...STEP 1: create a frame for the table and the buttons
    WIN09A <<- tcltk::tkframe(widget)
    #WIN09B <- tcltk::tkframe(widget)
    # ...main buttons
    SAVE.BTN <- tcltk2::tk2button(WIN09A, text = "Save", tip = "Save table", width = 10, command = function() {
      utils::write.table(x, append = FALSE, quote = FALSE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = widget, title = "Save table as...", initialfile = paste("Untitled"), defaultextension = ".txt")))
    })
    EXPORT.BTN <- tcltk2::tk2button(WIN09A, text = "Export", tip = "Export data.frame object to R", width = 10, command = function() {
      # ......create window for name entry
      WIN09C <- tcltk::tktoplevel()
      # ......entry
      NAME.ENTRY <- tcltk2::tk2entry(WIN09C, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
      # ......buttons
      CONFIRM.BTN <- tcltk2::tk2button(WIN09C, text = "Confirm", tip = "Confirm name and export to R", command = function() {
        # this will give an additional name to the exported object:
        My_data_from_trident <<- data.frame(x)
        makeActiveBinding(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY)), function() My_data_from_trident, .GlobalEnv)
        tcltk::tkdestroy(WIN09C)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN09C, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(WIN09C))
      # ......grid all
      tcltk::tkgrid(tcltk::tklabel(WIN09C, text = "Name:"), NAME.ENTRY)
      tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
    })
    # ...pack and grid all
    tcltk::tkpack(WIN09A, side = "top", fill = "both" , expand = TRUE)
    #tcltk::tkpack(WIN09B, side = "top", fill = "both" , expand = TRUE)
    #tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
    #
    # ...STEP2: Create table for tcltk widget
    Mymatrix <- as.matrix(x, ncol = length(colnames((x))))
    Mymatrix <- rbind(colnames(Mymatrix), Mymatrix)
    Myarray <- cbind(c(paste("\\"), row.names(x)), Mymatrix)
    # ...Calculate optimal column width
    MyColWidth <- as.integer(quantile(nchar(c(Myarray), type = "width"), 0.5))
    # ...Define a Tcl array and initialize it to that matrix :
    MytclArray <- tcltk::tclArray()
    for (i in (1:length(Myarray[, 1]))) {
      for (j in (1:length(Myarray[1, ]))) {
        MytclArray[[i-1, j-1]] <- Myarray[i, j]
      }
    }
    # ...STEP3: a function to display the built table into a Tcl Table
    displayInTable <- function(tclarray, height = height, width = width, nrow = -1, ncol = -1, colwidth = MyColWidth){
      # ...Create table widget and scrollbar widgets
      tcltk::tclRequire("Tktable")
      Table <- tcltk::tkwidget(WIN09A, "table", rows = nrow, cols = ncol, titlerows = 1, titlecols = 1, height = height, width = width, colwidth = colwidth,
                               xscrollcommand = function(...) tcltk::tkset(xscr, ...), yscrollcommand = function(...) tcltk::tkset(yscr, ...))
      xscr <- tcltk::tkscrollbar(WIN09A, orient = "horizontal", command = function(...) tcltk::tkxview(Table, ...))
      yscr <- tcltk::tkscrollbar(WIN09A, command = function(...) tcltk::tkyview(Table, ...))
      # ...Grid the table and scrollbars
      tcltk::tkgrid(Table, yscr)
      tcltk::tkgrid.configure(yscr, sticky = "nsw")
      tcltk::tkgrid(xscr, sticky = "new")
      tcltk::tkconfigure(Table, variable = tclarray, background = bg.table, selectmode = "extended")
      tcltk::tkconfigure(Table, selectmode = "extended", rowseparator = "\"\n\"", colseparator = "\"\t\"")
      # ...Configuration of table tags
      tcltk::tktag.configure(Table, "title", background = bg.title)
      # ...To control whether rows and/or columns can be resized
      tcltk::tkconfigure(Table, resizeborders = "both")
      #tcltk::tkcget(Table, options)
    }
    Table <- displayInTable(MytclArray, nrow = nrow(Myarray), ncol = ncol(Myarray), height = height, width = width)
    tcltk::tkgrid(SAVE.BTN, padx = 5, pady = 5)
    tcltk::tkgrid(EXPORT.BTN, padx = 5, pady = 5)
  }
  # --save.project.cmd----
  save.project.cmd <- function() {
    rlist::list.save(x = PROJECT, file = tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save project...", initialfile = paste("Untitled.rds", sep = ""))))
  }
  # --open.project.cmd----
  open.project.cmd <- function() {
    if (length(PROJECT$FILES) != 0) {
      # ...Open new window
      WIN03A <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN03A, paste("Open project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN03A, text = "Yes", command = function() {
        tcltk::tkdestroy(WIN03A)
        save.project.cmd()
        PROJECT <<- rlist::list.load(choose.files())
        refresh.cmd()
      })
      NO.BTN <- tcltk2::tk2button(WIN03A, text = "No", command = function() {
        WIN03B <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN03B, text = "OK", command = function() {
          tcltk::tkdestroy(WIN03B)
          tcltk::tkdestroy(WIN03A)
          PROJECT <<- rlist::list.load(choose.files())
          refresh.cmd()
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN03B, text = "Cancel", command = function() tcltk::tkdestroy(WIN03B))
        tcltk::tkgrid(tcltk::tklabel(WIN03B, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN03A, text = "Cancel", command = function() tcltk::tkdestroy(WIN03A))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN03A, text = "Do you want to save the current project?"), columnspan = 3)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
    }
    if (length(PROJECT$FILES) == 0) {
      PROJECT <<- rlist::list.load(choose.files())
      refresh.cmd()
    }
  }
  # --new.project.cmd----
  new.project.cmd <- function() {
    if (length(PROJECT$FILES) != 0) {
      # ...Open new window
      WIN02A <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN02A, paste("New project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN02A, text = "Yes", command = function() {
        tcltk::tkdestroy(WIN02A)
        save.project.cmd()
        PROJECT <<- NULL
        PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
      })
      NO.BTN <- tcltk2::tk2button(WIN02A, text = "No", command = function() {
        WIN02B <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN02B, text = "OK", command = function() {
          tcltk::tkdestroy(WIN02B)
          tcltk::tkdestroy(WIN02A)
          PROJECT <<- NULL
          PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN02B, text = "Cancel", command = function() tcltk::tkdestroy(WIN02B))
        tcltk::tkgrid(tcltk::tklabel(WIN02B, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN02A, text = "Cancel", command = function() tcltk::tkdestroy(WIN02A))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN02A, text = "Do you want to save the current project?"), columnspan = 3)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
    }
    if (length(PROJECT$FILES) == 0) {
      PROJECT <<- NULL
      PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
    }
  }
  # --refresh.cmd----
  refresh.cmd <- function() {
    # ...TABLE1: the main dataset table
    if (is.null(WIN00$TABLE1) == FALSE) tcltk::tkdestroy(WIN00$TABLE1)
    WIN00$TABLE1 <<- tcltk2::tk2frame(WIN00)
    build.table.cmd(PROJECT$DATASET, WIN00$TABLE1, bg.table = "papayawhip", bg.title = "tan")
    #
    # ...TABLE2: the numeric variables list
    if (is.null(WIN00$TABLE2) == FALSE) tcltk::tkdestroy(WIN00$TABLE2)
    WIN00$TABLE2 <<- tcltk2::tk2frame(WIN00)
    # ...create variable list
    PROJECT$VARIABLES <<- colnames(dplyr::select_if(PROJECT$DATASET, is.numeric))
    WIN00$TABLE2$VARLIST <<- tcltk2::tk2listbox(WIN00$TABLE2, values = PROJECT$VARIABLES, selectmode = "single", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...one button to rename them all
    RENAME.BTN <- tcltk2::tk2button(WIN00$TABLE2, text = "Rename", tip = "Rename selected variable", width = 10, command = function() {
      # ......create window for name entry
      WIN18A <- tcltk::tktoplevel()
      # ......create an entry widget
      txt_var <- tcltk::tclVar(paste(PROJECT$VARIABLES[tcltk2::selection(WIN00$TABLE2$VARLIST)]))
      NAME.ENTRY <- tcltk2::tk2entry(WIN18A, textvariable = txt_var)
      # ......create buttons
      CONFIRM.BTN <- tcltk2::tk2button(WIN18A, text = "Confirm", command = function() {
        Newname <- tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))
        Oldname <- paste(PROJECT$VARIABLES[tcltk2::selection(WIN00$TABLE2$VARLIST)])
        colnames(PROJECT$DATASET)[which(colnames(PROJECT$DATASET) == Oldname)] <<- Newname
        refresh.cmd()
        tcltk::tkdestroy(WIN18A)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN18A, text = "Cancel", command = function() tcltk::tkdestroy(WIN18A))
      # ......grid all
      tcltk::tkgrid(tcltk::tklabel(WIN18A, text = "Enter new variable name:"), columnspan = 2)
      tcltk::tkgrid(NAME.ENTRY, columnspan = 2)
      tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
    })
    # ...one button to remove them all
    REMOVE.BTN <- tcltk2::tk2button(WIN00$TABLE2, text = "Remove", tip = "Remove selected variable (from current dataset only)", width = 10, command = function() {
      # ......create window to confirm removal
      WIN18B <- tcltk::tktoplevel()
      # ......create buttons
      REMOVE.BTN <- tcltk2::tk2button(WIN18B, text = "Yes (remove)", command = function() {
        Myselect <- PROJECT$VARIABLES[tcltk2::selection(WIN00$TABLE2$VARLIST)]
        PROJECT$DATASET <<- PROJECT$DATASET[, -which(colnames(PROJECT$DATASET) == Myselect)]
        refresh.cmd()
        tcltk::tkdestroy(WIN18B)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN18B, text = "No (cancel)", command = function() tcltk::tkdestroy(WIN18B))
      # ......grid all
      tcltk::tkgrid(tcltk::tklabel(WIN18B, text = "Are you sure you want to remove\nselected variable?"))
      tcltk::tkgrid(REMOVE.BTN, CANCEL.BTN)
    })
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN00$TABLE2, text = "Variables"), columnspan = 2)
    tcltk::tkgrid(WIN00$TABLE2$VARLIST, columnspan = 2)
    tcltk::tkgrid(RENAME.BTN, REMOVE.BTN, ipadx = 5)
    #
    # ...TABLE3: the file list
    if (is.null(WIN00$TABLE3) == FALSE) tcltk::tkdestroy(WIN00$TABLE3)
    WIN00$TABLE3 <<- tcltk2::tk2frame(WIN00)
    FILELIST <- tcltk2::tk2listbox(WIN00$TABLE3, values = names(PROJECT$FILES), selectmode = "single", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    tcltk::tkgrid(tcltk::tklabel(WIN00$TABLE3, text = "Opened files"))
    tcltk::tkgrid(FILELIST)
    #
    # ...pack all
    tcltk::tkpack(WIN00$TABLE1, side = "left", expand = FALSE, fill = "x", anchor = "nw")
    tcltk::tkpack(WIN00$TABLE2, side = "top", expand = FALSE, fill = "x", anchor = "nw")
    tcltk::tkpack(WIN00$TABLE3, side = "top", expand = FALSE, fill = "x", anchor = "nw")
  }
  # --quit.cmd----
  quit.cmd <- function() {
    if (length(PROJECT$FILES) != 0) {
      # ...Open new window
      WIN09A <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN09A, paste("Save project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN09A, text = "Save", command = function() {
        tcltk::tkdestroy(WIN09A)
        cmd.save.project()
        tcltk::tkdestroy(WIN00)
      })
      NO.BTN <- tcltk2::tk2button(WIN09A, text = "Discard", command = function() {
        WIN09B <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN09B, text = "Yes (discard)", command = function() {
          tcltk::tkdestroy(WIN09B)
          tcltk::tkdestroy(WIN09A)
          tcltk::tkdestroy(WIN00)
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN09B, text = "Cancel", command = function() tcltk::tkdestroy(WIN09B))
        tcltk::tkgrid(tcltk::tklabel(WIN09B, text = "Are you sure?\nCurrent project will be deleted from memory"), columnspan = 2, padx = 5, pady = 5)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN09A, text = "Cancel", command = function() tcltk::tkdestroy(WIN09A))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN09A, text = "Do you want to save the current project?"), columnspan = 3, padx = 5, pady = 5)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
    }
    if (length(PROJECT$FILES) == 0) {
      tcltk::tkdestroy(WIN00)
    }
  }

  # TKGUI - MAIN WINDOW
  # --biplot.cmd----
  biplot.cmd <- function(df) {
    # ...a window to select x, y and the factor
    WIN41 <- tcltk::tktoplevel()
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myx <- NULL
    Myy <- NULL
    Myfactor <- NULL
    XLIST <- tcltk2::tk2listbox(WIN41, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    YLIST <- tcltk2::tk2listbox(WIN41, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(WIN41, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN41, text = "OK", command = function() {
      Myx <<- colnames(Numerics)[tcltk2::selection(XLIST)]
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
      tcltk::tkdestroy(WIN41)
      # ...Biplot window
      WIN41B <<- tcltk::tktoplevel()
      tcltk::tkconfigure(WIN41B, borderwidth = 10, bg = "tan")
      tcltk::tkwm.title(WIN41B, paste("trident", METADATA$VERSION, "- biplot"))
      WIN41B$PLOT <<- tcltk::tkframe(WIN41B)
      WIN41B$BUTTONS <- tcltk::tkframe(WIN41B)
      # ...Create plot for tcltk widget
      TKPLOT <- NULL
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = Mydf[, Myx], y = Mydf[, Myy], group = Mydf[, Myfactor])) +
        ggplot2::labs(x = Myx, y = Myy) +
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
        ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor]), size = 2) +
        ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
        ggplot2::scale_shape_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = c(16, 17, 15, 1, 2, 5, 7, 12)) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      TKPLOT <- tkrplot::tkrplot(WIN41B$PLOT, fun = function() {graphics::plot(Plot)})
      #
      # ...Create buttons
      SAVE.BTN <- tcltk2::tk2button(WIN41B$BUTTONS, text = "SAVE", command = function() {
        # ...a window to select saving parameters, such as resolution, size, etc.
        WIN41C <-  tcltk::tktoplevel()
        # ......spinboxes
        DPI.SPNBX <- tcltk2::tk2spinbox(WIN41C, from = 100, to = 1000, increment = 10)
        # ......entries
        HEIGHT.NTRY <- tcltk2::tk2entry(WIN41C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
        WIDTH.NTRY <- tcltk2::tk2entry(WIN41C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
        # ......comboboxes
        UNITS.CBBX <- tcltk2::tk2combobox(WIN41C, values = c("mm", "cm", "in"))
        # ......buttons
        OK.BTN <- tcltk2::tk2button(WIN41C, text = "Ok", tip = "", command = function() {
          PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
            PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
            PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}
          PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
          # ......then destroy window
          tcltk::tkdestroy(WIN41C)
          # ......now open tkgetsavefile window
          ggplot2::ggsave(Plot,
                          file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN00,
                                                                      title = "Save plot as...",
                                                                      initialfile = paste(Myx, "vs", Myy, "with", Myfactor, "as factor", sep = " "),
                                                                      filetypes = paste (
                                                                        "{{png files} {.png}}",
                                                                        "{{eps files} {.eps}}",
                                                                        "{{jpeg files} {.jpg .jpeg} }",
                                                                        "{{pdf files} {.pdf}}",
                                                                        "{{svg files} {.svg}}",
                                                                        "{{tiff files} {.tiff}}",
                                                                        "{{wmf files} {.wmf}}",
                                                                        "{{All files} {*}}", sep =" "),
                                                                      defaultextension = ".png")),
                          dpi = PROJECT$OPTIONS$PLOT.DPI,
                          height = PROJECT$OPTIONS$PLOT.HEIGHT,
                          width = PROJECT$OPTIONS$PLOT.WIDTH,
                          units = PROJECT$OPTIONS$PLOT.UNITS,
                          limitsize = TRUE)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN41C, text = "Cancel", tip = "", command = function() tcltk::tkdestroy(WIN41C))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN41C, text = "Height:"), HEIGHT.NTRY, tcltk::tklabel(WIN41C, text = "Width:"), WIDTH.NTRY)
        tcltk::tkgrid(tcltk::tklabel(WIN41C, text = "Units:"), UNITS.CBBX, columnspan = 2)
        tcltk::tkgrid(tcltk::tklabel(WIN41C, text = "Resolution (dpi):"), DPI.SPNBX, columnspan = 2)
        tcltk::tkgrid(OK.BTN, CANCEL.BTN, columnspan = 2)
        tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
        tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
      })
      EXPORT.BTN <- tcltk2::tk2button(WIN41B$BUTTONS, text = "Export", tip = "Export data.frame object to R", width = 10, command = function() {
        # ......create window for name entry
        WIN41D <- tcltk::tktoplevel()
        # ......entry
        NAME.ENTRY <- tcltk2::tk2entry(WIN41D, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        # ......buttons
        CONFIRM.BTN <- tcltk2::tk2button(WIN41D, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          # this will give an additional name to the exported object:
          My_graph_from_trident <<- Plot
          makeActiveBinding(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY)), function() My_graph_from_trident, .GlobalEnv)
          tcltk::tkdestroy(WIN41D)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN41D, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(WIN41D))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN41D, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })
      # Grid all
      tcltk::tkpack(WIN41B$PLOT, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkpack(WIN41B$BUTTONS, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
      tcltk::tkgrid(TKPLOT)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN41, text = "Cancel", command = function() tcltk::tkdestroy(WIN41))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN41, text = "Choose x-axis variable"),  tcltk::tklabel(WIN41, text = "Choose y-axis variable"), tcltk::tklabel(WIN41, text = "Choose factor"))
    tcltk::tkgrid(XLIST, YLIST, FACTORLIST)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }
  # --boxplot.cmd----
  boxplot.cmd <- function(df) {
    # ...a window to select y and the factor
    WIN42A <- tcltk::tktoplevel()
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myy <- NULL
    Myfactor <- NULL
    YLIST <- tcltk2::tk2listbox(WIN42A, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(WIN42A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...Jiggerplot checkbutton
    JIGGER.CHKBTN <- tcltk2::tk2checkbutton(WIN42A, text = "Jiggerplot")
    tcltk::tkconfigure(JIGGER.CHKBTN, variable = PROJECT$OPTIONS$JIGGER.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN42A, text = "OK", command = function() {
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
      tcltk::tkdestroy(WIN42A)
      # ...Biplot window
      WIN42B <<- tcltk::tktoplevel()
      tcltk::tkconfigure(WIN42B, borderwidth = 10, bg = "tan")
      tcltk::tkwm.title(WIN42B, paste("trident", METADATA$VERSION, "- biplot"))
      WIN42B$PLOT <<- tcltk::tkframe(WIN42B)
      WIN42B$BUTTONS <- tcltk::tkframe(WIN42B)
      # ...Create plot for tcltk widget
      TKPLOT <- NULL
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = Mydf[, Myfactor], y = Mydf[, Myy], group = Mydf[, Myfactor])) +
        ggplot2::labs(x = Myfactor, y = Myy) +
        ggplot2::guides(size = FALSE) +
        ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                       legend.position = "right", legend.title = ggplot2::element_text(size = 12),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                       panel.background = ggplot2::element_rect(fill = NA, colour = "#000000", linetype = "dashed"),
                       panel.grid.major = ggplot2::element_line(colour = "#A0A0A0"),
                       panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0"),
                       panel.ontop = FALSE,
                       axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                       axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 0) {
        Plot <- Plot +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Myfactor]), size = 0.6)
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 1) {
        Plot <- Plot +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = colorspace::lighten(PROJECT$OPTIONS$PLOT.COLORS, amount = 0.5)) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Myfactor]), size = 0.6, show.legend = FALSE) +
          ggplot2::geom_jitter(ggplot2::aes(col = Mydf[, Myfactor]), position = ggplot2::position_jitterdodge(jitter.width = 0.5))
      }

      TKPLOT <- tkrplot::tkrplot(WIN42B$PLOT, fun = function() {graphics::plot(Plot)})
      # ...Create buttons
      SAVE.BTN <- tcltk2::tk2button(WIN42B$BUTTONS, text = "SAVE", command = function() {
        # ...a window to select saving parameters, such as resolution, size, etc.
        WIN42C <-  tcltk::tktoplevel()
        # ......spinboxes
        DPI.SPNBX <- tcltk2::tk2spinbox(WIN42C, from = 100, to = 1000, increment = 10)
        # ......entries
        HEIGHT.NTRY <- tcltk2::tk2entry(WIN42C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
        WIDTH.NTRY <- tcltk2::tk2entry(WIN42C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
        # ......comboboxes
        UNITS.CBBX <- tcltk2::tk2combobox(WIN42C, values = c("mm", "cm", "in"))
        # ......buttons
        OK.BTN <- tcltk2::tk2button(WIN42C, text = "Ok", tip = "", command = function() {
          PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
            PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
            PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}
          PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
          # ......then destroy window
          tcltk::tkdestroy(WIN42C)
          # ......now open tkgetsavefile window
          ggplot2::ggsave(Plot,
                          file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN00,
                                                                      title = "Save plot as...",
                                                                      initialfile = paste(Myy, "with", Myfactor, "as factor", sep = " "),
                                                                      filetypes = paste (
                                                                        "{{png files} {.png}}",
                                                                        "{{eps files} {.eps}}",
                                                                        "{{jpeg files} {.jpg .jpeg} }",
                                                                        "{{pdf files} {.pdf}}",
                                                                        "{{svg files} {.svg}}",
                                                                        "{{tiff files} {.tiff}}",
                                                                        "{{wmf files} {.wmf}}",
                                                                        "{{All files} {*}}", sep =" "),
                                                                      defaultextension = ".png")),
                          dpi = PROJECT$OPTIONS$PLOT.DPI,
                          height = PROJECT$OPTIONS$PLOT.HEIGHT,
                          width = PROJECT$OPTIONS$PLOT.WIDTH,
                          units = PROJECT$OPTIONS$PLOT.UNITS,
                          limitsize = TRUE)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN42C, text = "Cancel", tip = "", command = function() tcltk::tkdestroy(WIN42C))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN42C, text = "Height:"), HEIGHT.NTRY, tcltk::tklabel(WIN42C, text = "Width:"), WIDTH.NTRY)
        tcltk::tkgrid(tcltk::tklabel(WIN42C, text = "Units:"), UNITS.CBBX, columnspan = 2)
        tcltk::tkgrid(tcltk::tklabel(WIN42C, text = "Resolution (dpi):"), DPI.SPNBX, columnspan = 2)
        tcltk::tkgrid(OK.BTN, CANCEL.BTN, columnspan = 2)
        tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
        tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
      })
      EXPORT.BTN <- tcltk2::tk2button(WIN42B$BUTTONS, text = "Export", tip = "Export data.frame object to R", command = function() {
        # ......create window for name entry
        WIN42D <- tcltk::tktoplevel()
        # ......entry
        NAME.ENTRY <- tcltk2::tk2entry(WIN42D, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        # ......buttons
        CONFIRM.BTN <- tcltk2::tk2button(WIN42D, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          # this will give an additional name to the exported object:
          My_graph_from_trident <<- Plot
          makeActiveBinding(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY)), function() My_graph_from_trident, .GlobalEnv)
          tcltk::tkdestroy(WIN42D)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN42D, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(WIN42D))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN42D, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })
      # Grid all
      tcltk::tkpack(WIN42B$PLOT, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkpack(WIN42B$BUTTONS, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
      tcltk::tkgrid(TKPLOT)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN42A, text = "Cancel", command = function() tcltk::tkdestroy(WIN42A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN42A, text = "Choose y-axis variable"), tcltk::tklabel(WIN42A, text = "Choose factor"))
    tcltk::tkgrid(YLIST, FACTORLIST)
    tcltk::tkgrid(JIGGER.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }
  # --plot.violin.cmd----
  plot.violin.cmd <- function(df) {
    # ...a window to select x and the factor
    WIN43A <- tcltk::tktoplevel()
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myy <- NULL
    Myfactor <- NULL
    YLIST <- tcltk2::tk2listbox(WIN43A, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(WIN43A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN43A, text = "OK", command = function() {
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
      tcltk::tkdestroy(WIN43A)
      # ...Biplot window
      WIN43B <<- tcltk::tktoplevel()
      tcltk::tkconfigure(WIN43B, borderwidth = 10, bg = "tan")
      tcltk::tkwm.title(WIN43B, paste("trident", METADATA$VERSION, "- biplot"))
      WIN43B$PLOT <<- tcltk::tkframe(WIN43B)
      WIN43B$BUTTONS <- tcltk::tkframe(WIN43B)
      # ...Create plot for tcltk widget
      TKPLOT <- NULL
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = "", y = Mydf[, Myy], group = Mydf[, Myfactor])) +
        ggplot2::labs(y = Myy, x = "") +
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
        ggplot2::geom_violin(ggplot2::aes(fill = Mydf[, Myfactor]), scale = "area", size = 0.5) +
        ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      TKPLOT <- tkrplot::tkrplot(WIN43B$PLOT, fun = function() {graphics::plot(Plot)})
      # ...Create buttons
      SAVE.BTN <- tcltk2::tk2button(WIN43B$BUTTONS, text = "SAVE", command = function() {
        # ...a window to select saving parameters, such as resolution, size, etc.
        WIN43C <-  tcltk::tktoplevel()
        # ......spinboxes
        DPI.SPNBX <- tcltk2::tk2spinbox(WIN43C, from = 100, to = 1000, increment = 10)
        # ......entries
        HEIGHT.NTRY <- tcltk2::tk2entry(WIN43C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
        WIDTH.NTRY <- tcltk2::tk2entry(WIN43C, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
        # ......comboboxes
        UNITS.CBBX <- tcltk2::tk2combobox(WIN43C, values = c("mm", "cm", "in"))
        # ......buttons
        OK.BTN <- tcltk2::tk2button(WIN43C, text = "Ok", tip = "", command = function() {
          PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
            PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
            PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}
          PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
          # ......then destroy window
          tcltk::tkdestroy(WIN43C)
          # ......now open tkgetsavefile window
          ggplot2::ggsave(Plot,
                          file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN00,
                                                                      title = "Save plot as...",
                                                                      initialfile = paste(Myy, "with", Myfactor, "as factor", sep = " "),
                                                                      filetypes = paste (
                                                                        "{{png files} {.png}}",
                                                                        "{{eps files} {.eps}}",
                                                                        "{{jpeg files} {.jpg .jpeg} }",
                                                                        "{{pdf files} {.pdf}}",
                                                                        "{{svg files} {.svg}}",
                                                                        "{{tiff files} {.tiff}}",
                                                                        "{{wmf files} {.wmf}}",
                                                                        "{{All files} {*}}", sep =" "),
                                                                      defaultextension = ".png")),
                          dpi = PROJECT$OPTIONS$PLOT.DPI,
                          height = PROJECT$OPTIONS$PLOT.HEIGHT,
                          width = PROJECT$OPTIONS$PLOT.WIDTH,
                          units = PROJECT$OPTIONS$PLOT.UNITS,
                          limitsize = TRUE)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN43C, text = "Cancel", tip = "", command = function() tcltk::tkdestroy(WIN43C))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN43C, text = "Height:"), HEIGHT.NTRY, tcltk::tklabel(WIN43C, text = "Width:"), WIDTH.NTRY)
        tcltk::tkgrid(tcltk::tklabel(WIN43C, text = "Units:"), UNITS.CBBX, columnspan = 2)
        tcltk::tkgrid(tcltk::tklabel(WIN43C, text = "Resolution (dpi):"), DPI.SPNBX, columnspan = 2)
        tcltk::tkgrid(OK.BTN, CANCEL.BTN, columnspan = 2)
        tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
        tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
      })
      EXPORT.BTN <- tcltk2::tk2button(WIN43B$BUTTONS, text = "Export", tip = "Export data.frame object to R", command = function() {
        # ......create window for name entry
        WIN43D <- tcltk::tktoplevel()
        # ......entry
        NAME.ENTRY <- tcltk2::tk2entry(WIN43D, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        # ......buttons
        CONFIRM.BTN <- tcltk2::tk2button(WIN43D, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          # this will give an additional name to the exported object:
          My_graph_from_trident <<- Plot
          makeActiveBinding(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY)), function() My_graph_from_trident, .GlobalEnv)
          tcltk::tkdestroy(WIN43D)
        })
        CANCEL.BTN <- tcltk2::tk2button(WIN43D, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(WIN43D))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(WIN43D, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })
      # Grid all
      tcltk::tkpack(WIN43B$PLOT, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkpack(WIN43B$BUTTONS, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
      tcltk::tkgrid(TKPLOT)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN43A, text = "Cancel", command = function() tcltk::tkdestroy(WIN43A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN43A, text = "Choose y-axis variable"), tcltk::tklabel(WIN43A, text = "Choose factor"))
    tcltk::tkgrid(YLIST, FACTORLIST)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }
  # --plot.pca.cmd----
  plot.pca.cmd <- function(df) {
    # ...a window to select variables for the PCA
    WIN44A <- tcltk::tktoplevel()
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myvars <- NULL
    Myfactor <- NULL
    VARLIST <- tcltk2::tk2listbox(WIN44A, values = colnames(Numerics), selectmode = "extended", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(WIN44A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN44A, text = "OK", command = function() {
      Myvars <<- colnames(Numerics)[tcltk2::selection(VARLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
      tcltk::tkdestroy(WIN44A)
      # ...PCA
      Mypca <- prcomp(Mydf[, which(colnames(Mydf) %in% Myvars)], center = TRUE, scale. = TRUE)
      Mypca.df <- data.frame(Mypca$x)
      Mycor.df <- data.frame(Mypca$rotation)

      # ...Window to select which PC to plot
      WIN44B <<- tcltk::tktoplevel()
      # ......screeplot
      TKPLOT <- NULL
      SCREEPLOT <- factoextra::fviz_eig(Mypca, addlabels = TRUE, ylim = c(0, 50), barfill = "lightgoldenrod", barcolor = "lightgoldenrod3")
      TKPLOT <- tkrplot::tkrplot(WIN44B, fun = function() graphics::plot(SCREEPLOT))
      # ......PCs
      MYPCLIST1 <- tcltk2::tk2listbox(WIN44B, values = colnames(Mypca.df), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
      MYPCLIST2 <- tcltk2::tk2listbox(WIN44B, values = colnames(Mypca.df), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)

      # ......Buttons
      PLOT.BTN <- tcltk2::tk2button(WIN44B, text = "Plot", tip = "Plot the selected PCs against each other", command = function() {
        # ...PCA plot window
        WIN44C <<- tcltk::tktoplevel()
        tcltk::tkconfigure(WIN44C, borderwidth = 10, bg = "tan")
        #tcltk::tkwm.title(WIN44C, paste("trident", METADATA$VERSION, "- pca"))
        WIN44C$PLOT <<- tcltk::tkframe(WIN44C)
        WIN44C$BUTTONS <- tcltk::tkframe(WIN44C)
        # ...Create plot for tcltk widget
        TKPLOT <- NULL
        Plot <- ggplot2::ggplot(data = Mypca.df, ggplot2::aes(x = Mypca.df[, tcltk2::selection(MYPCLIST1)], y = Mypca.df[, tcltk2::selection(MYPCLIST2)], group = Mydf[, which(colnames(Mydf) %in% Myfactor)])) +
          ggplot2::labs(x = colnames(Mypca.df)[tcltk2::selection(MYPCLIST1)], y = colnames(Mypca.df)[tcltk2::selection(MYPCLIST2)]) +
          ggplot2::guides(size = FALSE) +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                         legend.position = "bottom", legend.title = ggplot2::element_text(size = 12),
                         axis.text.x = ggplot2::element_text(size = 9, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = NA, colour = "#000000", linetype = "dashed"),
                         panel.grid.major = ggplot2::element_line(colour = "#A0A0A0"),
                         panel.grid.minor = ggplot2::element_line(colour = "#C0C0C0"),
                         panel.ontop = FALSE,
                         axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                         axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
          ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor]), size = 2) +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
          ggplot2::scale_shape_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = c(16, 17, 15, 1, 2, 5, 7, 12)) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))
        # ...Correlation circle
        # ......change radius of the correlation circle
        Myradius <- sqrt(abs((max(Mypca.df))*(min(Mypca.df))))/2
        Mydirections <- Myradius * Mycor.df

        for (i in c(1:length(rownames(Mycor.df)))) {
          Plot <- Plot +  ggplot2::geom_segment(
            x = 0, y = 0,
            xend = Mydirections[i, tcltk2::selection(MYPCLIST1)], yend = Mydirections[i, tcltk2::selection(MYPCLIST2)],
            lineend = "butt", # See available arrow types in example above
            linejoin = "mitre",
            size = 1,  col = "steelblue",
            arrow = ggplot2::arrow(length = ggplot2::unit(0.5, 'picas')))
          # Vers la droite
          if (Mycor.df[i, tcltk2::selection(MYPCLIST1)] > 0) {
            Plot <- Plot + ggplot2::geom_text(
              x = Mydirections[i, tcltk2::selection(MYPCLIST1)], y = Mydirections[i, tcltk2::selection(MYPCLIST2)],
              label = paste(" ", rownames(Mycor.df)[i], sep = ""),
              hjust = 0, vjust = 0,  col = "steelblue", size = 3,
              angle = atan(Mycor.df[i, tcltk2::selection(MYPCLIST2)]/Mycor.df[i, tcltk2::selection(MYPCLIST1)])/pi*180
            )
          }
          # Vers la gauche
          if (Mycor.df[i, tcltk2::selection(MYPCLIST1)] < 0) {
            Plot <- Plot + ggplot2::geom_text(
              x = Mydirections[i, tcltk2::selection(MYPCLIST1)], y = Mydirections[i, tcltk2::selection(MYPCLIST2)],
              label = paste(rownames(Mycor.df)[i], " ", sep = ""),
              hjust = 1, vjust = 0,  col = "steelblue", size = 3,
              angle = atan(Mycor.df[i, tcltk2::selection(MYPCLIST2)]/Mycor.df[i, tcltk2::selection(MYPCLIST1)])/pi*180
            )
          }
        }

        TKPLOT <- tkrplot::tkrplot(WIN44C$PLOT, hscale = 1.5, vscale = 1.5, fun = function() {graphics::plot(Plot)})
        #
        # ...Create buttons
        SAVE.BTN <- tcltk2::tk2button(WIN44C$BUTTONS, text = "SAVE", command = function() {
          # ...a window to select saving parameters, such as resolution, size, etc.
          WIN44D <-  tcltk::tktoplevel()
          # ......spinboxes
          DPI.SPNBX <- tcltk2::tk2spinbox(WIN44D, from = 100, to = 1000, increment = 10)
          # ......entries
          HEIGHT.NTRY <- tcltk2::tk2entry(WIN44D, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
          WIDTH.NTRY <- tcltk2::tk2entry(WIN44D, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
          # ......comboboxes
          UNITS.CBBX <- tcltk2::tk2combobox(WIN44D, values = c("mm", "cm", "in"))
          # ......buttons
          OK.BTN <- tcltk2::tk2button(WIN44D, text = "Ok", tip = "", command = function() {
            PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))
            if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
              PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}
            if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
              PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}
            PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
            # ......then destroy window
            tcltk::tkdestroy(WIN44D)
            # ......now open tkgetsavefile window
            ggplot2::ggsave(Plot,
                            file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN00,
                                                                        title = "Save plot as...",
                                                                        initialfile = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST1)], "vs", colnames(Mypca.df)[tcltk2::selection(MYPCLIST2)], sep = " "),
                                                                        filetypes = paste (
                                                                          "{{png files} {.png}}",
                                                                          "{{eps files} {.eps}}",
                                                                          "{{jpeg files} {.jpg .jpeg} }",
                                                                          "{{pdf files} {.pdf}}",
                                                                          "{{svg files} {.svg}}",
                                                                          "{{tiff files} {.tiff}}",
                                                                          "{{wmf files} {.wmf}}",
                                                                          "{{All files} {*}}", sep =" "),
                                                                        defaultextension = ".png")),
                            dpi = PROJECT$OPTIONS$PLOT.DPI,
                            height = PROJECT$OPTIONS$PLOT.HEIGHT,
                            width = PROJECT$OPTIONS$PLOT.WIDTH,
                            units = PROJECT$OPTIONS$PLOT.UNITS,
                            limitsize = TRUE)
          })
          CANCEL.BTN <- tcltk2::tk2button(WIN44D, text = "Cancel", tip = "", command = function() tcltk::tkdestroy(WIN44D))
          # ......grid all
          tcltk::tkgrid(tcltk::tklabel(WIN44D, text = "Height:"), HEIGHT.NTRY, tcltk::tklabel(WIN44D, text = "Width:"), WIDTH.NTRY)
          tcltk::tkgrid(tcltk::tklabel(WIN44D, text = "Units:"), UNITS.CBBX, columnspan = 2)
          tcltk::tkgrid(tcltk::tklabel(WIN44D, text = "Resolution (dpi):"), DPI.SPNBX, columnspan = 2)
          tcltk::tkgrid(OK.BTN, CANCEL.BTN, columnspan = 2)
          tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
          tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
        })
        EXPORT.BTN <- tcltk2::tk2button(WIN44C$BUTTONS, text = "Export", tip = "Export data.frame object to R", command = function() {
          # ......create window for name entry
          WIN44E <- tcltk::tktoplevel()
          # ......entry
          NAME.ENTRY <- tcltk2::tk2entry(WIN44E, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
          # ......buttons
          CONFIRM.BTN <- tcltk2::tk2button(WIN44E, text = "Confirm", tip = "Confirm name and export to R", command = function() {
            # this will give an additional name to the exported object:
            My_graph_from_trident <<- Plot
            makeActiveBinding(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY)), function() My_graph_from_trident, .GlobalEnv)
            tcltk::tkdestroy(WIN44E)
          })
          CANCEL.BTN <- tcltk2::tk2button(WIN44E, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(WIN44E))
          # ......grid all
          tcltk::tkgrid(tcltk::tklabel(WIN44E, text = "Name:"), NAME.ENTRY)
          tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
        })
        # Grid all
        tcltk::tkpack(WIN44C$PLOT, side = "top", fill = "both" , expand = TRUE)
        tcltk::tkpack(WIN44C$BUTTONS, side = "top", fill = "both" , expand = TRUE)
        tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
        tcltk::tkgrid(TKPLOT)
      })
      SAVE.BTN <- tcltk2::tk2button(WIN44B, text = "Done!", tip = "Save PCA data", command = function() {

      })
      EXPORT.BTN <- tcltk2::tk2button(WIN44B, text = "Done!", tip = "Export PCA files in R", command = function() {

      })
      DONE.BTN <- tcltk2::tk2button(WIN44B, text = "Done!", tip = "Clicking this will close this window", command = function() tcltk::tkdestroy(WIN44B))
      # Grid all
      tcltk::tkgrid(TKPLOT, columnspan = 2)
      tcltk::tkgrid(tcltk::tklabel(WIN44B, text = "Select PCs:"), columnspan = 2)
      tcltk::tkgrid(MYPCLIST1, MYPCLIST2)
      tcltk::tkgrid(PLOT.BTN, DONE.BTN)
      tcltk::tkgrid(SAVE.BTN, EXPORT.BTN)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN44A, text = "Cancel", command = function() tcltk::tkdestroy(WIN44A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN44A, text = "Choose variables"), tcltk::tklabel(WIN44A, text = "Choose factor"))
    tcltk::tkgrid(VARLIST, FACTORLIST)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }
  # --Window----
  WIN00 <<- tcltk::tktoplevel()
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkconfigure(WIN00, borderwidth = 10, bg = "tan")
  tcltk::tkwm.title(WIN00, paste("trident", METADATA$VERSION, "-", METADATA$DESCRIPTION))
  tcltk2::tk2ico.setFromFile(WIN00, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
  # TKGUI - MENU
  # --Menu----
  MENU <- tcltk2::tk2menu(WIN00)
  tcltk::tkconfigure(WIN00, menu = MENU)
  # --Menu 'File'----
  MENU$FILE <- tcltk::tkmenu(MENU, tearoff = FALSE)
  #
  # ...New project
  tcltk::tkadd(MENU$FILE, "command", label = "New project...      (Ctrl+N)", command = function() new.project.cmd())
  #
  # ...Open project
  tcltk::tkadd(MENU$FILE, "command", label = "Open project...     (Ctrl+O)", command = function() open.project.cmd())
  #
  # ...Save project
  MENU$FILE$SAVE <- tcltk::tkmenu(MENU$FILE, tearoff = FALSE)
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save...        (Ctrl+S)", command = function() save.project.cmd())
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save as...", command = function() save.project.cmd())
  tcltk::tkadd(MENU$FILE, "cascade", label = "Save project", menu = MENU$FILE$SAVE)
  #
  # ...Exit
  tcltk::tkadd(MENU$FILE, "separator")
  tcltk::tkadd(MENU$FILE, "command", label = "Quit...      (Ctrl+Q)", command = function() quit.cmd())
  #
  # ...Add File to menu
  tcltk::tkadd(MENU, "cascade", label = "File", menu = MENU$FILE)
  #
  # ...Shortcuts
  tcltk::tkbind(WIN00,"<Control-n>", function() new.project.cmd())
  tcltk::tkbind(WIN00,"<Control-o>", function() open.project.cmd())
  tcltk::tkbind(WIN00,"<Control-s>", function() save.project.cmd())
  tcltk::tkbind(WIN00,"<Control-q>", function() quit.cmd())

  # --Menu 'Edit'----
  MENU$EDIT <- tcltk::tkmenu(MENU, tearoff = FALSE)
  #
  # ...Undo
  tcltk::tkadd(MENU$EDIT, "command", label = "Undo...     (Ctrl+Z)", command = function(){})
  #
  # ...Redo
  tcltk::tkadd(MENU$EDIT, "command", label = "Redo...     (Ctrl+Y)", command = function(){})
  #
  # ...Add Edit to menu
  tcltk::tkadd(MENU, "cascade", label = "Edit", menu = MENU$EDIT)
  #
  # ...Shortcuts
  tcltk::tkbind(WIN00,"<Control-z>", function() tcltk::tkmessageBox(message = 'Undo'))
  tcltk::tkbind(WIN00,"<Control-y>", function() tcltk::tkmessageBox(message = 'Redo'))

  # --Menu 'About'----
  MENU$HELP <- tcltk::tkmenu(MENU, tearoff = FALSE)
  # ...About
  tcltk::tkadd(MENU$HELP, "command", label = "About", command = function(){
    tcltk::tkmessageBox(type = "ok",
    message = "package 'trident': A library for dental microwear
    texture analysis, allowing to further analyze the
    results using a variety of methods.
    The package includes functions to measure microwear
    texture, to transform the data, to rank variables,
    as well as a complete tcl/tk graphical user interface.\n
    version: 0.3.0
    release date: 17/07/20
    Authors: Ghislain Thiery, Arthur Francisco, Gildas
    Merceron
    Licence: GPL-3")
  })
  # ...Options
  tcltk::tkadd(MENU$HELP, "separator")
  tcltk::tkadd(MENU$HELP, "command", label = "Options", command = function(){})
  # ...Help files
  tcltk::tkadd(MENU$FILE, "separator")
  tcltk::tkadd(MENU$HELP, "command", label = "Francisco et al. (2018)", command = function(){
    Mypath <- 'inst/extdata/about/Francisco2018a.pdf'
    system(paste0('open "', Mypath, '"'))
    })
  tcltk::tkadd(MENU$HELP, "command", label = "Santos (2020)", command = function(){
    Mypath <- 'inst/extdata/about/Santos2019.pdf'
    system(paste0('open "', Mypath, '"'))
  })
  tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  #
  # ...Add About to menu
  tcltk::tkadd(MENU, "cascade", label = "Help", menu = MENU$HELP)
  # TKGUI - NOTEBOOK
  # --Notebook----
  NOTEBOOK <- tcltk2::tk2notebook(WIN00, height = 100, tabs = c("Data", "Statistics", "Variables", "Plots", "Microwear", "Batch analysis"))
  tcltk::tkpack(NOTEBOOK, side = "top", fill = "both" , expand = FALSE)
  # --Notetab 'Data'----
  NOTEBOOK$DATA <- tcltk2::tk2notetab(NOTEBOOK, "Data")
  # ...Create buttons
  OPEN.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Open", compound = "top", command = function() {
                                # ...Open the file and read it
                                File.tmp <- tcltk::tk_choose.files(filters = matrix(c("Calc", "R file", "Text", "All files", ".csv", ".txt", ".R", "*"), ncol = 2))
                                n <- length(PROJECT$FILES) +1
                                # REMARQUE: Utiliser les fonctions du package 'readr' à la place de 'read.table' ?
                                if (tools::file_ext(File.tmp) == "txt") PROJECT$FILES[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL))
                                if (tools::file_ext(File.tmp) == "csv") PROJECT$FILES[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL))
                                names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1])
                                if (anyDuplicated(names(PROJECT$FILES)) != 0) names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1], "(", n,")")
                                #
                                # ...Scenario 1: there already is an opened file
                                if (is.null(PROJECT$DATASET) == FALSE) {
                                  # ...a window to choose whether the current dataset must be replaced
                                  WIN10 <- tcltk::tktoplevel()
                                  # ...create buttons
                                  #
                                  YES.BTN <- tcltk2::tk2button(WIN10, text = "Yes", tip = "", command = function() {
                                    # ......replace current dataset with data from new file
                                    PROJECT$DATASET <<- PROJECT$FILES[[n]]
                                    refresh.cmd()
                                    # ......then destroy window
                                    tcltk::tkdestroy(WIN10)
                                  })
                                  #
                                  NO.BTN <- tcltk2::tk2button(WIN10, text = "No", tip = "", command = function() {
                                    # ......you still want to update WIN00$TABLE3 with new file name
                                    if (is.null(WIN00$TABLE3) == FALSE) tcltk::tkdestroy(WIN00$TABLE3)
                                    WIN00$TABLE3 <<- tcltk2::tk2frame(WIN00)
                                    FILELIST <- tcltk2::tk2listbox(WIN00$TABLE3, values = names(PROJECT$FILES), selectmode = "single", height = 12, width = 0,
                                                                   tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                    tcltk::tkgrid(tcltk::tklabel(WIN00$TABLE3, text = "Opened files"))
                                    tcltk::tkgrid(FILELIST)
                                    tcltk::tkpack(WIN00$TABLE3, side = "top", expand = FALSE, fill = "x", anchor = "nw")
                                    # ......then destroy window
                                    tcltk::tkdestroy(WIN10)
                                    })
                                  # ...grid all
                                  tcltk::tkgrid(tcltk::tklabel(WIN10, text = "Do you want to replace current dataset?"))
                                  tcltk::tkgrid(YES.BTN, NO.BTN, padx = 5, pady = 5, sticky = "ns")
                                }
                                # ...Scenario 2: there is no currently open file
                                if (is.null(PROJECT$DATASET) == TRUE) {
                                  PROJECT$DATASET <<- PROJECT$FILES[[n]]
                                  refresh.cmd()
                                }
                                #
                              })

  BUILD.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","build.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Build", compound = "top", command = function() {
                                 # ...a window to select variables used to build the new dataset
                                 WIN11 <- tcltk::tktoplevel()
                                 # ...create a list of all variables
                                 CombData <- data.frame(PROJECT$FILES)
                                 VARLIST <- tcltk2::tk2listbox(WIN11, values = colnames(CombData), selectmode = "extended", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                 # ...create buttons
                                 BUILD.BTN <- tcltk2::tk2button(WIN11, text = "Combine!", command = function() {
                                   # ......select variables, then update the table
                                   Myselect <- tcltk2::selection(VARLIST)
                                   PROJECT$DATASET <<- CombData[, Myselect]
                                   refresh.cmd()
                                   # ......destroy window
                                   tcltk::tkdestroy(WIN11)
                                 })
                                 CANCEL.BTN <- tcltk2::tk2button(WIN11, text = "Cancel", command = function() tcltk::tkdestroy(WIN11))
                                 # ...grid all
                                 tcltk::tkgrid(tcltk::tklabel(WIN11, text = "Please select variables\nto build the dataset from"), columnspan = 2)
                                 tcltk::tkgrid(VARLIST, columnspan = 2)
                                 tcltk::tkgrid(BUILD.BTN, CANCEL.BTN)
                               })

  COMBINE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","combine.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Combine", compound = "top", command = function() {
                                 # ...STEP1: a window to select datasets to combine
                                 WIN12A <- tcltk::tktoplevel()
                                 # ......list of files
                                 FILELIST <- tcltk2::tk2listbox(WIN12A, values = names(PROJECT$FILES), selectmode = "multiple", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                 # ......buttons
                                 SLCT.FILE.BTN <- tcltk2::tk2button(WIN12A, text = "Select...", tip = "Confirm that the selected data will be combined", command = function() {
                                   # ...select the files
                                   Myfiles <- tcltk2::selection(FILELIST)
                                   if (length(Myfiles) > 4) {
                                     tcltk::tkmessageBox(message = "Currently, no more than 4 datasets can be combined.\nIf you would like the package to support more\ncombinations in the future, please contact\nthe developers.")
                                   }
                                   # ...STEP2: a window to select a common variable
                                   if (length(Myfiles) <= 4) {
                                     WIN12B <- tcltk::tktoplevel()
                                     # ......list of common variables names
                                     Myvar <- colnames(PROJECT$FILES[[Myfiles[1]]])
                                     for (i in Myfiles[2:length(Myfiles)]) {
                                       Compared <- colnames(PROJECT$FILES[[i]])
                                       Myvar <- Myvar[Myvar %in% Compared]
                                     }
                                     VARLIST <- tcltk2::tk2listbox(WIN12B, values = Myvar, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                     # ......buttons
                                     SLCT.VAR.BTN <- tcltk2::tk2button(WIN12B, text = "Select...", tip = "Confirm that the selected variable will be used to combine the data", command = function() {
                                       if(is.null(tcltk2::selection(VARLIST)) == TRUE) tcltk::tkmessageBox(message = "Please select a single common variable")
                                       else MyCommonVar <- paste(Myvar[tcltk2::selection(VARLIST)])
                                       #
                                       # ...STEP3: combine datasets
                                       if (length(Myfiles) == 2) {
                                         PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$FILES[[Myfiles[1]]], y = PROJECT$FILES[[Myfiles[2]]], by = MyCommonVar,
                                                                              copy = FALSE, suffix = c(paste0(".", names(PROJECT$FILES)[1]), paste0(".", names(PROJECT$FILES)[2])))
                                       }
                                       if (length(Myfiles) == 3) {
                                         PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$DATASET, y = PROJECT$FILES[[Myfiles[3]]], by = MyCommonVar,
                                                                              copy = FALSE, suffix = c("", paste0(".", names(PROJECT$FILES)[3])))
                                       }
                                       if (length(Myfiles) == 4) {
                                         PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$DATASET, y = PROJECT$FILES[[Myfiles[3]]], by = MyCommonVar,
                                                                              copy = FALSE, suffix = c("", paste0(".", names(PROJECT$FILES)[3])))
                                       }
                                       # .......refresh tables
                                       refresh.cmd()
                                       # ......then destroy windows
                                       tcltk::tkdestroy(WIN12A)
                                       tcltk::tkdestroy(WIN12B)
                                     })
                                     CANCEL.VAR.BTN <- tcltk2::tk2button(WIN12B, text = "Cancel", tip = "Cancel variable selection", command = function() {tcltk::tkdestroy(WIN12B)})
                                     # ......grid all
                                     tcltk::tkgrid(tcltk::tklabel(WIN12B, text = "Select variables to use when combining the data"), columnspan = 2)
                                     tcltk::tkgrid(VARLIST, columnspan = 2)
                                     tcltk::tkgrid(SLCT.VAR.BTN, CANCEL.VAR.BTN)
                                   }
                                 })
                                 CANCEL.FILE.BTN <- tcltk2::tk2button(WIN12A, text = "Cancel", tip = "Cancel file selection", command = function() {tcltk::tkdestroy(WIN12A)})
                                 # ......grid all
                                 tcltk::tkgrid(tcltk::tklabel(WIN12A, text = "Select files to combine:"))
                                 tcltk::tkgrid(FILELIST, columnspan = 2)
                                 tcltk::tkgrid(SLCT.FILE.BTN, CANCEL.FILE.BTN)
                                })
  REFRESH.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","refresh.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Refresh (F5)", compound = "top", command = function() refresh.cmd())
  # ...Create menubuttons
  # .......Transformations:
  TRANS.MBTN <- tcltk::tkmenubutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","transform.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Transform", compound = "top")
  TRANS.MENU <- tcltk::tkmenu(TRANS.MBTN)
  tcltk::tkconfigure(TRANS.MBTN, menu = TRANS.MENU)
  tcltk::tkadd(TRANS.MENU, "command", label = "Boxcox transformation", command = function(){
    # ...prepare data
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
      tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
      stop('no factor variable')
    }
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Myfactor <- NULL
    # ...a window to select the factor
    WIN13 <- tcltk::tktoplevel()
    YLIST <- tcltk2::tk2listbox(WIN13, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...buttons
    OK.BTN <- tcltk2::tk2button(WIN13, text = "OK", command = function(){
      #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
      Myfactor <- tcltk2::selection(YLIST)
      Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])
      colnames(Numerics$boxcox) <- paste(colnames(Numerics$boxcox), "boxcox", sep = ".")
      PROJECT$DATASET <<- data.frame(Factors, Numerics$boxcox)
      tcltk::tkdestroy(WIN13)
      refresh.cmd()})
    CANCEL.BTN <- tcltk2::tk2button(WIN13, text = "Cancel", command = function() tcltk::tkdestroy(WIN13))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN13, text = "Choose factor"), padx = 5, pady = 5)
    tcltk::tkgrid(YLIST, columnspan = 2, padx = 5, pady = 5)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN, padx = 5, pady = 5)
  })
  tcltk::tkadd(TRANS.MENU, "command", label = "Remove outliers", command = function(){})
  #
  # ...Grid all the 'data' buttons
  tcltk::tkgrid(OPEN.BTN, COMBINE.BTN, BUILD.BTN, TRANS.MBTN, REFRESH.BTN, padx = 5, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(OPEN.BTN, "Open data")
  tcltk2::tk2tip(BUILD.BTN, "Build new dataset")
  tcltk2::tk2tip(COMBINE.BTN, "Combine two or more datasets")
  tcltk2::tk2tip(REFRESH.BTN, "Refresh tables")
  tcltk2::tk2tip(TRANS.MBTN, "Apply transformation algorithms to current dataset")
  # ...Shortcuts
  tcltk::tkbind(WIN00,"<F5>", function() refresh.cmd())

  # --Notetab 'Statistics'----
  NOTEBOOK$STATS <- tcltk2::tk2notetab(NOTEBOOK, "Statistics")
  # ...Create buttons
  SUM.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","summary.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Summary", compound = "top", command = function(){
                                # ...prepare data
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
                                  tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
                                  stop('no factor variable')
                                }
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                # ...a window to select the factor
                                WIN21A <- tcltk::tktoplevel()
                                WIN21A$FACTORLIST <- tcltk2::tk2listbox(WIN21A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...buttons
                                OK.BTN <- tcltk2::tk2button(WIN21A, text = "OK", command = function(){
                                  Myfactor <- Factors[, tcltk2::selection(WIN21A$FACTORLIST)]
                                  tcltk::tkdestroy(WIN21A)
                                  Mytable <- data.frame(Variable = colnames(Numerics),
                                    Min = t(plyr::colwise(min)(Numerics)),
                                    Quart1 = t(plyr::colwise(stats::quantile)(Numerics, probs = 0.25)),
                                    Median = t(plyr::colwise(stats::median)(Numerics)),
                                    Quart3 = t(plyr::colwise(stats::quantile)(Numerics, probs = 0.75)),
                                    Max = t(plyr::colwise(max)(Numerics)),
                                    Mean = t(plyr::colwise(mean)(Numerics)),
                                    SEM = t(plyr::colwise(function(x) stats::var(x)/sqrt(length(x)))(Numerics)))
                                  for (i in c(1:length(levels(Myfactor)))) {
                                    Group <- levels(Myfactor)[i]
                                    Subset <- Numerics[which(Myfactor == Group), ]
                                    Pergrouptable <- data.frame(Min = t(plyr::colwise(min)(Subset)),
                                                                Quart1 = t(plyr::colwise(stats::quantile)(Subset, probs = 0.25)),
                                                                Median = t(plyr::colwise(stats::median)(Subset)),
                                                                Quart3 = t(plyr::colwise(stats::quantile)(Subset, probs = 0.75)),
                                                                Max = t(plyr::colwise(max)(Subset)),
                                                                Mean = t(plyr::colwise(mean)(Subset)),
                                                                SEM = t(plyr::colwise(function(x) stats::var(x)/sqrt(length(x)))(Subset)))

                                    colnames(Pergrouptable) <- paste(colnames(Pergrouptable), Group, sep = ".")
                                    Mytable <- dplyr::bind_cols(Mytable, Pergrouptable)
                                  }
                                  # ......Build window with table
                                  WIN21B <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN21B, paste("trident", METADATA$VERSION, "- summary"))
                                  build.table.cmd(Mytable, WIN21B)
                                  tcltk::tcl("wm", "attributes", WIN21B, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN21B, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN21A, text = "Cancel", command = function() tcltk::tkdestroy(WIN21A))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN21A, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN21A$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN21A, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN21A, topmost = FALSE)
                              })
  DISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Discriminant", compound = "top", command = function(){
                                # ...prepare data
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
                                  tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
                                  stop('no factor variable')
                                }
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                # ...a window to select the factor
                                WIN22A <- tcltk::tktoplevel()
                                WIN22A$FACTORLIST <- tcltk2::tk2listbox(WIN22A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...buttons
                                OK.BTN <- tcltk2::tk2button(WIN22A, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN22A$FACTORLIST)
                                  tcltk::tkdestroy(WIN22A)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == TRUE)])
                                  # ......Build window with table
                                  WIN22B <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN22B, paste("trident", METADATA$VERSION, "- List of discriminant variables"))
                                  build.table.cmd(Mytable, WIN22B)
                                  tcltk::tcl("wm", "attributes", WIN22B, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN22B, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN22A, text = "Cancel", command = function() tcltk::tkdestroy(WIN22A))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN22A, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN22A$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN22A, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN22A, topmost = FALSE)
                              })
  NONDISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Non-discriminant", compound = "top", command = function(){
                                # ...prepare data
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
                                  tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
                                  stop('no factor variable')
                                }
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                # ...a window to select the factor
                                WIN23A <- tcltk::tktoplevel()
                                WIN23A$FACTORLIST <- tcltk2::tk2listbox(WIN23A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...buttons
                                OK.BTN <- tcltk2::tk2button(WIN23A, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN23A$FACTORLIST)
                                  tcltk::tkdestroy(WIN23A)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == FALSE)])
                                  # ......Build window with table
                                  WIN23B <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN23B, paste("trident", METADATA$VERSION, "- List of non-discriminant variables"))
                                  build.table.cmd(Mytable, WIN23B)
                                  tcltk::tcl("wm", "attributes", WIN23B, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN23B, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN23A, text = "Cancel", command = function() tcltk::tkdestroy(WIN23A))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN23A, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN23A$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN23A, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN23A, topmost = FALSE)
                              })
  MULTI.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","multicheck.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Multicheck", compound = "top", command = function(){
                                # ...prepare data
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
                                  tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
                                  stop('no factor variable')
                                }
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                # ...a window to select the factor
                                WIN24A <- tcltk::tktoplevel()
                                WIN24A$FACTORLIST <- tcltk2::tk2listbox(WIN24A, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...buttons
                                OK.BTN <- tcltk2::tk2button(WIN24A, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN24A$FACTORLIST)
                                  tcltk::tkdestroy(WIN24A)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(Mycheck)
                                  # ......Build window with table
                                  WIN24B <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN24B, paste("trident", METADATA$VERSION, "- Multicheck"))
                                  build.table.cmd(Mytable, WIN24B)
                                  tcltk::tcl("wm", "attributes", WIN24B, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN24B, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN24A, text = "Cancel", command = function() tcltk::tkdestroy(WIN24A))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(WIN24A, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN24A$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN24A, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN24A, topmost = FALSE)
                              })
  # ...Grid all
  tcltk::tkgrid(SUM.BTN, MULTI.BTN, DISC.BTN, NONDISC.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(SUM.BTN, "Descriptive statistics of dataset")
  tcltk2::tk2tip(MULTI.BTN, "Checks the basic assumptions for One-Way ANOVA")
  tcltk2::tk2tip(DISC.BTN, "For one factor, list of the discriminant variables")
  tcltk2::tk2tip(NONDISC.BTN, "For one factor, list of the non-discriminant variables")

  # --Notetab 'Variables'----
  NOTEBOOK$VARIA <- tcltk2::tk2notetab(NOTEBOOK, "Variables")
  # ...Create menubuttons
  # .......Rank by:
  RANK.MBTN <- tcltk::tkmenubutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","arrange.gif", package = "trident")), height = 50, relief = "flat", text = "Rank based on...", compound = "top")
  RANK.MENU <- tcltk::tkmenu(RANK.MBTN)
  tcltk::tkconfigure(RANK.MBTN, menu = RANK.MENU)
  # ANOVA
  tcltk::tkadd(RANK.MENU, "command", label = "ANOVA", command = function() {
    # ...a window to select the factor
    WIN31A <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...comboboxes
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN31A, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(WIN31A, values = c("F-stat", "P-value"))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN31A, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN31A, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(WIN31A, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN31A, text = "OK", command = function() {
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      if (Myby == "F-stat") Myby <- "f.stat"
      if (Myby == "P-value") Myby <- "aov.p.value"
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {
        #to do
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DISC.ONLY.VALUE) == 1) {
        Numerics <- Numerics[, which(trident::multicheck(df = Numerics, y = Factors[, Myfactor])$is.discriminant == TRUE)]
      }
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(WIN31A)
      # ......display in new window
      WIN31B <- tcltk::tktoplevel()
      WIN31B$BUTTONS <- tcltk::tkframe(WIN31B)
      BIPLOT.BTN <- tcltk2::tk2button(WIN31B$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(WIN31B$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(WIN31B$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(WIN31B$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.pca.cmd(df = Mydf)
      })
      # ...window's theme
      if (Myby == "F-stat") tcltk::tkwm.title(WIN31B, paste("trident", METADATA$VERSION, "- arranged by F-stat"))
      if (Myby == "P-value") tcltk::tkwm.title(WIN31B, paste("trident", METADATA$VERSION, "- arranged by ANOVA's P-value"))
      tcltk2::tk2ico.setFromFile(WIN31B, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
      # ...grid all
      build.table.cmd(Mytable, WIN31B)
      tcltk::tkpack(WIN31B$BUTTONS, side = "bottom")
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", WIN31B, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN31B, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN31A, text = "Cancel", command = function() tcltk::tkdestroy(WIN31A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN31A, text = "Rank using ANOVA's residuals"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN31A, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN31A, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.KTEST.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(WIN31A, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN31A, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DIXON.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN31A, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN31A, topmost = FALSE)
  })
  # K-TEST
  tcltk::tkadd(RANK.MENU, "command", label = "K-test", command = function() {
    # ...a window to select the factor
    WIN32A <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...comboboxes
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN32A, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(WIN32A, values = c("Kruskal's K", "P-value"))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN32A, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN32A, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(WIN32A, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN32A, text = "OK", command = function() {
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      if (Myby == "Kruskal's K") Myby <- "k"
      if (Myby == "P-value") Myby <- "k.p.value"
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {
        #to do
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DISC.ONLY.VALUE) == 1) {
        Numerics <- Numerics[, which(trident::multicheck(df = Numerics, y = Factors[, Myfactor])$is.discriminant == TRUE)]
      }
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(WIN32A)
      # ......display in new window
      WIN32B <- tcltk::tktoplevel()
      WIN32B$BUTTONS <- tcltk::tkframe(WIN32B)
      BIPLOT.BTN <- tcltk2::tk2button(WIN32B$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(WIN32B$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(WIN32B$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(WIN32B$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.pca.cmd(df = Mydf)
      })
      # ...window's theme
      if (Myby == "Kruskal's K") tcltk::tkwm.title(WIN32B, paste("trident", METADATA$VERSION, "- arranged by Kruskal's K"))
      if (Myby == "P-value") tcltk::tkwm.title(WIN32B, paste("trident", METADATA$VERSION, "- arranged by Kruskal's P-value"))
      tcltk2::tk2ico.setFromFile(WIN32B, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
      # ...grid all
      build.table.cmd(Mytable, WIN32B)
      tcltk::tkpack(WIN32B$BUTTONS, side = "bottom")
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", WIN32B, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN32B, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN32A, text = "Cancel", command = function() tcltk::tkdestroy(WIN32A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN32A, text = "Rank using Kruskal's results"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN32A, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN32A, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.KTEST.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(WIN32A, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN32A, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DIXON.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN32A, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN32A, topmost = FALSE)
  })
  # POSTHOC (MEAN)
  tcltk::tkadd(RANK.MENU, "command", label = "Post-hoc (mean)", command = function() {
    # ...a window to select the factor
    WIN33A <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...comboboxes
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN33A, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(WIN33A, values = c("Tukey's HSD", "Fisher's LSD"))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN33A, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN33A, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(WIN33A, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    GEOMEAN.CHKBTN <- tcltk2::tk2checkbutton(WIN33A, text = "Compute geometric mean?")
    tcltk::tkconfigure(GEOMEAN.CHKBTN, variable = PROJECT$OPTIONS$GEOMEAN.VALUE)
    BYNGR.CHKBTN <- tcltk2::tk2checkbutton(WIN33A, text = "Rank by number of discriminated groups?")
    tcltk::tkconfigure(BYNGR.CHKBTN, variable = PROJECT$OPTIONS$BYNGR.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN33A, text = "OK", command = function() {
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      if (Myby == "Tukey's HSD") Myby <- "hsd.mean.p.value"
      if (Myby == "Fisher's LSD") Myby <- "lsd.mean.p.value"
      if (tcltk::tclvalue(PROJECT$OPTIONS$GEOMEAN.VALUE) == 1) Mygeomean <- TRUE
      if (tcltk::tclvalue(PROJECT$OPTIONS$GEOMEAN.VALUE) == 0) Mygeomean <- FALSE
      if (tcltk::tclvalue(PROJECT$OPTIONS$BYNGR.VALUE) == 1) Mybyngr <- TRUE
      if (tcltk::tclvalue(PROJECT$OPTIONS$BYNGR.VALUE) == 0) Mybyngr <- FALSE

      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {
        #to do
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DISC.ONLY.VALUE) == 1) {
        Numerics <- Numerics[, which(trident::multicheck(df = Numerics, y = Factors[, Myfactor])$is.discriminant == TRUE)]
      }

      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby, geomean = Mygeomean, byngr = Mybyngr)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(WIN33A)
      # ......display in new window
      WIN33B <- tcltk::tktoplevel()
      WIN33B$BUTTONS <- tcltk::tkframe(WIN33B)
      BIPLOT.BTN <- tcltk2::tk2button(WIN33B$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(WIN33B$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(WIN33B$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(WIN33B$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors[, Myfactor], Numerics)
        colnames(Mydf)[1] <- Myfactor
        plot.pca.cmd(df = Mydf)
      })
      # ...window's theme
      if (Myby == "Tukey's HSD") tcltk::tkwm.title(WIN33B, paste("trident", METADATA$VERSION, "- arranged by mean Tukey's HSD"))
      if (Myby == "Fisher's LSD") tcltk::tkwm.title(WIN33B, paste("trident", METADATA$VERSION, "- arranged by mean Fisher's LSD"))
      tcltk2::tk2ico.setFromFile(WIN33B, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
      # ...grid all
      build.table.cmd(Mytable, WIN33B)
      tcltk::tkpack(WIN33B$BUTTONS, side = "bottom")
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", WIN33B, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN33B, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN33A, text = "Cancel", command = function() tcltk::tkdestroy(WIN33A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN33A, text = "Rank using post-hoc mean p-values"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN33A, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN33A, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.MEANPOSTHOC.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(WIN33A, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN33A, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DIXON.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(GEOMEAN.CHKBTN, columnspan = 2)
    tcltk::tkgrid(BYNGR.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN33A, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN33A, topmost = FALSE)
  })
  # POSTHOC(BY GROUPS)
  tcltk::tkadd(RANK.MENU, "command", label = "Post-hoc (by group)", command = function() {
    # ...a window to select the factor
    WIN34A <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...comboboxes
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN34A, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(WIN34A, values = c("Tukey's HSD", "Fisher's LSD"))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN34A, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN34A, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(WIN34A, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN34A, text = "OK", command = function() {
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      if (Myby == "Tukey's HSD") Myby <- "hsd.p.value"
      if (Myby == "Fisher's LSD") Myby <- "lsd.p.value"
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {
        #to do
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DISC.ONLY.VALUE) == 1) {
        Numerics <- Numerics[, which(trident::multicheck(df = Numerics, y = Factors[, Myfactor])$is.discriminant == TRUE)]
      }
      # ...generate list of all possible rank priorities
      names <- gtools::permutations(n = length(levels(Factors[, Myfactor])), r = length(levels(Factors[, Myfactor])), v = levels(Factors[, Myfactor]))
      numbers <- gtools::permutations(n = length(levels(Factors[, Myfactor])), r = length(levels(Factors[, Myfactor])), v = c(1:length(levels(Factors[, Myfactor]))))
      Mynames <- apply(names, 1, function(x) paste0(x, collapse = ", "))
      # ...create windox to select new group priority
      WIN34C <- tcltk::tktoplevel()
      PRIORITY.CBBX <- tcltk2::tk2combobox(WIN34C, values = Mynames)
      OK.BTN <- tcltk2::tk2button(WIN34C, text = "Rank!", command = function() {
        # ......change priority
        Mypriority <- numbers[which(Mynames == tcltk::tclvalue(tcltk::tkget(PRIORITY.CBBX))), ]
        # ......arrange
        Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby, gp.priority = Mypriority)
        Numerics <- Numerics[, rownames(Mytable)]
        tcltk::tkdestroy(WIN34A)
        # ......display in new window
        WIN34B <- tcltk::tktoplevel()
        WIN34B$BUTTONS <- tcltk::tkframe(WIN34B)
        BIPLOT.BTN <- tcltk2::tk2button(WIN34B$BUTTONS, text = "Biplot", tip = "", command = function() {
          Mydf <- data.frame(Factors[, Myfactor], Numerics)
          colnames(Mydf)[1] <- Myfactor
          biplot.cmd(df = Mydf)
        })
        BOXPLOT.BTN <- tcltk2::tk2button(WIN34B$BUTTONS, text = "Boxplot", tip = "", command = function() {
          Mydf <- data.frame(Factors[, Myfactor], Numerics)
          colnames(Mydf)[1] <- Myfactor
          boxplot.cmd(df = Mydf)
        })
        VIOLIN.BTN <- tcltk2::tk2button(WIN34B$BUTTONS, text = "Violin", tip = "", command = function() {
          Mydf <- data.frame(Factors[, Myfactor], Numerics)
          colnames(Mydf)[1] <- Myfactor
          plot.violin.cmd(df = Mydf)
        })
        PCA.BTN <- tcltk2::tk2button(WIN34B$BUTTONS, text = "PCA", tip = "", command = function() {
          Mydf <- data.frame(Factors[, Myfactor], Numerics)
          colnames(Mydf)[1] <- Myfactor
          plot.pca.cmd(df = Mydf)
        })
        # ...window's theme
        if (Myby == "Tukey's HSD") tcltk::tkwm.title(WIN34B, paste("trident", METADATA$VERSION, "- arranged by group Tukey's HSD"))
        if (Myby == "Fisher's LSD") tcltk::tkwm.title(WIN34B, paste("trident", METADATA$VERSION, "- arranged by group Fisher's LSD"))
        tcltk2::tk2ico.setFromFile(WIN34B, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
        # ...grid all
        build.table.cmd(Mytable, WIN34B)
        tcltk::tkpack(WIN34B$BUTTONS, side = "bottom")
        tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
        tcltk::tcl("wm", "attributes", WIN34B, topmost = TRUE)
        tcltk::tcl("wm", "attributes", WIN34B, topmost = FALSE)
        # ...then destroy WIN34C
        tcltk::tkdestroy(WIN34C)
      })
    CANCEL.BTN <- tcltk2::tk2button(WIN34C, text = "Cancel", command = function() tcltk::tkdestroy(WIN34C))

    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN34C, text = "Please rank group priority"), columnspan = 2)
    tcltk::tkgrid(PRIORITY.CBBX)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN34A, text = "Cancel", command = function() tcltk::tkdestroy(WIN34A))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN34A, text = "Rank using post-hoc by-group p-values"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN34A, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN34A, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.MEANPOSTHOC.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(WIN34A, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(WIN34A, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DIXON.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN34A, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN34A, topmost = FALSE)
  })
  # ...Create buttons
  TAG.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","tag.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Tag", compound = "top", command = function() {})
  TOP3.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Top 3", compound = "top", command = function() {
                               # ...a window to select the factor
                               WIN36A <- tcltk::tktoplevel()
                               Mydf <- PROJECT$DATASET
                               Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                               Mydf <- stats::na.omit(Mydf)
                               Numerics <- dplyr::select_if(Mydf, is.numeric)
                               Factors  <- dplyr::select_if(Mydf, is.factor)
                               # ...combobox for factor choice
                               FACTOR.CBBX <- tcltk2::tk2combobox(WIN36A, values = colnames(Factors))
                               # ...OK button
                               OK.BTN <- tcltk2::tk2button(WIN36A, text = "OK", command = function() {
                                 Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CBBX))
                                 Mytop <- trident::trident.top3(df = Numerics, y = Factors[, Myfactor])
                                 Mytable <- Mytop$ranked
                                 # ......BoxCox transformation for the plots
                                 Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
                                 colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
                                 # ......display in new window
                                 WIN36B <- tcltk::tktoplevel()
                                 WIN36B$BUTTONS <- tcltk::tkframe(WIN36B)
                                 BIPLOT.BTN <- tcltk2::tk2button(WIN36B$BUTTONS, text = "Biplot", tip = "", command = function() {
                                   Mydf <- data.frame(Factors[, Myfactor], Numerics[, unique(Mytop$top3var)])
                                   colnames(Mydf)[1] <- Myfactor
                                   biplot.cmd(df = Mydf)
                                 })
                                 BOXPLOT.BTN <- tcltk2::tk2button(WIN36B$BUTTONS, text = "Boxplot", tip = "", command = function() {
                                   Mydf <- data.frame(Factors[, Myfactor], Numerics[, unique(Mytop$top3var)])
                                   colnames(Mydf)[1] <- Myfactor
                                   boxplot.cmd(df = Mydf)
                                 })
                                 VIOLIN.BTN <- tcltk2::tk2button(WIN36B$BUTTONS, text = "Violin", tip = "", command = function() {
                                   Mydf <- data.frame(Factors[, Myfactor], Numerics[, unique(Mytop$top3var)])
                                   colnames(Mydf)[1] <- Myfactor
                                   plot.violin.cmd(df = Mydf)
                                 })
                                 PCA.BTN <- tcltk2::tk2button(WIN36B$BUTTONS, text = "PCA", tip = "", command = function() {
                                   Mydf <- data.frame(Factors[, Myfactor], Numerics[, unique(Mytop$top3var)])
                                   colnames(Mydf)[1] <- Myfactor
                                   plot.pca.cmd(df = Mydf)
                                 })
                                 # ...grid all
                                 build.table.cmd(Mytable, WIN36B)
                                 tcltk::tkpack(WIN36B$BUTTONS, side = "bottom")
                                 tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
                                 # ...window attributes
                                 tcltk::tkwm.title(WIN36B, paste("trident", METADATA$VERSION, "- top 3 best discriminating variables for each group"))
                                 tcltk2::tk2ico.setFromFile(WIN36B, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
                                 tcltk::tcl("wm", "attributes", WIN36B, topmost = TRUE)
                                 tcltk::tcl("wm", "attributes", WIN36B, topmost = FALSE)
                                 # ...then destroy window A
                                 tcltk::tkdestroy(WIN36A)
                               })
                               CANCEL.BTN <- tcltk2::tk2button(WIN36A, text = "Cancel", command = function() tcltk::tkdestroy(WIN36A))
                               # ...grid all
                               tcltk::tkgrid(tcltk::tklabel(WIN36A, text = "Please choose a factor:  "), FACTOR.CBBX)
                               tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                               tcltk::tcl("wm", "attributes", WIN36A, topmost = TRUE)
                               tcltk::tcl("wm", "attributes", WIN36A, topmost = FALSE)
                             })
  # ...Grid all
  tcltk::tkgrid(TAG.BTN, RANK.MBTN, TOP3.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(RANK.MBTN, "Classify variables from the most to the less discriminant, using...")
  tcltk2::tk2tip(TAG.BTN, "Not available yet!")
  tcltk2::tk2tip(TOP3.BTN, "Top 3 best discriminant variables, following Francisco et al. 2018a")

  # --Notetab 'Plots'----
  NOTEBOOK$PLOTS <- tcltk2::tk2notetab(NOTEBOOK, "Plots")
  # ...Create buttons
  BIPLOT.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","biplot.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Biplot", compound = "top", command = function() {
                                biplot.cmd(df = PROJECT$DATASET)
                                })
  BOXPLOT.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","boxplot.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Boxplot", compound = "top", command = function() {
                                  boxplot.cmd(df = PROJECT$DATASET)
                                  })
  VIOLIN.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","violin.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Violin", compound = "top", command = function() {
                                  plot.violin.cmd(df = PROJECT$DATASET)
                                  })
  PCA.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","pca.gif", package = "trident")), height = 50, relief = "flat",
                                text = "PCA", compound = "top", command = function() {
                                  plot.pca.cmd(df = PROJECT$DATASET)
                                  })
  DFA.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","dfa.gif", package = "trident")), height = 50, relief = "flat",
                                text = "DFA", compound = "top", command = function() {})
  # ...Grid all
  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN, DFA.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(BIPLOT.BTN, "Bivariate graph")
  tcltk2::tk2tip(BOXPLOT.BTN, "Box-and-whiskers graph")
  tcltk2::tk2tip(VIOLIN.BTN, "Violin graph")
  tcltk2::tk2tip(PCA.BTN, "Principal Component Analysis")
  tcltk2::tk2tip(DFA.BTN, "Discriminant Function Analysis")

  # --Notetab 'Microwear'----
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
  tcltk2::tk2tip(LOAD.BTN, "")


  # --Notetab 'Batch analysis'----
  NOTEBOOK$BATCH <- tcltk2::tk2notetab(NOTEBOOK, "Batch analysis")
  # ...Create buttons
  DIR.BTN <- tcltk::tkbutton(NOTEBOOK$BATCH, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","directory.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Directory", compound = "top", command = function(){})
  LAUNCH.BTN <- tcltk::tkbutton(NOTEBOOK$BATCH, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","batch.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Launch", compound = "top", command = function(){})
  # ...Grid all
  tcltk::tkgrid(DIR.BTN, LAUNCH.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(DIR.BTN, "Select input directory")
  tcltk2::tk2tip(DIR.BTN, "Select options and launch batch analysis")

  # TKGUI - WRAPPER----
  tcltk::tcl("wm", "attributes", WIN00, topmost = TRUE)
  tcltk::tcl("wm", "attributes", WIN00, topmost = FALSE)
  tcltk::tcl("wm", "protocol", WIN00, "WM_DELETE_WINDOW", function() quit.cmd())
  # ...Disabled buttons
  tcltk::tkentryconfigure(MENU$EDIT, 0, state = "disable")
  tcltk::tkentryconfigure(MENU$EDIT, 1, state = "disable")
  tcltk::tkentryconfigure(MENU$HELP, 1, state = "disable")
  tcltk::tkentryconfigure(MENU$HELP, 4, state = "disable")
  tcltk::tkentryconfigure(MENU$HELP, 5, state = "disable")
  tcltk::tkentryconfigure(MENU$HELP, 6, state = "disable")
  tcltk::tkconfigure(TAG.BTN, state = "disable")
  tcltk::tkconfigure(DFA.BTN, state = "disable")
  tcltk::tkconfigure(LOAD.BTN, state = "disable")
  tcltk::tkconfigure(CLEAN.BTN, state = "disable")
  tcltk::tkconfigure(ABBOTT.BTN, state = "disable")
  tcltk::tkconfigure(ELLI.BTN, state = "disable")
  tcltk::tkconfigure(FACET.BTN, state = "disable")
  tcltk::tkconfigure(INDFRAC.BTN, state = "disable")
  tcltk::tkconfigure(COMPLEX.BTN, state = "disable")
  tcltk::tkconfigure(TOPO.BTN, state = "disable")
  tcltk::tkconfigure(DIR.BTN, state = "disable")
  tcltk::tkconfigure(LAUNCH.BTN, state = "disable")



}
