#' @title trident.gui
#' @description Open the trident graphical user interface.
#' @export
trident.gui <- function() {
  # TCLTK OBJECTS----
  # ...Metadata
  METADATA <- list(VERSION = '9.9.1', DESCRIPTION = "Topologic Multiverse")
  # ...Project data to be saved
  PROJECT <- NULL
  PROJECT <- list(SAVESTATE = FALSE,
                  SAVENAME = NULL,
                  FILES = list(),
                  DATASET = NULL,
                  VARIABLES = NULL,
                  NAMES = NULL,
                  OPTIONS = NULL,
                  TEMP = tcltk::tclVar(paste("Enter factor here")))
  PROJECT$NAMES <- list(EXPORT = tcltk::tclVar(paste("My_export")))
  PROJECT$OPTIONS <- list(JIGGER.VALUE = tcltk::tclVar("0"),
                          DENSITY.VALUE = tcltk::tclVar("0"),
                          BOXCOX.VALUE = tcltk::tclVar("0"),
                          DISC.ONLY.VALUE = tcltk::tclVar("1"),
                          GEOMEAN.VALUE = tcltk::tclVar("0"),
                          BYNGR.VALUE = tcltk::tclVar("0"),
                          BY.ANOVA.VALUE = "P-value",
                          BY.KTEST.VALUE = "P-value",
                          BY.MEANPOSTHOC.VALUE = "Tukey_s HSD",
                          BY.GRPOSTHOC.VALUE = "Tukey_s HSD",
                          BATCH.COMPLEX.VALUE = tcltk::tclVar("1"),
                          BATCH.ELEV.VALUE = tcltk::tclVar("1"),
                          BATCH.SPAT.VALUE = tcltk::tclVar("1"),
                          BATCH.TOPO.VALUE = tcltk::tclVar("0"),
                          BATCH.COMPLEX.H.VALUE = tcltk::tclVar("1"),
                          BATCH.ELEV.H.VALUE = tcltk::tclVar("1"),
                          BATCH.SPAT.H.VALUE = tcltk::tclVar("1"),
                          STD.VALUE = tcltk::tclVar("0"),
                          BATCH.REMOVAL.VALUE = tcltk::tclVar("None"),
                          PLOT.COLORS = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3), #24 levels max
                          PLOT.PCH = c(1, 15:18, 9:10, 18:15, 10:8, 11:14, 2:7),
                          PLOT.DPI = 300,
                          PLOT.HEIGHT = 150,
                          PLOT.WIDTH = 140,
                          PLOT.UNITS = "mm",
                          SEPARATOR = tcltk::tclVar(paste("_")),
                          ELEMENT = tcltk::tclVar("1"),
                          PREFIX = tcltk::tclVar(paste("")),
                          SUFFIX = tcltk::tclVar(paste("")),
                          FACTOR.NAME = tcltk::tclVar(paste("Factor")))
  # ...DMTA data NOT to be saved
  DMTA <- NULL
  DMTA <- list(SURF = NULL,
               COMPUT = NULL,
               COMPUT.HTR = NULL,
               DATASET = NULL,
               VARIABLES = NULL,
               SIZE = NULL,
               COLORS = NULL,
               LEVELS = NULL)
  DMTA$COMPUT <- list(COMPLEX = NULL,
                      ELEV = NULL,
                      SPAT = NULL,
                      TOPO = NULL)
  DMTA$COMPUT.HTR <- list(COMPLEX = NULL,
                          ELEV = NULL,
                          SPAT = NULL)
  DMTA$SIZE <- list(X = tcltk::tclVar("256"),
                    Y = tcltk::tclVar("256"),
                    N = tcltk::tclVar("256"))
  # ...Multicheck temporary object
  TEMPORARY <- NULL
  TEMPORARY <- list(MULTICHECK = list())
  # ...Screen resolution NOT to be saved
  SCREEN.RES <- NULL
  SCREEN.RES <- list(H = NULL, V = NULL)
  testh <- suppressWarnings(as.double(unlist(strsplit(system("wmic path Win32_VideoController get CurrentHorizontalResolution /format:value", intern = TRUE), "="))))
  SCREEN.RES$H <- testh[!is.na(testh)]
  testv <- suppressWarnings(as.double(unlist(strsplit(system("wmic path Win32_VideoController get CurrentVerticalResolution /format:value", intern = TRUE), "="))))
  SCREEN.RES$V <- testv[!is.na(testv)]

  # TCLTK COMMANDS----
  # ...open.project.cmd----
  # ...internal command to open a project
  open.project.cmd <- function() {
    # ...1st case: a project is currently opened
    if (!is.null(PROJECT$DATASET)) {
      # ...A first window to save current project
      TRIDENT.SaveCurrent1234 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.SaveCurrent1234, paste("trident", METADATA$VERSION, "- Open project..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.SaveCurrent1234, system.file("extdata","pics","trident.ico", package = "trident"))
      YES.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Yes", command = function() {
        tcltk::tkdestroy(TRIDENT.SaveCurrent1234)
        save.cmd()
        PROJECT <<- rlist::list.load(utils::choose.files())
        refresh.cmd()
        # ...re-able
        if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
        tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
        tcltk::tkconfigure(BUILD.BTN, state = "normal")
        tcltk::tkconfigure(REFRESH.BTN, state = "normal")
        tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
        if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
          tcltk::tkconfigure(TRANS.MBTN, state = "normal")
          tcltk::tkconfigure(SUM.BTN, state = "normal")
          tcltk::tkconfigure(MULTI.BTN, state = "normal")
          tcltk::tkconfigure(DISC.BTN, state = "normal")
          tcltk::tkconfigure(NONDISC.BTN, state = "normal")
          tcltk::tkconfigure(TOP3.BTN, state = "normal")
          tcltk::tkconfigure(RANK.MBTN, state = "normal")
          tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
          tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
          tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
          tcltk::tkconfigure(PCA.BTN, state = "normal")
        }
      })
      NO.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "No", command = function() {
        # ...A second window to confirm that there will be no save
        TRIDENT.NoSave1234 <<- tcltk::tktoplevel()
        OK2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "OK", command = function() {
          tcltk::tkdestroy(TRIDENT.NoSave1234)
          tcltk::tkdestroy(TRIDENT.SaveCurrent1234)
          PROJECT <<- rlist::list.load(utils::choose.files())
          refresh.cmd()
          # ...re-able
          if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
          tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
          tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
          tcltk::tkconfigure(BUILD.BTN, state = "normal")
          tcltk::tkconfigure(REFRESH.BTN, state = "normal")
          if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
            tcltk::tkconfigure(TRANS.MBTN, state = "normal")
            tcltk::tkconfigure(SUM.BTN, state = "normal")
            tcltk::tkconfigure(MULTI.BTN, state = "normal")
            tcltk::tkconfigure(DISC.BTN, state = "normal")
            tcltk::tkconfigure(NONDISC.BTN, state = "normal")
            tcltk::tkconfigure(TOP3.BTN, state = "normal")
            tcltk::tkconfigure(RANK.MBTN, state = "normal")
            tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
            tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
            tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
            tcltk::tkconfigure(PCA.BTN, state = "normal")
          }
        })
        CANCEL2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.NoSave1234))
        # ...Grid all in save window
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NoSave1234, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK2.BTN, CANCEL2.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.SaveCurrent1234))
      # ...Grid all in first window
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveCurrent1234, text = "Do you want to save the current project?"), columnspan = 3)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = FALSE)
    }
    # ...2nd case: there is no project currently opened
    if (is.null(PROJECT$DATASET)) {
      PROJECT <<- rlist::list.load(utils::choose.files())
      refresh.cmd()
      # ...re-able
      if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
      tcltk::tkconfigure(BUILD.BTN, state = "normal")
      tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
      tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
      tcltk::tkconfigure(REFRESH.BTN, state = "normal")
      if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
        tcltk::tkconfigure(TRANS.MBTN, state = "normal")
        tcltk::tkconfigure(SUM.BTN, state = "normal")
        tcltk::tkconfigure(MULTI.BTN, state = "normal")
        tcltk::tkconfigure(DISC.BTN, state = "normal")
        tcltk::tkconfigure(NONDISC.BTN, state = "normal")
        tcltk::tkconfigure(TOP3.BTN, state = "normal")
        tcltk::tkconfigure(RANK.MBTN, state = "normal")
        tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
        tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
        tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
        tcltk::tkconfigure(PCA.BTN, state = "normal")
      }
    }
  }

  # ...new.project.cmd----
  # ...internal command to start a new project
  new.project.cmd <- function() {
    # ...1st case: there is no project currently opened = button(s) should be disabled and launching the command will have no effect
    # ...2nd case: a project is currently opened
    if (!is.null(PROJECT$DATASET)) {
      # ...A first window to save current project
      TRIDENT.SaveCurrent1234 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.SaveCurrent1234, paste("trident", METADATA$VERSION, "- New project..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.SaveCurrent1234, system.file("extdata","pics","trident.ico", package = "trident"))
      YES.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Yes", command = function() {
        tcltk::tkdestroy(TRIDENT.SaveCurrent1234)
        Preferred.options <- PROJECT$OPTIONS
        Preferred.names <- PROJECT$NAMES
        save.cmd()
        PROJECT <<- NULL
        PROJECT <<- list(SAVESTATE = FALSE,
                         SAVENAME = NULL,
                         FILES = list(),
                         DATASET = NULL,
                         VARIABLES = NULL,
                         NAMES = Preferred.names,
                         OPTIONS = Preferred.options,
                         TEMP = NULL)
        # ...disable
        tcltk::tkentryconfigure(MENU$FILE, 0, state = "disable")
        tcltk::tkentryconfigure(MENU$FILE, 2, state = "disable")
        tcltk::tkentryconfigure(MENU$FILE, 3, state = "disable")
        tcltk::tkconfigure(COMBINE.BTN, state = "disable")
        tcltk::tkconfigure(BUILD.BTN, state = "disable")
        tcltk::tkconfigure(TRANS.MBTN, state = "disable")
        tcltk::tkconfigure(REFRESH.BTN, state = "disable")
        tcltk::tkconfigure(ADD.VAR.BTN, state = "disable")
        tcltk::tkconfigure(SUM.BTN, state = "disable")
        tcltk::tkconfigure(MULTI.BTN, state = "disable")
        tcltk::tkconfigure(DISC.BTN, state = "disable")
        tcltk::tkconfigure(NONDISC.BTN, state = "disable")
        tcltk::tkconfigure(TOP3.BTN, state = "disable")
        tcltk::tkconfigure(RANK.MBTN, state = "disable")
        tcltk::tkconfigure(BIPLOT.BTN, state = "disable")
        tcltk::tkconfigure(BOXPLOT.BTN, state = "disable")
        tcltk::tkconfigure(VIOLIN.BTN, state = "disable")
        tcltk::tkconfigure(PCA.BTN, state = "disable")
        tcltk::tkconfigure(EXPORT.BTN, state = "disable")
        tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
        tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
        tcltk::tkconfigure(HETERO.MBTN, state = "disable")
        tcltk::tkconfigure(HISTO.BTN, state = "disable")
        tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME1)
        tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME2)
      })
      NO.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "No", command = function() {
        # ...A second window to confirm that there will be no save
        TRIDENT.NoSave1234 <<- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.NoSave1234, paste("Confirm..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.NoSave1234, system.file("extdata","pics","trident.ico", package = "trident"))
        OK2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "OK", command = function() {
          tcltk::tkdestroy(TRIDENT.NoSave1234)
          tcltk::tkdestroy(TRIDENT.SaveCurrent1234)
          Preferred.options <- PROJECT$OPTIONS
          Preferred.names <- PROJECT$NAMESPROJECT <- NULL
          PROJECT <<- NULL
          PROJECT <<- list(SAVESTATE = FALSE,
                           SAVENAME = NULL,
                           FILES = list(),
                           DATASET = NULL,
                           VARIABLES = NULL,
                           NAMES = Preferred.names,
                           OPTIONS = Preferred.options,
                           TEMP = NULL)
          # ...disable
          tcltk::tkentryconfigure(MENU$FILE, 0, state = "disable")
          tcltk::tkentryconfigure(MENU$FILE, 2, state = "disable")
          tcltk::tkentryconfigure(MENU$FILE, 3, state = "disable")
          tcltk::tkconfigure(COMBINE.BTN, state = "disable")
          tcltk::tkconfigure(BUILD.BTN, state = "disable")
          tcltk::tkconfigure(TRANS.MBTN, state = "disable")
          tcltk::tkconfigure(REFRESH.BTN, state = "disable")
          tcltk::tkconfigure(ADD.VAR.BTN, state = "disable")
          tcltk::tkconfigure(SUM.BTN, state = "disable")
          tcltk::tkconfigure(MULTI.BTN, state = "disable")
          tcltk::tkconfigure(DISC.BTN, state = "disable")
          tcltk::tkconfigure(NONDISC.BTN, state = "disable")
          tcltk::tkconfigure(TOP3.BTN, state = "disable")
          tcltk::tkconfigure(RANK.MBTN, state = "disable")
          tcltk::tkconfigure(BIPLOT.BTN, state = "disable")
          tcltk::tkconfigure(BOXPLOT.BTN, state = "disable")
          tcltk::tkconfigure(VIOLIN.BTN, state = "disable")
          tcltk::tkconfigure(PCA.BTN, state = "disable")
          tcltk::tkconfigure(EXPORT.BTN, state = "disable")
          tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
          tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
          tcltk::tkconfigure(HETERO.MBTN, state = "disable")
          tcltk::tkconfigure(HISTO.BTN, state = "disable")
          tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME1)
          tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME2)
        })
        CANCEL2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.NoSave1234))
        # ... grid saving window
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NoSave1234, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK2.BTN, CANCEL2.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.SaveCurrent1234))
      # ...Grid all in first window
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveCurrent1234, text = "Do you want to save the current project?"), columnspan = 3)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = FALSE)
    }
  }

  # ...save.cmd----
  # ..internal command to save a project
  save.cmd <- function() {
    if (!isTRUE(PROJECT$SAVESTATE)) save.as.cmd()
    if (isTRUE(PROJECT$SAVESTATE)) rlist::list.save(x = PROJECT, file = PROJECT$SAVENAME)
  }

  # ...save.as.cmd----
  # ..internal command to save a project as new .rds file
  save.as.cmd <- function() {
    PROJECT$SAVENAME <<- tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save project...", initialfile = paste("Untitled.rds", sep = "")))
    rlist::list.save(x = PROJECT, file = PROJECT$SAVENAME)
    PROJECT$SAVESTATE <<- TRUE
  }

  # ...quit.cmd----
  # ..internal command to close the GUI
  quit.cmd <- function() {
    if (!is.null(PROJECT$DATASET)) {
      # ...A first window to save current project
      TRIDENT.SaveCurrent1234 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.SaveCurrent1234, paste("trident", METADATA$VERSION, "- Save project..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.SaveCurrent1234, system.file("extdata","pics","trident.ico", package = "trident"))
      YES.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Save", command = function() {
        save.cmd()
        if (exists("TRIDENT.mainwin1234")) tcltk::tkdestroy(TRIDENT.mainwin1234) ; rm(TRIDENT.mainwin1234)
        if (exists("TRIDENT.CorCircle1234")) tcltk::tkdestroy(TRIDENT.CorCircle1234) ; rm(TRIDENT.CorCircle1234)
        if (exists("TRIDENT.ManFactor1234")) tcltk::tkdestroy(TRIDENT.ManFactor1234) ; rm(TRIDENT.ManFactor1234)
        if (exists("TRIDENT.ModeSelect1234")) tcltk::tkdestroy(TRIDENT.ModeSelect1234) ; rm(TRIDENT.ModeSelect1234)
        if (exists("TRIDENT.NoSave1234")) tcltk::tkdestroy(TRIDENT.NoSave1234) ; rm(TRIDENT.NoSave1234)
        if (exists("TRIDENT.PCAPlot5678")) tcltk::tkdestroy(TRIDENT.PCAPlot5678) ; rm(TRIDENT.PCAPlot5678)
        if (exists("TRIDENT.PCSelect5678")) tcltk::tkdestroy(TRIDENT.PCSelect5678) ; rm(TRIDENT.PCSelect5678)
        if (exists("TRIDENT.PlotWin1234")) tcltk::tkdestroy(TRIDENT.PlotWin1234) ; rm(TRIDENT.PlotWin1234)
        if (exists("TRIDENT.SaveCurrent1234")) tcltk::tkdestroy(TRIDENT.SaveCurrent1234) ; rm(TRIDENT.SaveCurrent1234)
        if (exists("TRIDENT.TableWin1234")) tcltk::tkdestroy(TRIDENT.TableWin1234) ; rm(TRIDENT.TableWin1234)
      })
      NO.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Discard", command = function() {
        # ...A second window to confirm that there will be no save
        TRIDENT.NoSave1234 <<- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.NoSave1234, paste("trident", METADATA$VERSION, "- Confirm..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.NoSave1234, system.file("extdata","pics","trident.ico", package = "trident"))
        OK2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "Yes (discard)", command = function() {
          if (exists("TRIDENT.mainwin1234")) tcltk::tkdestroy(TRIDENT.mainwin1234) ; rm(TRIDENT.mainwin1234)
          if (exists("TRIDENT.CorCircle1234")) tcltk::tkdestroy(TRIDENT.CorCircle1234) ; rm(TRIDENT.CorCircle1234)
          if (exists("TRIDENT.ManFactor1234")) tcltk::tkdestroy(TRIDENT.ManFactor1234) ; rm(TRIDENT.ManFactor1234)
          if (exists("TRIDENT.ModeSelect1234")) tcltk::tkdestroy(TRIDENT.ModeSelect1234) ; rm(TRIDENT.ModeSelect1234)
          if (exists("TRIDENT.NoSave1234")) tcltk::tkdestroy(TRIDENT.NoSave1234) ; rm(TRIDENT.NoSave1234)
          if (exists("TRIDENT.PCAPlot5678")) tcltk::tkdestroy(TRIDENT.PCAPlot5678) ; rm(TRIDENT.PCAPlot5678)
          if (exists("TRIDENT.PCSelect5678")) tcltk::tkdestroy(TRIDENT.PCSelect5678) ; rm(TRIDENT.PCSelect5678)
          if (exists("TRIDENT.PlotWin1234")) tcltk::tkdestroy(TRIDENT.PlotWin1234) ; rm(TRIDENT.PlotWin1234)
          if (exists("TRIDENT.SaveCurrent1234")) tcltk::tkdestroy(TRIDENT.SaveCurrent1234) ; rm(TRIDENT.SaveCurrent1234)
          if (exists("TRIDENT.TableWin1234")) tcltk::tkdestroy(TRIDENT.TableWin1234) ; rm(TRIDENT.TableWin1234)
        })
        CANCEL2.BTN <- tcltk2::tk2button(TRIDENT.NoSave1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.NoSave1234))
        # ...grid all in save window
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NoSave1234, text = "Are you sure?\nCurrent project will be deleted from memory"), columnspan = 2, padx = 5, pady = 5)
        tcltk::tkgrid(OK2.BTN, CANCEL2.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SaveCurrent1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.SaveCurrent1234))
      # ...Grid all in first window
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveCurrent1234, text = "Do you want to save the current project?"), columnspan = 3, padx = 5, pady = 5)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.SaveCurrent1234, topmost = FALSE)

    }
    if (is.null(PROJECT$DATASET)) {
      tcltk::tkdestroy(TRIDENT.mainwin1234)
    }
  }

  # ...add.to.dataset.cmd----
  # ...a command to build a button for adding an observation to the main dataset
  add.to.dataset.cmd <- function(widget, data, variables) {
    ADD.BTN <- tcltk::tkbutton(widget, text = "Add to\ndataset", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","add.gif", package = "trident")), compound = "left", relief = "flat", command = function() {
      # ...1st case: there is already a dataset
      if (!is.null(PROJECT$DATASET)) {
        if (length(setdiff(colnames(PROJECT$DATASET), names(data))) != 0) tcltk::tk_messageBox(type = "okcancel", icon = "warning", message = paste0(c("There are some non-matching variable names:", setdiff(names(PROJECT$DATASET), names(data))), collapse = "\n"))
        PROJECT$DATASET <<- merge(PROJECT$DATASET, data, by = intersect(names(PROJECT$DATASET), names(data)), all = TRUE, sort = FALSE, no.dups = TRUE)
        PROJECT$VARIABLES <<- dplyr::union(PROJECT$VARIABLES, variables)
        refresh.cmd()
        # ...re-able
        if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
        tcltk::tkconfigure(BUILD.BTN, state = "normal")
        tcltk::tkconfigure(REFRESH.BTN, state = "normal")
        tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
        tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
        if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
          tcltk::tkconfigure(TRANS.MBTN, state = "normal")
          tcltk::tkconfigure(SUM.BTN, state = "normal")
          tcltk::tkconfigure(MULTI.BTN, state = "normal")
          tcltk::tkconfigure(DISC.BTN, state = "normal")
          tcltk::tkconfigure(NONDISC.BTN, state = "normal")
          tcltk::tkconfigure(TOP3.BTN, state = "normal")
          tcltk::tkconfigure(RANK.MBTN, state = "normal")
          tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
          tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
          tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
          tcltk::tkconfigure(PCA.BTN, state = "normal")
        }
        tcltk::tkconfigure(ADD.BTN, state = "disable")
      }
      # ...2nd case: no dataset
      if (is.null(PROJECT$DATASET)) {
        PROJECT$DATASET <<- data
        PROJECT$VARIABLES <<- variables
        refresh.cmd()
        # ...re-able
        if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
        tcltk::tkconfigure(BUILD.BTN, state = "normal")
        tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
        tcltk::tkconfigure(REFRESH.BTN, state = "normal")
        tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
        if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
          tcltk::tkconfigure(TRANS.MBTN, state = "normal")
          tcltk::tkconfigure(SUM.BTN, state = "normal")
          tcltk::tkconfigure(MULTI.BTN, state = "normal")
          tcltk::tkconfigure(DISC.BTN, state = "normal")
          tcltk::tkconfigure(NONDISC.BTN, state = "normal")
          tcltk::tkconfigure(TOP3.BTN, state = "normal")
          tcltk::tkconfigure(RANK.MBTN, state = "normal")
          tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
          tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
          tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
          tcltk::tkconfigure(PCA.BTN, state = "normal")
        }
        tcltk::tkconfigure(ADD.BTN, state = "disable")
      }
    })
    tcltk::tkgrid(ADD.BTN, padx = 5, pady = 5)
    tcltk2::tk2tip(ADD.BTN, message = "Merge current observation with the main dataset")
  }
  # ...build.table.cmd----
  # ..internal command to build table in a widget
  build.table.cmd <- function(df, widget, bg.table = "white", bg.title = "azure4", fg.title = "azure1",
                              height = floor((SCREEN.RES$V * 0.7 - 120) / (16 * 1.5)),
                              width =  floor((SCREEN.RES$H * 0.7 - 120) / (16 / 1.5)),
                              selectmode = "single", save.btn = TRUE, export.btn = TRUE, return = FALSE) {
    # ...make mclistbox
    MCLISTBOX <- tcltk2::tk2mclistbox(widget, width = width, selectmode = selectmode, resizablecolumns = TRUE, labelrelief = "flat", relief = "flat", columnrelief = "raised", columnborderwidth = 1,
                                      xscrollcommand = function(...) tcltk::tkset(XSCR, ...),
                                      yscrollcommand = function(...) tcltk::tkset(YSCR, ...))
    # ...mclistbox columns
    for (i in c(1:length(df[1, ]))) {
      Width = max(stats::na.omit(c(nchar(as.character(df[, i])), nchar(as.character(colnames(df)[i])))))
      tcltk2::tk2column(MCLISTBOX, label = colnames(df)[i], width = Width + 1)
    }
    # ...mclistbox scrollbars
    XSCR <- tcltk::tkscrollbar(widget, command = function(...) tcltk::tkxview(MCLISTBOX,...), orient = "horizontal")
    YSCR <- tcltk::tkscrollbar(widget, command = function(...) tcltk::tkyview(MCLISTBOX,...))
    # ...grid all (widget)
    tcltk::tkgrid(MCLISTBOX, YSCR, sticky = "ns")
    tcltk::tkgrid(XSCR, sticky = "ew")
    if (save.btn == TRUE) {
      SAVE.BTN <- tcltk2::tk2button(widget, text = "Save", tip = "Save table", width = 10, command = function() {
        save.table.cmd(df)
      })
      # ...grid all (widget)
      tcltk::tkgrid(SAVE.BTN, padx = 5)
    }
    if (export.btn == TRUE) {
      EXPORT.BTN <- tcltk2::tk2button(widget, text = "Export to R", tip = "Export data.frame object to R", width = 10, command = function() {
        # ...open window for name entry
        TRIDENT.NameEntry1234 <- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.NameEntry1234, paste("trident", METADATA$VERSION, "- Enter name..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.NameEntry1234, system.file("extdata","pics","trident.ico", package = "trident"))
        NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.NameEntry1234, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          My_data_from_trident <- data.frame(df, stringsAsFactors = TRUE)
          assign(paste(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))), My_data_from_trident, envir = .GlobalEnv)
          tcltk::tkdestroy(TRIDENT.NameEntry1234)
        })
        CANCEL.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(TRIDENT.NameEntry1234))
        # ...grid all (name entry window)
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NameEntry1234, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })
      # ...grid all (widget)
      tcltk::tkgrid(EXPORT.BTN, padx = 5)
    }

    # ...populate mclistbox
    for (i in c(1:length(df[, 1]))) {
      tcltk2::tk2insert.multi(MCLISTBOX, "end", df[i, ])
    }
    # ...aesthetics mclistbox
    tcltk::tkconfigure(MCLISTBOX, labelbackground = bg.title)
    tcltk::tkconfigure(MCLISTBOX, labelforeground = fg.title)
    tcltk::tkconfigure(MCLISTBOX, background = bg.table)
    tcltk::tkconfigure(MCLISTBOX, height = height)
    if(return == TRUE) return(MCLISTBOX)
  }
  # ...save.table.cmd----
  # ..internal command to save table
  save.table.cmd <- function(x) {
    File <- tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save table as...",
                                                 initialfile = paste("Untitled"),
                                                 defaultextension = ".txt",
                                                 filetypes = paste (
                                                   "{{text files} {.txt}}",
                                                   "{{CSV files} {.csv}}",
                                                   "{{Excel files} {.xls}}",
                                                   "{{All files} {*}}", sep = " ")))
    if (utils::tail(unlist(strsplit(File, as.character("\\."))), 1) == "csv") readr::write_excel_csv(x, path = File)
    if (utils::tail(unlist(strsplit(File, as.character("\\."))), 1) == "xls") readr::write_excel_csv2(x, path = File)
    if (utils::tail(unlist(strsplit(File, as.character("\\."))), 1) == "txt") readr::write_delim(x, path = File, delim = " ")
  }
  # ...build.vartable.cmd----
  build.vartable.cmd <- function(x) { # where x is a data.frame object
    # If modifying this function : for 'Type' definition, pay extra attention to the order, as grep() will look for ALL matching strings
    # ex: looking for 'Sa' will affect both 'Sa' and 'Sal', so the type of Sa is changed first and then the type of 'Sal' is corrected)
    Names <- colnames(x)
    Results <- data.frame(Variables = Names, Type = Names)
    Results$Type[grep('File', Names, value = FALSE)] <- "factor"
    Results$Type[grep('Asfc', Names, value = FALSE)] <- "complexity"
    Results$Type[grep('Sa', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sp', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sq', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sv', Names, value = FALSE)] <- "height"
    Results$Type[grep('Ssk', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sku', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sdar', Names, value = FALSE)] <- "height"
    Results$Type[grep('Sm', Names, value = FALSE)] <- "height"
    Results$Type[grep('Smd', Names, value = FALSE)] <- "height"
    Results$Type[grep('Rmax', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('Sal', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('Std', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('Stri', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('b.sl', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('r.sl', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('s.sl', Names, value = FALSE)] <- "spatial"
    Results$Type[grep('Sk1', Names, value = FALSE)] <- "topology"
    Results$Type[grep('Sk2', Names, value = FALSE)] <- "topology"
    Results$Type[grep('Scm', Names, value = FALSE)] <- "topology"
    Results$Type[grep('Snb', Names, value = FALSE)] <- "topology"
    Results$Type[grep('Sh', Names, value = FALSE)] <- "topology"
    return(Results)
  }

  # ...refresh.cmd----
  refresh.cmd <- function(na.rm = TRUE) {
    # ...Step 1: define the width of the tables
    Varlist.width <- 2 + max(nchar(PROJECT$VARIABLES$Variables)) + max(nchar(PROJECT$VARIABLES$Type))
    Dataset.width <- floor(SCREEN.RES$H / (16 / 1.5)) - Varlist.width
    # ...Step 2: remove NAs?
    if (na.rm) {
      if (is.na(max(unlist(PROJECT$DATASET)))) {
        df <- PROJECT$DATASET
        Mylist <- colnames(df)[colSums(is.na(df)) > 0]
        TRIDENT.narm1234 <- tcltk::tktoplevel()
        # afficher les noms des colonnes avec des NAs
        VARLIST <- tcltk2::tk2listbox(TRIDENT.narm1234, values = Mylist, selectmode = "extended", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
        # OK button supprime les variables sélectionnées
        OK.BTN <- tcltk2::tk2button(TRIDENT.narm1234, text = "Remove", command = function() {
          Myselect <- Mylist[tcltk2::selection(VARLIST)]
          PROJECT$DATASET <<- df[, which(!colnames(df) %in% Myselect)]
          PROJECT$VARIABLES <<- PROJECT$VARIABLES[which(!PROJECT$VARIABLES$Variables %in% Myselect), ]
          refresh.cmd(na.rm = FALSE)
          tcltk::tkdestroy(TRIDENT.narm1234)
        })
        # Ignore button
        CANCEL.BTN <- tcltk2::tk2button(TRIDENT.narm1234, text = "Ignore", tip = "", command = function() tcltk::tkdestroy(TRIDENT.narm1234))
        # grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.narm1234, text = "The following variables contain NAs\nDo you want to remove variables?"), columnspan = 2)
        tcltk::tkgrid(VARLIST)
        tcltk::tkgrid(OK.BTN, CANCEL.BTN)
      }
    }

    # ...TABLE1: the main dataset
    if (!is.null(TRIDENT.mainwin1234$FRAME1)) tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME1)
    TRIDENT.mainwin1234$FRAME1 <<- tcltk2::tk2frame(TRIDENT.mainwin1234)
    build.table.cmd(df = PROJECT$DATASET, widget = TRIDENT.mainwin1234$FRAME1,
                    height = floor((SCREEN.RES$V * 0.8 - 200) / (16 * 1.5)),
                    width = Dataset.width,
                    bg.table = "papayawhip", bg.title = "tan")
    # ...TABLE2: the list of variables
    if (!is.null(TRIDENT.mainwin1234$FRAME2)) tcltk::tkdestroy(TRIDENT.mainwin1234$FRAME2)
    TRIDENT.mainwin1234$FRAME2 <<- tcltk2::tk2frame(TRIDENT.mainwin1234)
    build.table.cmd(df = PROJECT$VARIABLES, widget = TRIDENT.mainwin1234$FRAME2,
                    height = floor((SCREEN.RES$V * 0.8 - 200) / (16 * 1.5)),
                    width = Varlist.width,
                    bg.table = "snow", bg.title = "tomato3", save.btn = FALSE, export.btn = FALSE)

    EDIT.NAME.BTN <- tcltk2::tk2button(TRIDENT.mainwin1234$FRAME2, text = "Edit name", tip = "Edit variable name", width = 10, command = function() {
      # ...a window to rename variable
      TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Edit name..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
      Myvar <- PROJECT$VARIABLES$Variables
      Myentry <- tcltk::tclVar("My_variable")
      VARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = Myvar, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
      NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.VarSelect1234, textvariable = Myentry)
      CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Confirm", command = function() {
        colnames(PROJECT$DATASET)[tcltk2::selection(VARLIST)] <<- tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))
        PROJECT$VARIABLES$Variables[tcltk2::selection(VARLIST)] <<- tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))
        refresh.cmd()
        tcltk::tkdestroy(TRIDENT.VarSelect1234)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
      # ......grid all
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Select variable:"), columnspan = 2)
      tcltk::tkgrid(VARLIST, columnspan = 2)
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Enter new variable name:"), columnspan = 2)
      tcltk::tkgrid(NAME.ENTRY, columnspan = 2)
      tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
    })
    EDIT.TYPE.BTN <- tcltk2::tk2button(TRIDENT.mainwin1234$FRAME2, text = "Edit type", tip = "Edit type of the variable", width = 10, command = function() {
      # ...a window to change variable type
      TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Edit type..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
      Myvar <- PROJECT$VARIABLES$Variables
      Myentry <- tcltk::tclVar("My_type")
      VARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = Myvar, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
      TYPE.ENTRY <- tcltk2::tk2entry(TRIDENT.VarSelect1234, textvariable = Myentry)
      CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Confirm", command = function() {
        PROJECT$VARIABLES$Type[tcltk2::selection(VARLIST)] <<- tcltk::tclvalue(tcltk::tkget(TYPE.ENTRY))
        refresh.cmd()
        tcltk::tkdestroy(TRIDENT.VarSelect1234)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
      # ......grid all
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Select variable:"), columnspan = 2)
      tcltk::tkgrid(VARLIST, columnspan = 2)
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Enter new variable type:"), columnspan = 2)
      tcltk::tkgrid(TYPE.ENTRY, columnspan = 2)
      tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
    })

    # ...grid all
    tcltk::tkgrid(EDIT.NAME.BTN, ipadx = 5)
    tcltk::tkgrid(EDIT.TYPE.BTN, ipadx = 5)
    # ...pack all
    tcltk::tkpack(TRIDENT.mainwin1234$FRAME1, side = "left", expand = FALSE, fill = "x", anchor = "nw")
    tcltk::tkpack(TRIDENT.mainwin1234$FRAME2, side = "top", expand = FALSE, fill = "x", anchor = "nw")
    # ...some additionnal effects of refreshing
    TEMPORARY$MULTICHECK <<- list()

  }


  # ...make.plot.cmd----
  make.plot.cmd <- function(myplot) {
    TRIDENT.PLOT <<- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.PLOT, paste("trident", METADATA$VERSION))
    tcltk2::tk2ico.setFromFile(TRIDENT.PLOT, system.file("extdata","pics","trident.ico", package = "trident"))

    # Build re-sizable plot
    tkRplotR::tkRplot(W = TRIDENT.PLOT,
                      fun = function() graphics::plot(myplot),
                      width = 600,
                      height = 500)

    # SAVE button
    TRIDENT.PLOT$BUTTONS <- tcltk::tkframe(TRIDENT.PLOT)
    tcltk::tkpack(TRIDENT.PLOT$BUTTONS, side = "bottom")
    SAVE.BTN <- tcltk2::tk2button(TRIDENT.PLOT$BUTTONS,
                                  text = "SAVE",
                                  tip = "Save graph to...",
                                  command = function() {

                                    # ...a window to select saving parameters, such as resolution, size, etc.
                                    TRIDENT.SaveWin1234 <- tcltk::tktoplevel()
                                    tcltk::tkwm.title(TRIDENT.SaveWin1234, paste("trident", METADATA$VERSION, "- Image size..."))
                                    tcltk2::tk2ico.setFromFile(TRIDENT.SaveWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
                                    DPI.SPNBX <- tcltk2::tk2spinbox(TRIDENT.SaveWin1234, from = 100, to = 1000, increment = 10)
                                    HEIGHT.NTRY <- tcltk2::tk2entry(TRIDENT.SaveWin1234, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
                                    WIDTH.NTRY <- tcltk2::tk2entry(TRIDENT.SaveWin1234, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
                                    UNITS.CBBX <- tcltk2::tk2combobox(TRIDENT.SaveWin1234, values = c("mm", "cm", "in"))
                                    OK.BTN <- tcltk2::tk2button(TRIDENT.SaveWin1234, text = "Ok", tip = "", command = function() {
                                      PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))

                                      if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
                                        PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}

                                      if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
                                        PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}

                                      PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
                                      tcltk::tkdestroy(TRIDENT.SaveWin1234)
                                      ggplot2::ggsave(myplot,
                                                      file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = TRIDENT.mainwin1234,
                                                                                                  title = "Save plot as...",
                                                                                                  initialfile = paste("New_plot"),
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

                                    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SaveWin1234,
                                                                    text = "Cancel",
                                                                    tip = "",
                                                                    command = function() tcltk::tkdestroy(TRIDENT.SaveWin1234))

                                    # ...grid all
                                    tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Height:"),
                                                  HEIGHT.NTRY,
                                                  tcltk::tklabel(TRIDENT.SaveWin1234, text = "Width:"),
                                                  WIDTH.NTRY)
                                    tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Units:"),
                                                  UNITS.CBBX,
                                                  columnspan = 2)
                                    tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Resolution (dpi):"),
                                                  DPI.SPNBX,
                                                  columnspan = 2)
                                    tcltk::tkgrid(OK.BTN,
                                                  CANCEL.BTN,
                                                  columnspan = 2)
                                    tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
                                    tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
                                  })

    #EXPORT button
    EXPORT.BTN <- tcltk2::tk2button(TRIDENT.PLOT$BUTTONS,
                                    text = "Export",
                                    tip = "Export data.frame object to R",
                                    command = function() {
      # ...a second window for PCA plot name entry
      TRIDENT.NameEntry1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.NameEntry1234, paste("trident", METADATA$VERSION, "- Enter name..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.NameEntry1234, system.file("extdata","pics","trident.ico", package = "trident"))
      NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.NameEntry1234, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
      CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Confirm", tip = "Confirm name and export to R", command = function() {
        My_graph_from_trident <- myplot
        assign(paste(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))), My_graph_from_trident, envir = .GlobalEnv)
        tcltk::tkdestroy(TRIDENT.NameEntry1234)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(TRIDENT.NameEntry1234))
      # ...grid all
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.NameEntry1234, text = "Name:"), NAME.ENTRY)
      tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
    })

    # Grid all
    tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5, sticky = "ew")
  }


  # ...biplot.cmd----
  biplot.cmd <- function(df) {
    # ...a window to select x, y and the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myx <- NULL
    Myy <- NULL
    Myfactor <- NULL
    XLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234,
                                values = colnames(Numerics),
                                selectmode = "single",
                                height = 12,
                                tip = "",
                                scroll = "y",
                                autoscroll = "x",
                                enabled = TRUE)
    YLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234,
                                values = colnames(Numerics),
                                selectmode = "single",
                                height = 12,
                                tip = "",
                                scroll = "y",
                                autoscroll = "x",
                                enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234,
                                     values = colnames(Factors),
                                     selectmode = "single",
                                     height = 12,
                                     tip = "",
                                     scroll = "y",
                                     autoscroll = "x",
                                     enabled = TRUE)
    FACTORLIST2 <- tcltk2::tk2listbox(TRIDENT.VarSelect1234,
                                      values = colnames(Factors),
                                      selectmode = "single",
                                      height = 12,
                                      tip = "",
                                      scroll = "y",
                                      autoscroll = "x",
                                      enabled = TRUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      if (length(tcltk2::selection(XLIST)) == 0 | length(tcltk2::selection(YLIST)) == 0) {
        tcltk::tkmessageBox(message = "Please select 1 variable for each axis", icon = "warning", type = "ok")
        stop()
      }

      if (length(tcltk2::selection(FACTORLIST)) == 0) {
        tcltk::tkmessageBox(message = "Please select 1 factor", icon = "warning", type = "ok")
        stop()
      }

      if (isTRUE(tcltk2::selection(FACTORLIST) == tcltk2::selection(FACTORLIST2))) {
        tcltk::tkmessageBox(message = "1st and 2nd factor should be different", icon = "warning", type = "ok")
        stop()
      }

      Myx <<- colnames(Numerics)[tcltk2::selection(XLIST)]
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]

      # ...Build biplot
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = Mydf[, Myx], y = Mydf[, Myy])) +
        ggplot2::labs(x = Myx, y = Myy) +
        ggplot2::guides(size = "none") +
        ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                       legend.position = "right", legend.title = ggplot2::element_text(size = 14),
                       axis.text.x = ggplot2::element_text(size = 12, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                       axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                       panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.ontop = FALSE,
                       axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                       axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      if (length(tcltk2::selection(FACTORLIST2)) == 0) {
        Plot <- Plot +
          ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor], fill = Mydf[, Myfactor]), size = 3) +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_shape_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.PCH)
      }
      if (length(tcltk2::selection(FACTORLIST2)) == 1) {
        Factor2 <- colnames(Factors)[tcltk2::selection(FACTORLIST2)]
        Plot <- Plot +
          ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor], fill = Mydf[, Myfactor]), size = 3) +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_shape_manual(name = Factor2, labels = levels(Mydf[, Factor2]), values = PROJECT$OPTIONS$PLOT.PCH)
      }

      # ...Make plot window
      make.plot.cmd(myplot = Plot)

      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })

    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234,
                                    text = "Cancel",
                                    command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))

    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose x-axis variable"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose y-axis variable"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose 1st factor"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose 2nd factor (optional)"))
    tcltk::tkgrid(XLIST, YLIST, FACTORLIST, FACTORLIST2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }

  # ...boxplot.cmd----
  boxplot.cmd <- function(df) {
    # ...a window to select y and the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myy <- NULL
    Myfactor <- NULL
    YLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST2 <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    JIGGER.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Jiggerplot")
    tcltk::tkconfigure(JIGGER.CHKBTN, variable = PROJECT$OPTIONS$JIGGER.VALUE)

    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      if (length(tcltk2::selection(YLIST)) == 0) {
        tcltk::tkmessageBox(message = "Please select 1 variable", icon = "warning", type = "ok")
        stop()
      }
      if (length(tcltk2::selection(FACTORLIST)) == 0) {
        tcltk::tkmessageBox(message = "Please select 1 factor", icon = "warning", type = "ok")
        stop()
      }
      if (isTRUE(tcltk2::selection(FACTORLIST) == tcltk2::selection(FACTORLIST2))) {
        tcltk::tkmessageBox(message = "1st and 2nd factor should be different", icon = "warning", type = "ok")
        stop()
      }
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]

      # ...Build boxplot
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = Mydf[, Myfactor], y = Mydf[, Myy])) +
        ggplot2::labs(x = Myfactor, y = Myy) +
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
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 0 & length(tcltk2::selection(FACTORLIST2)) == 0) {
        Plot <- Plot +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Myfactor]), size = 1) +
          ggplot2::theme(axis.text.x = ggplot2::element_blank())

      }

      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 1 & length(tcltk2::selection(FACTORLIST2)) == 0) {
        Plot <- Plot +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = colorspace::lighten(PROJECT$OPTIONS$PLOT.COLORS, amount = 0.5)) +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Myfactor]), size = 1, show.legend = FALSE) +
          ggplot2::geom_jitter(ggplot2::aes(col = Mydf[, Myfactor]), position = ggplot2::position_jitterdodge(jitter.width = 0.5)) +
          ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }

      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 0 & length(tcltk2::selection(FACTORLIST2)) == 1) {
        Factor2 <- colnames(Factors)[tcltk2::selection(FACTORLIST2)]
        Plot <- Plot +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Factor2]), size = 1) +
          ggplot2::scale_fill_manual(name = Factor2, labels = levels(Mydf[, Factor2]), values = PROJECT$OPTIONS$PLOT.COLORS)
      }

      if (tcltk::tclvalue(PROJECT$OPTIONS$JIGGER.VALUE) == 1 & length(tcltk2::selection(FACTORLIST2)) == 1) {
        Factor2 <- colnames(Factors)[tcltk2::selection(FACTORLIST2)]
        Plot <- Plot +
          ggplot2::geom_boxplot(ggplot2::aes(fill = Mydf[, Factor2]), size = 1, show.legend = FALSE) +
          ggplot2::geom_jitter(ggplot2::aes(col = Mydf[, Factor2]), position = ggplot2::position_jitterdodge(jitter.width = 0.5)) +
          ggplot2::scale_color_manual(name = Factor2, labels = levels(Mydf[, Factor2]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Factor2, labels = levels(Mydf[, Factor2]), values = colorspace::lighten(PROJECT$OPTIONS$PLOT.COLORS, amount = 0.5))
        }

    # ...Make plot window
    make.plot.cmd(myplot = Plot)

    tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })

    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))

    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose y-axis variable"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose 1st factor"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose 2nd factor (optional)"))
    tcltk::tkgrid(YLIST, FACTORLIST, FACTORLIST2)
    tcltk::tkgrid(JIGGER.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }

  # ...plot.violin.cmd----
  plot.violin.cmd <- function(df) {
    # ...a window to select x and the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myy <- NULL
    Myfactor <- NULL
    YLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)

    # ...OK button
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]

      # ...Build violin plot
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = "", y = Mydf[, Myy], group = Mydf[, Myfactor])) +
        ggplot2::labs(y = Myy, x = "") +
        ggplot2::guides(size = "none") +
        ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 12, face = "bold"),
                       legend.position = "right", legend.title = ggplot2::element_text(size = 14),
                       axis.text.x = ggplot2::element_text(size = 12, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                       axis.text.y = ggplot2::element_text(size = 12, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                       panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.ontop = FALSE,
                       axis.title.x = ggplot2::element_text(size = 14, angle = 00, face = "plain"),
                       axis.title.y = ggplot2::element_text(size = 14, angle = 90, face = "plain")) +
        ggplot2::geom_violin(ggplot2::aes(fill = Mydf[, Myfactor]), scale = "area", size = 0.5) +
        ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

      # ...Make plot window
      make.plot.cmd(myplot = Plot)

      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })

    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))

    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose y-axis variable"), tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"))
    tcltk::tkgrid(YLIST, FACTORLIST)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }

  # ...plot.pca.cmd----
  plot.pca.cmd <- function(df) {
    # ...a first window to select variables for the PCA
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- df
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    Mydf <- data.frame(Factors, Numerics)
    Myvars <- NULL
    Mysupvars <- NULL
    Myfactor <- NULL
    VARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Numerics), selectmode = "extended", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    #####
    SUPVARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Numerics), selectmode = "extended", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    #####
    FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)

    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      Myvars <<- colnames(Numerics)[tcltk2::selection(VARLIST)]
      Mysupvars <<- colnames(Numerics)[tcltk2::selection(SUPVARLIST)]
      Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      Mypcadata <- data.frame(Mydf[, c(which(colnames(Mydf) %in% Myvars), which(colnames(Mydf) %in% Mysupvars))])
      Mypca <- FactoMineR::PCA(Mypcadata, quanti.sup = c((length(Myvars) + 1):(length(Myvars) + length(Mysupvars))), ncp = ncol(length(Myvars)))
      Mypca.df <- data.frame(Mypca$ind$coord)
      Mycor.df <- data.frame(Mypca$var$coord)
      Contrib <- data.frame(Mypca$eig)[, 2]

      # ...a second window to select which PC to plot
      TRIDENT.PCSelect5678 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.PCSelect5678, paste("trident", METADATA$VERSION, "- Screeplot..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.PCSelect5678, system.file("extdata","pics","trident.ico", package = "trident"))
      TKPLOT <- NULL
      SCREEPLOT <- factoextra::fviz_eig(Mypca, addlabels = TRUE, ylim = c(0, max(Mypca$eig[, 2])), barfill = "lightgoldenrod", barcolor = "lightgoldenrod3")
      TKPLOT <- tkrplot::tkrplot(TRIDENT.PCSelect5678, fun = function() graphics::plot(SCREEPLOT))
      MYPCLIST1 <- tcltk2::tk2listbox(TRIDENT.PCSelect5678, values = colnames(Mypca.df), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
      MYPCLIST2 <- tcltk2::tk2listbox(TRIDENT.PCSelect5678, values = colnames(Mypca.df), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)


      PLOT.SUP.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678, text = "Add individuals", tip = "Add supplementary individuals to the PCA", command = function() {

        File.tmp <- tcltk::tk_choose.files(filters = matrix(c("All files", "Text", "Calc", "Excel", "*", ".txt", ".csv", ".xls"), ncol = 2))

        if (tools::file_ext(File.tmp) == "txt") {
          Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = "", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
        }

        if (tools::file_ext(File.tmp) == "csv") {
          Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
        }

        if (tools::file_ext(File.tmp) == "xls") {
          Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
        }




        Splitted <- data.frame(Mytable[, c(which(colnames(Mytable) %in% Myvars), which(colnames(Mytable) %in% Mysupvars))])
        if (length(Splitted[1, ]) != length (Mydf[1, ])) {
          tcltk::tkmessageBox(type = "ok",
                              message = "Different number of columns,\nplease check column names")
          stop()
        }





        Mynewpcadata <- dplyr::union(Mypcadata, Splitted)

        Mypca <- FactoMineR::PCA(Mynewpcadata,
                                 ind.sup = c((length(Mydf[, 1]) + 1):(length(Mydf[, 1]) + length(Mytable[, 1]))),
                                 quanti.sup = c((length(Myvars) + 1):(length(Myvars) + length(Mysupvars))),
                                 ncp = ncol(length(Myvars)))


        Plot <- ggplot2::ggplot(data = Mypca.df, ggplot2::aes(x = Mypca.df[, tcltk2::selection(MYPCLIST1)], y = Mypca.df[, tcltk2::selection(MYPCLIST2)], group = Mydf[, which(colnames(Mydf) %in% Myfactor)])) +
          ggplot2::labs(x = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST1)], " (", round(Contrib[tcltk2::selection(MYPCLIST1)], 1), " %)", sep = ""),
                        y = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST2)], " (", round(Contrib[tcltk2::selection(MYPCLIST2)], 1), " %)", sep = "")) +
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
          ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor], fill = Mydf[, Myfactor]), size = 3) +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_shape_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.PCH) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

        make.plot.cmd(myplot = Plot)

      })



      PLOT.COR.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678,
                                        text = "Correlation\ncircle",
                                        tip = "Plot the correlation circle, with supplementary variables",
                                        command = function() {
        # ...Build correlation circle plot
        Plot2 <- factoextra::fviz_pca_var(Mypca, axes = c(tcltk2::selection(MYPCLIST1), tcltk2::selection(MYPCLIST2)), repel = TRUE, col.var="steelblue") +
          ggplot2::labs(x = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST1)], " (", round(Contrib[tcltk2::selection(MYPCLIST1)], 1), " %)", sep = ""),
                        y = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST2)], " (", round(Contrib[tcltk2::selection(MYPCLIST2)], 1), " %)", sep = "")) +
          ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                         axis.text.x = ggplot2::element_text(size = 10, angle = 00, hjust = 0.5, vjust = 0.5, face = "plain"),
                         axis.text.y = ggplot2::element_text(size = 10, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                         panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                         panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.ontop = FALSE,
                         subtitle = NULL,
                         axis.title.x = ggplot2::element_text(size = 12, angle = 00, face = "plain"),
                         axis.title.y = ggplot2::element_text(size = 12, angle = 90, face = "plain")) +
          ggplot2::labs(title = NULL)

        # ...Make plot window
        make.plot.cmd(myplot = Plot2)
      })


      PLOT.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678,
                                    text = "Plot",
                                    tip = "Plot the selected PCs against each other",
                                    command = function() {

        # ...Build plot
        Plot <- ggplot2::ggplot(data = Mypca.df, ggplot2::aes(x = Mypca.df[, tcltk2::selection(MYPCLIST1)], y = Mypca.df[, tcltk2::selection(MYPCLIST2)], group = Mydf[, which(colnames(Mydf) %in% Myfactor)])) +
          ggplot2::labs(x = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST1)], " (", round(Contrib[tcltk2::selection(MYPCLIST1)], 1), " %)", sep = ""),
                        y = paste(colnames(Mypca.df)[tcltk2::selection(MYPCLIST2)], " (", round(Contrib[tcltk2::selection(MYPCLIST2)], 1), " %)", sep = "")) +
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
          ggplot2::geom_point(ggplot2::aes(shape = Mydf[, Myfactor], color = Mydf[, Myfactor], fill = Mydf[, Myfactor]), size = 3) +
          ggplot2::scale_color_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_fill_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.COLORS) +
          ggplot2::scale_shape_manual(name = Myfactor, labels = levels(Mydf[, Myfactor]), values = PROJECT$OPTIONS$PLOT.PCH) +
          ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))

        # ...Make plot window
        make.plot.cmd(myplot = Plot2)
      })

      SAVE.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678, text = "Save", tip = "Save PCs data", command = function() {
        #utils::write.table(Mypca.df, append = FALSE, quote = FALSE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE,
        #                   file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = TRIDENT.PCSelect5678, title = "Save PCs as...", initialfile = paste("Untitled"), defaultextension = ".txt")))

        sink(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = TRIDENT.PCSelect5678, title = "Save PCs as...", initialfile = paste("Untitled"), defaultextension = ".txt")))
        cat("Eigenvalues\n")
        print(factoextra::get_eigenvalue(Mypca), quote = FALSE)
        cat("\n")
        cat("\n--------------------------------------------------\n")
        cat("\nIndividuals\n")
        cat("Coordinates for the individuals\n")
        print(factoextra::get_pca_ind(Mypca)$coord, quote = FALSE)
        cat("Cos2 for the individuals\n")
        print(factoextra::get_pca_ind(Mypca)$cos2, quote = FALSE)
        cat("\n")
        cat("Contributions of the individuals\n")
        print(factoextra::get_pca_ind(Mypca)$contrib, quote = FALSE)
        cat("\n")
        cat("\n--------------------------------------------------\n")
        cat("\nVariables\n")
        cat("Coordinates for the variables\n")
        print(factoextra::get_pca_var(Mypca)$coord, quote = FALSE)
        cat("\n")
        cat("Correlation between variables and dimensions\n")
        print(factoextra::get_pca_var(Mypca)$cor, quote = FALSE)
        cat("\n")
        cat("Cos2 for the variables\n")
        print(factoextra::get_pca_var(Mypca)$cos2, quote = FALSE)
        cat("\n")
        cat("Contributions of the variables\n")
        print(factoextra::get_pca_var(Mypca)$contrib, quote = FALSE)
        cat("\n")
        cat("\n--------------------------------------------------\n")
        cat("\nSupplementary variables\n")
        cat("Coordinates for the variables\n")
        print(Mypca$quanti.sup$coord, quote = FALSE)
        cat("Correlation between the variables and the dimensions\n")
        cat("\n")
        print(Mypca$quanti.sup$cor, quote = FALSE)
        cat("\n")
        cat("Cos2 for the variables\n")
        print(Mypca$quanti.sup$cos2, quote = FALSE)
        cat("\n")
        cat("\n--------------------------------------------------\n")
        cat("\nSupplementary individuals\n")
        cat("Coordinates for the individuals\n")
        print(Mypca$ind.sup$coord, quote = FALSE)
        cat("\n")
        cat("Cos2 for the individuals\n")
        print(Mypca$ind.sup$cos2, quote = FALSE)

        sink()
        })

      EXPORT.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678, text = "Export", tip = "Export PCA files in R", command = function() {
        # ......create window for PCA data name entry
        TRIDENT.NameEntry1234 <- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.NameEntry1234, paste("trident", METADATA$VERSION, "- Enter name..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.NameEntry1234, system.file("extdata","pics","trident.ico", package = "trident"))
        NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.NameEntry1234, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          My_data_from_trident <- Mypca
          assign(paste(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))), My_data_from_trident, envir = .GlobalEnv)
          tcltk::tkdestroy(TRIDENT.NameEntry1234)
        })

        CANCEL.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(TRIDENT.NameEntry1234))

        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NameEntry1234, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })

      DONE.BTN <- tcltk2::tk2button(TRIDENT.PCSelect5678, text = "Done!", tip = "Clicking this will close this window", command = function() tcltk::tkdestroy(TRIDENT.PCSelect5678))

      # Grid all
      tcltk::tkgrid(TKPLOT, columnspan = 2)
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.PCSelect5678, text = "Select PCs:"),
                    columnspan = 2)
      tcltk::tkgrid(MYPCLIST1, MYPCLIST2)
      tcltk::tkgrid(PLOT.BTN,
                    DONE.BTN,
                    sticky = "ew",
                    pady = 5)
      tcltk::tkgrid(PLOT.SUP.BTN,
                    PLOT.COR.BTN,
                    sticky = "nsew",
                    pady = 10)
      tcltk::tkgrid(SAVE.BTN,
                    EXPORT.BTN,
                    sticky = "ew",
                    pady = 5)
    })

    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))

    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose variables"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose supplementary\nvariables (optional)"),
                  tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"))
    tcltk::tkgrid(VARLIST, SUPVARLIST, FACTORLIST)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  }

  # ...undo.cmd----
  undo.cmd <- function() {

    refresh.cmd()
  }

  # ...redo.cmd----
  redo.cmd <- function() {

    refresh.cmd()
  }

  # TKGUI - MAIN WINDOW----
  TRIDENT.mainwin1234 <<- tcltk::tktoplevel()
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkconfigure(TRIDENT.mainwin1234, borderwidth = 10, bg = "tan")
  tcltk::tkwm.title(TRIDENT.mainwin1234, paste("trident", METADATA$VERSION, "-", METADATA$DESCRIPTION))
  tcltk2::tk2ico.setFromFile(TRIDENT.mainwin1234, system.file("extdata","pics","trident.ico", package = "trident"))
  tcltk::tkwm.geometry(TRIDENT.mainwin1234, paste(floor(SCREEN.RES$H * 0.8), "x", floor((SCREEN.RES$V * 0.8) - 90), "+0+0", sep = ""))

  # TKGUI - MENU----
  MENU <- tcltk2::tk2menu(TRIDENT.mainwin1234)
  tcltk::tkconfigure(TRIDENT.mainwin1234, menu = MENU)
  # ...Menu 'File'----
  MENU$FILE <- tcltk::tkmenu(MENU, tearoff = FALSE)
  # ...New project
  tcltk::tkadd(MENU$FILE, "command", label = "New project...      (Ctrl+N)", command = function() new.project.cmd())
  # ...Open project
  tcltk::tkadd(MENU$FILE, "command", label = "Open project...     (Ctrl+O)", command = function() open.project.cmd())
  # ...Save project
  MENU$FILE$SAVE <- tcltk::tkmenu(MENU$FILE, tearoff = FALSE)
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save...        (Ctrl+S)", command = function() save.cmd())
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save as...", command = function() save.as.cmd())
  tcltk::tkadd(MENU$FILE, "cascade", label = "Save project", menu = MENU$FILE$SAVE)
  # ...Exit
  tcltk::tkadd(MENU$FILE, "separator")
  tcltk::tkadd(MENU$FILE, "command", label = "Quit...      (Ctrl+Q)", command = function() quit.cmd())
  # ...Add File to menu
  tcltk::tkadd(MENU, "cascade", label = "File", menu = MENU$FILE)
  #
  # ...Shortcuts
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-n>", function() new.project.cmd())
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-o>", function() open.project.cmd())
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-s>", function() save.cmd())
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-q>", function() quit.cmd())

  # ...Menu 'Edit'----
  MENU$EDIT <- tcltk::tkmenu(MENU, tearoff = FALSE)
  # ...Undo
  tcltk::tkadd(MENU$EDIT, "command", label = "Undo...     (Ctrl+Z)", command = function(){undo.cmd()})
  # ...Redo
  tcltk::tkadd(MENU$EDIT, "command", label = "Redo...     (Ctrl+Y)", command = function(){redo.cmd()})
  # ...Add Edit to menu
  tcltk::tkadd(MENU, "cascade", label = "Edit", menu = MENU$EDIT)
  # ...Shortcuts
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-z>", function() undo.cmd())
  tcltk::tkbind(TRIDENT.mainwin1234,"<Control-y>", function() redo.cmd())
  # ...Menu 'About'----
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
  tcltk::tkadd(MENU$HELP, "command", label = "Options", command = function(){

  })
  # ...Help files
  tcltk::tkadd(MENU$HELP, "separator")
  tcltk::tkadd(MENU$HELP, "command", label = "Francisco et al. (2018) Surf Topogr Metrol Prop", command = function(){
    Mypath <- 'inst/extdata/about/Francisco2018a.pdf'
    system(paste0('open "', Mypath, '"'))
  })
  tcltk::tkadd(MENU$HELP, "command", label = "Francisco et al. (2018) Technologies", command = function(){
    Mypath <- 'inst/extdata/about/Francisco2018b.pdf'
    system(paste0('open "', Mypath, '"'))
  })
  #tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  #tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  #tcltk::tkadd(MENU$HELP, "command", label = "<ESPACE RESERVE>", command = function(){})
  # ...Add About to menu
  tcltk::tkadd(MENU, "cascade", label = "Help", menu = MENU$HELP)

  # TKGUI - NOTEBOOK----
  NOTEBOOK <- tcltk2::tk2notebook(TRIDENT.mainwin1234, height = 100, tabs = c("Microwear", "Data", "Variables", "Plots"))
  tcltk::tkpack(NOTEBOOK, side = "top", fill = "both" , expand = FALSE)
  # 1) Notetab 'Microwear'----
  NOTEBOOK$MICRO <- tcltk2::tk2notetab(NOTEBOOK, "Microwear")
  # ...Load button----
  LOAD.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, text = "Load\nsurface", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","load.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    # ...Scenario 1: there is already a loaded file
    if (!is.null(DMTA$SURF)) {
      DMTA$SURF <<- tcltk::tk_choose.files(filters = matrix(c("Fichier SUR", ".sur"), ncol = 2))
      DMTA$DATASET <<- NULL
      DMTA$VARIABLES <<- NULL
      DMTA$COMPUT$COMPLEX <<- NULL
      DMTA$COMPUT$ELEV <<- NULL
      DMTA$COMPUT$SPAT <<- NULL
      DMTA$COMPUT$TOPO <<- NULL
      DMTA$COMPUT.HTR$COMPLEX <<- NULL
      DMTA$COMPUT.HTR$ELEV <<- NULL
      DMTA$COMPUT.HTR$SPAT <<- NULL
    }
    # ...Scenario 2: there is no loaded file
    if (is.null(DMTA$SURF)) {
      DMTA$SURF <<- tcltk::tk_choose.files(filters = matrix(c("Fichier SUR", ".sur"), ncol = 2))
    }
    # ...re-able
    if (!is.null(DMTA$SURF)) {
      tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
      tcltk::tkconfigure(EXPORT.BTN, state = "normal")
      tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
      tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    }
    if (!is.null(DMTA$COMPUT.HTR)) {
      tcltk::tkconfigure(HISTO.BTN, state = "normal")
    }
  })

  # ...Export button----
  EXPORT.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, text = "Export\nto R", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","export.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    # First check if surface file exists
    if (is.null(DMTA$SURF) == TRUE)
    {
      tcltk::tkmessageBox(message = "No surface currently loaded", icon = "warning")
      stop()
    }
    if (is.null(DMTA$SURF) == FALSE) {
      if(file.exists(DMTA$SURF) == FALSE) {
        tcltk::tkmessageBox(message = "Surface file does not exist", icon = "warning")
        stop()
      }
    }
    # ......create window for name entry
    TRIDENT.NameEntry1234 <<- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.NameEntry1234, paste("trident", METADATA$VERSION, "- Enter name..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.NameEntry1234, system.file("extdata","pics","trident.ico", package = "trident"))
    NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.NameEntry1234, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
    CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Confirm", tip = "Confirm name and export to R", command = function() {
      My_surf_from_trident <- DMTA$SURF
      assign(paste(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))), My_surf_from_trident, envir = .GlobalEnv)
      tcltk::tkdestroy(TRIDENT.NameEntry1234)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(TRIDENT.NameEntry1234))
    # ......grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.NameEntry1234, text = "Name:"), NAME.ENTRY)
    tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
  })

  # ...Removal menubutton----
  CLEAN.MBTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, text = "Removal...", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","clean.gif", package = "trident")), height = 50, relief = "flat", compound = "top")
  REMOVAL.MENU <- tcltk::tkmenu(CLEAN.MBTN)
  tcltk::tkconfigure(CLEAN.MBTN, menu = REMOVAL.MENU)
  # ......Polynome 8----
  tcltk::tkadd(REMOVAL.MENU, "command", label = "8th order polynomial", command = function() {
    if (is.null(DMTA$SURF))
    {
      tcltk::tkmessageBox(message = "No surface currently loaded", icon = "warning")
      stop()
    }
    if (!is.null(DMTA$SURF)) {
      if(file.exists(DMTA$SURF) == FALSE) {
        tcltk::tkmessageBox(message = "Surface file does not exist", icon = "warning")
        stop()
      }
    }
    # disables
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # save the file and record path
    trident::polynom.sur(DMTA$SURF, deg = 8)
    DMTA$SURF <<- stringr::str_replace_all(file.path(paste0(unlist(strsplit(DMTA$SURF, ".sur")), "_X1_008_Y1_008.sur")), "\\\\", "/")
    # re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
  })
  # ......Polynome 2----
  tcltk::tkadd(REMOVAL.MENU, "command", label = "2nd order polynomial", command = function() {
    # First check if surface file exists
    if (is.null(DMTA$SURF) == TRUE)
    {
      tcltk::tkmessageBox(message = "No surface currently loaded", icon = "warning")
      stop()
    }
    if (is.null(DMTA$SURF) == FALSE) {
      if(file.exists(DMTA$SURF) == FALSE) {
        tcltk::tkmessageBox(message = "Surface file does not exist", icon = "warning")
        stop()
      }
    }
    # disables
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # save the file and record path
    trident::polynom.sur(DMTA$SURF, deg = 2)
    DMTA$SURF <<- stringr::str_replace_all(file.path(paste0(unlist(strsplit(DMTA$SURF, ".sur")), "_X1_002_Y1_002.sur")), "\\\\", "/")
    # re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
  })

  # ...Compute menubutton----
  COMPUTE.MBTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, text = "Compute (global)...", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","compute.gif", package = "trident")), height = 50, relief = "flat", compound = "top")
  COMPUTE.MENU <- tcltk::tkmenu(COMPUTE.MBTN)
  tcltk::tkconfigure(COMPUTE.MBTN, menu = COMPUTE.MENU)
  # ......Complexity----
  tcltk::tkadd(COMPUTE.MENU, "command", label = "Complexity", command = function() {
    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Complexity <- trident::dmta.asfc(sur = DMTA$SURF, type = "single")
    DMTA$COMPUT$COMPLEX <<- Complexity[3]
    Mytable <- data.frame(File = Complexity[1], Asfc = Complexity[3], stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", paste(unlist(Complexity[2]))))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })

  # ......Height----
  tcltk::tkadd(COMPUTE.MENU, "command", label = "Height parameters", command = function() {
    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Elev <- trident::dmta.height(sur = DMTA$SURF, type = "single")
    DMTA$COMPUT$ELEV <<- Elev[3:11]
    Mytable <- data.frame(File = Elev[1],
                          Sa = Elev[3],
                          Sp = Elev[4],
                          Sq = Elev[5],
                          Sv = Elev[6],
                          Ssk = Elev[7],
                          Sku = Elev[8],
                          Sdar = Elev[9],
                          Sm = Elev[10],
                          Smd = Elev[11], stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Elev[2])), 9)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })

  # ......Spatial----
  tcltk::tkadd(COMPUTE.MENU, "command", label = "Spatial parameters", command = function() {
    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Spat <- trident::dmta.spatial(sur = DMTA$SURF, type = "single")
    DMTA$COMPUT$SPAT <<- Spat[3:9]
    Mytable <- data.frame(File = Spat[1],
                          Rmax = Spat[3],
                          Sal = Spat[4],
                          Stri = Spat[5],
                          Std = Spat[6],
                          b.sl = Spat[7],
                          r.sl = Spat[8],
                          s.sl = Spat[9], stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Spat[2])), 7)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })

  # ......Topology----
  tcltk::tkadd(COMPUTE.MENU, "command", label = "Topology", command = function() {
    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Topo <- trident::dmta.topology(sur = DMTA$SURF, type = "single")
    DMTA$COMPUT$TOPO <<- Topo[3:9]
    Mytable <- data.frame(File = Topo[1],
                          Sk1 = Topo[3],
                          Sk2 = Topo[4],
                          Scm1 = Topo[5],
                          Scm2 = Topo[6],
                          Snb1 = Topo[7],
                          Snb2 = Topo[8],
                          Sh = Topo[9], stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Topo[2])), 7)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })

  # ......Compute all----
  tcltk::tkadd(COMPUTE.MENU, "separator")
  tcltk::tkadd(COMPUTE.MENU, "command", label = "Compute all", command = function() {
    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Complexity <- trident::dmta.asfc(sur = DMTA$SURF, type = "single")
    Elev <- trident::dmta.height(sur = DMTA$SURF, type = "single")
    Spat <- trident::dmta.spatial(sur = DMTA$SURF, type = "single")
    Topo <- trident::dmta.topology(sur = DMTA$SURF, type = "single")
    DMTA$COMPUT$COMPLEX <<- Complexity$Asfc
    DMTA$COMPUT$ELEV <<- Elev[3:11]
    DMTA$COMPUT$SPAT <<- Spat[3:9]
    DMTA$COMPUT$TOPO <<- Topo[3:9]
    Mytable <- data.frame(File = Complexity[1],
                          Asfc = Complexity[3],
                          Sa = Elev[3],
                          Sp = Elev[4],
                          Sq = Elev[5],
                          Sv = Elev[6],
                          Ssk = Elev[7],
                          Sku = Elev[8],
                          Sdar = Elev[9],
                          Sm = Elev[10],
                          Smd = Elev[11],
                          Rmax = Spat[3],
                          Sal = Spat[4],
                          Stri = Spat[5],
                          Std = Spat[6],
                          b.sl = Spat[7],
                          r.sl = Spat[8],
                          s.sl = Spat[9],
                          Sk1 = Topo[3],
                          Sk2 = Topo[4],
                          Scm1 = Topo[5],
                          Scm2 = Topo[6],
                          Snb1 = Topo[7],
                          Snb2 = Topo[8],
                          Sh = Topo[9], stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", paste(unlist(Complexity[2])), rep(paste(unlist(Elev[2])), 9), rep(paste(unlist(Spat[2])), 7), rep(paste(unlist(Topo[2])), 7)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })

  # ...Heterogeneity Menubutton----
  HETERO.MBTN <- tcltk::tkmenubutton(NOTEBOOK$MICRO, text = "Heterogeneity...", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","hetero.gif", package = "trident")), compound = "top", height = 50, relief = "flat")
  HETERO.MENU <- tcltk::tkmenu(HETERO.MBTN)
  tcltk::tkconfigure(HETERO.MBTN, menu = HETERO.MENU)

  # ......Complexity----
  tcltk::tkadd(HETERO.MENU, "command", label = "Complexity", command = function() {
    TRIDENT.size1234 <- tcltk::tktoplevel()
    SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
    OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
      Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
      Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
      Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
      tcltk::tkdestroy(TRIDENT.size1234)

      # ...disable
      tcltk::tkconfigure(EXPORT.BTN, state = "disable")
      tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
      tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
      tcltk::tkconfigure(HETERO.MBTN, state = "disable")
      tcltk::tkconfigure(HISTO.BTN, state = "disable")
      tcltk::tkconfigure(BATCH.BTN, state = "disable")
      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))
      # ...Computation
      Complexity <- trident::dmta.asfc(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
      DMTA$COMPUT.HTR$COMPLEX <<- data.frame(Complexity[3])
      Mytable <- data.frame(File = Complexity[1], c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$COMPLEX))), stringsAsFactors = TRUE)
      #Mytable$Hasfc <- abs(Mytable$Asfc.sd/Mytable$Asfc.median)
      #Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Complexity[2])), 15+1)))
      Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Complexity[2])), 15)))
      if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
      if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
      if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
      if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
      # destroy the waiting message
      tcltk::tkdestroy(TRIDENT.busy)
      # ...A window to display the results
      TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
      build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
      add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
      # ...re-able
      tcltk::tkconfigure(EXPORT.BTN, state = "normal")
      tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
      tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
      tcltk::tkconfigure(HETERO.MBTN, state = "normal")
      tcltk::tkconfigure(BATCH.BTN, state = "normal")
      if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
    })

    CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
  })

  # ......Height----
  tcltk::tkadd(HETERO.MENU, "command", label = "Height parameters", command = function() {
    TRIDENT.size1234 <- tcltk::tktoplevel()
    SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
    OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
      Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
      Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
      Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
      tcltk::tkdestroy(TRIDENT.size1234)

    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Elev <- trident::dmta.height(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
    DMTA$COMPUT.HTR$ELEV <<- data.frame(Elev[3:11])
    Mytable <- data.frame(File = Elev[1], c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$ELEV))), stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Elev[2])), 9*15)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })
    CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
  })

  # ......Spatial----
  tcltk::tkadd(HETERO.MENU, "command", label = "Spatial parameters", command = function() {
    TRIDENT.size1234 <- tcltk::tktoplevel()
    SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
    OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
      Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
      Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
      Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
      tcltk::tkdestroy(TRIDENT.size1234)

    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Spat <- trident::dmta.spatial(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
    DMTA$COMPUT.HTR$SPAT <<- data.frame(Spat[3:9])
    Mytable <- data.frame(File = Spat[1], c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$SPAT))), stringsAsFactors = TRUE)
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Spat[2])), 7*15)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })
    CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
  })


  # ......Topology----
  tcltk::tkadd(HETERO.MENU, "command", label = "Topology parameters", command = function() {
    TRIDENT.size1234 <- tcltk::tktoplevel()
    SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
    OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
      Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
      Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
      Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
      tcltk::tkdestroy(TRIDENT.size1234)

      # ...disable
      tcltk::tkconfigure(EXPORT.BTN, state = "disable")
      tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
      tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
      tcltk::tkconfigure(HETERO.MBTN, state = "disable")
      tcltk::tkconfigure(HISTO.BTN, state = "disable")
      tcltk::tkconfigure(BATCH.BTN, state = "disable")
      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))
      # ...Computation
      TOPO <- trident::dmta.spatial(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
      DMTA$COMPUT.HTR$SPAT <<- data.frame(Spat[3:9])
      Mytable <- data.frame(File = Spat[1], c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$SPAT))), stringsAsFactors = TRUE)
      Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Spat[2])), 7*15)))
      if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
      if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
      if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
      if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
      # destroy the waiting message
      tcltk::tkdestroy(TRIDENT.busy)
      # ...A window to display the results
      TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
      build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
      add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
      # ...re-able
      tcltk::tkconfigure(EXPORT.BTN, state = "normal")
      tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
      tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
      tcltk::tkconfigure(HETERO.MBTN, state = "normal")
      tcltk::tkconfigure(BATCH.BTN, state = "normal")
      if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
    })
    CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
  })


  # ......Compute all----
  tcltk::tkadd(HETERO.MENU, "separator")
  tcltk::tkadd(HETERO.MENU, "command", label = "Compute all", command = function() {
    TRIDENT.size1234 <- tcltk::tktoplevel()
    SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
    SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
    OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
      Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
      Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
      Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
      tcltk::tkdestroy(TRIDENT.size1234)

    # ...disable
    tcltk::tkconfigure(EXPORT.BTN, state = "disable")
    tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
    tcltk::tkconfigure(HETERO.MBTN, state = "disable")
    tcltk::tkconfigure(HISTO.BTN, state = "disable")
    tcltk::tkconfigure(BATCH.BTN, state = "disable")
    # Pop a waiting message
    TRIDENT.busy <- tcltk::tktoplevel()
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
    tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.busy, paste(""))
    # ...Computation
    Complexity <- trident::dmta.asfc(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
    Elev <- trident::dmta.height(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
    Spat <- trident::dmta.spatial(sur = DMTA$SURF, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
    DMTA$COMPUT.HTR$COMPLEX <<- data.frame(Complexity[3])
    DMTA$COMPUT.HTR$ELEV <<- data.frame(Elev[3:11])
    DMTA$COMPUT.HTR$SPAT <<- data.frame(Spat[3:9])
    Mytable <- data.frame(File = Complexity[1], c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$COMPLEX))), stringsAsFactors = TRUE)
    Mytable$Hasfc <- abs(Mytable$Asfc.sd/Mytable$Asfc.median)
    Mytable <- dplyr::bind_cols(Mytable, data.frame(c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$ELEV)))))
    Mytable <- dplyr::bind_cols(Mytable, data.frame(c(plyr::colwise(trident::trident.hetero)(data.frame(DMTA$COMPUT.HTR$SPAT)))))
    Myvartable <- data.frame(Variables = colnames(Mytable), Type = c("integer", rep(paste(unlist(Complexity[2])), 15+1), rep(paste(unlist(Elev[2])), 9*15), rep(paste(unlist(Spat[2])), 7*15)))
    if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
    if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
    if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
    if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
    # destroy the waiting message
    tcltk::tkdestroy(TRIDENT.busy)
    # ...A window to display the results
    TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
    build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234, bg.table = "honeydew2", bg.title = "darkseagreen", height = 1)
    add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    # ...re-able
    tcltk::tkconfigure(EXPORT.BTN, state = "normal")
    tcltk::tkconfigure(CLEAN.MBTN, state = "normal")
    tcltk::tkconfigure(COMPUTE.MBTN, state = "normal")
    tcltk::tkconfigure(HETERO.MBTN, state = "normal")
    tcltk::tkconfigure(BATCH.BTN, state = "normal")
    if (!is.null(DMTA$COMPUT.HTR)) tcltk::tkconfigure(HISTO.BTN, state = "normal")
  })
    CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
    tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
    tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
  })

  # ...Histogram button----
  HISTO.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, text = "Histograms", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","histo.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    # ...a first window to select x
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variable..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- dplyr::bind_cols(DMTA$COMPUT.HTR$COMPLEX, DMTA$COMPUT.HTR$ELEV, DMTA$COMPUT.HTR$SPAT)
    Mycolnames <- colnames(Mydf)
    Mydf <- data.frame(Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ])
    Mydf <- stats::na.omit(Mydf)
    Myx <- NULL
    XLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = Mycolnames, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    DENSITY.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Density?")
    tcltk::tkconfigure(DENSITY.CHKBTN, variable = PROJECT$OPTIONS$DENSITY.VALUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      Myx <- tcltk2::selection(XLIST)
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      # ...Graph window
      TRIDENT.PlotWin1234 <<- tcltk::tktoplevel()
      tcltk::tkconfigure(TRIDENT.PlotWin1234, borderwidth = 10, bg = "tan")
      tcltk::tkwm.title(TRIDENT.PlotWin1234, paste("trident", METADATA$VERSION, "- histogram"))
      tcltk2::tk2ico.setFromFile(TRIDENT.PlotWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      TRIDENT.PlotWin1234$PLOT <<- tcltk::tkframe(TRIDENT.PlotWin1234)
      TRIDENT.PlotWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.PlotWin1234)
      TKPLOT <- NULL
      Plot <- ggplot2::ggplot(data = Mydf, ggplot2::aes(x = Mydf[, Myx])) +
        ggplot2::labs(x = Mycolnames[Myx]) +
        ggplot2::guides(size = FALSE) +
        ggplot2::theme(text = ggplot2::element_text(family = "serif"), legend.text = ggplot2::element_text(colour = "black", size = 10, face = "bold"),
                       legend.position = "right", legend.title = ggplot2::element_text(size = 12),
                       axis.text.y = ggplot2::element_text(size = 9, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
                       panel.background = ggplot2::element_rect(fill = "#ffffff", colour = "#000000", linetype = "solid"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.ontop = FALSE,
                       axis.title.x = ggplot2::element_text(size = 10, angle = 00, face = "italic"),
                       axis.title.y = ggplot2::element_text(size = 10, angle = 90, face = "italic")) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 2)))
      if (tcltk::tclvalue(PROJECT$OPTIONS$DENSITY.VALUE) == 0) {
        Plot <- Plot + ggplot2::geom_histogram(bins = 20, color = "black", fill = "antiquewhite")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DENSITY.VALUE) == 1) {
        Plot <- Plot + ggplot2::geom_density(fill = "lightblue", size = 1, linetype = "solid") +
          ggplot2::geom_density(color = "steelblue", size = 1.2, linetype = "solid") +
          ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 20, color = "black", fill = "antiquewhite")

      }
      TKPLOT <- tkrplot::tkrplot(TRIDENT.PlotWin1234$PLOT, fun = function() graphics::plot(Plot))
      SAVE.BTN <- tcltk2::tk2button(TRIDENT.PlotWin1234$BUTTONS, text = "SAVE", command = function() {
        # ...a window to select saving parameters, such as resolution, size, etc.
        TRIDENT.SaveWin1234 <-  tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.SaveWin1234, paste("trident", METADATA$VERSION, "- Image size..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.SaveWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
        DPI.SPNBX <- tcltk2::tk2spinbox(TRIDENT.SaveWin1234, from = 100, to = 1000, increment = 10)
        HEIGHT.NTRY <- tcltk2::tk2entry(TRIDENT.SaveWin1234, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.HEIGHT)))
        WIDTH.NTRY <- tcltk2::tk2entry(TRIDENT.SaveWin1234, textvariable = tcltk::tclVar(paste(PROJECT$OPTIONS$PLOT.WIDTH)))
        UNITS.CBBX <- tcltk2::tk2combobox(TRIDENT.SaveWin1234, values = c("mm", "cm", "in"))
        OK.BTN <- tcltk2::tk2button(TRIDENT.SaveWin1234, text = "Ok", tip = "", command = function() {
          PROJECT$OPTIONS$PLOT.DPI <<- as.numeric(tcltk::tclvalue(tcltk::tkget(DPI.SPNBX)))
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))) == FALSE) {
            PROJECT$OPTIONS$PLOT.HEIGHT <<- as.numeric(tcltk::tclvalue(tcltk::tkget(HEIGHT.NTRY)))}
          if (is.na(as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))) == FALSE){
            PROJECT$OPTIONS$PLOT.WIDTH <<- as.numeric(tcltk::tclvalue(tcltk::tkget(WIDTH.NTRY)))}
          PROJECT$OPTIONS$PLOT.UNITS <<- tcltk::tclvalue(tcltk::tkget(UNITS.CBBX))
          tcltk::tkdestroy(TRIDENT.SaveWin1234)
          # ......now open tkgetsavefile window
          ggplot2::ggsave(Plot,
                          file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = TRIDENT.mainwin1234,
                                                                      title = "Save plot as...",
                                                                      initialfile = paste(Mycolnames[Myx], "histogram", sep = " "),
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
        CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SaveWin1234, text = "Cancel", tip = "", command = function() tcltk::tkdestroy(TRIDENT.SaveWin1234))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Height:"), HEIGHT.NTRY, tcltk::tklabel(TRIDENT.SaveWin1234, text = "Width:"), WIDTH.NTRY)
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Units:"), UNITS.CBBX, columnspan = 2)
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.SaveWin1234, text = "Resolution (dpi):"), DPI.SPNBX, columnspan = 2)
        tcltk::tkgrid(OK.BTN, CANCEL.BTN, columnspan = 2)
        tcltk::tkset(DPI.SPNBX, PROJECT$OPTIONS$PLOT.DPI)
        tcltk::tkset(UNITS.CBBX, PROJECT$OPTIONS$PLOT.UNITS)
      })
      EXPORT.BTN <- tcltk2::tk2button(TRIDENT.PlotWin1234$BUTTONS, text = "Export", tip = "Export data.frame object to R", command = function() {
        # ......create window for name entry
        TRIDENT.NameEntry1234 <- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.NameEntry1234, paste("trident", METADATA$VERSION, "- Enter name..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.NameEntry1234, system.file("extdata","pics","trident.ico", package = "trident"))
        NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.NameEntry1234, tip = "Enter the object's name in R", textvariable = PROJECT$NAMES$EXPORT)
        CONFIRM.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Confirm", tip = "Confirm name and export to R", command = function() {
          My_graph_from_trident <- Plot
          assign(paste(tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))), My_graph_from_trident, envir = .GlobalEnv)
          tcltk::tkdestroy(TRIDENT.NameEntry1234)
        })
        CANCEL.BTN <- tcltk2::tk2button(TRIDENT.NameEntry1234, text = "Cancel", tip = "Cancel exportation", command = function() tcltk::tkdestroy(TRIDENT.NameEntry1234))
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.NameEntry1234, text = "Name:"), NAME.ENTRY)
        tcltk::tkgrid(CONFIRM.BTN, CANCEL.BTN)
      })
      # Grid all
      tcltk::tkpack(TRIDENT.PlotWin1234$PLOT, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkpack(TRIDENT.PlotWin1234$BUTTONS, side = "top", fill = "both" , expand = TRUE)
      tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
      tcltk::tkgrid(TKPLOT)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...Grid all in first window
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose x-axis variable"), columnspan = 2)
    tcltk::tkgrid(XLIST, columnspan = 2)
    tcltk::tkgrid(DENSITY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
  })

  # ...Map surface button----
  MAPSUR.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, text = "3d map", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","sur.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    if (is.null(DMTA$SURF)) {
      tcltk::tkmessageBox(message = "No surface loaded", icon = "warning", type = "ok")
      stop()
    }
    Mydf <- trident::sur.read(DMTA$SURF, type = "xyz")
    # ...a first window to select options
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Map options..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))

    COL.CBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = c("Grayscale", "Temperature", "Colorblind"), textvariable = DMTA$COLORS <<- tcltk::tclVar("Grayscale"))
    COL.LEVELS.SPNBX <- tcltk2::tk2spinbox(TRIDENT.VarSelect1234, textvariable = DMTA$LEVELS <<- tcltk::tclVar(10), from = 2, to = 100, increment = 1)
    MAP.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Display map", command = function() {

      col.levels <- as.numeric(tcltk::tclvalue(tcltk::tkget(COL.LEVELS.SPNBX)))
      if (tcltk::tclvalue(tcltk::tkget(COL.CBBX)) == "Grayscale") col.range <- c("black","white")
      if (tcltk::tclvalue(tcltk::tkget(COL.CBBX)) == "Temperature") col.range <- c("royalblue4","royalblue","lightseagreen","olivedrab3","yellow1","orange","orangered","red","firebrick4")
      if (tcltk::tclvalue(tcltk::tkget(COL.CBBX)) == "Colorblind") col.range <- c("lightgreen","goldenrod1","yellow1","white","lightskyblue","dodgerblue4","royalblue")
      col.palette <- grDevices::colorRampPalette(col.range)(col.levels)

      z.levels <- seq(min(Mydf$z), max(Mydf$z), length.out = col.levels + 1)
      col.tmp <- c(1:length(Mydf$z))
      for (i in c(1:col.levels)){
        col.tmp[Mydf$z >= z.levels[i]] <- col.palette[i]
      }
      rgl::open3d()
      rgl::par3d(windowRect = c(20, 30, 800, 800))
      rgl::plot3d(x = Mydf$x, y = Mydf$y, z = Mydf$z, col = col.tmp,
                  xlab = "X", ylab = "Y", zlab = "Z",
                  aspect = c(1, 1, 0.25), box = FALSE, axes = TRUE,
                  xlim = c(0, max(Mydf$x)), ylim = c(0, max(Mydf$y)))
      # Set the parallax to 0:
      rgl::par3d(FOV = 0)
    })
    SAVE.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Save", command = function() {
      rgl::snapshot3d(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = TRIDENT.VarSelect1234, title = "Save map as...", initialfile = "untitled", defaultextension = ".png")), fmt = "png")
    })
    DONE.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Done!", command = function() {
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose a color hue:"), columnspan = 2, padx = 5, pady = 5)
    tcltk::tkgrid(COL.CBBX, columnspan = 2, padx = 5, pady = 0)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Color levels:"), COL.LEVELS.SPNBX, padx = 5, pady = 5)
    tcltk::tkgrid(MAP.BTN, sticky = "ew", columnspan = 2, padx = 5, pady = 0)
    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.VarSelect1234, orientation = "horizontal"), sticky = "ew", columnspan = 2, padx = 5, pady = 10)
    tcltk::tkgrid(SAVE.BTN, DONE.BTN, padx = 5, pady = 10, sticky = "e")
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ...Batch analysis button----
  BATCH.BTN <- tcltk::tkbutton(NOTEBOOK$MICRO, text = "Batch analysis", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","batch.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    #...A first window for selecting options
    TRIDENT.BatchDmta1234 <<- tcltk::tktoplevel()
    # Radiobuttons
    NONE.RBTN <- tcltk::tkradiobutton(TRIDENT.BatchDmta1234, text = "None")
    POLY2.RBTN <- tcltk::tkradiobutton(TRIDENT.BatchDmta1234, text = "2nd order polynomial")
    POLY8.RBTN <- tcltk::tkradiobutton(TRIDENT.BatchDmta1234, text = "8th order polynomial")
    tcltk::tkconfigure(NONE.RBTN, variable = PROJECT$OPTIONS$BATCH.REMOVAL.VALUE, value = "None")
    tcltk::tkconfigure(POLY2.RBTN, variable = PROJECT$OPTIONS$BATCH.REMOVAL.VALUE, value = "2nd order polynomial")
    tcltk::tkconfigure(POLY8.RBTN, variable = PROJECT$OPTIONS$BATCH.REMOVAL.VALUE, value = "8th order polynomial")
    # Checkbuttons
    COMPLEX.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Complexity")
    ELEV.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Height parameters")
    SPAT.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Spatial parameters")
    TOPO.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Topology")
    COMPLEX.H.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Complexity's heterogeneity")
    ELEV.H.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Height's heterogeneity")
    SPAT.H.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Spatial heterogeneity")
    STD.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.BatchDmta1234, text = "Compute Std? *")
    tcltk::tkconfigure(COMPLEX.CHKBTN, variable = PROJECT$OPTIONS$BATCH.COMPLEX.VALUE)
    tcltk::tkconfigure(ELEV.CHKBTN, variable = PROJECT$OPTIONS$BATCH.ELEV.VALUE)
    tcltk::tkconfigure(SPAT.CHKBTN, variable = PROJECT$OPTIONS$BATCH.SPAT.VALUE)
    tcltk::tkconfigure(TOPO.CHKBTN, variable = PROJECT$OPTIONS$BATCH.TOPO.VALUE)
    tcltk::tkconfigure(COMPLEX.H.CHKBTN, variable = PROJECT$OPTIONS$BATCH.COMPLEX.H.VALUE)
    tcltk::tkconfigure(ELEV.H.CHKBTN, variable = PROJECT$OPTIONS$BATCH.ELEV.H.VALUE)
    tcltk::tkconfigure(SPAT.H.CHKBTN, variable = PROJECT$OPTIONS$BATCH.SPAT.H.VALUE)
    tcltk::tkconfigure(STD.CHKBTN, variable = PROJECT$OPTIONS$STD.VALUE)
    # Buttons
    SELECT.BTN <- tcltk2::tk2button(TRIDENT.BatchDmta1234, text = "Select files", command = function() {
      DMTA$SURF <<- tcltk::tk_choose.files(filters = matrix(c("Fichier SUR", ".sur"), ncol = 2), multi = TRUE)
    })
    OK.BTN <- tcltk2::tk2button(TRIDENT.BatchDmta1234, text = "OK", command = function() {
      # A window to change the size parameters
      TRIDENT.size1234 <- tcltk::tktoplevel()
      SIZEX.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$X, width = 20, from = 100, to = 256, increment = 1, tip = "")
      SIZEY.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$Y, width = 20, from = 100, to = 256, increment = 1, tip = "")
      SIZEN.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.size1234, textvariable = DMTA$SIZE$N, width = 20, from = 100, to = 256, increment = 1, tip = "")
      OK.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Ok", tip = "", command = function () {
        DMTA$SIZE$X <<- tcltk::tkget(SIZEX.SPINBOX)
        DMTA$SIZE$Y <<- tcltk::tkget(SIZEY.SPINBOX)
        DMTA$SIZE$N <<- tcltk::tkget(SIZEN.SPINBOX)
        DMTA$DATASET <<- NULL
        DMTA$VARIABLES <<- NULL
        DMTA$COMPUT$COMPLEX <<- NULL
        DMTA$COMPUT$ELEV <<- NULL
        DMTA$COMPUT$SPAT <<- NULL
        DMTA$COMPUT$TOPO <<- NULL
        DMTA$COMPUT.HTR$COMPLEX <<- NULL
        DMTA$COMPUT.HTR$ELEV <<- NULL
        DMTA$COMPUT.HTR$SPAT <<- NULL

        # ...A function for batch computation
        Batch.fun <- function(sur, tasklist, removal, do.std) {
          # Removal of polynoms:
          if (removal == "None") {
            sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), ".sur")), "\\\\", "/")
          }
          if (removal == "2nd order polynomial") {
            trident::polynom.sur(sur, deg = 2)
            sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), "_X1_002_Y1_002.sur")), "\\\\", "/")
          }
          if (removal == "8th order polynomial") {
            trident::polynom.sur(sur, deg = 8)
            sur <- stringr::str_replace_all(file.path(paste0(unlist(strsplit(sur, ".sur")), "_X1_008_Y1_008.sur")), "\\\\", "/")
          }
          # Data frame with surface file as first column:
          Mytable <- data.frame(File = paste0(utils::tail(unlist(strsplit(sur[1:which(stringr::str_detect(sur, ".sur"))], "/")), which(stringr::str_detect(sur, ".sur")), collapse = " ")), stringsAsFactors = TRUE)
          # DMTA Computation:
          if (tasklist[1] == "1") {
            Complexity <- trident::dmta.asfc(sur = sur, type = "single", size.x = Size.x, size.y = Size.y)
            Mytable$Asfc <- Complexity[3]
          }
          if (tasklist[2] == "1") {
            Elev <- trident::dmta.height(sur = sur, type = "single", size.x = Size.x, size.y = Size.y)
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
          if (tasklist[3] == "1") {
            Spat <- trident::dmta.spatial(sur = sur, type = "single", size.x = Size.x, size.y = Size.y)
            Mytable$Rmax <- Spat[3]
            Mytable$Sal <- Spat[4]
            Mytable$Stri <- Spat[5]
            if (do.std) Mytable$Std <- Spat[6]
            Mytable$b.sl <- Spat[7]
            Mytable$r.sl <- Spat[8]
            Mytable$s.sl <- Spat[9]
          }
          if (tasklist[4] == "1") {
            Topo <- trident::dmta.topology(sur = sur, type = "single", size.x = Size.x, size.y = Size.y)
            Mytable$Sk1 <- Topo[3]
            Mytable$Sk2 <- Topo[4]
            Mytable$Scm1 <- Topo[5]
            Mytable$Scm2 <- Topo[6]
            Mytable$Snb1 <- Topo[7]
            Mytable$Snb2 <- Topo[8]
            Mytable$Sh <- Topo[9]
          }
          if (tasklist[5] == "1") {
            HComplexity <- trident::dmta.asfc(sur = sur, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
            Mytable <- data.frame(Mytable, c(plyr::colwise(trident::trident.hetero)(data.frame(HComplexity[3]))))
          }
          if (tasklist[6] == "1") {
            HElev <- trident::dmta.height(sur = sur, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
            Mytable <- data.frame(Mytable, c(plyr::colwise(trident::trident.hetero)(data.frame(HElev[3:11]))))
          }
          if (tasklist[7] == "1") {
            HSpat <- trident::dmta.spatial(sur = sur, type = "multi", size.x = Size.x, size.y = Size.y, size.n = Size.n)
            if (do.std) Mytable <- data.frame(Mytable, c(plyr::colwise(trident::trident.hetero)(data.frame(HSpat[3:9]))))
            if (!do.std) Mytable <- data.frame(Mytable, c(plyr::colwise(trident::trident.hetero)(data.frame(HSpat[c(3,4,5,7,8,9)]))))
          }
          return(Mytable)
        }

        # Prepare data
        Size.x <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEX.SPINBOX)))
        Size.y <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEY.SPINBOX)))
        Size.n <- as.numeric(tcltk::tclvalue(tcltk::tkget(SIZEN.SPINBOX)))
        Mysurtest <- DMTA$SURF
        Myremoval <- tcltk::tclvalue(PROJECT$OPTIONS$BATCH.REMOVAL.VALUE)
        Mytasklist <- c(tcltk::tclvalue(PROJECT$OPTIONS$BATCH.COMPLEX.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.ELEV.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.SPAT.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.TOPO.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.COMPLEX.H.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.ELEV.H.VALUE),
                        tcltk::tclvalue(PROJECT$OPTIONS$BATCH.SPAT.H.VALUE))
        if (tcltk::tclvalue(PROJECT$OPTIONS$STD.VALUE) == "1") Mydo.std <- TRUE
        if (tcltk::tclvalue(PROJECT$OPTIONS$STD.VALUE) == "0") Mydo.std <- FALSE
        tcltk::tkdestroy(TRIDENT.size1234)
        tcltk::tkdestroy(TRIDENT.BatchDmta1234)

        # PROGRESSBAR
        Mypb <- utils::winProgressBar(title = "Preparation - this might take some time", min = 1, max = length(Mysurtest), width = 300)
        progress <- function(n) setWinProgressBar(Mypb, n, title = paste(round(n/length(Mysurtest)*100, 0), "% done"))
        opts <- list(progress = progress)

        # Parallel loop
        doSNOW::registerDoSNOW(snow::makeSOCKcluster(parallel::detectCores() - 1))
        Mytable <- foreach::foreach(i = 1:length(Mysurtest), .options.snow = opts, .combine = "rbind") %dopar% Batch.fun(sur = Mysurtest[i], tasklist = Mytasklist, removal = Myremoval, do.std = Mydo.std)
        # here it is necessary to manually unlist the numerical variables
        Mytable <- data.frame(File = Mytable[, 1], apply(Mytable[, -1], 2, unlist))

        # Build DMTA$DATASET
        if (!is.null(DMTA$DATASET)) DMTA$DATASET <<- merge(DMTA$DATASET, Mytable, by = intersect(names(DMTA$DATASET), names(Mytable)), all = TRUE, sort = FALSE, no.dups = TRUE)
        if (is.null(DMTA$DATASET)) DMTA$DATASET <<- Mytable
        # Build DMTA$VARIABLES
        Myvartable <- build.vartable.cmd(Mytable)
        if (!is.null(DMTA$VARIABLES))  DMTA$VARIABLES <<- dplyr::union(DMTA$VARIABLES, Myvartable)
        if (is.null(DMTA$VARIABLES)) DMTA$VARIABLES <<- Myvartable
        # Close progress bar
        close(Mypb)

        # ...A fourth windox to display the results
        TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- DMTA"))
        build.table.cmd(DMTA$DATASET, TRIDENT.TableWin1234)
        add.to.dataset.cmd(widget = TRIDENT.TableWin1234, data = DMTA$DATASET, variable = DMTA$VARIABLES)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
      })
      CANCEL.BTN2 <- tcltk2::tk2button(TRIDENT.size1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.size1234))
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the dimension of the surface?"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "X-axis (px)"), SIZEX.SPINBOX, padx = 50, pady = 20, sticky = "e")
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Y-axis (px)"), SIZEY.SPINBOX, padx = 50, pady = 20, sticky = "e")
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Change the number of grid cells for heterogeneity?\n(please choose a square number)"), columnspan = 2, padx = 50, pady = 20, sticky = "ew")
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.size1234, text = "Total number of cells"), SIZEN.SPINBOX, padx = 50, pady = 20, sticky = "e")
      tcltk::tkgrid(OK.BTN2, CANCEL.BTN2, padx = 50, pady = 20, sticky = "ew")
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.BatchDmta1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.BatchDmta1234))

    # ...Grid all in batch window
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.BatchDmta1234, text = "Please select surface files:"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(SELECT.BTN, sticky = "ew", columnspan = 2)

    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.BatchDmta1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.BatchDmta1234, text = "Removal:"), sticky = "w", columnspan = 2)
    tcltk::tkgrid(NONE.RBTN, sticky = "w")
    tcltk::tkgrid(POLY2.RBTN, sticky = "w")
    tcltk::tkgrid(POLY8.RBTN, sticky = "w")

    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.BatchDmta1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.BatchDmta1234, text = "Parameters:"), sticky = "w", columnspan = 2)
    tcltk::tkgrid(COMPLEX.CHKBTN, COMPLEX.H.CHKBTN, sticky = "w")
    tcltk::tkgrid(ELEV.CHKBTN, ELEV.H.CHKBTN, sticky = "w")
    tcltk::tkgrid(SPAT.CHKBTN, SPAT.H.CHKBTN, sticky = "w")
    tcltk::tkgrid(TOPO.CHKBTN, STD.CHKBTN, sticky = "w")
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.BatchDmta1234, text = "* When computing Std, make sure that surfaces have been\nscanned using a standardized direction"), sticky = "w", columnspan = 2)

    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.BatchDmta1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN, sticky = "ew")

    tcltk::tkwm.title(TRIDENT.BatchDmta1234, paste("trident", METADATA$VERSION, "- DMTA"))
    tcltk::tkconfigure(TRIDENT.BatchDmta1234, borderwidth = 10)
    tcltk2::tk2ico.setFromFile(TRIDENT.BatchDmta1234, system.file("extdata","pics","trident.ico", package = "trident"))
    tcltk::tcl("wm", "attributes", TRIDENT.BatchDmta1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.BatchDmta1234, topmost = FALSE)

  })

  # ...Grid all 'Microwear'----
  tcltk::tkgrid(LOAD.BTN, BATCH.BTN, EXPORT.BTN, CLEAN.MBTN, COMPUTE.MBTN, HETERO.MBTN, HISTO.BTN, MAPSUR.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips 'Microwear'----
  tcltk2::tk2tip(LOAD.BTN, "Load SUR file")
  tcltk2::tk2tip(EXPORT.BTN, "Export path of SUR file to R")
  tcltk2::tk2tip(CLEAN.MBTN, "Substract polynoms to the surface")
  tcltk2::tk2tip(COMPUTE.MBTN, "Compute DMTA parameters for the whole surface")
  tcltk2::tk2tip(HETERO.MBTN, "Compute spatial variation of DMTA parameters\nbetween subsamples (16*16)")
  tcltk2::tk2tip(HISTO.BTN, "Visualize spatial variation of DMTA parameters\nbetween subsamples (16*16)")
  tcltk2::tk2tip(MAPSUR.BTN, "Visualize 3d map of the surface")
  tcltk2::tk2tip(BATCH.BTN, "Choose DMTA parameters and compute them for\na selection of SUR files")
  # 2) Notetab 'Data'----
  NOTEBOOK$DATA <- tcltk2::tk2notetab(NOTEBOOK, "Data")
  # ...Open button----
  OPEN.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, text = "Open", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function() {
    # ...Open the file, read it and add it to the project's files stack
    # REMARQUE: Utiliser les fonctions du package 'readr' à la place de 'read.table' ?
    File.tmp <- tcltk::tk_choose.files(filters = matrix(c("All files", "Text", "Calc", "Excel", "*", ".txt", ".csv", ".xls"), ncol = 2))
    #File.tmp <- utils::choose.files()
    n <- length(PROJECT$FILES) + 1
    if (tools::file_ext(File.tmp) == "txt") {
      Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = "", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
      PROJECT$FILES[[n]] <<- Mytable
      }
    if (tools::file_ext(File.tmp) == "csv") {
      Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
      PROJECT$FILES[[n]] <<- Mytable
    }
    if (tools::file_ext(File.tmp) == "xls") {
      Mytable <- data.frame(utils::read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL, as.is = FALSE), stringsAsFactors = TRUE)
      PROJECT$FILES[[n]] <<- Mytable
    }
    names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1])
    if (anyDuplicated(names(PROJECT$FILES)) != 0) names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1], "(", n,")")
    # ...1st case: a dataset is currently opened
    if (is.null(PROJECT$DATASET) == FALSE) {
      # ...A window to choose whether the current dataset must be replaced
      TRIDENT.OverCurrentDataset1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.OverCurrentDataset1234, paste("trident", METADATA$VERSION, "- Overwrite dataset..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.OverCurrentDataset1234, system.file("extdata","pics","trident.ico", package = "trident"))
      YES.BTN <- tcltk2::tk2button(TRIDENT.OverCurrentDataset1234, text = "Yes", tip = "", command = function() {
        tcltk::tkdestroy(TRIDENT.OverCurrentDataset1234)
        PROJECT$DATASET <<- data.frame(PROJECT$FILES[[n]], stringsAsFactors = TRUE)
        PROJECT$VARIABLES <<- data.frame(Variables = colnames(PROJECT$DATASET), Type = sapply(PROJECT$DATASET,typeof))
        refresh.cmd()
        # ...re-able
        if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
        tcltk::tkconfigure(BUILD.BTN, state = "normal")
        tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
        tcltk::tkconfigure(REFRESH.BTN, state = "normal")
        tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
        if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) >= 1) {
          tcltk::tkconfigure(TRANS.MBTN, state = "normal")
          tcltk::tkconfigure(SUM.BTN, state = "normal")
          tcltk::tkconfigure(MULTI.BTN, state = "normal")
          tcltk::tkconfigure(DISC.BTN, state = "normal")
          tcltk::tkconfigure(NONDISC.BTN, state = "normal")
          tcltk::tkconfigure(TOP3.BTN, state = "normal")
          tcltk::tkconfigure(RANK.MBTN, state = "normal")
          tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
          tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
          tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
          tcltk::tkconfigure(PCA.BTN, state = "normal")
        }
      })
      NO.BTN <- tcltk2::tk2button(TRIDENT.OverCurrentDataset1234, text = "No", tip = "", command = function() tcltk::tkdestroy(TRIDENT.OverCurrentDataset1234))
      # ...grid all
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.OverCurrentDataset1234, text = "Do you want to replace current dataset?"))
      tcltk::tkgrid(YES.BTN, NO.BTN, padx = 5, pady = 5)
    }
    # ...2nd case: there is no dataset currently opened
    if (is.null(PROJECT$DATASET) == TRUE) {
      PROJECT$DATASET <<- data.frame(PROJECT$FILES[[n]], stringsAsFactors = TRUE)
      PROJECT$VARIABLES <<- data.frame(Variables = colnames(PROJECT$DATASET), Type = sapply(PROJECT$DATASET,typeof))
      refresh.cmd()
      # ...re-able
      if (length(PROJECT$FILES) > 1) tcltk::tkconfigure(COMBINE.BTN, state = "normal")
      tcltk::tkconfigure(BUILD.BTN, state = "normal")
      tcltk::tkconfigure(CORTEST.MBTN, state = "normal")
      tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
      tcltk::tkconfigure(REFRESH.BTN, state = "normal")
      if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) >= 1) {
        tcltk::tkconfigure(SUM.BTN, state = "normal")
        tcltk::tkconfigure(TRANS.MBTN, state = "normal")
        tcltk::tkconfigure(MULTI.BTN, state = "normal")
        tcltk::tkconfigure(DISC.BTN, state = "normal")
        tcltk::tkconfigure(NONDISC.BTN, state = "normal")
        tcltk::tkconfigure(TOP3.BTN, state = "normal")
        tcltk::tkconfigure(RANK.MBTN, state = "normal")
        tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
        tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
        tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
        tcltk::tkconfigure(PCA.BTN, state = "normal")
      }
    }
  })
  # ...Build button----
  BUILD.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, text = "Build", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","build.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function() {
    # ...a window to select variables used to build the new dataset
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Combined.data <- PROJECT$DATASET
    VARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Combined.data), selectmode = "extended", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    BUILD.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Combine!", command = function() {
      Myselect <- tcltk2::selection(VARLIST)
      PROJECT$DATASET <<- Combined.data[, Myselect]
      PROJECT$VARIABLES <<- data.frame(Variables = colnames(PROJECT$DATASET), Type = sapply(PROJECT$DATASET,typeof))
      refresh.cmd()
      # ...re-able
      if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
        tcltk::tkconfigure(TRANS.MBTN, state = "normal")
        tcltk::tkconfigure(SUM.BTN, state = "normal")
        tcltk::tkconfigure(MULTI.BTN, state = "normal")
        tcltk::tkconfigure(DISC.BTN, state = "normal")
        tcltk::tkconfigure(NONDISC.BTN, state = "normal")
        tcltk::tkconfigure(TOP3.BTN, state = "normal")
        tcltk::tkconfigure(RANK.MBTN, state = "normal")
        tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
        tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
        tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
        tcltk::tkconfigure(PCA.BTN, state = "normal")
      }
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Please select variables\nto build the dataset from"), columnspan = 2)
    tcltk::tkgrid(VARLIST, columnspan = 2)
    tcltk::tkgrid(BUILD.BTN, CANCEL.BTN)
  })

  # ...Combine button----
  COMBINE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, text = "Combine", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","combine.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function() {
    # ...a first window to select files to combine
    TRIDENT.FileSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.FileSelect1234, paste("trident", METADATA$VERSION, "- Select files..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.FileSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    FILELIST <- tcltk2::tk2listbox(TRIDENT.FileSelect1234, values = names(PROJECT$FILES), selectmode = "multiple", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    SLCT.FILE.BTN <- tcltk2::tk2button(TRIDENT.FileSelect1234, text = "Select...", tip = "Confirm that the selected data will be combined", command = function() {
      Myfiles <- tcltk2::selection(FILELIST)
      if (length(Myfiles) > 4) tcltk::tkmessageBox(type = "ok", message = "Currently, no more than 4 datasets can be combined.\nIf you would like the package to support more\ncombinations in the future, please contact\nthe developers.")
      # ...a second window to select one common variable
      if (length(Myfiles) <= 4) {
        TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select variables..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
        Myvar <- colnames(PROJECT$FILES[[Myfiles[1]]])
        for (i in Myfiles[2:length(Myfiles)]) {
          Compared <- colnames(PROJECT$FILES[[i]])
          Myvar <- Myvar[Myvar %in% Compared]
        }
        VARLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = Myvar, selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
        SLCT.VAR.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Select...", tip = "Confirm that the selected variable will be used to combine the data", command = function() {
          if(is.null(tcltk2::selection(VARLIST)) == TRUE) tcltk::tkmessageBox(message = "Please select a single common variable")
          else MyCommonVar <- paste(Myvar[tcltk2::selection(VARLIST)])
          if (length(Myfiles) == 2) {
            PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$FILES[[Myfiles[1]]], y = PROJECT$FILES[[Myfiles[2]]], by = MyCommonVar,
                                                 copy = FALSE, suffix = c(paste0(".", names(PROJECT$FILES)[1]), paste0(".", names(PROJECT$FILES)[2])))
          }
          if (length(Myfiles) == 3) {
            PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$DATASET, y = PROJECT$FILES[[Myfiles[3]]], by = MyCommonVar,
                                                 copy = FALSE, suffix = c("", paste0(".", names(PROJECT$FILES)[3])))
          }
          if (length(Myfiles) == 4) {
            PROJECT$DATASET <<- dplyr::left_join(x = PROJECT$DATASET, y = PROJECT$FILES[[Myfiles[4]]], by = MyCommonVar,
                                                 copy = FALSE, suffix = c("", paste0(".", names(PROJECT$FILES)[4])))
          }
          PROJECT$VARIABLES <<- data.frame(Variables = colnames(PROJECT$DATASET), Type = sapply(PROJECT$DATASET, typeof))
          refresh.cmd()
          tcltk::tkdestroy(TRIDENT.FileSelect1234)
          tcltk::tkdestroy(TRIDENT.VarSelect1234)
        })
        CANCEL.VAR.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", tip = "Cancel variable selection", command = function() {tcltk::tkdestroy(TRIDENT.VarSelect1234)})
        # ......grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Select variables to use when combining the data"), columnspan = 2)
        tcltk::tkgrid(VARLIST, columnspan = 2)
        tcltk::tkgrid(SLCT.VAR.BTN, CANCEL.VAR.BTN)
      }
    })
    CANCEL.FILE.BTN <- tcltk2::tk2button(TRIDENT.FileSelect1234, text = "Cancel", tip = "Cancel file selection", command = function() {tcltk::tkdestroy(TRIDENT.FileSelect1234)})
    # ......grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.FileSelect1234, text = "Select files to combine:"))
    tcltk::tkgrid(FILELIST, columnspan = 2)
    tcltk::tkgrid(SLCT.FILE.BTN, CANCEL.FILE.BTN)
  })
  # ...Refresh button----
  REFRESH.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, text = "Refresh (F5)", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","refresh.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function() refresh.cmd())

  # ...Remove cases button----
  REMOVE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, text = "Remove cases", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","rmcase.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function() {

    # ...a window to select the cases
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("Select cases..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    TRIDENT.VarSelect1234$MCLIST <- tcltk::tkframe(TRIDENT.VarSelect1234)
    MYLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = paste(PROJECT$DATASET[, 1], "__", PROJECT$DATASET[, 2]), selectmode = "extended", height = 12, width = 48, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...buttons
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Remove", command = function() {
      Myselection <- tcltk2::selection(MYLIST)
      PROJECT$DATASET <<- PROJECT$DATASET[- Myselection, ]
      refresh.cmd()
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Select cases to remove"), columnspan = 2)
    tcltk::tkgrid(MYLIST, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
    })

  # ...Transform menubutton----
  TRANS.MBTN <- tcltk::tkmenubutton(NOTEBOOK$DATA, text = "Transform", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","transform.gif", package = "trident")), height = 50, relief = "flat", compound = "top")
  TRANS.MENU <- tcltk::tkmenu(TRANS.MBTN)
  tcltk::tkconfigure(TRANS.MBTN, menu = TRANS.MENU)
  tcltk::tkadd(TRANS.MENU, "command", label = "Boxcox transformation", command = function(){
    # ...prepare data: removal of Na, NaN, Inf and -Inf
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
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    YLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function(){
      Myfactor <- tcltk2::selection(YLIST)
      Boxcox <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])
      colnames(Boxcox$boxcox) <- paste(colnames(Boxcox$boxcox), "boxcox", sep = ".")
      PROJECT$VARIABLES$Variables[which(PROJECT$VARIABLES$Variables %in% colnames(dplyr::select_if(PROJECT$DATASET, is.numeric)))]  <<- paste(colnames(Numerics), "boxcox", sep = ".")
      PROJECT$DATASET <<- data.frame(Factors, Boxcox$boxcox)
      refresh.cmd()
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"), padx = 5, pady = 5)
    tcltk::tkgrid(YLIST, columnspan = 2, padx = 5, pady = 5)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN, padx = 5, pady = 5)
  })
  #
  # ...Grid all 'Data'----
  tcltk::tkgrid(OPEN.BTN, COMBINE.BTN, BUILD.BTN, REMOVE.BTN, TRANS.MBTN, REFRESH.BTN, padx = 5, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips 'Data'----
  tcltk2::tk2tip(OPEN.BTN, "Open data")
  tcltk2::tk2tip(BUILD.BTN, "Build new dataset")
  tcltk2::tk2tip(COMBINE.BTN, "Combine two or more datasets")
  tcltk2::tk2tip(REFRESH.BTN, "Refresh tables")
  tcltk2::tk2tip(TRANS.MBTN, "Apply transformation algorithms to current dataset")
  # ...Shortcuts 'Data'----
  tcltk::tkbind(TRIDENT.mainwin1234,"<F5>", function() refresh.cmd())

  # 3) Notetab 'Variables'----
  NOTEBOOK$VARIA <- tcltk2::tk2notetab(NOTEBOOK, "Variables")

  # ...Add factor button----
  ADD.VAR.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, text = "Add\nfactor", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","addvar.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    # ...A first window to select a mode of factor selection
    TRIDENT.ModeSelect1234 <<- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.ModeSelect1234, paste("trident", METADATA$VERSION, "- Select method..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.ModeSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    # ...First button : automatic factor selection (from an integer variable...)
    AUTOMATIC.BTN <- tcltk2::tk2button(TRIDENT.ModeSelect1234, text = "Automatic...", tip = "Extract factor from file name", command = function() {
      tcltk::tkdestroy(TRIDENT.ModeSelect1234)
     #...A second window
      TRIDENT.Factor1234 <- tcltk::tktoplevel()
      #tcltk::tkwm.title(TRIDENT.Factor1234, paste("trident", METADATA$VERSION, "- Factor (auto)..."))
      #tcltk2::tk2ico.setFromFile(TRIDENT.Factor1234, system.file("extdata","pics","trident.ico", package = "trident"))
      # ...prepare data
      Mydf <- PROJECT$DATASET
      if(is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
        tcltk::tkmessageBox(text = "No integer variable in the current dataset\nPlease check data structure")
        stop('no integer variable')
      }
      Myintegers <- dplyr::select_if(Mydf, is.factor)
      FACTOR.LISTBOX <- tcltk2::tk2listbox(TRIDENT.Factor1234, values = colnames(Myintegers), selectmode = "single", height = 2, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
      ELEMENT.SPINBOX <- tcltk2::tk2spinbox(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$ELEMENT, width = 20, from = 1, to = 30, increment = 1, tip = "")
        NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$FACTOR.NAME)
        SEPARATOR.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$SEPARATOR, tip = "For linebreaks, type '\\\\n'")
        PREFIX.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$PREFIX, tip = "For linebreaks, type '\\\\n'")
        SUFFIX.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$SUFFIX, tip = "For linebreaks, type '\\\\n'")

        OK3.BTN <- tcltk2::tk2button(TRIDENT.Factor1234, text = "Ok", tip = "", command = function () {
          Make.factor.cmd <- function(x, separator, rank, suffix, prefix) {
            Strings <- strsplit(as.character(x), as.character(separator))
            Dataframe <- data.frame(matrix(unlist(Strings), nrow = length(Strings[[1]])))
            return(paste(prefix, unlist(Dataframe[rank,]), suffix, sep = ""))
          }

          Myelement <- tcltk::tclvalue(tcltk::tkget(ELEMENT.SPINBOX))
          Mysep <- tcltk::tclvalue(tcltk::tkget(SEPARATOR.ENTRY))
          Myfactorname <- tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))
          Myprefix <- tcltk::tclvalue(tcltk::tkget(PREFIX.ENTRY))
          Mysuffix <- tcltk::tclvalue(tcltk::tkget(SUFFIX.ENTRY))
          Myfactor <- data.frame(Make.factor.cmd(x = Myintegers[, tcltk2::selection(FACTOR.LISTBOX)],
                          separator = Mysep,
                          rank = Myelement,
                          suffix = Mysuffix,
                          prefix = Myprefix), stringsAsFactors = TRUE)
          PROJECT$OPTIONS$SEPARATOR <<- tcltk::tclVar(Mysep)
          PROJECT$OPTIONS$FACTOR.NAME <<- tcltk::tclVar(Myfactorname)
          PROJECT$OPTIONS$ELEMENT <<- tcltk::tclVar(Myelement)
          PROJECT$OPTIONS$PREFIX <<- tcltk::tclVar(Myprefix)
          PROJECT$OPTIONS$SUFFIX <<- tcltk::tclVar(Mysuffix)

          colnames(Myfactor) <- Myfactorname
          if (length(Myfactor[, 1]) > length(PROJECT$DATASET[, 1])) tcltk::tkmessageBox(type = "ok", message = "This factor is too long!")
          if (length(Myfactor[, 1]) < length(PROJECT$DATASET[, 1])) tcltk::tkmessageBox(type = "ok", message = "This factor is too short!")
          if (length(Myfactor[, 1]) == length(PROJECT$DATASET[, 1])) {
            tcltk::tkdestroy(TRIDENT.Factor1234)
            PROJECT$DATASET <<- dplyr::bind_cols(PROJECT$DATASET, Myfactor)
            PROJECT$VARIABLES <<- dplyr::union(PROJECT$VARIABLES, data.frame(Variables = Myfactorname, Type = "factor"))
            refresh.cmd()
            # ...re-able
            if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
              tcltk::tkconfigure(TRANS.MBTN, state = "normal")
              tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
              tcltk::tkconfigure(SUM.BTN, state = "normal")
              tcltk::tkconfigure(MULTI.BTN, state = "normal")
              tcltk::tkconfigure(DISC.BTN, state = "normal")
              tcltk::tkconfigure(NONDISC.BTN, state = "normal")
              tcltk::tkconfigure(TOP3.BTN, state = "normal")
              tcltk::tkconfigure(RANK.MBTN, state = "normal")
              tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
              tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
              tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
              tcltk::tkconfigure(PCA.BTN, state = "normal")
            }
          }
          })
        CANCEL3.BTN <- tcltk2::tk2button(TRIDENT.Factor1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.Factor1234))
        # ...grid all
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Select a variable:"))
        tcltk::tkgrid(FACTOR.LISTBOX)
        tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.Factor1234, orientation = "horizontal"), pady = 5, sticky = "ew")
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Choose which element to use:"), ELEMENT.SPINBOX)
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Separator:"),  tcltk::tklabel(TRIDENT.Factor1234, text = "Factor name:"))
        tcltk::tkgrid(SEPARATOR.ENTRY, NAME.ENTRY)
        tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Prefix:"),  tcltk::tklabel(TRIDENT.Factor1234, text = "Suffix:"))
        tcltk::tkgrid(PREFIX.ENTRY, SUFFIX.ENTRY)
        tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.Factor1234, orientation = "horizontal"), columnspan = 2, pady = 5, sticky = "ew")
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN)
    })
    # ...Second button : manual factor entry
    MANUAL.BTN <- tcltk2::tk2button(TRIDENT.ModeSelect1234, text = "Manual...", tip = "Enter factor manually", command = function() {
      tcltk::tkdestroy(TRIDENT.ModeSelect1234)
      # ...A second window
      TRIDENT.Factor1234 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.Factor1234, paste("trident", METADATA$VERSION, "- Describe factor..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.Factor1234, system.file("extdata","pics","trident.ico", package = "trident"))
      SEPARATOR.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$SEPARATOR, tip = "For linebreaks, type '\\\\n'")
      FACTOR.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$TEMP)
      NAME.ENTRY <- tcltk2::tk2entry(TRIDENT.Factor1234, textvariable = PROJECT$OPTIONS$FACTOR.NAME)
      OK2.BTN <- tcltk2::tk2button(TRIDENT.Factor1234, text = "Ok", tip = "", command = function () {
        Mysep <- tcltk::tclvalue(tcltk::tkget(SEPARATOR.ENTRY))
        Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.ENTRY))
        Myfactorname <- tcltk::tclvalue(tcltk::tkget(NAME.ENTRY))
        PROJECT$OPTIONS$SEPARATOR <<- tcltk::tclVar(Mysep)
        PROJECT$OPTIONS$FACTOR.NAME <<- tcltk::tclVar(Myfactorname)
        Myfactor <- data.frame(unlist(strsplit(Myfactor, Mysep)), stringsAsFactors = TRUE)
        colnames(Myfactor) <- Myfactorname
        if (length(Myfactor[, 1]) > length(PROJECT$DATASET[, 1])) tcltk::tkmessageBox(type = "ok", message = "This factor is too long!")
        if (length(Myfactor[, 1]) < length(PROJECT$DATASET[, 1])) tcltk::tkmessageBox(type = "ok", message = "This factor is too short!")
        if (length(Myfactor[, 1]) == length(PROJECT$DATASET[, 1])) {
          tcltk::tkdestroy(TRIDENT.Factor1234)
          PROJECT$DATASET <<- dplyr::bind_cols(PROJECT$DATASET, Myfactor)
          PROJECT$VARIABLES <<- dplyr::union(PROJECT$VARIABLES, data.frame(Variables = Myfactorname, Type = "factor"))
          refresh.cmd()
          # ...re-able
          if (match(TRUE, sapply(PROJECT$DATASET, is.factor)) <= 1) {
            tcltk::tkconfigure(TRANS.MBTN, state = "normal")
            tcltk::tkconfigure(ADD.VAR.BTN, state = "normal")
            tcltk::tkconfigure(SUM.BTN, state = "normal")
            tcltk::tkconfigure(MULTI.BTN, state = "normal")
            tcltk::tkconfigure(DISC.BTN, state = "normal")
            tcltk::tkconfigure(NONDISC.BTN, state = "normal")
            tcltk::tkconfigure(TOP3.BTN, state = "normal")
            tcltk::tkconfigure(RANK.MBTN, state = "normal")
            tcltk::tkconfigure(BIPLOT.BTN, state = "normal")
            tcltk::tkconfigure(BOXPLOT.BTN, state = "normal")
            tcltk::tkconfigure(VIOLIN.BTN, state = "normal")
            tcltk::tkconfigure(PCA.BTN, state = "normal")
          }
        }
      })
      CANCEL2.BTN <- tcltk2::tk2button(TRIDENT.Factor1234, text = "Cancel", tip = "", command = function () tcltk::tkdestroy(TRIDENT.Factor1234))
      # ...grid all in second window
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Factor name:"), tcltk::tklabel(TRIDENT.Factor1234, text = "Separator:"))
      tcltk::tkgrid(NAME.ENTRY, SEPARATOR.ENTRY, padx = 5)
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.Factor1234, text = "Please enter complete factor:"), columnspan = 2)
      tcltk::tkgrid(FACTOR.ENTRY, padx = 10, columnspan = 2)
      tcltk::tkgrid(OK2.BTN, CANCEL2.BTN, padx = 5)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.ModeSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.ModeSelect1234))
    # ...grid all in first window
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.ModeSelect1234, text = "Please choose method for factor selection:"))
    tcltk::tkgrid(AUTOMATIC.BTN, padx = 5)
    tcltk::tkgrid(MANUAL.BTN, padx = 5)
    tcltk::tkgrid(CANCEL.BTN, padx = 5)
  })

  # ...Correlation test menubuttons----
  CORTEST.MBTN <- tcltk::tkmenubutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","cortest.gif", package = "trident")), height = 50, relief = "flat", text = "Correlation...", compound = "top")
  CORTEST.MENU <- tcltk::tkmenu(CORTEST.MBTN)
  tcltk::tkconfigure(CORTEST.MBTN, menu = CORTEST.MENU)
  # ......Pearson menu----
  tcltk::tkadd(CORTEST.MENU, "command", label = "Pearson's linear correlation", command = function() {
    # ...prepare data
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Mycortable <- picante::cor.table(Numerics, cor.method = "pearson")
    Mycortable$r <- round(Mycortable$r, 4)
    Mycortable$r[Mycortable$P >= 0.05] <- "NS"
    Mycortable$r[Mycortable$P < 0.05] <- paste(Mycortable$r[Mycortable$P < 0.05], "*", sep = "")
    Mycortable$r[Mycortable$P < 0.01] <- paste(Mycortable$r[Mycortable$P < 0.01], "*", sep = "")
    Mycortable$r[Mycortable$P < 0.001] <- paste(Mycortable$r[Mycortable$P < 0.001], "*", sep = "")
    Mytable <- data.frame(Variables = colnames(Mycortable$r), Mycortable$r)
    # ......Build window with table
    TRIDENT.TableWin1234 <- tcltk::tktoplevel()
    TRIDENT.TableWin1234$TEXT <- tcltk::tkframe(TRIDENT.TableWin1234)
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- correlation (Pearson)"))
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    build.table.cmd(Mytable, TRIDENT.TableWin1234)
    tcltk::tkgrid(TRIDENT.TableWin1234$TEXT)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.TableWin1234$TEXT, text = "NS Non significant; * P < 0.05; ** P < 0.01; *** P < 0.001"))
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
  })

  # ......Spearman menu----
  tcltk::tkadd(CORTEST.MENU, "command", label = "Spearman's non-linear correlation", command = function() {
    # ...prepare data
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Mycortable <- picante::cor.table(Numerics, cor.method = "spearman")
    Mycortable$r <- round(Mycortable$r, 4)
    Mycortable$r[Mycortable$P >= 0.05] <- "NS"
    Mycortable$r[Mycortable$P < 0.05] <- paste(Mycortable$r[Mycortable$P < 0.05], "*", sep = "")
    Mycortable$r[Mycortable$P < 0.01] <- paste(Mycortable$r[Mycortable$P < 0.01], "*", sep = "")
    Mycortable$r[Mycortable$P < 0.001] <- paste(Mycortable$r[Mycortable$P < 0.001], "*", sep = "")
    Mytable <- data.frame(Variables = colnames(Mycortable$r), Mycortable$r)
    # ......Build window with table
    TRIDENT.TableWin1234 <- tcltk::tktoplevel()
    TRIDENT.TableWin1234$TEXT <- tcltk::tkframe(TRIDENT.TableWin1234)
    tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- correlation (Spearman)"))
    tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
    build.table.cmd(Mytable, TRIDENT.TableWin1234)
    tcltk::tkgrid(TRIDENT.TableWin1234$TEXT)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.TableWin1234$TEXT, text = "NS Non significant; * P < 0.05; ** P < 0.01; *** P < 0.001"))
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
  })

  # ...Summary button----
  SUM.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, text = "Summary", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","summary.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
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
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    TRIDENT.VarSelect1234$FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function(){
      Myfactor <- Factors[, tcltk2::selection(TRIDENT.VarSelect1234$FACTORLIST)]
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      Mytable <- data.frame(Variable = colnames(Numerics),
                            Min = t(plyr::colwise(min)(Numerics, na.rm = TRUE)),
                            Quart1 = t(plyr::colwise(stats::quantile)(Numerics, probs = 0.25, na.rm = TRUE)),
                            Median = t(plyr::colwise(stats::median)(Numerics, na.rm = TRUE)),
                            Quart3 = t(plyr::colwise(stats::quantile)(Numerics, probs = 0.75, na.rm = TRUE)),
                            Max = t(plyr::colwise(max)(Numerics, na.rm = TRUE)),
                            Mean = t(plyr::colwise(mean)(Numerics, na.rm = TRUE)),
                            SEM = t(plyr::colwise(function(x) {
                              if(is.na(var(x))) return(0)
                              Squared <- stats::var(x, na.rm = TRUE)/length(x)
                              if (isTRUE(Squared == 0)) return(0)
                              if (!isTRUE(Squared == 0)) return(sqrt(Squared))
                            })(Numerics)))
      for (i in c(1:length(levels(Myfactor)))) {
        Group <- levels(Myfactor)[i]
        Subset <- Numerics[which(Myfactor == Group), ]
        Pergrouptable <- data.frame(Min = t(plyr::colwise(min)(Subset, na.rm = TRUE)),
                                    Quart1 = t(plyr::colwise(stats::quantile)(Subset, probs = 0.25, na.rm = TRUE)),
                                    Median = t(plyr::colwise(stats::median)(Subset, na.rm = TRUE)),
                                    Quart3 = t(plyr::colwise(stats::quantile)(Subset, probs = 0.75, na.rm = TRUE)),
                                    Max = t(plyr::colwise(max)(Subset, na.rm = TRUE)),
                                    Mean = t(plyr::colwise(mean)(Subset, na.rm = TRUE)),
                                    SEM = t(plyr::colwise(function(x) {
                                      if(is.na(var(x))) return(0)
                                      Squared <- stats::var(x, na.rm = TRUE)/length(x)
                                      if (isTRUE(Squared == 0)) return(0)
                                      if (!isTRUE(Squared == 0)) return(sqrt(Squared))
                                      })(Subset)))

        colnames(Pergrouptable) <- paste(colnames(Pergrouptable), Group, sep = ".")
        Mytable <- dplyr::bind_cols(Mytable, Pergrouptable)
      }
      # ......Build window with table
      TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- summary"))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      build.table.cmd(Mytable, TRIDENT.TableWin1234)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"), columnspan = 2)
    tcltk::tkgrid(TRIDENT.VarSelect1234$FACTORLIST, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ...Discriminant variables button----
  DISC.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, text = "Discriminant", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
    # ...prepare data
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    if (is.null(dplyr::select_if(Mydf, is.factor)) == TRUE) {
      tcltk::tkmessageBox(text = "No factor variable in the current dataset\nPlease check data structure")
      stop('no factor variable')
    }
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...a window to select the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    TRIDENT.VarSelect1234$FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...buttons
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function(){

      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))

      Myfactor <- tcltk2::selection(TRIDENT.VarSelect1234$FACTORLIST)
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
      if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
        Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
        TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
      }
      # 1st case: no discriminant variable
      if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) == 0) {
        tcltk::tk_messageBox(type = "ok", message = " There are no discriminant variable ", icon = "warning")
      }
      # 2nd case: there are some discriminant variables
      if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) != 0) {
        Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == TRUE)])
        # ......Build window with table
        TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- List of discriminant variables"))
        tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
        # Buttons for plots
        TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
        BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          biplot.cmd(df = Mydf)
        })
        BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          boxplot.cmd(df = Mydf)
        })
        VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          plot.violin.cmd(df = Mydf)
        })
        PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          plot.pca.cmd(df = Mydf)
        })

        # destroy the waiting message
        tcltk::tkdestroy(TRIDENT.busy)

        # ...grid all
        build.table.cmd(Mytable, TRIDENT.TableWin1234, width = 1 + max(nchar(Mytable$Variable)), height = min(floor((SCREEN.RES$V * 0.7 - 120) / (16 * 1.5)), length(Mytable$Variable)))
        tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
        tcltk::tkgrid(BOXPLOT.BTN, VIOLIN.BTN)
        tcltk::tkgrid(BIPLOT.BTN, PCA.BTN)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
      }
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"), columnspan = 2)
    tcltk::tkgrid(TRIDENT.VarSelect1234$FACTORLIST, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ...Non-discriminant variables button----
  NONDISC.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, text = "Non-discriminant", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), height = 50, relief = "flat", compound = "top", command = function(){
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
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    TRIDENT.VarSelect1234$FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    # ...buttons
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function(){

      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))


      Myfactor <- tcltk2::selection(TRIDENT.VarSelect1234$FACTORLIST)
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
      if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
        Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
        TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
      }
      # 1st case: no non-discriminant variable
      if (length(Mycheck$variable[which(Mycheck$is.discriminant == FALSE)]) == 0) {
        tcltk::tk_messageBox(type = "ok", message = " There are no non-discriminant variable ", icon = "warning")
      }
      # 2nd case: there are some discriminant variables
      if (length(Mycheck$variable[which(Mycheck$is.discriminant == FALSE)]) != 0) {
        Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == FALSE)])
        # ......Build window with table
        TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- List of non-discriminant variables"))
        tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
        build.table.cmd(Mytable, TRIDENT.TableWin1234, width = 1 + max(nchar(Mytable$Variable)), height = min(floor((SCREEN.RES$V * 0.7 - 120) / (16 * 1.5)), length(Mytable$Variable)))

        # Button for removing
        TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
        RMVAR.BTN <- tcltk::tkbutton(TRIDENT.TableWin1234$BUTTONS, text = "Remove from\ndataset", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","rm.gif", package = "trident")), compound = "top", relief = "flat", command = function() {
          PROJECT$DATASET <<- PROJECT$DATASET[, !colnames(PROJECT$DATASET) %in% Mytable$Variable]
          PROJECT$VARIABLES <<- PROJECT$VARIABLES[!PROJECT$VARIABLES$Variables %in% Mytable$Variable, ]
          refresh.cmd()
          tcltk::tkconfigure(RMVAR.BTN, state = "disable")
        })
        # Grid all
        tcltk::tkgrid(RMVAR.BTN)
        tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
        # destroy the waiting message
        tcltk::tkdestroy(TRIDENT.busy)
      }
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"), columnspan = 2)
    tcltk::tkgrid(TRIDENT.VarSelect1234$FACTORLIST, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ...Multicheck button----
  MULTI.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","multicheck.gif", package = "trident")), height = 50, relief = "flat",
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
                                 TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
                                 tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
                                 TRIDENT.VarSelect1234$FACTORLIST <- tcltk2::tk2listbox(TRIDENT.VarSelect1234, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                 tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
                                 # ...buttons
                                 OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function(){

                                   # Pop a waiting message
                                   TRIDENT.busy <- tcltk::tktoplevel()
                                   tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
                                   tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
                                   tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
                                   tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
                                   tcltk::tkwm.title(TRIDENT.busy, paste(""))

                                   Myfactor <- tcltk2::selection(TRIDENT.VarSelect1234$FACTORLIST)
                                   tcltk::tkdestroy(TRIDENT.VarSelect1234)
                                   if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
                                   if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
                                     Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                     TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
                                   }
                                   Mytable <- data.frame(Mycheck)

                                   # destroy the waiting message
                                   tcltk::tkdestroy(TRIDENT.busy)
                                   # ......Build window with table
                                   TRIDENT.TableWin1234 <<- tcltk::tktoplevel()
                                   tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- Multicheck"))
                                   tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
                                   build.table.cmd(Mytable, TRIDENT.TableWin1234)
                                   tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
                                   tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
                                 })
                                 CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
                                 # ...grid all
                                 tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor"), columnspan = 2)
                                 tcltk::tkgrid(TRIDENT.VarSelect1234$FACTORLIST, columnspan = 2)
                                 tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                 tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
                                 tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
                               })
  # ...Top3 button----
  TOP3.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Top 3", compound = "top", command = function() {
                                # ...a window to select the factor
                                TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
                                tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
                                tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                FACTOR.CBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = colnames(Factors))
                                OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
                                  # Pop a waiting message
                                  TRIDENT.busy <- tcltk::tktoplevel()
                                  tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
                                  tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
                                  tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
                                  tcltk::tkwm.title(TRIDENT.busy, paste(""))

                                  Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CBBX))
                                  Mytop <- trident::trident.top3(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- Mytop$ranked
                                  # Remark: Boxcoxed data for the plots
                                  Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
                                  colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
                                  n <- length(PROJECT$FILES)
                                  # ......display in new window
                                  TRIDENT.TableWin1234 <- tcltk::tktoplevel()
                                  tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- top 3 best discriminating variables for each group"))
                                  tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
                                  TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
                                  BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
                                    Mydf <- data.frame(Factors, Numerics[, unique(Mytop$top3var)])
                                    biplot.cmd(df = Mydf)
                                  })
                                  BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
                                    Mydf <- data.frame(Factors, Numerics[, unique(Mytop$top3var)])
                                    boxplot.cmd(df = Mydf)
                                  })
                                  VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
                                    Mydf <- data.frame(Factors, Numerics[, unique(Mytop$top3var)])
                                    plot.violin.cmd(df = Mydf)
                                  })
                                  PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
                                    Mydf <- data.frame(Factors, Numerics[, unique(Mytop$top3var)])
                                    plot.pca.cmd(df = Mydf)
                                  })
                                  # destroy the waiting message
                                  tcltk::tkdestroy(TRIDENT.busy)
                                  # ...grid all
                                  Mytable <- data.frame(Variables = rownames(Mytable), Mytable)
                                  build.table.cmd(Mytable, TRIDENT.TableWin1234)
                                  tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
                                  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
                                  # ...window attributes
                                  tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
                                  tcltk::tkdestroy(TRIDENT.VarSelect1234)

                                })
                                CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
                                # ...grid all
                                tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Please choose a factor:"), FACTOR.CBBX)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
                              })
  # ...Arrange by menubuttons----
  RANK.MBTN <- tcltk::tkmenubutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","arrange.gif", package = "trident")), height = 50, relief = "flat", text = "Rank based on...", compound = "top")
  RANK.MENU <- tcltk::tkmenu(RANK.MBTN)
  tcltk::tkconfigure(RANK.MBTN, menu = RANK.MENU)
  # ......ANOVA menu----
  tcltk::tkadd(RANK.MENU, "command", label = "ANOVA", command = function() {
    # ...a first window to select the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    FACTOR.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = c("F-stat", "P-value"))
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))

      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      Disc.only <- PROJECT$OPTIONS$DISC.ONLY.VALUE
      Myboxcox <- PROJECT$OPTIONS$BOXCOX.VALUE
      n <- length(PROJECT$FILES)

      if (Myby == "F-stat") Myby <- "f.stat"
      if (Myby == "P-value") Myby <- "aov.p.value"
      if (tcltk::tclvalue(Myboxcox) == "1") {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(Disc.only) == "1") {
        if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
        if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
          Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
          TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
        }
        # 1st case: no discriminant variable
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) == 0) {
          tcltk::tk_messageBox(type = "ok", message = " There are no discriminant variable ", icon = "warning")
        }
        # 2nd case: there are some discriminant variables
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) != 0) {
          Numerics <- Numerics[, Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]]
        }
      }

      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      # ...a second window to display the results
      TRIDENT.TableWin1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- Variables ordered by", Myby,"..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
      BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.pca.cmd(df = Mydf)
      })
      # destroy the waiting message
      tcltk::tkdestroy(TRIDENT.busy)
      # ...grid all
      if (Myby == "F-stat") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by F-stat"))
      if (Myby == "P-value") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by ANOVA's P-value"))
      Mytable <- data.frame(Variables = rownames(Mytable), Mytable)
      build.table.cmd(Mytable, TRIDENT.TableWin1234)
      tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank using ANOVA's residuals"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.KTEST.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.VarSelect1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ......KTEST menu----
  tcltk::tkadd(RANK.MENU, "command", label = "K-test", command = function() {
    # ...a first window to select the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors <- dplyr::select_if(Mydf, is.factor)
    FACTOR.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = c("Kruskal's K", "P-value"))
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))

      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      Disc.only <- PROJECT$OPTIONS$DISC.ONLY.VALUE
      Myboxcox <- PROJECT$OPTIONS$BOXCOX.VALUE
      n <- length(PROJECT$FILES)

      if (Myby == "Kruskal's K") Myby <- "k"
      if (Myby == "P-value") Myby <- "k.p.value"
      if (tcltk::tclvalue(Myboxcox) == "1") {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(Disc.only) == "1") {
        if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
        if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
          Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
          TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
        }
        # 1st case: no discriminant variable
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) == 0) {
          tcltk::tk_messageBox(type = "ok", message = " There are no discriminant variable ", icon = "warning")
        }
        # 2nd case: there are some discriminant variables
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) != 0) {
        Numerics <- Numerics[, Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]]
        }
      }

      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      # ...a second window to display the results
      TRIDENT.TableWin1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- Variables ordered by", Myby,"..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
      BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.pca.cmd(df = Mydf)
      })
      # destroy the waiting message
      tcltk::tkdestroy(TRIDENT.busy)
      # ...grid all
      if (Myby == "Kruskal's K") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by Kruskal's K"))
      if (Myby == "P-value") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by Kruskal's P-value"))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      Mytable <- data.frame(Variables = rownames(Mytable), Mytable)
      build.table.cmd(Mytable, TRIDENT.TableWin1234)
      tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank using Kruskal's results"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.KTEST.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.VarSelect1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })

  # ......POSTHOC (MEAN) menu----
  tcltk::tkadd(RANK.MENU, "command", label = "Post-hoc (mean)", command = function() {
    # ...a first window to select the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
    tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
    tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    FACTOR.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = c("Tukey_s HSD", "Fisher_s LSD"))
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    GEOMEAN.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Compute geometric mean?")
    tcltk::tkconfigure(GEOMEAN.CHKBTN, variable = PROJECT$OPTIONS$GEOMEAN.VALUE)
    BYNGR.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Rank by number of discriminated groups?")
    tcltk::tkconfigure(BYNGR.CHKBTN, variable = PROJECT$OPTIONS$BYNGR.VALUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      # Pop a waiting message
      TRIDENT.busy <- tcltk::tktoplevel()
      tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
      tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
      tcltk::tkwm.title(TRIDENT.busy, paste(""))

      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      Disc.only <- PROJECT$OPTIONS$DISC.ONLY.VALUE
      Boxcox <- PROJECT$OPTIONS$BOXCOX.VALUE
      Geomean <- PROJECT$OPTIONS$GEOMEAN.VALUE
      Byngr <- PROJECT$OPTIONS$BYNGR.VALUE
      n <- length(PROJECT$FILES)

      if (Myby == "Tukey_s HSD") Myby <- "hsd.mean.p.value"
      if (Myby == "Fisher_s LSD") Myby <- "lsd.mean.p.value"
      if (tcltk::tclvalue(Geomean) == "1") Mygeomean <- TRUE
      if (tcltk::tclvalue(Geomean) == "0") Mygeomean <- FALSE
      if (tcltk::tclvalue(Byngr) == "1") Mybyngr <- TRUE
      if (tcltk::tclvalue(Byngr) == "0") Mybyngr <- FALSE
      if (tcltk::tclvalue(Boxcox) == "1") {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(Disc.only) == "1") {
        if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
        if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
          Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
          TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
        }
        # 1st case: no discriminant variable
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) == 0) {
          tcltk::tk_messageBox(type = "ok", message = " There are no discriminant variable ", icon = "warning")
        }
        # 2nd case: there are some discriminant variables
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) != 0) {
          Numerics <- Numerics[, Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]]
        }
      }

      Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby, geomean = Mygeomean, byngr = Mybyngr)
      Numerics <- Numerics[, rownames(Mytable)]
      tcltk::tkdestroy(TRIDENT.VarSelect1234)
      # ...a second window to display the results
      TRIDENT.TableWin1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- Variables ordered by", Myby,"..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
      BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        biplot.cmd(df = Mydf)
      })
      BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        boxplot.cmd(df = Mydf)
      })
      VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.violin.cmd(df = Mydf)
      })
      PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
        Mydf <- data.frame(Factors, Numerics)
        plot.pca.cmd(df = Mydf)
      })
      # destroy the waiting message
      tcltk::tkdestroy(TRIDENT.busy)
      # ...grid all
      if (Myby == "Tukey_s HSD") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by mean Tukey_s HSD"))
      if (Myby == "Fisher_s LSD") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by mean Fisher_s LSD"))
      tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
      Mytable <- data.frame(Variables = rownames(Mytable), Mytable)
      build.table.cmd(Mytable, TRIDENT.TableWin1234)
      tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
      tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
      tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank using post-hoc mean p-values"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.MEANPOSTHOC.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.VarSelect1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(GEOMEAN.CHKBTN, columnspan = 2)
    tcltk::tkgrid(BYNGR.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })

  # ......POSTHOC (BY GROUP) menu----
  tcltk::tkadd(RANK.MENU, "command", label = "Post-hoc (by group)", command = function() {
    # ...a first window to select the factor
    TRIDENT.VarSelect1234 <- tcltk::tktoplevel()
   # tcltk::tkwm.title(TRIDENT.VarSelect1234, paste("trident", METADATA$VERSION, "- Select factor..."))
   # tcltk2::tk2ico.setFromFile(TRIDENT.VarSelect1234, system.file("extdata","pics","trident.ico", package = "trident"))
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    FACTOR.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = colnames(Factors))
    BY.CMBBX <- tcltk2::tk2combobox(TRIDENT.VarSelect1234, values = c("Tukey_s HSD", "Fisher_s LSD"))
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DISC.ONLY.CHKBTN <- tcltk2::tk2checkbutton(TRIDENT.VarSelect1234, text = "Remove non-discriminant variables?")
    tcltk::tkconfigure(DISC.ONLY.CHKBTN, variable = PROJECT$OPTIONS$DISC.ONLY.VALUE)
    OK.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "OK", command = function() {
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      Myby <- tcltk::tclvalue(tcltk::tkget(BY.CMBBX))
      Disc.only <- PROJECT$OPTIONS$DISC.ONLY.VALUE
      Boxcox <- PROJECT$OPTIONS$BOXCOX.VALUE
      n <- length(PROJECT$FILES)

      if (Myby == "Tukey_s HSD") Myby <- "hsd.p.value"
      if (Myby == "Fisher_s LSD") Myby <- "lsd.p.value"
      if (tcltk::tclvalue(Boxcox) == "1") {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(Disc.only) == "1") {
        if (!is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) Mycheck <- TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]]
        if (is.null(TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]])) {
          Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
          TEMPORARY$MULTICHECK[[colnames(Factors)[Myfactor]]] <<- Mycheck
        }
        # 1st case: no discriminant variable
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) == 0) {
          tcltk::tk_messageBox(type = "ok", message = " There are no discriminant variable ", icon = "warning")
        }
        # 2nd case: there are some discriminant variables
        if (length(Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]) != 0) {
          Numerics <- Numerics[, Mycheck$variable[which(Mycheck$is.discriminant == TRUE)]]
        }
      }

      Mynames <- utils::combn(levels(Factors[, Myfactor]), 2, paste, collapse = ' vs. ')
      # ...a second window to select group priority
      TRIDENT.SelectPriority1234 <- tcltk::tktoplevel()
      tcltk::tkwm.title(TRIDENT.SelectPriority1234, paste("trident", METADATA$VERSION, "- Group priority..."))
      tcltk2::tk2ico.setFromFile(TRIDENT.SelectPriority1234, system.file("extdata","pics","trident.ico", package = "trident"))
      PRIORITY.CBBX <- tcltk2::tk2combobox(TRIDENT.SelectPriority1234, values = Mynames)
      OK.BTN <- tcltk2::tk2button(TRIDENT.SelectPriority1234, text = "Rank!", command = function() {
        # Pop a waiting message
        TRIDENT.busy <- tcltk::tktoplevel()
        tcltk::tkgrid(tcltk2::tk2label(TRIDENT.busy, text = "Please wait a moment\nProcessing surface..."), padx = 50, pady = 20, sticky = "ew")
        tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = TRUE)
        tcltk::tcl("wm", "attributes", TRIDENT.busy, topmost = FALSE)
       # tcltk2::tk2ico.setFromFile(TRIDENT.busy, system.file("extdata","pics","trident.ico", package = "trident"))
        tcltk::tkwm.title(TRIDENT.busy, paste(""))

        Myselect <- unlist(strsplit(tcltk::tclvalue(tcltk::tkget(PRIORITY.CBBX)), " vs. "))
        Mypriority <- which(levels(Factors[, Myfactor]) %in% Myselect)
        Mytable <- trident::trident.arrange(df = Numerics, y = Factors[, Myfactor], by = Myby, gp.priority = Mypriority)
        Numerics <- Numerics[, rownames(Mytable)]
        tcltk::tkdestroy(TRIDENT.VarSelect1234)
        # ...a third window to display the results
        TRIDENT.TableWin1234 <- tcltk::tktoplevel()
        tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- Variables ordered by", Myby,"..."))
        tcltk2::tk2ico.setFromFile(TRIDENT.TableWin1234, system.file("extdata","pics","trident.ico", package = "trident"))
        TRIDENT.TableWin1234$BUTTONS <- tcltk::tkframe(TRIDENT.TableWin1234)
        BIPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Biplot", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          biplot.cmd(df = Mydf)
        })
        BOXPLOT.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Boxplot", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          boxplot.cmd(df = Mydf)
        })
        VIOLIN.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "Violin", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          plot.violin.cmd(df = Mydf)
        })
        PCA.BTN <- tcltk2::tk2button(TRIDENT.TableWin1234$BUTTONS, text = "PCA", tip = "", command = function() {
          Mydf <- data.frame(Factors, Numerics)
          plot.pca.cmd(df = Mydf)
        })

        # ...grid all
        if (Myby == "Tukey_s HSD") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by group Tukey_s HSD"))
        if (Myby == "Fisher_s LSD") tcltk::tkwm.title(TRIDENT.TableWin1234, paste("trident", METADATA$VERSION, "- arranged by group Fisher_s LSD"))
        Mytable <- data.frame(Variables = rownames(Mytable), Mytable)
        build.table.cmd(Mytable, TRIDENT.TableWin1234)
        tcltk::tkgrid(TRIDENT.TableWin1234$BUTTONS)
        tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = TRUE)
        tcltk::tcl("wm", "attributes", TRIDENT.TableWin1234, topmost = FALSE)
        tcltk::tkdestroy(TRIDENT.SelectPriority1234)
        tcltk::tkdestroy(TRIDENT.busy)
      })
      CANCEL.BTN <- tcltk2::tk2button(TRIDENT.SelectPriority1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.SelectPriority1234))
      # ...grid all
      tcltk::tkgrid(tcltk::tklabel(TRIDENT.SelectPriority1234, text = "Please rank group priority"), columnspan = 2)
      tcltk::tkgrid(PRIORITY.CBBX)
      tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    })
    CANCEL.BTN <- tcltk2::tk2button(TRIDENT.VarSelect1234, text = "Cancel", command = function() tcltk::tkdestroy(TRIDENT.VarSelect1234))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank using post-hoc by-group p-values"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Rank by:  "), BY.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkset(BY.CMBBX, PROJECT$OPTIONS$BY.MEANPOSTHOC.VALUE)
    tcltk::tkgrid(tcltk2::tk2separator(TRIDENT.VarSelect1234, orientation = "horizontal"), sticky = "ew", columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(TRIDENT.VarSelect1234, text = "Options:"), columnspan = 2)
    tcltk::tkgrid(BOXCOX.CHKBTN, columnspan = 2)
    tcltk::tkgrid(DISC.ONLY.CHKBTN, columnspan = 2)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = TRUE)
    tcltk::tcl("wm", "attributes", TRIDENT.VarSelect1234, topmost = FALSE)
  })
  # ...Grid all 'Variables'----
  tcltk::tkgrid(ADD.VAR.BTN, CORTEST.MBTN, SUM.BTN, MULTI.BTN, DISC.BTN, NONDISC.BTN, TOP3.BTN, RANK.MBTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips 'Variables'----
  tcltk2::tk2tip(RANK.MBTN, "Classify variables from the most to the less discriminant, using...")
  tcltk2::tk2tip(ADD.VAR.BTN, "Add numeric or factor variable")
  tcltk2::tk2tip(CORTEST.MBTN, "Correlation between all variables")
  tcltk2::tk2tip(TOP3.BTN, "Top 3 best discriminant variables, following Francisco et al. 2018a")
  tcltk2::tk2tip(SUM.BTN, "Descriptive statistics of dataset")
  tcltk2::tk2tip(MULTI.BTN, "Checks the basic assumptions for One-Way ANOVA")
  tcltk2::tk2tip(DISC.BTN, "For one factor, list of the discriminant variables")
  tcltk2::tk2tip(NONDISC.BTN, "For one factor, list of the non-discriminant variables")

  # 4) Notetab 'Plots'----
  NOTEBOOK$PLOTS <- tcltk2::tk2notetab(NOTEBOOK, "Plots")
  # ...Buttons----
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

  # ...Grid all 'Plots'----
  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips 'Plots'----
  tcltk2::tk2tip(BIPLOT.BTN, "Bivariate graph")
  tcltk2::tk2tip(BOXPLOT.BTN, "Box-and-whiskers graph")
  tcltk2::tk2tip(VIOLIN.BTN, "Violin graph")
  tcltk2::tk2tip(PCA.BTN, "Principal Component Analysis")

  # TKGUI - WRAPPER----
  tcltk::tcl("wm", "attributes", TRIDENT.mainwin1234, topmost = TRUE)
  tcltk::tcl("wm", "attributes", TRIDENT.mainwin1234, topmost = FALSE)
  tcltk::tcl("wm", "protocol", TRIDENT.mainwin1234, "WM_DELETE_WINDOW", function() quit.cmd())
  # ...Disabled buttons
  tcltk::tkconfigure(COMBINE.BTN, state = "disable")
  tcltk::tkconfigure(BUILD.BTN, state = "disable")
  tcltk::tkconfigure(TRANS.MBTN, state = "disable")
  tcltk::tkconfigure(REFRESH.BTN, state = "disable")
  tcltk::tkconfigure(ADD.VAR.BTN, state = "disable")
  tcltk::tkconfigure(CORTEST.MBTN, state = "disable")
  tcltk::tkconfigure(SUM.BTN, state = "disable")
  tcltk::tkconfigure(MULTI.BTN, state = "disable")
  tcltk::tkconfigure(DISC.BTN, state = "disable")
  tcltk::tkconfigure(NONDISC.BTN, state = "disable")
  tcltk::tkconfigure(TOP3.BTN, state = "disable")
  tcltk::tkconfigure(RANK.MBTN, state = "disable")
  tcltk::tkconfigure(BIPLOT.BTN, state = "disable")
  tcltk::tkconfigure(BOXPLOT.BTN, state = "disable")
  tcltk::tkconfigure(VIOLIN.BTN, state = "disable")
  tcltk::tkconfigure(PCA.BTN, state = "disable")
  tcltk::tkconfigure(EXPORT.BTN, state = "disable")
  tcltk::tkconfigure(CLEAN.MBTN, state = "disable")
  tcltk::tkconfigure(COMPUTE.MBTN, state = "disable")
  tcltk::tkconfigure(HETERO.MBTN, state = "disable")
  tcltk::tkconfigure(HISTO.BTN, state = "disable")

  # ...disabled because they're not available yet:
  tcltk::tkentryconfigure(MENU$EDIT, 0, state = "disable")
  tcltk::tkentryconfigure(MENU$EDIT, 1, state = "disable")
  tcltk::tkentryconfigure(MENU$HELP, 2, state = "disable")
}
