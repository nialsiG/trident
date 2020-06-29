#' @title trident.gui
#' @description Open the trident graphical user interface.
#' @export
trident.gui <- function(){
  # TCLTK OBJECTS----
  METADATA <- list(VERSION = '0.2.1', DESCRIPTION = "It's alive (or is it?)")
  PROJECT <- NULL
  PROJECT <- list(FILES = list(), DATASET = NULL, VARIABLES = NULL, OPTIONS = NULL)
  PROJECT$OPTIONS <- list(JIGGER.VALUE = tcltk::tclVar("0"),
                                BOXCOX.VALUE = tcltk::tclVar("0"),
                                DIXON.VALUE = tcltk::tclVar("0"),
                                PLOT.COLORS = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

  # TCLTK COMMANDS
  # --build.table.cmd----
  build.table.cmd <- function (x, widget, bg.table = "ivory", bg.title = "dimgray", height = 27, width = 12){
    WIN2 <<- widget
    WIN2$TABLE <<- tcltk::tkframe(WIN2)
    WIN2$BUTTONS <- tcltk::tkframe(WIN2)
    SAVE.BTN <- tcltk2::tk2button(WIN2$BUTTONS, text = "Save", tip = "Save table", width = 10, command = function(){
      utils::write.table(x, file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN2, title = "Save table as...", initialfile = paste("Untitled"), defaultextension = ".txt")),
                         append = FALSE, quote = FALSE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    })
    EXPORT.BTN <- tcltk2::tk2button(WIN2$BUTTONS, text = "Export", tip = "Export data.frame object to R", width = 10, command = function(){
      My_data_from_trident <<- data.frame(x)
    })
    tcltk::tkpack(WIN2$TABLE, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkpack(WIN2$BUTTONS, side = "top", fill = "both" , expand = TRUE)
    tcltk::tkgrid(SAVE.BTN, EXPORT.BTN, padx = 5, pady = 5)
    # ...Create table for tcltk widget
    Mymatrix <- as.matrix(x, ncol = length(colnames((x))))
    Mymatrix <- rbind(colnames(Mymatrix), Mymatrix)
    Myarray <- cbind(c(paste("\\"), row.names(x)), Mymatrix)
    # ...Calculate optimal column width
    MyColWidth <- as.integer(mean(nchar(c(Myarray), type = "width")))
    #MyColWidth <- c()
    #for (i in c(1:length(Myarray[1, ]))) {
    #  MyColWidth[i] <- max(nchar(c(Myarray[, i]), type = "width"))
    #}
    # ...Define a Tcl array and initialize it to that matrix :
    MytclArray <- tcltk::tclArray()
    for (i in (1:length(Myarray[, 1]))) {
      for (j in (1:length(Myarray[1, ]))) {
        MytclArray[[i-1, j-1]] <- Myarray[i, j]
      }
    }
    displayInTable <- function(tclarray, height = height, width = width, nrow = -1, ncol = -1, colwidth = MyColWidth){
      # ...Create table widget and scrollbar widgets
      tcltk::tclRequire("Tktable")
      Table <- tcltk::tkwidget(WIN2$TABLE, "table", rows = nrow, cols = ncol, titlerows = 1, titlecols = 1, height = height, width = width, colwidth = colwidth,
                               xscrollcommand = function(...) tcltk::tkset(xscr, ...), yscrollcommand = function(...) tcltk::tkset(yscr, ...))
      xscr <- tcltk::tkscrollbar(WIN2$TABLE, orient = "horizontal", command = function(...) tcltk::tkxview(Table, ...))
      yscr <- tcltk::tkscrollbar(WIN2$TABLE, command = function(...) tcltk::tkyview(Table, ...))
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
  }
  # --save.project.cmd----
  save.project.cmd <- function() {
    rlist::list.save(x = PROJECT, file = tcltk::tclvalue(tcltk::tkgetSaveFile(title = "Save project...", initialfile = paste("Untitled.rds", sep = ""))))
  }
  # --open.project.cmd----
  open.project.cmd <- function() {
    if (length(PROJECT$FILES) != 0) {
      # ...Open new window
      WIN4782 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN4782, paste("Open project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN4782, text = "Yes", command = function() {
        tcltk::tkdestroy(WIN4782)
        save.project.cmd()
        PROJECT <<- rlist::list.load(choose.files())
        refresh.cmd()
      })
      NO.BTN <- tcltk2::tk2button(WIN4782, text = "No", command = function() {
        WIN3339 <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN3339, text = "OK", command = function() {
          tcltk::tkdestroy(WIN3339)
          tcltk::tkdestroy(WIN4782)
          PROJECT <<- rlist::list.load(choose.files())
          refresh.cmd()
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN3339, text = "Cancel", command = function() tcltk::tkdestroy(WIN3339))
        tcltk::tkgrid(tcltk::tklabel(WIN3339, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN4782, text = "Cancel", command = function() tcltk::tkdestroy(WIN4782))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN4782, text = "Do you want to save the current project?"), columnspan = 3)
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
      WIN4782 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN4782, paste("New project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN4782, text = "Yes", command = function() {
        tcltk::tkdestroy(WIN4782)
        save.project.cmd()
        PROJECT <<- NULL
        PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
      })
      NO.BTN <- tcltk2::tk2button(WIN4782, text = "No", command = function() {
        WIN3339 <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN3339, text = "OK", command = function() {
          tcltk::tkdestroy(WIN3339)
          tcltk::tkdestroy(WIN4782)
          PROJECT <<- NULL
          PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN3339, text = "Cancel", command = function() tcltk::tkdestroy(WIN3339))
        tcltk::tkgrid(tcltk::tklabel(WIN3339, text = "Are you sure? Current project will be deleted from memory."), columnspan = 2)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN4782, text = "Cancel", command = function() tcltk::tkdestroy(WIN4782))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN4782, text = "Do you want to save the current project?"), columnspan = 3)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
    }
    if (length(PROJECT$FILES) == 0) {
      PROJECT <<- NULL
      PROJECT <<- list(FILES = list(), DATASET = NULL, VARIABLES = NULL)
    }
  }
  # --refresh.cmd----
  refresh.cmd <- function() {
    if (is.null(WIN$TABLE1) == FALSE) tcltk::tkdestroy(WIN$TABLE1)
    WIN$TABLE1 <<- tcltk2::tk2frame(WIN)
    build.table.cmd(PROJECT$DATASET, WIN$TABLE1, bg.table = "papayawhip", bg.title = "tan")
    if (is.null(WIN$TABLE2) == FALSE) tcltk::tkdestroy(WIN$TABLE2)
    WIN$TABLE2 <<- tcltk2::tk2frame(WIN)
    PROJECT$VARIABLES <<- as.factor(colnames(dplyr::select_if(PROJECT$DATASET, is.numeric)))
    VARLIST <- tcltk2::tk2listbox(WIN$TABLE2, values = PROJECT$VARIABLES, selectmode = "single", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    RENAME.BTN <- tcltk2::tk2button(WIN$TABLE2, text = "Rename", command = function(){
      WIN9511 <- tcltk::tktoplevel()
      Varname <- tcltk::tclVar(PROJECT$VARIABLES[tcltk2::selection(VARLIST)])
      VARNAME.ENT <- tcltk2::tk2entry(WIN9511, textvariable = Varname, width = 20)
      OK.BTN <- tcltk2::tk2button(WIN9511, text = "OK", command = function(){
        colnames(PROJECT$DATASET[colnames(PROJECT$VARIABLES)[tcltk2::selection(VARLIST)]]) <<- tcltk::tclvalue(Varname)
        tcltk::tkdestroy(WIN9511)
        refresh.cmd()
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN9511, text = "Cancel", command = function() tcltk::tkdestroy(WIN9511))
      tcltk::tkgrid(tcltk::tklabel(WIN9511, text = "Enter new variable name:"))
      tcltk::tkgrid(VARNAME.ENT)
      tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    })
    tcltk::tkgrid(tcltk::tklabel(WIN$TABLE2, text = "Variables"))
    tcltk::tkgrid(VARLIST)
    tcltk::tkgrid(RENAME.BTN)
    if (is.null(WIN$TABLE3) == FALSE) tcltk::tkdestroy(WIN$TABLE3)
    WIN$TABLE3 <<- tcltk2::tk2frame(WIN)
    FILELIST <- tcltk2::tk2listbox(WIN$TABLE3, values = names(PROJECT$FILES), selectmode = "single", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
    tcltk::tkgrid(tcltk::tklabel(WIN$TABLE3, text = "Opened files"))
    tcltk::tkgrid(FILELIST)


    tcltk::tkpack(WIN$TABLE1, side = "left", expand = FALSE, fill = "x", anchor = "nw")
    tcltk::tkpack(WIN$TABLE2, side = "top", expand = FALSE, fill = "x", anchor = "nw")
    tcltk::tkpack(WIN$TABLE3, side = "top", expand = FALSE, fill = "x", anchor = "nw")

  }
  # --quit.cmd----
  quit.cmd <- function() {
    if (length(PROJECT$FILES) != 0) {
      # ...Open new window
      WIN4444 <<- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN4444, paste("Save project..."))
      # ...Buttons
      YES.BTN <- tcltk2::tk2button(WIN4444, text = "Save", command = function() {
        tcltk::tkdestroy(WIN4444)
        cmd.saveproject()
        # todo: insérer un jalon ici
        tcltk::tkdestroy(WIN)
      })
      NO.BTN <- tcltk2::tk2button(WIN4444, text = "Discard", command = function() {
        WIN8888 <<- tcltk::tktoplevel()
        OK3.BTN <- tcltk2::tk2button(WIN8888, text = "Yes (discard)", command = function() {
          tcltk::tkdestroy(WIN8888)
          tcltk::tkdestroy(WIN4444)
          tcltk::tkdestroy(WIN)
        })
        CANCEL3.BTN <- tcltk2::tk2button(WIN8888, text = "Cancel", command = function() tcltk::tkdestroy(WIN8888))
        tcltk::tkgrid(tcltk::tklabel(WIN8888, text = "Are you sure?\nCurrent project will be deleted from memory"), columnspan = 2, padx = 5, pady = 5)
        tcltk::tkgrid(OK3.BTN, CANCEL3.BTN, padx = 5, pady = 5)
      })
      CANCEL.BTN <- tcltk2::tk2button(WIN4444, text = "Cancel", command = function() tcltk::tkdestroy(WIN4444))
      # ...Grid all
      tcltk::tkgrid(tcltk::tklabel(WIN4444, text = "Do you want to save the current project?"), columnspan = 3, padx = 5, pady = 5)
      tcltk::tkgrid(YES.BTN, NO.BTN, CANCEL.BTN, padx = 5, pady = 5)
    }
    if (length(PROJECT$FILES) == 0) {
      tcltk::tkdestroy(WIN)
    }
  }

  # TKGUI - MAIN WINDOW
  # --Window----
  WIN <<- tcltk::tktoplevel()
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkconfigure(WIN, borderwidth = 10, bg = "tan")
  tcltk::tkwm.title(WIN, paste("trident", METADATA$VERSION, "-", METADATA$DESCRIPTION))
  tcltk2::tk2ico.setFromFile(WIN, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
  # TKGUI - MENU
  # --Menu----
  MENU <- tcltk2::tk2menu(WIN)
  tcltk::tkconfigure(WIN, menu = MENU)
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
  tcltk::tkadd(MENU$FILE$SAVE, "command", label = "Save as...", command = function(){})
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
  tcltk::tkbind(WIN,"<Control-n>", function() new.project.cmd())
  tcltk::tkbind(WIN,"<Control-o>", function() open.project.cmd())
  tcltk::tkbind(WIN,"<Control-s>", function() save.project.cmd())
  tcltk::tkbind(WIN,"<Control-q>", function() quit.cmd())

  # --Menu 'Edit'----
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

  # --Menu 'About'----
  MENU$ABOUT <- tcltk::tkmenu(MENU, tearoff = FALSE)
  # ...About
  tcltk::tkadd(MENU$ABOUT, "command", label = "About...", command = function(){})
  # Add About to menu
  tcltk::tkadd(MENU, "cascade", label = "About", menu = MENU$ABOUT)
  # TKGUI - NOTEBOOK
  # --Notebook----
  NOTEBOOK <- tcltk2::tk2notebook(WIN, height = 100, tabs = c("Data", "Statistics", "Variables", "Plots", "Microwear", "Batch analysis"))
  tcltk::tkpack(NOTEBOOK, side = "top", fill = "both" , expand = FALSE)
  # --Notetab 'Data'----
  NOTEBOOK$DATA <- tcltk2::tk2notetab(NOTEBOOK, "Data")
  # ...Create buttons
  OPEN.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Open", compound = "top", command = function(){
                                # ...Open the file and read it
                                File.tmp <- tcltk::tk_choose.files(filters = matrix(c("Calc", "R file", "Text", "All files", ".csv", ".txt", ".R", "*"), ncol = 2))
                                n <- length(PROJECT$FILES) +1
                                # REMARQUE: Utiliser les fonctions du package 'readr' à la place de 'read.table' ?
                                if (tools::file_ext(File.tmp) == "txt") PROJECT$FILES[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = "\t", dec = ".", row.names = NULL))
                                if (tools::file_ext(File.tmp) == "csv") PROJECT$FILES[[n]] <<- data.frame(read.table(file = File.tmp, header = TRUE, sep = ",", dec = ".", row.names = NULL))
                                names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1])
                                if (anyDuplicated(names(PROJECT$FILES)) != 0) names(PROJECT$FILES)[[n]] <<- paste0(rev(unlist(base::strsplit(File.tmp, "/")))[1], "(", n,")")
                                if (is.null(PROJECT$DATASET) == FALSE) {
                                  # ...a window to choose whether the current dataset must be replaced
                                  WIN3 <- tcltk::tktoplevel()
                                  # ...Yes
                                  YES.BTN <- tcltk::tkbutton(WIN3, text = "Yes", command = function() {
                                    PROJECT$DATASET <<- PROJECT$FILES[[n]]
                                    tcltk::tkdestroy(WIN3)
                                    refresh.cmd()
                                  })
                                  # ...No
                                  NO.BTN <- tcltk::tkbutton(WIN3, text = "No", command = function() {tcltk::tkdestroy(WIN3)})
                                  tcltk::tkgrid(tcltk::tklabel(WIN3, text = "Do you want to replace current dataset?"))
                                  tcltk::tkgrid(YES.BTN, NO.BTN, padx = 5, pady = 5, sticky = "ns")
                                  if (is.null(WIN$TABLE3) == FALSE) tcltk::tkdestroy(WIN$TABLE3)
                                  WIN$TABLE3 <<- tcltk2::tk2frame(WIN)
                                  FILELIST <- tcltk2::tk2listbox(WIN$TABLE3, values = names(PROJECT$FILES), selectmode = "single", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  tcltk::tkgrid(tcltk::tklabel(WIN$TABLE3, text = "Opened files"))
                                  tcltk::tkgrid(FILELIST)
                                  tcltk::tkpack(WIN$TABLE3, side = "top", expand = FALSE, fill = "x", anchor = "nw")
                                }
                                # ...Display dataset in the adequate frame
                                if (is.null(PROJECT$DATASET) == TRUE) {
                                  PROJECT$DATASET <<- PROJECT$FILES[[n]]
                                  refresh.cmd()
                                }

                              })
  BUILD.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","build.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Build", compound = "top", command = function() {
                                 WIN6843 <- tcltk::tktoplevel()
                                 CombData <- data.frame(PROJECT$FILES)
                                 VARLIST <- tcltk2::tk2listbox(WIN6843, values = colnames(CombData), selectmode = "extended", height = 12, width = 0, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                 BUILD.BTN <- tcltk2::tk2button(WIN6843, text = "Combine!", command = function() {
                                   Myselect <- tcltk2::selection(VARLIST)
                                   tcltk::tkdestroy(WIN6843)
                                   PROJECT$DATASET <<- CombData[, Myselect]
                                   refresh.cmd()
                                 })
                                 CANCEL.BTN <- tcltk2::tk2button(WIN6843, text = "Cancel", command = function() tcltk::tkdestroy(WIN6843))
                                 tcltk::tkgrid(tcltk::tklabel(WIN6843, text = "Please select variables\nto build the dataset from"), columnspan = 2)
                                 tcltk::tkgrid(VARLIST, columnspan = 2)
                                 tcltk::tkgrid(BUILD.BTN, CANCEL.BTN)
                               })
  COMBINE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","combine.gif", package = "trident")), height = 50, relief = "flat",
                               text = "Combine", compound = "top", command = function() {
                                 WIN6842 <- tcltk::tktoplevel()
                                 FILELIST <- tcltk2::tk2listbox(WIN6842, values = names(PROJECT$FILES), selectmode = "multiple", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)

                                 COMBINE.BTN <- tcltk2::tk2button(WIN6842, text = "Combine!", command = function() {
                                   Myselect <- tcltk2::selection(FILELIST)
                                   tcltk::tkdestroy(WIN6842)
                                   PROJECT$DATASET <<- data.frame(PROJECT$FILES[Myselect])
                                   refresh.cmd()
                                 })
                                 CANCEL.BTN <- tcltk2::tk2button(WIN6842, text = "Cancel", command = function() {tcltk::tkdestroy(WIN6842)})
                                 # Grid all
                                 tcltk::tkgrid(tcltk::tklabel(WIN6842, text = "Please select files to combine"))
                                 tcltk::tkgrid(FILELIST)
                                 tcltk::tkgrid(COMBINE.BTN, CANCEL.BTN)
                               })
  IMPORT.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","import.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Import", compound = "top", command = function() {})
  REMOVE.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","rm.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Remove", compound = "top", command = function() {})
  REFRESH.BTN <- tcltk::tkbutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","refresh.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Refresh (F5)", compound = "top", command = function() refresh.cmd())
  # ...Create menubuttons
  # .......Transformations:
  TRANS.MBTN <- tcltk::tkmenubutton(NOTEBOOK$DATA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","transform.gif", package = "trident")), height = 50, relief = "flat",
                                    text = "Transform", compound = "top")
  TRANS.MENU <- tcltk::tkmenu(TRANS.MBTN)
  tcltk::tkconfigure(TRANS.MBTN, menu = TRANS.MENU)
  tcltk::tkadd(TRANS.MENU, "command", label = "Boxcox transformation...",
               command = function(){
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
                 WIN2589 <- tcltk::tktoplevel()
                 YLIST <- tcltk2::tk2listbox(WIN2589, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                 # ...buttons
                 OK.BTN <- tcltk2::tk2button(WIN2589, text = "OK", command = function(){
                   #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
                   Myfactor <- tcltk2::selection(YLIST)
                   Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])
                   colnames(Numerics$boxcox) <- paste(colnames(Numerics$boxcox), "boxcox", sep = ".")
                   PROJECT$DATASET <<- data.frame(Factors, Numerics$boxcox)
                   tcltk::tkdestroy(WIN2589)
                   refresh.cmd()})
                 CANCEL.BTN <- tcltk2::tk2button(WIN2589, text = "Cancel", command = function() tcltk::tkdestroy(WIN2589))
                 # ...grid all
                 tcltk::tkgrid(tcltk::tklabel(WIN2589, text = "Choose factor"), padx = 5, pady = 5)
                 tcltk::tkgrid(YLIST, columnspan = 2, padx = 5, pady = 5)
                 tcltk::tkgrid(OK.BTN, CANCEL.BTN, padx = 5, pady = 5)


               })
  tcltk::tkadd(TRANS.MENU, "command", label = "Remove outliers...", command = function(){})
  # ...Grid all
  tcltk::tkgrid(OPEN.BTN, COMBINE.BTN, BUILD.BTN, IMPORT.BTN, REMOVE.BTN, TRANS.MBTN, REFRESH.BTN, padx = 5, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips
  tcltk2::tk2tip(OPEN.BTN, "Open data...")
  tcltk2::tk2tip(BUILD.BTN, "Build new dataset...")
  tcltk2::tk2tip(COMBINE.BTN, "Combine two or more datasets...")
  tcltk2::tk2tip(IMPORT.BTN, "Import variable...")
  tcltk2::tk2tip(REMOVE.BTN, "Remove variable...")
  # ...Shortcuts
  tcltk::tkbind(WIN,"<F5>", function() refresh.cmd())

  # --Notetab 'Statistics'----
  NOTEBOOK$STATS <- tcltk2::tk2notetab(NOTEBOOK, "Statistics")
  # ...Create buttons
  SUM.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","summary.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Summary", compound = "top", command = function(){
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
                                WIN2259 <- tcltk::tktoplevel()
                                WIN2259$FACTORLIST <- tcltk2::tk2listbox(WIN2259, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2259, text = "OK", command = function(){
                                  Myfactor <- Factors[, tcltk2::selection(WIN2259$FACTORLIST)]
                                  tcltk::tkdestroy(WIN2259)
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
                                  # Build window with table
                                  WIN5566 <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- summary"))
                                  build.table.cmd(Mytable, WIN5566)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2259, text = "Cancel", command = function() tcltk::tkdestroy(WIN2259))
                                tcltk::tkgrid(tcltk::tklabel(WIN2259, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN2259$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = FALSE)
                              })
  DISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Discriminant", compound = "top", command = function(){

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
                                WIN2259 <- tcltk::tktoplevel()
                                WIN2259$FACTORLIST <- tcltk2::tk2listbox(WIN2259, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2259, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN2259$FACTORLIST)
                                  tcltk::tkdestroy(WIN2259)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == TRUE)])
                                  # Build window with table
                                  WIN5566 <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- List of discriminant variables"))
                                  build.table.cmd(Mytable, WIN5566)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2259, text = "Cancel", command = function() tcltk::tkdestroy(WIN2259))
                                tcltk::tkgrid(tcltk::tklabel(WIN2259, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN2259$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = FALSE)
                              })
  NONDISC.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Non-discriminant", compound = "top", command = function(){
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
                                WIN2259 <- tcltk::tktoplevel()
                                WIN2259$FACTORLIST <- tcltk2::tk2listbox(WIN2259, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2259, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN2259$FACTORLIST)
                                  tcltk::tkdestroy(WIN2259)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(Variable = Mycheck$variable[which(Mycheck$is.discriminant == FALSE)])
                                  # Build window with table
                                  WIN5566 <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- List of non-discriminant variables"))
                                  build.table.cmd(Mytable, WIN5566)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2259, text = "Cancel", command = function() tcltk::tkdestroy(WIN2259))
                                tcltk::tkgrid(tcltk::tklabel(WIN2259, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN2259$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = FALSE)
                              })
  MULTI.BTN <- tcltk::tkbutton(NOTEBOOK$STATS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","multicheck.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Multicheck", compound = "top", command = function(){
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
                                WIN2259 <- tcltk::tktoplevel()
                                WIN2259$FACTORLIST <- tcltk2::tk2listbox(WIN2259, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2259, text = "OK", command = function(){
                                  Myfactor <- tcltk2::selection(WIN2259$FACTORLIST)
                                  tcltk::tkdestroy(WIN2259)
                                  Mycheck <- trident::multicheck(df = Numerics, y = Factors[, Myfactor])
                                  Mytable <- data.frame(unlist(Mycheck))
                                  # Build window with table
                                  WIN5566 <<- tcltk::tktoplevel()
                                  tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- Multicheck"))
                                  build.table.cmd(Mytable, WIN5566)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
                                  tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
                                })
                                CANCEL.BTN <- tcltk2::tk2button(WIN2259, text = "Cancel", command = function() tcltk::tkdestroy(WIN2259))
                                tcltk::tkgrid(tcltk::tklabel(WIN2259, text = "Choose factor"), columnspan = 2)
                                tcltk::tkgrid(WIN2259$FACTORLIST, columnspan = 2)
                                tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = TRUE)
                                tcltk::tcl("wm", "attributes", WIN2259, topmost = FALSE)
                              })
  # ...Grid all
  tcltk::tkgrid(SUM.BTN, MULTI.BTN, DISC.BTN, NONDISC.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips

  # --Notetab 'Variables'----
  NOTEBOOK$VARIA <- tcltk2::tk2notetab(NOTEBOOK, "Variables")
  # ...Create menubuttons
  # .......Arrange by:
  ARRNG.MBTN <- tcltk::tkmenubutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","arrange.gif", package = "trident")), height = 50, relief = "flat", text = "Arrange by", compound = "top")
  ARRNG.MENU <- tcltk::tkmenu(ARRNG.MBTN)
  tcltk::tkconfigure(ARRNG.MBTN, menu = ARRNG.MENU)
  tcltk::tkadd(ARRNG.MENU, "command", label = "F-stat...", command = function() {
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "f.stat")
      tcltk::tkdestroy(WIN5885)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by F-stat"))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)
  })
  tcltk::tkadd(ARRNG.MENU, "command", label = "ANOVA P-value...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "aov.p.value")
      tcltk::tkdestroy(WIN5885)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by ANOVA's p-value"))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)
    })
  tcltk::tkadd(ARRNG.MENU, "command", label = "Kruskall P-value...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "k.p.value")
      tcltk::tkdestroy(WIN5885)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by Kruskall's p-value"))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)  })
  tcltk::tkadd(ARRNG.MENU, "command", label = "Mean HSD P-value...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "mean.hsd.p.value")
      tcltk::tkdestroy(WIN5885)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by mean HSD p-value"))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)

  })
  tcltk::tkadd(ARRNG.MENU, "command", label = "Mean LSD P-value...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "mean.lsd.p.value")
      tcltk::tkdestroy(WIN5885)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by mean LSD p-value"))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)
  })
  tcltk::tkadd(ARRNG.MENU, "command", label = "HSD P-value by group priority...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...combobox for group priority
    Mygroups <- NULL
    GROUP.CMBBX <- tcltk2::tk2combobox(WIN5885, values = "Mygroups")
    tcltk::tkbind(FACTOR.CMBBX,"<<ComboboxSelected>>", function() {
      # ......Groups of pair-wise comparisons
      y <- Factors[, tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))]
      Mygroups <- c()
      Mygroups$AB <- paste(levels(y)[1], levels(y)[2], sep = "-")
      Mygroups$AC <- paste(levels(y)[1], levels(y)[3], sep = "-")
      Mygroups$BC <- paste(levels(y)[2], levels(y)[3], sep = "-")
      if (length(levels(y)) > 3) {
        Mygroups$AD <- paste(levels(y)[1], levels(y)[4], sep = "-")
        Mygroups$BD <- paste(levels(y)[2], levels(y)[4], sep = "-")
        Mygroups$CD <- paste(levels(y)[3], levels(y)[4], sep = "-")
        if (length(levels(y)) > 4) {
          Mygroups$AE <- paste(levels(y)[1], levels(y)[5], sep = "-")
          Mygroups$BE <- paste(levels(y)[2], levels(y)[5], sep = "-")
          Mygroups$CE <- paste(levels(y)[3], levels(y)[5], sep = "-")
          Mygroups$DE <- paste(levels(y)[4], levels(y)[5], sep = "-")
          if (length(levels(y)) > 5) {
            Mygroups$AF <- paste(levels(y)[1], levels(y)[6], sep = "-")
            Mygroups$BF <- paste(levels(y)[2], levels(y)[6], sep = "-")
            Mygroups$CF <- paste(levels(y)[3], levels(y)[6], sep = "-")
            Mygroups$DF <- paste(levels(y)[4], levels(y)[6], sep = "-")
            Mygroups$EF <- paste(levels(y)[5], levels(y)[6], sep = "-")
          }
        }
      }
      tcltk::tkconfigure(GROUP.CMBBX, values = c(unlist(Mygroups)))
    })
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......group priority
      if (is.null(Mygroups)) {Mypriority <- NULL}
      if (!is.null(Mygroups)) {
        Mypriority <- names(c(unlist(Mygroups)))[tcltk::tcl(GROUP.CMBBX, "current")]
      }
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "hsd.p.value", gp.priority = Mypriority)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by HSD p-value - priority:", c(unlist(Mygroups))[tcltk::tcl(GROUP.CMBBX, "current")]))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
      tcltk::tkdestroy(WIN5885)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Give priority to one category?  "), GROUP.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)
  })
  tcltk::tkadd(ARRNG.MENU, "command", label = "LSD P-value by group priority...", command = function(){
    # ...a window to select the factor
    WIN5885 <- tcltk::tktoplevel()
    Mydf <- PROJECT$DATASET
    Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
    Mydf <- stats::na.omit(Mydf)
    Numerics <- dplyr::select_if(Mydf, is.numeric)
    Factors  <- dplyr::select_if(Mydf, is.factor)
    # ...combobox for factor choice
    FACTOR.CMBBX <- tcltk2::tk2combobox(WIN5885, values = colnames(Factors))
    # ...combobox for group priority
    Mygroups <- NULL
    GROUP.CMBBX <- tcltk2::tk2combobox(WIN5885, values = "Mygroups")
    tcltk::tkbind(FACTOR.CMBBX,"<<ComboboxSelected>>", function() {
      # ......Groups of pair-wise comparisons
      y <- Factors[, tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))]
      Mygroups <- c()
      Mygroups$AB <- paste(levels(y)[1], levels(y)[2], sep = "-")
      Mygroups$AC <- paste(levels(y)[1], levels(y)[3], sep = "-")
      Mygroups$BC <- paste(levels(y)[2], levels(y)[3], sep = "-")
      if (length(levels(y)) > 3) {
        Mygroups$AD <- paste(levels(y)[1], levels(y)[4], sep = "-")
        Mygroups$BD <- paste(levels(y)[2], levels(y)[4], sep = "-")
        Mygroups$CD <- paste(levels(y)[3], levels(y)[4], sep = "-")
        if (length(levels(y)) > 4) {
          Mygroups$AE <- paste(levels(y)[1], levels(y)[5], sep = "-")
          Mygroups$BE <- paste(levels(y)[2], levels(y)[5], sep = "-")
          Mygroups$CE <- paste(levels(y)[3], levels(y)[5], sep = "-")
          Mygroups$DE <- paste(levels(y)[4], levels(y)[5], sep = "-")
          if (length(levels(y)) > 5) {
            Mygroups$AF <- paste(levels(y)[1], levels(y)[6], sep = "-")
            Mygroups$BF <- paste(levels(y)[2], levels(y)[6], sep = "-")
            Mygroups$CF <- paste(levels(y)[3], levels(y)[6], sep = "-")
            Mygroups$DF <- paste(levels(y)[4], levels(y)[6], sep = "-")
            Mygroups$EF <- paste(levels(y)[5], levels(y)[6], sep = "-")
          }
        }
      }
      tcltk::tkconfigure(GROUP.CMBBX, values = c(unlist(Mygroups)))
    })
    # ...checkbuttons for options
    BOXCOX.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "BoxCox transformation?")
    tcltk::tkconfigure(BOXCOX.CHKBTN, variable = PROJECT$OPTIONS$BOXCOX.VALUE)
    DIXON.CHKBTN <- tcltk2::tk2checkbutton(WIN5885, text = "Remove outliers?")
    tcltk::tkconfigure(DIXON.CHKBTN, variable = PROJECT$OPTIONS$DIXON.VALUE)
    # ...OK button
    OK.BTN <- tcltk2::tk2button(WIN5885, text = "OK", command = function(){
      Myfactor <- tcltk::tclvalue(tcltk::tkget(FACTOR.CMBBX))
      # ......options
      if (tcltk::tclvalue(PROJECT$OPTIONS$BOXCOX.VALUE) == 1) {
        Numerics <- trident::trident.boxcox(df = Numerics, y = Factors[, Myfactor])$boxcox
        colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")
      }
      if (tcltk::tclvalue(PROJECT$OPTIONS$DIXON.VALUE) == 1) {}
      # ......group priority
      if (is.null(Mygroups)) {Mypriority <- NULL}
      if (!is.null(Mygroups)) {
        Mypriority <- names(c(unlist(Mygroups)))[tcltk::tcl(GROUP.CMBBX, "current")]
      }
      # ......arrange
      Mytable <- trident::trident.arrange(df = Numerics, y = Mydf[, Myfactor], by = "lsd.p.value", gp.priority = Mypriority)
      # ......display in new window
      WIN5566 <- tcltk::tktoplevel()
      tcltk::tkwm.title(WIN5566, paste("trident", METADATA$VERSION, "- arranged by LSD p-value - priority:", c(unlist(Mygroups))[tcltk::tcl(GROUP.CMBBX, "current")]))
      build.table.cmd(Mytable, WIN5566)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = TRUE)
      tcltk::tcl("wm", "attributes", WIN5566, topmost = FALSE)
      tcltk::tkdestroy(WIN5885)
    })
    CANCEL.BTN <- tcltk2::tk2button(WIN5885, text = "Cancel", command = function() tcltk::tkdestroy(WIN5885))
    # ...grid all
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Choose factor:  "), FACTOR.CMBBX)
    tcltk::tkgrid(tcltk::tklabel(WIN5885, text = "Give priority to one category?  "), GROUP.CMBBX)
    tcltk::tkgrid(BOXCOX.CHKBTN)
    tcltk::tkgrid(DIXON.CHKBTN)
    tcltk::tkgrid(OK.BTN, CANCEL.BTN)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = TRUE)
    tcltk::tcl("wm", "attributes", WIN5885, topmost = FALSE)
  })
  # ...Create buttons
  #TAG.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","tag.gif", package = "trident")), height = 50, relief = "flat",
  #                            text = "Tag", compound = "top", command = function(){})
  TOP3.BTN <- tcltk::tkbutton(NOTEBOOK$VARIA, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), height = 50, relief = "flat",
                             text = "Top 3", compound = "top", command = function(){})
  # ...Grid all
  tcltk::tkgrid(ARRNG.MBTN, TOP3.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips

  # --Notetab 'Plots'----
  NOTEBOOK$PLOTS <- tcltk2::tk2notetab(NOTEBOOK, "Plots")
  # ...Create buttons
  BIPLOT.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","biplot.gif", package = "trident")), height = 50, relief = "flat",
                              text = "Biplot", compound = "top", command = function() {
                                # ...a window to select x, y and the factor
                                WIN2 <- tcltk::tktoplevel()
                                Mydf <- PROJECT$DATASET
                                Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                Mydf <- stats::na.omit(Mydf)
                                Numerics <- dplyr::select_if(Mydf, is.numeric)
                                Factors  <- dplyr::select_if(Mydf, is.factor)
                                Mydf <- data.frame(Factors, Numerics)
                                Myx <- NULL
                                Myy <- NULL
                                Myfactor <- NULL
                                XLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                YLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                FACTORLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                # ...OK button
                                OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function() {
                                  Myx <<- colnames(Numerics)[tcltk2::selection(XLIST)]
                                  Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
                                  Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
                                  tcltk::tkdestroy(WIN2)
                                  # ...Biplot window
                                  WIN3 <<- tcltk::tktoplevel()
                                  tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                  tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                  WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                  WIN3$SAVE <- tcltk::tkframe(WIN3)
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

                                  TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function() {graphics::plot(Plot)})
                                  # ...Create buttons
                                  SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function() {
                                    ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(Myx, "_vs_", Myy, sep =), defaultextension = ".png")), plot = Plot)
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
                                text = "Boxplot", compound = "top", command = function() {
                                  # ...a window to select y and the factor
                                  WIN2 <- tcltk::tktoplevel()
                                  Mydf <- PROJECT$DATASET
                                  Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                  Mydf <- stats::na.omit(Mydf)
                                  Numerics <- dplyr::select_if(Mydf, is.numeric)
                                  Factors  <- dplyr::select_if(Mydf, is.factor)
                                  Mydf <- data.frame(Factors, Numerics)
                                  Myy <- NULL
                                  Myfactor <- NULL
                                  YLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  FACTORLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  # ...Jiggerplot checkbutton
                                  JIGGER.CHKBTN <- tcltk2::tk2checkbutton(WIN2, text = "Jiggerplot")
                                  tcltk::tkconfigure(JIGGER.CHKBTN, variable = PROJECT$OPTIONS$JIGGER.VALUE)
                                  # ...OK button
                                  OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function() {
                                    Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
                                    Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
                                    tcltk::tkdestroy(WIN2)
                                    # ...Biplot window
                                    WIN3 <<- tcltk::tktoplevel()
                                    tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                    tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                    WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                    WIN3$SAVE <- tcltk::tkframe(WIN3)
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

                                    TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function() {graphics::plot(Plot)})
                                    # ...Create buttons
                                    SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function() {
                                      ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(Myy, "_vs_", Myfactor, sep =), defaultextension = ".png")), plot = Plot)
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
                                  tcltk::tkgrid(JIGGER.CHKBTN, columnspan = 2)
                                  tcltk::tkgrid(OK.BTN, CANCEL.BTN)
                                })
  VIOLIN.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","violin.gif", package = "trident")), height = 50, relief = "flat",
                                text = "Violin", compound = "top", command = function() {

                                  # ...a window to select x and the factor
                                  WIN2 <- tcltk::tktoplevel()
                                  Mydf <- PROJECT$DATASET
                                  Mydf <- Mydf[!is.infinite(rowSums(dplyr::select_if(Mydf, is.numeric))), ]
                                  Mydf <- stats::na.omit(Mydf)
                                  Numerics <- dplyr::select_if(Mydf, is.numeric)
                                  Factors  <- dplyr::select_if(Mydf, is.factor)
                                  Mydf <- data.frame(Factors, Numerics)
                                  Myy <- NULL
                                  Myfactor <- NULL
                                  YLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Numerics), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  FACTORLIST <- tcltk2::tk2listbox(WIN2, values = colnames(Factors), selectmode = "single", height = 12, tip = "", scroll = "y", autoscroll = "x", enabled = TRUE)
                                  # ...OK button
                                  OK.BTN <- tcltk2::tk2button(WIN2, text = "OK", command = function() {
                                    Myy <<- colnames(Numerics)[tcltk2::selection(YLIST)]
                                    Myfactor <<- colnames(Factors)[tcltk2::selection(FACTORLIST)]
                                    tcltk::tkdestroy(WIN2)
                                    # ...Biplot window
                                    WIN3 <<- tcltk::tktoplevel()
                                    tcltk::tkconfigure(WIN3, borderwidth = 10, bg = "tan")
                                    tcltk::tkwm.title(WIN3, paste("trident", METADATA$VERSION, "- biplot"))
                                    WIN3$PLOT <<- tcltk::tkframe(WIN3)
                                    WIN3$SAVE <- tcltk::tkframe(WIN3)
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

                                    TKPLOT <- tkrplot::tkrplot(WIN3$PLOT, fun = function() {graphics::plot(Plot)})
                                    # ...Create buttons
                                    SAVEBUTTON <- tcltk::tkbutton(WIN3$SAVE, text = "SAVE", justify = "left", width = 10, command = function() {
                                      ggplot2::ggsave(file = tcltk::tclvalue(tcltk::tkgetSaveFile(parent = WIN3, title = "Save plot as...", initialfile = paste(Myy, "_vs_", Myfactor, sep =), defaultextension = ".png")), plot = Plot)
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
                                text = "PCA", compound = "top", command = function() {})
  DFA.BTN <- tcltk::tkbutton(NOTEBOOK$PLOTS, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","dfa.gif", package = "trident")), height = 50, relief = "flat",
                                text = "DFA", compound = "top", command = function() {})
  # ...Grid all
  tcltk::tkgrid(BIPLOT.BTN, BOXPLOT.BTN, VIOLIN.BTN, PCA.BTN, DFA.BTN, padx = 0, pady = 10, ipadx = 5, ipady = 10, sticky = "ns")
  # ...Tooltips


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
  # TKGUI - WRAPPER----
  tcltk::tcl("wm", "attributes", WIN, topmost = TRUE)
  tcltk::tcl("wm", "attributes", WIN, topmost = FALSE)
  tcltk::tkbind(OPEN.BTN,"<Destroy>", function() quit.cmd())
}
