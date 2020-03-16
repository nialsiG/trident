# Graphical User Interface----
#' @title grazr
#' @description Open the grazr graphical user interface.
#' @export
grazr <- function(){
  # Tcltk Objects: Metadata----
  METADATA <- list(VERSION = '0.1.0', DESCRIPTION = "Dusty Sandbox")
  # Tkgui Main Window----
  WIN <<- tcltk::tktoplevel()
  tcltk::tkconfigure(WIN, borderwidth = 10, width = 300, bg = "tan")
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkwm.title(WIN, paste("grazr", METADATA$VERSION))
  tcltk2::tk2ico.setFromFile(WIN, system.file("extdata","pics","mini_grazr.ico", package = "grazr"))
  # Basic layout:
  WIN$MENU <- tcltk2::tk2menu(WIN)
  tcltk::tkconfigure(WIN, menu = WIN$MENU)

  WIN$NOTEBOOK <- tcltk2::tk2notebook(WIN, tabs = c("Collect", "Analyze", "Visualize"))
  tcltk::tkpack(WIN$NOTEBOOK, side = "left", fill = "both" , expand = TRUE)

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


  # Notetab 'collect'----
  WIN$NOTEBOOK$VIS <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Collect")

  # Notetab 'analysis'----
  WIN$NOTEBOOK$VIS <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Analysis")

  # Notetab 'visualize'----
  WIN$NOTEBOOK$VIS <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Visualize")
  # Create buttons
  BOXPLOT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, text = " Boxplot ", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "grazr")), compound = "top", width = 60, command = function(){})
  BIPLOT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$VIS, text = " Biplot ", image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "grazr")), compound = "top", width = 60, command = function(){})
  # Grid all
  tcltk::tkgrid(tcltk::tklabel(WIN$NOTEBOOK$VIS, text = "Plots..."), padx = 5, pady = 5, columnspan = 6)
  tcltk::tkgrid(BOXPLOT.BTN, BIPLOT.BTN, padx = 5, pady = 5, columnspan = 3)
  tcltk::tkgrid(tcltk2::tk2separator(WIN$NOTEBOOK$VIS, orientation = "horizontal"), ipadx = 180, columnspan = 6)
  # Tooltips
  tcltk2::tk2tip(BOXPLOT.BTN, "...")
  tcltk2::tk2tip(BIPLOT.BTN, "...")










  }
