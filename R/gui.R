# Graphical User Interface----
#' @title trident
#' @description Open the trident graphical user interface.
#' @export
trident <- function(){
  # Tcltk Objects: Metadata----
  METADATA <- list(VERSION = '0.1.0', DESCRIPTION = "Dusty Sandbox")
  # Tkgui Main Window----
  WIN <<- tcltk::tktoplevel()
  tcltk::tkconfigure(WIN, borderwidth = 10, width = 300, bg = "tan")
  tcltk2::tk2theme(theme = "radiance")
  tcltk::tkwm.title(WIN, paste("trident", METADATA$VERSION))
  tcltk2::tk2ico.setFromFile(WIN, system.file("extdata","pics","mini_grazr.ico", package = "trident"))
  # Basic layout:
  WIN$MENU <- tcltk2::tk2menu(WIN)
  tcltk::tkconfigure(WIN, menu = WIN$MENU)
  WIN$NOTEBOOK <- tcltk2::tk2notebook(WIN, tabs = c("Microwear", "Data", "Organize", "Visualize", "Batch analysis"))
  tcltk::tkpack(WIN$NOTEBOOK, side = "left", fill = "both" , expand = TRUE)


  # Notetab 'Microwear'----
  WIN$NOTEBOOK$COL <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Microwear")


  # Notetab 'Data'----
  WIN$NOTEBOOK$DAT <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Data")
  # Create buttons
  OPEN.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","open.gif", package = "trident")), width = 50, height = 50, command = function(){})
  IMPORT.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$DAT, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  # Grid all
  tcltk::tkgrid(OPEN.BTN, IMPORT.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$DAT, orientation = "vertical"),
                padx = 5, pady = 5, sticky = "ns")
  # Tooltips
  tcltk2::tk2tip(OPEN.BTN, "Open...")
  tcltk2::tk2tip(IMPORT.BTN, "Import variable...")



  # Notetab 'Organize'----
  WIN$NOTEBOOK$ORG <- tcltk2::tk2notetab(WIN$NOTEBOOK, "Organize")
  # Create buttons
  NORM.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  HOMO.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  MULTI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  BOXCOX.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  ASSIGN.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  OUTLIERS.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  PVAL.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  DISCRI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","discrim.gif", package = "trident")), width = 50, height = 50, command = function(){})
  NONDISCRI.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","nondiscrim.gif", package = "trident")), width = 50, height = 50, command = function(){})
  AUTOTOP.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","top3.gif", package = "trident")), width = 50, height = 50, command = function(){})
  CLASSBY.BTN <- tcltk::tkbutton(WIN$NOTEBOOK$ORG, image = tcltk::tkimage.create("photo", file = system.file("extdata","pics","wip.gif", package = "trident")), width = 50, height = 50, command = function(){})
  # Grid all
  tcltk::tkgrid(NORM.BTN, HOMO.BTN, MULTI.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                BOXCOX.BTN, OUTLIERS.BTN, ASSIGN.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                PVAL.BTN, DISCRI.BTN, NONDISCRI.BTN,
                tcltk2::tk2separator(WIN$NOTEBOOK$ORG, orientation = "vertical"),
                AUTOTOP.BTN, CLASSBY.BTN,
                padx = 5, pady = 5, sticky = "ns")
  # Tooltips
  tcltk2::tk2tip(NORM.BTN, "Normality?")
  tcltk2::tk2tip(HOMO.BTN, "Homoscedasticity?")
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
