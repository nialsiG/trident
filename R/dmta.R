# dmta.spatial----
#' @title dmta.spatial
#' @description Description
#' @param sur surface file
#' @param size.x total x length of the surface, in pixels
#' @param size.y total y length of the surface, in pixels
#' @param size.n in case type = "multi", the number of cells for the calculation of heterogeneity
#' @param type character vector indicating whether a single value should be calculated for the whole surface ("single"); or if multiple values should be calculated for each n cells ("multi")
#' @return the result of fun
#' @examples
#' #to do
#' @export
dmta.spatial <- function(sur, size.x = 256, size.y = 256, size.n = 256, type = "single") {
  #1-Preparation
  tmp <- tempdir()
  Tmp <- stringr::str_replace_all(tmp, "\\\\", "/")
  file.copy(system.file("extdata", "structure", "prg", "prg.exe", package = "trident"), Tmp)
  utils::write.table("", file = file.path(Tmp, "deroulement.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "sortie.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_spatial.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #2-Recoding
  Sur <- stringr::str_replace_all(sur, "\\\\", "/")
  Sample.size <- "-0001"
  if (type != "single") {
    if (type == "multi") {
      Sample.size <- paste0("+", size.n)
    }
    else {
      stop("type must be 'single' or 'multi")
    }
  }

  #3-Build the script
  Myscript <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'elli_acv'
    '", file.path(Tmp, "results_spatial.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript, file = file.path(Tmp, "spatial_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #4-Run the script
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "spatial_analysis.f90")))

  #5-Scan and return the results
  Scanned <- scan(file = file.path(Tmp, "results_spatial.txt"), what = "character()")
  Filename <- paste0(utils::tail(unlist(strsplit(Scanned[1:which(stringr::str_detect(Scanned, ".sur"))], "/")), which(stringr::str_detect(Scanned, ".sur"))), collapse = " ")
  Results <- t(matrix(Scanned[-(1:which(stringr::str_detect(Scanned, ".sur")))], nrow = 16))
  Spatial <- list(file = Filename,
              type = "spatial",
              rmax = as.numeric(Results[, 1]),
              sal = as.numeric(Results[, 2]),
              stri = as.numeric(Results[, 3]),
              std = as.numeric(Results[, 4]),
              b.sl = as.numeric(Results[, 6]),
              r.sl = as.numeric(Results[, 7]),
              s.sl = as.numeric(Results[, 5]))
  names(Spatial) <- c("File", "Type", "Rmax", "Sal", "Stri", "Std", "b.sl", "r.sl", "s.sl")
  return(Spatial)
}

# dmta.asfc----
#' @title dmta.asfc
#' @description Description
#' @param sur surface file
#' @param size.x total x length of the surface, in pixels
#' @param size.y total y length of the surface, in pixels
#' @param size.n in case type = "multi", the number of cells for the calculation of heterogeneity
#' @param type character vector indicating whether a single value should be calculated for the whole surface ("single"); or if multiple values should be calculated for each n cells ("multi")
#' @return the result of fun
#' @examples
#' #to do
#' @export
dmta.asfc <- function(sur, size.x = 256, size.y = 256, size.n = 256, type = "single") {
  #1-Preparation
  tmp <- tempdir()
  Tmp <- stringr::str_replace_all(tmp, "\\\\", "/")
  file.copy(system.file("extdata", "structure", "prg", "prg.exe", package = "trident"), Tmp)
  utils::write.table("", file = file.path(Tmp, "deroulement.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "sortie.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_complex.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #2-Recoding
  Sur <- stringr::str_replace_all(sur, "\\\\", "/")
  Sample.size <- "-0001"
  if (type != "single") {
    if (type == "multi") {
      Sample.size <- paste0("+", size.n)
    }
    else {
      stop("type must be 'single' or 'multi")
    }
  }
  #3-Build the script
  Myscript <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'complexi'
    '", file.path(Tmp, "results_complex.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript, file = file.path(Tmp, "complex_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #4-Run the script
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "complex_analysis.f90")))

  #5-Scan and return the results
  Scanned <- scan(file = file.path(Tmp, "results_complex.txt"), what = "character()")
  Filename <- paste0(utils::tail(unlist(strsplit(Scanned[1:which(stringr::str_detect(Scanned, ".sur"))], "/")), which(stringr::str_detect(Scanned, ".sur"))), collapse = " ")
  Results <- t(matrix(Scanned[-(1:which(stringr::str_detect(Scanned, ".sur")))], nrow = 6))
  Asfc <- list(file = Filename, type = "complexity", asfc = - 1000 * as.numeric(Results[, 2]))
  names(Asfc) <- c("File", "Type", "Asfc")
  return(Asfc)
}

# dmta.height----
#' @title dmta.height
#' @description Description
#' @param sur surface file
#' @param size.x total x length of the surface, in pixels
#' @param size.y total y length of the surface, in pixels
#' @param size.n in case type = "multi", the number of cells for the calculation of heterogeneity
#' @param type character vector indicating whether a single value should be calculated for the whole surface ("single"); or if multiple values should be calculated for each n cells ("multi")
#' @return the result of fun
#' @examples
#' #to do
#' @export
dmta.height <- function(sur, size.x = 256, size.y = 256, size.n = 256, type = "single") {

  #1-Preparation
  tmp <- tempdir()
  Tmp <- stringr::str_replace_all(tmp, "\\\\", "/")
  file.copy(system.file("extdata", "structure", "prg", "prg.exe", package = "trident"), Tmp)
  utils::write.table("", file = file.path(Tmp, "deroulement.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "sortie.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_height.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_faces.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #2-Recoding
  Sur <- stringr::str_replace_all(sur, "\\\\", "/")
  Sample.size <- "-0001"
  if (type != "single") {
    if (type == "multi") {
      Sample.size <- paste0("+", size.n)
    }
    else {
      stop("type must be 'single' or 'multi")
    }
  }
  #3-Build the scripts
  #...script 01
  Myscript1 <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'statisti'
    '", file.path(Tmp, "results_height.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript1, file = file.path(Tmp, "height_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #...script 02
  Myscript2 <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'facettes'
    '", file.path(Tmp, "results_faces.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript2, file = file.path(Tmp, "facet_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)

  #4-Run the scripts
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "height_analysis.f90")))
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "facet_analysis.f90")))

  #5-Scan and return the results
  Scanned <- scan(file = file.path(Tmp, "results_height.txt"), what = "character()")
  Scanned2 <- scan(file = file.path(Tmp, "results_faces.txt"), what = "character()")
  Results <- t(matrix(Scanned[-(1:which(stringr::str_detect(Scanned, ".sur")))], nrow = 19))
  Results2 <- t(matrix(Scanned2[-(1:which(stringr::str_detect(Scanned2, ".sur")))], nrow = 12))
  Filename <- paste0(utils::tail(unlist(strsplit(Scanned[1:which(stringr::str_detect(Scanned, ".sur"))], "/")), which(stringr::str_detect(Scanned, ".sur"))), collapse = " ")
  Height <- list(file = Filename,
                 type = "height",
                 Sa = as.numeric(Results[, 4]),
                 Sp = as.numeric(Results[, 2]),
                 Sq = as.numeric(Results[, 6]),
                 Sv = as.numeric(Results[, 1]),
                 Ssk = as.numeric(Results[, 7]),
                 Sku = as.numeric(Results[, 8]),
                 Sdar = as.numeric(Results2[, 2]),
                 Sm = as.numeric(Results[, 5]),
                 Smd = as.numeric(Results[, 3]))
  names(Height) <- c("File", "Type", "Sa", "Sp", "Sq", "Sv", "Ssk", "Sku", "Sdar", "Sm", "Smd")
  return(Height)
}

# dmta.topology----
#' @title dmta.topology
#' @description Description
#' @param sur surface file
#' @param size.x total x length of the surface, in pixels
#' @param size.y total y length of the surface, in pixels
#' @param size.n in case type = "multi", the number of cells for the calculation of heterogeneity
#' @param type character vector indicating whether a single value should be calculated for the whole surface ("single"); or if multiple values should be calculated for each n cells ("multi")
#' @return the result of fun
#' @examples
#' #to do
#' @export
dmta.topology <- function(sur, size.x = 256, size.y = 256, size.n = 256, type = "single") {
  #1-Preparation
  tmp <- tempdir()
  Tmp <- stringr::str_replace_all(tmp, "\\\\", "/")
  file.copy(system.file("extdata", "structure", "prg", "prg.exe", package = "trident"), Tmp)
  utils::write.table("", file = file.path(Tmp, "deroulement.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "sortie.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_topology.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results_faces.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  #2-Recoding
  Sur <- stringr::str_replace_all(sur, "\\\\", "/")
  Sample.size <- "-0001"
  #version multi ne marche pas
  #if (type != "single") {
  #  if (type == "multi") {
  #    Sample.size <- paste0("+", size.n)
  #  }
  #  else {
  #    stop("type must be 'single' or 'multi")
  #  }
  #}
  #3-Build the scripts
  #...script 01
  Myscript1 <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'topology'
    '", file.path(Tmp, "results_topology.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript1, file = file.path(Tmp, "topology_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  #...script 02
  Myscript2 <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +006
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    TAB_BOOT",
    "\n+", size.x,
    "\n+", size.y,
    "\n", Sample.size,
    "\nSTA_LOOP
    +0001 ", Sample.size,
    "\nANALYSES
    'facettes'
    '", file.path(Tmp, "results_faces.txt"), "'
    .true.
    -50
    END_LOOP
    enddo
    FIN_____")
  utils::write.table(Myscript2, file = file.path(Tmp, "facet_analysis.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  #4-Run the scripts
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "topology_analysis.f90")))
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "facet_analysis.f90")))
  #5-Scan and return the results
  Scanned <- scan(file = file.path(Tmp, "results_topology.txt"), what = "character()")
  Scanned2 <- scan(file = file.path(Tmp, "results_faces.txt"), what = "character()")
  Results <- t(matrix(Scanned[-(1:which(stringr::str_detect(Scanned, ".sur")))], nrow = 8))
  Results2 <- t(matrix(Scanned2[-(1:which(stringr::str_detect(Scanned2, ".sur")))], nrow = 12))
  Filename <- paste0(utils::tail(unlist(strsplit(Scanned[1:which(stringr::str_detect(Scanned, ".sur"))], "/")), which(stringr::str_detect(Scanned, ".sur"))), collapse = " ")
  Topology <- list(file = Filename,
                   type = "topology",
                   Sk1 = as.numeric(Results[, 3]),
                   Sk2 = as.numeric(Results[, 6]),
                   Scm1 = as.numeric(Results[, 2]),
                   Scm2 = as.numeric(Results[, 5]),
                   Snb1 = as.numeric(Results[, 1]),
                   Snb2 = as.numeric(Results[, 4]),
                   Sh = as.numeric(Results2[, 1]))
  # The f90 routine exports results such as NAs or Inf using unrecognized suit of characters, which need to be replaced by NAs here:
  Topology$Sk1[which(!is.finite(Topology$Sk1))] <- 0
  Topology$Sk2[which(!is.finite(Topology$Sk2))] <- 0
  Topology$Scm1[which(!is.finite(Topology$Scm1))] <- 0
  Topology$Scm2[which(!is.finite(Topology$Scm2))] <- 0
  Topology$Snb1[which(!is.finite(Topology$Snb1))] <- 0
  Topology$Snb2[which(!is.finite(Topology$Snb2))] <- 0

  names(Topology) <- c("File", "Type", "Sk1", "Sk2", "Scm1", "Scm2", "Snb1", "Snb2", "Sh")
  return(Topology)
}

# polynom.sur----
#' @title polynom.sur
#' @description Description
#' @param sur surface file
#' @param deg the polynom degree to be substracted to the surface
#' @param path logical, indicates if the path of the cleaned surface should be returned as a character string in R
#' @param copy logical, indicates if a copy of the cleaned .sur file should be made in the working directory. If FALSE, it remains in a temporary file
#' @return a cleaned surface, with a path to its current location if path = TRUE (default)
#' @examples
#' #to do
#' @export
polynom.sur <- function(sur, deg = 8, path = TRUE, copy = FALSE) {

  #1-Preparation
  tmp <- tempdir()
  Tmp <- stringr::str_replace_all(tmp, "\\\\", "/")
  file.copy(system.file("extdata", "structure", "prg", "prg.exe", package = "trident"), Tmp)
  file.copy(from = sur, to = Tmp)
  utils::write.table("", file = file.path(Tmp, "deroulement.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "sortie.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  utils::write.table("", file = file.path(Tmp, "results.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  Name <- unlist(strsplit(utils::tail(unlist(strsplit(sur, "\\\\")), 1), ".sur"))
  Sur <- file.path(Tmp, Name, ".sur")
  Sur <- stringr::str_replace_all(sur, "\\\\", "/")

  # Build the script
  Myscript <- paste0(
    "do
    VERBOSES
    .false.
    .false.
    '", file.path(Tmp, "deroulement.txt"), "'
    +066
    '", file.path(Tmp, "sortie.txt"), "'
    DEBUT___
    NB_PROCS
    -1
    LECT_BAS
    '", Sur, "'
    .false.
    .false.
    SMOOTH__
    LSSQ_IMG
    +000
    +000
    '", file.path(Tmp, "results.txt"), "'
    ", paste0("+", deg), " ", paste0("+", deg), "
    .true.
    .false.
    enddo
    FIN_____")


  # Write the script to the Tmp directory
  utils::write.table(Myscript, file = file.path(Tmp, "polynom.f90"), quote = FALSE, row.names = FALSE, col.names = FALSE)
  # Run the script
  system(paste(sep = " ", file.path(Tmp, "prg.exe"), file.path(Tmp, "polynom.f90")))

  # ...return path
  if (path == TRUE) {
    Path <- file.path(Tmp, paste0(Name, "_X1_00", deg, "_Y1_00", deg,".sur"))
    return(Path)
  }
}



# trident.hetero----
#' @title trident.hetero
#' @description Description
#' @param x A numeric vector
#' @param var.name A character string defining the name of the variable from which heterogeneity should be computed; default 'X'
#' @return A data frame of parameters assessing heterogeneity
#' @examples
#' #to do
#' @export
trident.hetero <- function(x, var.name ="X") {
  if (!is.numeric(x)) stop("x has to be a numeric vector")
  if (!is.vector(x)) stop("x has to be a numeric vector")
  Results <- data.frame(min = min(x, na.rm = TRUE),
                        max = max(x, na.rm = TRUE),
                        sd = stats::sd(x, na.rm = TRUE),
                        mean = mean(x, na.rm = TRUE),
                        median = as.numeric(stats::quantile(x, 0.5, na.rm = TRUE)),
                        fst.05 = as.numeric(stats::quantile(x, 0.05, na.rm = TRUE)),
                        lst.05 = as.numeric(stats::quantile(x, 0.95, na.rm = TRUE)),
                        min.05 = mean(x[which(x <= stats::quantile(x, 0.05, na.rm = TRUE))], na.rm = TRUE),
                        max.05 = mean(x[which(x >= stats::quantile(x, 0.95, na.rm = TRUE))], na.rm = TRUE),
                        fst.25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
                        lst.25 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE)),
                        min.25 = mean(x[which(x <= stats::quantile(x, 0.25, na.rm = TRUE))], na.rm = TRUE),
                        max.25 = mean(x[which(x >= stats::quantile(x, 0.75, na.rm = TRUE))], na.rm = TRUE),
                        skw = DescTools::Skew(x, na.rm = TRUE),
                        kurt = DescTools::Kurt(x, na.rm = TRUE))
  return(Results)
}