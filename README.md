
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{trident}`

A library for dental microwear texture analysis, allowing to further
analyze the results using a variety of methods. The package includes
function to measure microwear texture, to transform the data, to rank
variables, as well as a complete shiny app.

Note: The core executable (`prg.exe` or `main`) is automatically
selected and downloaded according to the operating system (Linux or
Windows).

The executable (fortran binaries) both for Linux and Windows can be
found here: [Github repository for
executables](https://github.com/TRIBO-Pprime/TRIDENT_V1)

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Installation

You can install `{trident}` in a safe, isolated environment using
`{renv}`:

``` r
dir.create("test-trident")
setwd("test-trident")

install.packages("renv")
renv::init()
```

It is recommended to create a new empty RStudio Project for this folder.
Once the project is created, verify that it is correctly linked to the
isolated environment

``` r
.libPaths()
```

This should return a project-specific (local) library path rather than
your global R library.

------------------------------------------------------------------------

You can then install `{trident}` from the provided archive:

``` r
renv::install("path/to/trident_2.0.0.tar.gz")
```

## Run

You can launch the application by running:

``` r
trident::run_app()
```

## About

You are reading the doc about version : 2.0.0

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-04-21 16:23:27 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading trident
#> ── R CMD check results ────────────────────────────────────── trident 2.0.0 ────
#> Duration: 6m 56.6s
#> 
#> ❯ checking for executable files ... WARNING
#>   Found the following executable file:
#>     inst/extdata/structure/prg/main
#>   Source packages should not contain undeclared executable files.
#>   See section ‘Package structure’ in the ‘Writing R Extensions’ manual.
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   Found the following assignments to the global environment:
#>   Fichier ‘trident/R/mod_BatchAnalysis.R’ :
#>     assign(input$name, data.frame(batchData$data), envir = .GlobalEnv)
#>   Fichier ‘trident/R/mod_DataSet.R’ :
#>     assign(input$name, data.frame(v$data), envir = .GlobalEnv)
#>   Fichier ‘trident/R/mod_Multivariate.R’ :
#>     assign(paste0(exportName$name), myPlot, envir = .GlobalEnv)
#>   Fichier ‘trident/R/mod_Univariate.R’ :
#>     assign(paste0(exportName$name), myPlot, envir = .GlobalEnv)
#>   Fichier ‘trident/R/mod_Variables.R’ :
#>     assign(input$name, data.frame(v$data), envir = .GlobalEnv)
#> 
#> 0 errors ✔ | 1 warning ✖ | 2 notes ✖
#> Error:
#> ! R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> trident Coverage: 34.97%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/golem_utils_server.R: 0.00%
#> R/golem_utils_ui.R: 0.00%
#> R/mod_BatchAnalysis.R: 0.00%
#> R/mod_DataSet.R: 0.00%
#> R/mod_Multivariate.R: 0.00%
#> R/mod_Univariate.R: 0.00%
#> R/mod_Variables.R: 0.00%
#> R/run_app.R: 0.00%
#> R/utils.R: 24.44%
#> R/graphics.R: 93.82%
#> R/statistics.R: 94.12%
#> R/analyses.R: 96.88%
```
