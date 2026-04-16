
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{trident}`

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Installation

You can install the development version of `{trident}` like so:

… To complete

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
#> [1] "2026-04-16 15:01:45 CEST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading trident
#> Warning: remplacement de l'importation précédente 'shiny::runExample' par
#> 'shinyjs::runExample' lors du chargement de 'trident'
#> 
#> Attachement du package : 'shinyjs'
#> 
#> Les objets suivants sont masqués depuis 'package:methods':
#> 
#>     removeClass, show
#> ── R CMD check results ────────────────────────────────────── trident 2.0.0 ────
#> Duration: 5m 35.8s
#> 
#> ❯ checking for executable files ... WARNING
#>   Found the following executable file:
#>     inst/extdata/structure/prg/prg.exe
#>   Source packages should not contain undeclared executable files.
#>   See section 'Package structure' in the 'Writing R Extensions' manual.
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   File
#>     LICENSE
#>   is not mentioned in the DESCRIPTION file.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   Found the following assignments to the global environment:
#>   Fichier 'trident/R/mod_BatchAnalysis.R' :
#>     assign(input$name, data.frame(batchData$data), envir = .GlobalEnv)
#>   Fichier 'trident/R/mod_DataSet.R' :
#>     assign(input$name, data.frame(v$data), envir = .GlobalEnv)
#>   Fichier 'trident/R/mod_Multivariate.R' :
#>     assign(paste0(exportName$name), myPlot, envir = .GlobalEnv)
#>   Fichier 'trident/R/mod_Univariate.R' :
#>     assign(paste0(exportName$name), myPlot, envir = .GlobalEnv)
#>   Fichier 'trident/R/mod_Variables.R' :
#>     assign(input$name, data.frame(v$data), envir = .GlobalEnv)
#> 
#> 0 errors ✔ | 1 warning ✖ | 3 notes ✖
#> Error:
#> ! R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> trident Coverage: 35.27%
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
#> R/utils.R: 9.68%
#> R/graphics.R: 93.85%
#> R/statistics.R: 94.12%
#> R/analyses.R: 95.57%
```
