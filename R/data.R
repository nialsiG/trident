# exData----
#' @title exData
#' @description An example of microwear dataset
exData <- read.csv(system.file("extdata", "data", "example-data.csv", package = "trident"))
usethis::use_data(exData, overwrite = TRUE)
'exData'
