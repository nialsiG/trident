#' @title trident
#' @description A package for statistical processing of DMTA files
#' @docType package
#' @name trident
#' @importFrom plyr "colwise"
#' @importFrom dplyr "bind_cols"
#' @importFrom dplyr "union"
#' @importFrom dplyr "select_if"
#' @importFrom car "leveneTest"
#' @importFrom DescTools "Skew"
#' @importFrom DescTools "Kurt"
#' @importFrom DescTools "PostHocTest"
#' @importFrom doSNOW "registerDoSNOW"
#' @importFrom DT "datatable"
#' @importFrom FactoMineR "PCA"
#' @importFrom foreach "%dopar%"
#' @importFrom foreach "%:%"
#' @importFrom foreach "foreach"
#' @importFrom MASS "boxcox"
#' @importFrom nortest "ad.test"
#' @importFrom parallel "detectCores"
#' @importFrom snow "makeSOCKcluster"
#' @importFrom stats "var"
#' @importFrom stats "kruskal.test"
#' @importFrom stats "aov"
#' @importFrom stats "qchisq"
#' @importFrom stats "formula"
#' @importFrom stats "bartlett.test"
#' @importFrom stats "shapiro.test"
#' @importFrom stats "oneway.test"
#' @importFrom stats "sd"
#' @importFrom stats "na.omit"
#' @importFrom stats "quantile"
#' @importFrom stringr "str_replace_all"
#' @importFrom stringr "str_detect"
#' @importFrom utils "winProgressBar"
#' @importFrom utils "write.table"
#' @importFrom utils "tail"
#' @importFrom utils "combn"
#' @importFrom ggpubr "stat_chull"
#' @import factoextra
#' @import ggplot2
#' @import shiny
#' @import shinyjs
#' @import shinyFiles
#' @importMethodsFrom foreach
#' @importFrom picante "cor.table"
