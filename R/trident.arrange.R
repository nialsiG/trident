# trident.arrange----
#' @title trident.arrange
#' @description Classify variables by their ability to discriminate categories
#' @param df A vector or a dataframe of numeric values
#' @return the result of fun
#' @examples
#' #to do
#' @export
trident.arrange <- function(df, y, by = "mean.hsd.p.value", gp.priority = NULL) {

  # BEFORE, check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(y = as.factor(y), df)
  Mydf <- Mydf[!is.infinite(rowSums(Mydf[, -1])),]
  Mydf <- stats::na.omit(Mydf)

  #Groups of pair-wise comparisons
  Names <- NULL
  Names$AB <- paste(levels(y)[1], levels(y)[2], sep = "-")
  Names$AC <- paste(levels(y)[1], levels(y)[3], sep = "-")
  Names$BC <- paste(levels(y)[2], levels(y)[3], sep = "-")
  if (length(levels(y)) > 3) {
    Names$AD <- paste(levels(y)[1], levels(y)[4], sep = "-")
    Names$BD <- paste(levels(y)[2], levels(y)[4], sep = "-")
    Names$CD <- paste(levels(y)[3], levels(y)[4], sep = "-")
    if (length(levels(y)) > 4) {
      Names$AE <- paste(levels(y)[1], levels(y)[5], sep = "-")
      Names$BE <- paste(levels(y)[2], levels(y)[5], sep = "-")
      Names$CE <- paste(levels(y)[3], levels(y)[5], sep = "-")
      Names$DE <- paste(levels(y)[4], levels(y)[5], sep = "-")
      if (length(levels(y)) > 5) {
        Names$AF <- paste(levels(y)[1], levels(y)[6], sep = "-")
        Names$BF <- paste(levels(y)[2], levels(y)[6], sep = "-")
        Names$CF <- paste(levels(y)[3], levels(y)[6], sep = "-")
        Names$DF <- paste(levels(y)[4], levels(y)[6], sep = "-")
        Names$EF <- paste(levels(y)[5], levels(y)[6], sep = "-")
      }
    }
  }
  #Recoding gp.priority
  if (is.null(gp.priority) == FALSE) {
    Gp.Priority <- NULL
    for (g in 1:length(gp.priority)){
      Gp.Priority[g] <- Names[unlist(gp.priority[g])]
    }
  }

  #2A Compute ANOVA's F & P
  compute.aov <- function(x, y) {
    Myaov <- stats::oneway.test(x ~ y)
    F_AOV <- as.numeric(Myaov$statistic)
    P_AOV <- Myaov$p.value
    return(c(F_AOV, P_AOV))
  }
  AOV <- t(plyr::colwise(compute.aov)(Mydf[,-1], y = Mydf[, 1]))
  AOV <- data.frame(variable = rownames(AOV), f.stat = AOV[, 1], aov.p.value = AOV[, 2], row.names = NULL)

  #2B Compute Kruskal's K & P
  compute.ktest <- function(x, y) {
    Myktest <- stats::kruskal.test(x ~ y)
    K_KTEST <- as.numeric(Myktest$statistic)
    P_KTEST <- Myktest$p.value
    return(c(K_KTEST, P_KTEST))
  }
  KTEST <- t(plyr::colwise(compute.ktest)(Mydf[,-1], y = Mydf[, 1]))
  KTEST <- data.frame(variable = rownames(KTEST), k = KTEST[, 1], k.p.value = KTEST[, 2], row.names = NULL)

  #2C Compute HSD's p-value (using package 'DescTools')
  compute.hsd <- function(x, y){
    Myhsd <- DescTools::PostHocTest(x = stats::aov(x ~ y), method = "hsd", conf.level = NA)
    P_HSD <- NULL
    P_HSD$ab <- Myhsd$y[1, 1]
    P_HSD$ac <- Myhsd$y[2, 1]
    P_HSD$bc <- Myhsd$y[2, 2]
    if (length(levels(y)) > 3) {
      P_HSD$ad <- Myhsd$y[3, 1]
      P_HSD$bd <- Myhsd$y[3, 2]
      P_HSD$cd <- Myhsd$y[3, 3]
      if (length(levels(y)) > 4) {
        P_HSD$ae <- Myhsd$y[4, 1]
        P_HSD$be <- Myhsd$y[4, 2]
        P_HSD$ce <- Myhsd$y[4, 3]
        P_HSD$de <- Myhsd$y[4, 4]
        if (length(levels(y)) > 5) {
          P_HSD$af <- Myhsd$y[5, 1]
          P_HSD$bf <- Myhsd$y[5, 2]
          P_HSD$cf <- Myhsd$y[5, 3]
          P_HSD$df <- Myhsd$y[5, 4]
          P_HSD$ef <- Myhsd$y[5, 5]
        }
      }
    }
    return(unlist(P_HSD))
  }
  HSD <- t(plyr::colwise(compute.hsd)(Mydf[, -1], y = Mydf[, 1]))
  HSD <- data.frame(variable = rownames(HSD), HSD, row.names = NULL)
  colnames(HSD) <- c("variable", paste0("hsd.p.", Names, sep = ""))
  HSD$mean.hsd.p.value <- apply(HSD[, -1], 1, mean)


  #2D Compute LSD's p-value (using package 'DescTools')
  compute.lsd <- function(x, y){
    Mylsd <- DescTools::PostHocTest(x = stats::aov(x ~ y), method = "lsd", conf.level = NA)
    P_LSD <- NULL
    P_LSD$ab <- Mylsd$y[1, 1]
    P_LSD$ac <- Mylsd$y[2, 1]
    P_LSD$bc <- Mylsd$y[2, 2]
    if (length(levels(y)) > 3) {
      P_LSD$ad <- Mylsd$y[3, 1]
      P_LSD$bd <- Mylsd$y[3, 2]
      P_LSD$cd <- Mylsd$y[3, 3]
      if (length(levels(y)) > 4) {
        P_LSD$ae <- Mylsd$y[4, 1]
        P_LSD$be <- Mylsd$y[4, 2]
        P_LSD$ce <- Mylsd$y[4, 3]
        P_LSD$de <- Mylsd$y[4, 4]
        if (length(levels(y)) > 5) {
          P_LSD$af <- Mylsd$y[5, 1]
          P_LSD$bf <- Mylsd$y[5, 2]
          P_LSD$cf <- Mylsd$y[5, 3]
          P_LSD$df <- Mylsd$y[5, 4]
          P_LSD$ef <- Mylsd$y[5, 5]
        }
      }
    }
    return(unlist(P_LSD))
  }
  LSD <- t(plyr::colwise(compute.lsd)(Mydf[, -1], y = Mydf[, 1]))
  LSD <- data.frame(variable = rownames(LSD), LSD, row.names = NULL)
  colnames(LSD) <- c("variable", paste0("lsd.p.", Names, sep = ""))
  LSD$mean.lsd.p.value <- apply(LSD[, -1], 1, mean)

  #3 Group all in a big table
  Mytable <- dplyr::inner_join(AOV, KTEST, by = "variable")
  Mytable <- dplyr::inner_join(Mytable, HSD, by = "variable")
  Mytable <- dplyr::inner_join(Mytable, LSD, by = "variable")

  #4 Arrangement factor
  Argmt.factor <- NULL
  for (h in 1:length(by)) {
    if (by[h] == "k") Argmt.factor <- c(Argmt.factor, 'k')
    if (by[h] == "k.p.value") Argmt.factor <- c(Argmt.factor, 'k.p.value')
    if (by[h] == "f.stat") Argmt.factor <- c(Argmt.factor, 'f.stat')
    if (by[h] == "aov.p.value") Argmt.factor <- c(Argmt.factor, 'aov.p.value')
    if (by[h] == "hsd.p.value") {
      if (is.null(gp.priority) == TRUE) Argmt.factor <- c(Argmt.factor, colnames(HSD[, -1]))
      else if (is.null(gp.priority) == FALSE) {
        vec.tmp <- NULL
        for (i in 1:length(Gp.Priority)) {
          vec.tmp[i] <- paste("hsd.p.", Gp.Priority[i], sep = "")
        }
        Argmt.factor <- c(Argmt.factor,  vec.tmp)
      }
    }
    if (by[h] == "lsd.p.value") {
      if (is.null(gp.priority) == TRUE) Argmt.factor <- c(Argmt.factor, colnames(LSD[, -1]))
      else if (is.null(gp.priority) == FALSE) {
        vec.tmp <- NULL
        for (j in 1:length(Gp.Priority)) {
          vec.tmp[j] <- paste("lsd.p.", Gp.Priority[j], sep = "")
        }
        Argmt.factor <- c(Argmt.factor,  vec.tmp)
      }
    }
    if (by[h] == "mean.hsd.p.value") Argmt.factor <- c(Argmt.factor, "mean.hsd.p.value")
    if (by[h] == "mean.lsd.p.value") Argmt.factor <- c(Argmt.factor, "mean.lsd.p.value")
  }

  #5 Arrange variables by selected method (grouped in Argmt.factor)
  #Mynewtable <- plyr::arrange(Mytable, paste(Argmt.factor, collapse =", "))
  if (length(Argmt.factor) == 1) Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]])
  if (length(Argmt.factor) == 1 & Argmt.factor[1] == "k") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]))
  if (length(Argmt.factor) == 1 & Argmt.factor[1] == "f.stat") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]))

  if (length(Argmt.factor) == 2) Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], Mytable[, Argmt.factor[2]])
  if (length(Argmt.factor) == 2 & Argmt.factor[1] == "k") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]), Mytable[, Argmt.factor[2]])
  if (length(Argmt.factor) == 2 & Argmt.factor[1] == "f.stat") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]), Mytable[, Argmt.factor[2]])
  if (length(Argmt.factor) == 2 & Argmt.factor[2] == "k") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], plyr::desc(Mytable[, Argmt.factor[2]]))
  if (length(Argmt.factor) == 2 & Argmt.factor[2] == "f.stat") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], plyr::desc(Mytable[, Argmt.factor[2]]))

  if (length(Argmt.factor) >= 3) Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], Mytable[, Argmt.factor[2]], Mytable[, Argmt.factor[3]])
  if (length(Argmt.factor) >= 3 & Argmt.factor[1] == "k") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]), Mytable[, Argmt.factor[2]], Mytable[, Argmt.factor[3]])
  if (length(Argmt.factor) >= 3 & Argmt.factor[1] == "f.stat") Mynewtable <- plyr::arrange(Mytable, plyr::desc(Mytable[, Argmt.factor[1]]), Mytable[, Argmt.factor[2]], Mytable[, Argmt.factor[3]])
  if (length(Argmt.factor) >= 3 & Argmt.factor[2] == "k") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], plyr::desc(Mytable[, Argmt.factor[2]]), Mytable[, Argmt.factor[3]])
  if (length(Argmt.factor) >= 3 & Argmt.factor[2] == "f.stat") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], plyr::desc(Mytable[, Argmt.factor[2]]), Mytable[, Argmt.factor[3]])
  if (length(Argmt.factor) >= 3 & Argmt.factor[3] == "k") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], Mytable[, Argmt.factor[2]], plyr::desc(Mytable[, Argmt.factor[3]]))
  if (length(Argmt.factor) >= 3 & Argmt.factor[3] == "f.stat") Mynewtable <- plyr::arrange(Mytable, Mytable[, Argmt.factor[1]], Mytable[, Argmt.factor[2]], plyr::desc(Mytable[, Argmt.factor[3]]))

  return(Mynewtable)
}

