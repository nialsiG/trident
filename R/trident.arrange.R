# trident.arrange----
#' @title trident.arrange
#' @description Classify variables by their ability to discriminate categories
#' @param df A vector or a dataframe of numeric values
#' @return the result of fun
#' @examples
#' #to do
#' @export
trident.arrange <- function(df, y, by = "hsd.mean.p.value", alpha = 0.05, byngr = FALSE, geomean = FALSE, gp.priority = c(1:length(levels(y)))) {
  #check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #changing levels of factor if gp.priority
  y <- factor(y, levels = levels(y)[gp.priority])

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(y = as.factor(y), df)
  Mydf <- Mydf[!is.infinite(rowSums(Mydf[, -1])), ]
  Mydf <- stats::na.omit(Mydf)

  #2A Compute ANOVA's F & P
  compute.aov <- function(x, y) {
    Myaov <- stats::oneway.test(x ~ y)
    F_AOV <- as.numeric(Myaov$statistic)
    P_AOV <- Myaov$p.value
    return(c(F_AOV, P_AOV))
  }
  AOV <- t(plyr::colwise(compute.aov)(Mydf[, -1], y = Mydf[, 1]))
  colnames(AOV) <- c("f.stat", "aov.p.value")

  #2B Compute Kruskal's K & P
  compute.ktest <- function(x, y) {
    Myktest <- stats::kruskal.test(x ~ y)
    K_KTEST <- as.numeric(Myktest$statistic)
    P_KTEST <- Myktest$p.value
    return(c(K_KTEST, P_KTEST))
  }
  KTEST <- t(plyr::colwise(compute.ktest)(Mydf[,-1], y = Mydf[, 1]))
  colnames(KTEST) <- c("k", "k.p.value")

  #2C Compute HSD's p-value (using package 'DescTools')
  compute.hsd <- function(x, y){
    Myhsd <- DescTools::PostHocTest(x = stats::aov(x ~ y), method = "hsd", conf.level = NA)
    Myhsd <- c(stats::na.omit(unlist(Myhsd)))
    mean.signif.hsd <- mean(Myhsd[which(Myhsd < alpha)])
    geomean.signif.hsd <- exp(mean(log(Myhsd[which(Myhsd < alpha)])))
    number.sign.grp <- length(Myhsd[which(Myhsd < alpha)])
    return(c(Myhsd, mean.signif.hsd, geomean.signif.hsd, number.sign.grp))
  }
  HSD <- t(plyr::colwise(compute.hsd)(Mydf[, -1], y = Mydf[, 1]))

  #2D Compute LSD's p-value (using package 'DescTools')
  compute.lsd <- function(x, y){
    Mylsd <- DescTools::PostHocTest(x = stats::aov(x ~ y), method = "lsd", conf.level = NA)
    Mylsd <- c(stats::na.omit(unlist(Mylsd)))
    mean.signif.lsd <- mean(Mylsd[which(Mylsd < alpha)])
    geomean.signif.lsd <- exp(mean(log(Mylsd[which(Mylsd < alpha)])))
    number.sign.grp <- length(Mylsd[which(Mylsd < alpha)])
    return(c(Mylsd, mean.signif.lsd, geomean.signif.lsd, number.sign.grp))
  }
  LSD <- t(plyr::colwise(compute.lsd)(Mydf[, -1], y = Mydf[, 1]))

  # 3 Groupnames
  Groupnames <- NULL
  for (i in c(1:length(levels(y)) - 1)) {
    Right <- levels(y)[-(0:i)]
    Left <- rep(levels(y)[i], length(Right))
    Groupnames <- c(Groupnames, paste0(Left, ".vs.", Right))
  }
  # ...note: we have got to remove the first element of vector 'Groupnames' (".vs.")
  Groupnames <- Groupnames[-1]
  # ...then give column names to HSD and LSD
  colnames(HSD) <- paste0("hsd.", c(Groupnames, "mean", "geomean", "signif.groups"))
  colnames(LSD) <- paste0("lsd.", c(Groupnames, "mean", "geomean", "signif.groups"))

  #4 Arrange by
  # ...group all in a big table
  Mytable <- data.frame(AOV, KTEST, HSD, LSD)
  # ...now arrange by factor
  # ......aov
  if (by == "f.stat") Mynewtable <- Mytable[with(Mytable, order(f.stat, decreasing = TRUE)), ]
  if (by == "aov.p.value") Mynewtable <- Mytable[with(Mytable, order(aov.p.value, decreasing = FALSE)), ]
  # ......k
  if (by == "k") Mynewtable <- Mytable[with(Mytable, order(k, decreasing = TRUE)), ]
  if (by == "k.p.value") Mynewtable <- Mytable[with(Mytable, order(k.p.value, decreasing = FALSE)), ]
  # ......hsd
  if (by == "hsd.p.value") Mynewtable <- Mytable[with(Mytable, order(Mytable[, which(colnames(Mytable) == "k.p.value") + 1],
                                                                     Mytable[, which(colnames(Mytable) == "k.p.value") + 2],
                                                                     decreasing = c(FALSE, FALSE))), ]
  if (by == "hsd.mean.p.value") {
    if (byngr == FALSE & geomean == FALSE) Mynewtable <- Mytable[with(Mytable, order(hsd.mean, decreasing = FALSE)), ]
    if (byngr == TRUE & geomean == FALSE) Mynewtable <- Mytable[with(Mytable, order(hsd.signif.groups, hsd.mean, decreasing = c(TRUE, FALSE))), ]
    if (byngr == FALSE & geomean == TRUE) Mynewtable <- Mytable[with(Mytable, order(hsd.geomean, decreasing = FALSE)), ]
    if (byngr == TRUE & geomean == TRUE) Mynewtable <- Mytable[with(Mytable, order(hsd.signif.groups, hsd.geomean, decreasing = c(TRUE, FALSE))), ]
  }
  # ......lsd
  if (by == "lsd.p.value") Mynewtable <- Mytable[with(Mytable, order(Mytable[, which(colnames(Mytable) == "hsd.signif.groups") + 1],
                                                                     Mytable[, which(colnames(Mytable) == "hsd.signif.groups") + 2],
                                                                     decreasing = c(FALSE, FALSE))), ]
  if (by == "lsd.mean.p.value") {
    if (byngr == FALSE & geomean == FALSE) Mynewtable <- Mytable[with(Mytable, order(lsd.mean, decreasing = FALSE)), ]
    if (byngr == TRUE & geomean == FALSE) Mynewtable <- Mytable[with(Mytable, order(lsd.signif.groups, lsd.mean, decreasing = c(TRUE, FALSE))), ]
    if (byngr == FALSE & geomean == TRUE) Mynewtable <- Mytable[with(Mytable, order(lsd.geomean, decreasing = FALSE)), ]
    if (byngr == TRUE & geomean == TRUE) Mynewtable <- Mytable[with(Mytable, order(lsd.signif.groups, lsd.geomean, decreasing = c(TRUE, FALSE))), ]
  }
  return(Mynewtable)
}
