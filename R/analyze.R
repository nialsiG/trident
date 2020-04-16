# trident.boxcox----
#' @title trident.boxcox
#' @description Transforms x according to the Box-Cox formula, using the parameter lambda
#' @param df A vector or a dataframe of numeric values
#' @param y A factor
#' @param list.lambda Logical, if TRUE the list of lambda values is added to the results
#' @return A list containing a dataframe of the transformed values
#' @details to do
#' @examples
#' #to do
#' @export
trident.boxcox <- function(df, y, list.lambda = FALSE) {
  #BEFORE, check data structure:
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(as.factor(y), df)
  Mydf <- Mydf[!is.infinite(rowSums(Mydf[, -1])),]
  Mydf <- stats::na.omit(Mydf)

  #Data translation above zero:
  data.translation <- function(x) return(x - min(x) + 1)
  Mydf[, -1] <- plyr::colwise(data.translation)(data.frame(Mydf[, -1]))

  #Boucle de transformation BoxCox:
  Myboxcox <- Mydf[, -1]
  Mylambda <- NULL
  for(i in c(1:ncol(Mydf[, -1]))) {
    #Lambda evaluation on a large range first:
    aov_k <- stats::formula(Mydf[, i + 1] ~ Mydf[, 1])
    bc1 <- MASS::boxcox(aov_k, lambda = seq(-5, 5, 1./100), data = Mydf, plotit = FALSE)
    #NOTE: routine de départ explorait entre -6 et 6, mais la plupart des boxcox supposent un lambda entre -5 et 5, ce qui semble suffisant
    #Refine lambda values with a smaller range:
    range_lambda <- range(bc1$x[bc1$y > max(bc1$y) - qchisq(0.95, 1) / 2] )
    range_lambda <- seq(range_lambda[1], range_lambda[2], 1./100)
    bc2 <- MASS::boxcox(aov_k, lambda = range_lambda, data = Mydf, plotit = FALSE)
    #Determine lambda:
    range_lambda <- range(bc2$x[bc2$y > max(bc2$y) - qchisq(0.95, 1) / 2] )
    Mylambda[i] <- round(mean(range_lambda), digits = 1)
    #Boxcox transformation:
    if (Mylambda[i] == 0) Myboxcox[, i] <- log(Mydf[, i + 1])
    else Myboxcox[, i] <- (Mydf[, i + 1] ^ Mylambda[i] - 1) / Mylambda[i]
  }
  Results <- list()
  Results$boxcox <- Myboxcox
  if (list.lambda == TRUE) Results$lambda <- Mylambda
  return(Results)
}

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
    if (by[h] == "aov.p.value") Argmt.factor <- c(Argmt.factor, 'k.aov.value')
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


# multicheck----
#' @title multicheck
#' @description Perform various statistical checks
#' @param df A dataframe or a subset of a dataset entirely composed of numeric variables
#' @param y A factor
#' @param check.anderson Logical indicating whether an Anderson-Darling test (normality) should be performed
#' @param check.anova Logical indicating whether the p-value of an ANOVA should be calculated
#' @param check.bartlett Logical indicating whether a Bartlett test (homoscedasticity) should be performed
#' @param check.kruskal Logical indicating whether the p-value of Kruskall-Wallis should be calculated
#' @param check.levene Logical indicating whether a Levene test (homoscedasticity) should be performed
#' @param check.shapiro Logical indicating whether a Shapiro-Wilk test (normality) should be performed
#' @param check.skwratio Logical indicating whether skewness ratio should be calculated
#' @param check.varratio Logical indicating whether variance ratio should be calculated
#' @return A list of ratios and p-values for all the options which where selected using check.- parameters
#' @examples
#' #to do
#' @export
multicheck <- function(df, y, check.anderson = TRUE, check.anova = TRUE, check.bartlett = TRUE, check.kruskal = TRUE,
                       check.levene = TRUE, check.shapiro = TRUE, check.skwratio = TRUE, check.varratio = TRUE) {
  # BEFORE, check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  # Parameters:
  P.ANDERSON <- NULL
  P.ANOVA <- NULL
  P.BARTLETT <- NULL
  P.KRUSKAL <- NULL
  P.LEVENE <- NULL
  P.SHAPIRO <- NULL
  SKW.RATIO <- NULL
  VAR.RATIO <- NULL
  VARIABLES <- c(colnames(df))

  # pvalue ANOVA
  if (check.anova == TRUE) {
    for (i in 1:length(colnames(df))) {
      P.ANOVA[i] <- stats::oneway.test(formula = df[, i] ~ y, data = df, subset = NULL, na.action = "na.omit")$p.value
    }
  }

  # pvalue kruskal-wallis
  if(check.kruskal == TRUE) {
    for (i in 1:length(colnames(df))) {
      P.KRUSKAL[i] <- stats::kruskal.test(df[, i] ~ y)$p.value
    }
  }

  # Variables based on AOV residuals:
  for(i in  1:length(colnames(df))) {
    Myvariable <- df[, i]
    Myvariable <- Myvariable[Myvariable != Inf & Myvariable != -Inf & is.na(Myvariable) == FALSE & is.nan(Myvariable) == FALSE]
    Myfactor <- y[which(Myvariable != Inf & Myvariable != -Inf & is.na(Myvariable) == FALSE & is.nan(Myvariable) == FALSE)]
    Residuals <- aov(Myvariable ~ Myfactor)$residuals #Residuals of AOV

    # pvalue Anderson-Darling
    if (check.anderson == TRUE) {
      P.ANDERSON[i] <- nortest::ad.test(Residuals)$p.value
    }

    # pvalue Bartlett
    if (check.bartlett == TRUE) {
      P.BARTLETT[i] <- bartlett.test(Residuals~Myfactor)$p.value

    }

    # pvalue Levene
    if (check.levene == TRUE) {
      P.LEVENE[i] <- car::leveneTest(Residuals, Myfactor, center = "median")$`Pr(>F)`[1]
    }

    # pvalue Shapiro-wilk
    if (check.shapiro == TRUE) {
      P.SHAPIRO[i] <- shapiro.test(Residuals)$p.value
    }

    # Skewness ratio
    if (check.skwratio == TRUE) {
      # unbiased skewness with its confidence interval - bootstrap strategy
      Skewness = DescTools::Skew(x = Residuals, method = 3, conf.level = 0.95, ci.type = "bca", R = 1000)
      # skewness divided by its confidence interval
      SKW.RATIO[i] = abs(Skewness[[1]] / (Skewness[[3]] - Skewness[[2]]))
      }

    # Variance ratio
    if (check.varratio == TRUE) {

      VariancePerCategory <- NULL
      for (j in 1:length(levels(Myfactor))) {
        VariancePerCategory[j] <- stats::var(Residuals[Myfactor == levels(Myfactor)[j]])
      }
      VAR.RATIO[i] <- max(VariancePerCategory) / min(VariancePerCategory)
    }
  }

  Results <- list(variable = VARIABLES,
                  anderson_p.value = P.ANDERSON,
                  anova_p.value = P.ANOVA,
                  bartlett_p.value = P.BARTLETT,
                  kruskall_p.value = P.KRUSKAL,
                  levene_p.value = P.LEVENE,
                  shapiro_p.value = P.SHAPIRO,
                  skewness.ratio = SKW.RATIO,
                  variance.ratio = VAR.RATIO)
  return(Results)
}

# rm.outliers----
#' @title rm.outliers
#' @description Short description of fun
#' @param x x
#' @return the result of fun
#' @examples
#' #to do
#' @export
rm.outliers <- function(x){



}

