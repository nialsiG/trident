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
trident.arrange <- function(df, y, by = "p.value", method = "lsd"){

  #BEFORE, check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(as.factor(y), df)
  Mydf <- Mydf[!is.infinite(rowSums(Mydf[, -1])),]
  Mydf <- stats::na.omit(Mydf)

  #2A Compute ANOVA's F & P
    compute.aov <- function(x, y){
      Myaov <- stats::oneway.test(x ~ y)
      F_AOV <- as.numeric(Myaov$statistic)
      P_AOV <- Myaov$p.value
      return(c(F_AOV, P_AOV))
    }

    AOV <- t(plyr::colwise(compute.aov)(Mydf[,-1], y = Mydf[, 1]))
    AOV <- data.frame(variable = rownames(AOV), F_stat = AOV[, 1], p.value = AOV[, 2], row.names = NULL)

  #2B Compute Kruskal's K & P
    compute.ktest <- function(x, y){
      Myktest <- stats::kruskal.test(x ~ y)
      K_KTEST <- as.numeric(Myktest$statistic)
      P_KTEST <- Myktest$p.value
      return(c(K_KTEST, P_KTEST))
    }
    K <- t(plyr::colwise(compute.ktest)(Mydf[,-1], y = Mydf[, 1]))
    K <- data.frame(variable = rownames(K), K_stat = K[, 1], p.value = K[, 2], row.names = NULL)

    #2C Compute HSD's p-value
    compute.aov <- function(x, y){
      Myaov <- stats::oneway.test(x ~ y)
      Myhsd <-
#      F_AOV <- as.numeric(Myaov$statistic)
#      P_AOV <- Myaov$p.value
      return(c(F_AOV, P_AOV))
    }

    HSD <- t(plyr::colwise(compute.aov)(Mydf[,-1], y = Mydf[, 1]))
    HSD <- data.frame(variable = rownames(AOV), F_stat = AOV[, 1], p.value = AOV[, 2], row.names = NULL)


  #3 Arrange variables by selected method
  if (method == "aov") Stats <- plyr::arrange(Stats, plyr::desc(Stats[, 2]))
  if (method == "k") Stats <- plyr::arrange(Stats, Stats[, 3])

  #4 Gestion de la priorité de groupe?
  #to do

  return(Stats)
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

