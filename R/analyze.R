# grazr.boxcox----
#' @title dustyboxcox
#' @description Transforms x according to the Box-Cox formula, using the parameter lambda
#' @param df A vector or a dataframe of numeric values
#' @param lambda lambda
#' @return A vector of transformed values
#' @details to do
#' @examples
#' #to do
#' @export
grazr.boxcox <- function(x, lambda) {
  #preparation of dataset: removal of Na, NaN, Inf and -Inf
  Mydf <- data.frame(x)
  InfRemoved <- Mydf[!is.infinite(rowSums(Mydf)),]
  NaRemoved <- na.omit(InfRemoved)

  #data translation
  data.translation <- function(y) {
    Translated <- y + min(y) + 1
    return(Translated)
  }
  ReadyForBoxcox <- plyr::colwise(data.translation)(data.frame(NaRemoved))
  #boxcox transformation
  if (lambda == 0) return(log(ReadyForBoxcox))
  else return((ReadyForBoxcox^lambda - 1) / lambda)
}

# grazr.classify----
#' @title grazr.classify
#' @description Classify variables by their ability to discriminate categories
#' @param df A vector or a dataframe of numeric values
#' @return the result of fun
#' @examples
#' #to do
#' @export
grazr.arrange <- function(df, y, is.parametric = TRUE, method = "p.value"){

  #BEFORE, check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #1 Data transformation
  Variables <- c(colnames(df))
  Mydf <- data.frame(as.factor(y), df)
  Mydf <- stats::na.omit(Mydf)

  #2A Compute ANOVA's F & P
  if (is.parametric == TRUE) {
    compute.aov <- function(x, y){
      Myaov <- stats::oneway.test(x ~ y)
      F_AOV <- as.numeric(Myaov$statistic)
      P_AOV <- Myaov$p.value
      return(c(F_AOV, P_AOV))
    }
    Stats <- t(plyr::colwise(compute.stats)(Mydf[,-1], y = Mydf[, 1]))
    Stats <- data.frame(variable = rownames(Stats), F_stat = Stats[, 1], p.value = Stats[, 2], row.names = NULL)
  }

  #2B Compute Kruskal's K & P
  if (is.parametric == FALSE) {
    compute.ktest <- function(x, y){
      Myktest <- stats::kruskal.test(x ~ y)
      K_KTEST <- as.numeric(Myktest$statistic)
      P_KTEST <- Myktest$p.value
      return(c(K_KTEST, P_KTEST))
    }
    Stats <- t(plyr::colwise(compute.ktest)(Mydf[,-1], y = Mydf[, 1]))
    Stats <- data.frame(variable = rownames(Stats), K_stat = Stats[, 1], p.value = Stats[, 2], row.names = NULL)
  }

  #3 Arrange variables by selected method
  if (method == "statistic") Stats <- plyr::arrange(Stats, plyr::desc(Stats[, 2]))
  if (method == "p.value") Stats <- plyr::arrange(Stats, Stats[, 3])

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

