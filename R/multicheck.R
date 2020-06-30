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
                       check.levene = TRUE, check.shapiro = TRUE, check.skwratio = TRUE, check.varratio = TRUE, alpha = 0.05) {
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
  IS.NORMAL <- NULL
  IS.HOMOSCEDASTIC <- NULL
  IS.DISCRIMINANT <- NULL
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
  #Decision tree
  if (is.null(SKW.RATIO) == FALSE & is.null(P.SHAPIRO) == FALSE) {
    IS.NORMAL[P.SHAPIRO < alpha] <- TRUE
    IS.NORMAL[P.SHAPIRO >= alpha & SKW.RATIO < 2] <- "nearly"
    IS.NORMAL[P.SHAPIRO >= alpha & SKW.RATIO >= 2] <- FALSE
    if (is.null(P.BARTLETT) == FALSE & is.null(P.LEVENE) == FALSE & is.null(VAR.RATIO) == FALSE) {
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT < alpha] <- TRUE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT >= alpha & VAR.RATIO < 3] <- TRUE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT >= alpha & VAR.RATIO >= 3] <- FALSE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & IS.HOMOSCEDASTIC == FALSE & P.LEVENE < alpha] <- "nearly"
      if (is.null(P.ANOVA) == FALSE) {
        IS.DISCRIMINANT[((IS.NORMAL == TRUE & IS.HOMOSCEDASTIC == "nearly") | (IS.HOMOSCEDASTIC == TRUE & IS.NORMAL == "nearly")) & P.ANOVA < alpha] <- TRUE
        IS.DISCRIMINANT[(IS.HOMOSCEDASTIC == TRUE | IS.HOMOSCEDASTIC == "nearly") & P.KRUSKAL < alpha] <- TRUE
        IS.DISCRIMINANT[IS.HOMOSCEDASTIC == FALSE | is.na(IS.HOMOSCEDASTIC) == TRUE | P.ANOVA >= alpha & P.KRUSKAL >= alpha | (is.na(P.ANOVA) == TRUE & is.na(P.KRUSKAL) == TRUE )] <- FALSE
      }
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
                  variance.ratio = VAR.RATIO,
                  is.normal = IS.NORMAL,
                  is.homoscedastic = IS.HOMOSCEDASTIC,
                  is.discriminant = IS.DISCRIMINANT)
  return(Results)
}
