# multicheck----
#' @title multicheck2
#' @description Perform various statistical checks
#' @param df A dataframe or a subset of a dataset entirely composed of numeric variables
#' @param y A factor
#' @param alpha Significance level
#' @return A list of ratios and p-values for all the options which where selected using check.- parameters
#' @examples
#' #to do
#' @export
multicheck <- function(df, y, alpha = 0.05) {
  # BEFORE, check data structure
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(y = as.factor(y), df)
  Mydf <- Mydf[is.finite(rowSums(Mydf[, -1])), ]

  # Vectorization
  Mymatrix <- t(matrix(data = unlist(Mydf[, -1]), ncol = length(df[1, ])))
  Myfactor <- as.factor(Mydf[, 1])

  # Prepare parallel
  doSNOW::registerDoSNOW(snow::makeSOCKcluster(parallel::detectCores() - 1))

  # Parameters
  IS.NORMAL <- c(colnames(df))
  IS.HOMOSCEDASTIC <- c(colnames(df))
  IS.DISCRIMINANT <- c(colnames(df))
  VARIABLES <- c(colnames(df))
  #P.ANOVA <- foreach::foreach(i = 1:length(df[1, ])) %dopar% stats::oneway.test(formula = df[, i] ~ Myfactor, data = df, subset = NULL, na.action = "na.omit")$p.value
  P.ANOVA <- foreach::foreach(i = 1:length(Mymatrix[, 1]), .combine = "c") %dopar% stats::oneway.test(formula = Mymatrix[i, ] ~ Myfactor, data = Mymatrix, subset = NULL, na.action = "na.omit")$p.value
  P.KRUSKAL <- foreach::foreach(i = 1:length(Mymatrix[, 1]), .combine = "c") %dopar% stats::kruskal.test(Mymatrix[i, ] ~ Myfactor)$p.value
  # Variables based on AOV residuals:
  Residuals <- foreach::foreach(i = 2:length(Mydf[1, ])) %dopar% stats::aov(Mydf[, i] ~ Mydf[, 1])$residuals
  Residuals <- t(matrix(data = unlist(Residuals), ncol = length(df[1, ])))
  # pvalue Anderson-Darling
  P.ANDERSON <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% nortest::ad.test(Residuals[i, ])$p.value
  # pvalue Bartlett
  P.BARTLETT <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% stats::bartlett.test(Residuals[i, ] ~ Myfactor)$p.value
  # pvalue Levene
  P.LEVENE <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% car::leveneTest(Residuals[i, ], Myfactor, center = "median")$`Pr(>F)`[1]
  # pvalue Shapiro-wilk
  P.SHAPIRO <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% stats::shapiro.test(Residuals[i, ])$p.value
  # Skewness ratio
  # unbiased skewness with its confidence interval - bootstrap strategy
  SKW.RATIO <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% {
    Skewness = DescTools::Skew(x = Residuals[i, ], method = 3, conf.level = 0.95, ci.type = "bca", R = 1000)
    # skewness divided by its confidence interval
    abs(Skewness[[1]] / (Skewness[[3]] - Skewness[[2]]))
  }
  # Variance ratio
  VariancePerCategory <- foreach::foreach(j = 1:length(levels(Myfactor)), .combine = "cbind") %:%
    foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% stats::var(Residuals[i, which(Myfactor == levels(Myfactor)[j])])
  VAR.RATIO <- foreach::foreach(i = 1:length(Residuals[, 1]), .combine = "c") %dopar% {max(VariancePerCategory[i, ]) / min(VariancePerCategory[i, ])}

  #Decision tree
    IS.NORMAL[P.SHAPIRO > alpha] <- TRUE
    IS.NORMAL[P.SHAPIRO <= alpha & SKW.RATIO < 2] <- "nearly"
    IS.NORMAL[P.SHAPIRO <= alpha & SKW.RATIO >= 2] <- FALSE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT > alpha] <- TRUE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT <= alpha & VAR.RATIO < 3] <- TRUE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & P.BARTLETT <= alpha & VAR.RATIO >= 3] <- FALSE
      IS.HOMOSCEDASTIC[(IS.NORMAL == TRUE | IS.NORMAL == "nearly") & IS.HOMOSCEDASTIC == FALSE & P.LEVENE > alpha] <- "nearly"
        IS.DISCRIMINANT[] <- FALSE  #...False by default
        IS.DISCRIMINANT[((IS.NORMAL == TRUE & IS.HOMOSCEDASTIC == "nearly") | (IS.HOMOSCEDASTIC == TRUE & IS.NORMAL == "nearly") | (IS.HOMOSCEDASTIC == TRUE & IS.NORMAL == TRUE))
                        & P.ANOVA < alpha] <- TRUE
        IS.DISCRIMINANT[(IS.HOMOSCEDASTIC == TRUE | IS.HOMOSCEDASTIC == "nearly")
                        & P.KRUSKAL < alpha] <- TRUE

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
