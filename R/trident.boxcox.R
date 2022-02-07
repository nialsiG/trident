# trident.boxcox----
#' @title trident.boxcox
#' @description Transforms x according to the Box-Cox formula, using the parameter lambda
#' @param df A vector or a dataframe of numeric values
#' @param y A factor
#' @param list.lambda Logical, if TRUE the list of lambda values is added to the results
#' @return A list containing a dataframe of the transformed values
#' @export
trident.boxcox <- function(df, y, list.lambda = FALSE) {
  #BEFORE, check data structure:
  if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
  if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
  if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")
  if (length(levels(y)) < 2) stop("trident.boxcox can be applied only if y is a factor with 2 or more levels")

  #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
  Mydf <- data.frame(as.factor(y), df)
  Mydf <- Mydf[is.finite(rowSums(Mydf[, -1])),]

  #Data translation above one:
  data.translation <- function(x) return(x - min(x) + 1)
  Mydf[, -1] <- plyr::colwise(data.translation)(data.frame(Mydf[, -1]))

  #Boucle de transformation BoxCox:
  Myboxcox <- Mydf[, -1]
  Mylambda <- NULL
  for(i in c(1:ncol(Mydf[, -1]))) {
    if (stats::var(Mydf[, i + 1]) > 0) {
      #Lambda evaluation on a large range first:
      aov_k <- stats::formula(Mydf[, i + 1] ~ Mydf[, 1])
      bc1 <- MASS::boxcox(aov_k, lambda = seq(-5, 5, 1./100), data = Mydf, plotit = FALSE)
      #Refine lambda values with a smaller range:
      range_lambda <- range(bc1$x[bc1$y > max(bc1$y) - stats::qchisq(0.95, 1) / 2] )
      range_lambda <- seq(range_lambda[1], range_lambda[2], 1./100)
      bc2 <- MASS::boxcox(aov_k, lambda = range_lambda, data = Mydf, plotit = FALSE)
      #Determine lambda:
      range_lambda <- range(bc2$x[bc2$y > max(bc2$y) - stats::qchisq(0.95, 1) / 2] )
      Mylambda[i] <- round(mean(range_lambda), digits = 1)
      #Boxcox transformation:
      if (Mylambda[i] == 0) Myboxcox[, i] <- log(Mydf[, i + 1]) else Myboxcox[, i] <- (Mydf[, i + 1] ^ Mylambda[i] - 1) / Mylambda[i]
    }
  }
  Results <- list()
  Results$boxcox <- Myboxcox
  if (list.lambda == TRUE) Results$lambda <- Mylambda
  return(Results)
}
