#trident.top3----
  #' @title trident.top3
  #' @description Classify top 3 variables by their ability to discriminate categories for each group of the factor variable.
  #' @param df A vector or a dataframe of numeric values
  #' @param y A factor
  #' @return the result of fun
  #' @examples
  #' #to do
  #' @export
  trident.top3 <- function(df, y) {
    #check data structure
    if (is.data.frame(df) == FALSE) stop("Dataframe df should be an object of class data.frame (see 'is.data.frame')")
    if (is.factor(y) == FALSE) stop("Category variable 'y' is not a factor")
    if (length(which(apply(df, 2, FUN = is.numeric) == FALSE)) != 0) stop("All variables of dataframe 'df' should be numeric")

    #Preparation of dataset: removal of Na, NaN, Inf and -Inf:
    Mydf <- data.frame(as.factor(y), df)
    Mydf <- Mydf[is.finite(rowSums(Mydf[, -1])),]

    # ...BoxCox transformation
    Numerics <- trident::trident.boxcox(df = Mydf[, -1], y = Mydf[, 1])$boxcox
    colnames(Numerics) <- paste(colnames(Numerics), "boxcox", sep = ".")

    # ...multicheck
    Multicheck <- trident::multicheck(df = Numerics, y = Mydf[, 1])
    if(!any(as.logical(Multicheck$is.discriminant)))
    {
      warning("No discriminant variable found; top3 procedure aborted")
      return(NULL)
    }
    else Numerics <- Numerics[, which(trident::multicheck(df = Numerics, y = Mydf[, 1])$is.discriminant == TRUE)]

    # ...for each group, rank variables and isolate top 3
    Mylist <- list()
    Mylist$top3var <- NULL
    Mypairs <- utils::combn(levels(Mydf[, 1]), 2, paste, collapse = ' vs. ')
    for (i in c(1:length(Mypairs))) {
      Mycurrentpair <- unlist(strsplit(Mypairs[i], " vs. "))
      Mypriority <- which(levels(Mydf[, 1]) %in% Mycurrentpair)
      Myrank <- trident::trident.arrange(df = Numerics, y = Mydf[, 1], by = "hsd.p.value", gp.priority = Mypriority)
      Mylist$top3var <- c(Mylist$top3var, rownames(Myrank[1:3, ]))
    }
    # ...now export arranged table with variables from Mylist
    Mylist$ranked <- data.frame(group = rep(Mypairs, each = 3), trident::trident.arrange(df = Numerics, y = Mydf[, 1])[Mylist$top3var, ])
    return(Mylist)
  }
