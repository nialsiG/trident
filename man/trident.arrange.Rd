% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trident.arrange.R
\name{trident.arrange}
\alias{trident.arrange}
\title{trident.arrange}
\usage{
trident.arrange(
  df,
  y,
  by = "hsd.mean.p.value",
  alpha = 0.05,
  byngr = FALSE,
  geomean = FALSE,
  gp.priority = c(1:length(levels(y)))
)
}
\arguments{
\item{df}{A vector or a dataframe of numeric values}

\item{y}{A factor}

\item{by}{A vector of character strings, indicating what parameter should be used to classify data.
Should be in c("f.stat", "aov.p.value", "k", "k.p.value", "hsd.p.value", "hsd.mean.p.value", "lsd.p.value", "lsd.mean.p.value")}

\item{alpha}{Level of significance}

\item{byngr}{Logical, should data be classified by the largest number of significantly different groups?}

\item{geomean}{Logical, should average values be calculated as geometric means?}

\item{gp.priority}{A vector indicating which levels should be compared in priority}
}
\value{
A data frame of variables arranged by a parameter selected to classify data
}
\description{
Classify variables by their ability to discriminate categories
}
