% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmta.R
\name{dmta.topology}
\alias{dmta.topology}
\title{dmta.topology}
\usage{
dmta.topology(sur, size.x = 256, size.y = 256, size.n = 256, type = "single")
}
\arguments{
\item{sur}{surface file}

\item{size.x}{total x length of the surface, in pixels}

\item{size.y}{total y length of the surface, in pixels}

\item{size.n}{in case type = "multi", the number of cells for the calculation of heterogeneity}

\item{type}{character vector indicating whether a single value should be calculated for the whole surface ("single"); or if multiple values should be calculated for each n cells ("multi")}
}
\value{
A list containing the .sur file, the type (topology), Sk1, Sk2, Smc1, Smc2, Snb1, Snb2 and Sh
}
\description{
Computes topology variables
}
