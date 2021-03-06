% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asm_graph.R
\name{asm_graphCummean}
\alias{asm_graphCummean}
\title{Plot Cumulative Mean of data}
\usage{
asm_graphCummean(data, legend = T)
}
\arguments{
\item{data}{a vector, list or data.frame of data values.}

\item{legend}{logical add legend.}
}
\description{
This function plots the fluctuation of the mean if its calculated as cumulated mean.
}
\examples{
# normal data vs. mixed non-id data
asm_graphCummean(list(N = rnorm(60), M = c(rnorm(20), rnorm(20, 2), rnorm(20, 10))))
}
