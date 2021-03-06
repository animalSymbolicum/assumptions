% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asm_graph.R
\name{asm_graphRuns}
\alias{asm_graphRuns}
\title{Runs Plot of data}
\usage{
asm_graphRuns(data, separateBy = median(data), legend = T)
}
\arguments{
\item{data}{a vector, list or data.frame of data values.}

\item{separateBy}{a function to seperate the data points by (default is median).}

\item{legend}{logical add legend.}
}
\description{
This function plots the runs of data. A run as change in the trend of a data point series is coded
by dashed lines. Red points are above the estimate of the separateBy function (default is median) and
blue points are below the estimate of the separateBy function. The Runs Plot is a graphical
representation of the Wald-Wolfowitz Runs Test.
See also the \link[randtests]{runs.test} function.
}
\examples{
asm_graphRuns((1:100/10)+rnorm(10))
}
