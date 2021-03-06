% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asm_simulate.R
\name{asm_simStrategy}
\alias{asm_simStrategy}
\title{Apply pre-test strategy to simulation result}
\usage{
asm_simStrategy(simulationTable, strategies = NULL)
}
\arguments{
\item{simulationTable}{asm_simulate() result table.}

\item{strategies}{a list with quoted expression to reference on columns of asm_simulate() result table.}
}
\description{
This function can be used to apply a pre-test strategy to simulation result.
}
\examples{
asm_simStrategy(
  asm_simulate(10),
  list(example = quote(ifelse(pre_W_X & pre_W_Y, post_ttest, post_wilcox)))
)[, list(pre_W_X, pre_W_Y, post_ttest, post_wilcox, strat_example)]
}
