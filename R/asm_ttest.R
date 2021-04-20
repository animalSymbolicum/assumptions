#' t-Test with explicit assumptions
#'
#' This function is a wrapper for the  \link[stats]{t.test} function. Before conducting the t-Test diagnostic
#' plots are shown to assess the assumptions of the t-Test.
#' @param x numeric vector of data values.
#' @param y NULL or a numeric vector of data values.
#' @param ... further arguments passed to t.test functions (like mu, paired).
#' @param confirm logical. If TRUE ask to confirm assumptions.
#' @param diagnosePlots logical. If TRUE print diagnostic plots.
#' @param color logical. If TRUE the assumption print is colored.
#' @examples
#' asm_ttest(rnorm(30), rnorm(30, 2))
#' @import cowplot
#' @export

asm_ttest <- function(x, y = NULL, ..., confirm = TRUE, diagnosePlots = TRUE, color = T) {

    args <- list(...)
    if (!is.null(y) && "var.equal" %in% names(args) && args$var.equal) {
        warning("Not using Welch-Test (var.equal=F) is not recommended because t-Test performs poor under unequal variances.")
    }

    # confirm assumptions
    if (confirm) {
        assumptions(asm_ttest, color = color)
        cat("\n")
        cat(paste0(rep("*", 83), collapse = ""))
        cat("\n")
        cat(
            "If you confirm to acknowledge that under violation(s) of the assumptions the t-test\nthe t-test results might be wrong.\n"
        )
        readline(prompt="Press [enter] to confirm")
    }

    # plot diagnostic plots
    if (diagnosePlots) {

        # print plots
        plotData <- c(x = list(x), y = list(y))
        if (is.null(plotData[[2]])) plotData[2] <- NULL
        print(suppressMessages({cowplot::plot_grid(
            asm_graphBoxplot(plotData, legend = FALSE),
            asm_graphDotplot(plotData),
            asm_graphEcdf(plotData),
            asm_graphHist(plotData)
        )}))
        readline(prompt="Press [enter] for the next plot")
        print(suppressMessages({cowplot::plot_grid(
            asm_graphCummean(plotData, legend = FALSE),
            asm_graphCumvar(plotData),
            asm_graphPPlot(plotData),
            asm_graphQQplot(plotData)
        )}))
        readline(prompt="Press [enter] for the next plot")
        print(suppressMessages({cowplot::plot_grid(
            asm_graphRuns(plotData, legend = FALSE),
            asm_graphDifferencesign(plotData),
            asm_graphScatterplot(plotData),
            {if (length(plotData[[1]]) > 10) asm_graphWindow(plotData) else NULL}
        )}))
        readline(
            prompt="Press [enter] to run Welch-Test - if you are convinced your data fulfills the assumptions."
        )
    }

    # conduct welch test
    t.test(x, y, ...)


}
