#' Wilcoxon Test with explicit assumptions
#'
#' This function is a wrapper for the \link[stats]{wilcox.test} function. Before conducting the wilcoxon test diagnostic
#' plots are shown to assess the assumptions of the wilcoxon test.
#' @param x numeric vector of data values.
#' @param y NULL or a numeric vector of data values.
#' @param ... further arguments passed to t.test functions (like mu, paired).
#' @param confirm logical. If TRUE ask to confirm assumptions.
#' @param diagnosePlots logical. If TRUE print diagnostic plots.
#' @param color logical. If TRUE the assumption print is colored.
#' @examples
#' asm_wilcox(rnorm(30), rnorm(30, 2))
#' @import cowplot
#' @export

asm_wilcox <- function(x, y = NULL, ..., confirm = T, diagnosePlots = T, color = T) {

    # confirm assumptions
    if (confirm) {
        assumptions(asm_wilcox, color = color)
        cat("\n")
        cat(paste0(rep("*", 90), collapse = ""))
        cat("\n")
        cat(
            "If you confirm to acknowladge that under violation(s) of the assumptions the wilcoxon test\nthe wilcoxon results might be wrong.\n"
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
            prompt="Press [enter] to run wilcoxon test - if you convinced your data fullfills the assumptions"
        )
    }

    # conduct
    wilcox.test(x, y, ...)

}
