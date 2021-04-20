#' Plot data as Boxplot
#'
#' This function plots data as boxplot.
#' @param data a vector, list or data.frame of data values.
#' @param legend logical add legend.
#' @examples
#' asm_graphBoxplot(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import data.table
#' @export
asm_graphBoxplot <- function(data, legend = T) {

    # check if multiple vars
    if (is.list(data)) {

        data <- lapply(data, asm_intCheckData)

        if (is.null(names(data))) names(data) <- LETTERS[1:length(data)]

        data <- rbindlist(
            mapply(function(x, var) data.table(variable = var, value = x),
            data, names(data),
            SIMPLIFY = F)
        )
        setDF(data)

        ggplot(data, aes(x = value, group = variable, fill = variable)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = NULL) +
        {if (!legend) theme(legend.position = "none")}

    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(data.frame(X=data), aes(x = X)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = NULL) +
        {if (!legend) theme(legend.position = "none")}
    }

}

#' Plot data as dotplot
#'
#' This function plots data as dotplot.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' asm_graphDotplot(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphDotplot <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphDotplot),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(data.frame(X=data), aes(x = X)) +
        geom_dotplot() +
        labs(x = NULL)
    }

}

#' Plot Cummulative Distribution Function of data
#'
#' This function plots the empiricial cummulative distribution function derived from data against
#' the theoretical cummulative distribution function of the normal distribution with mean and sd
#' calculated from data.
#' @param data a vector, list or data.frame of data values.
#' @param theoMean mean for theoretic distribution.
#' @param theoSD standard deviation for theoretic distribution.
#' @examples
#' asm_graphEcdf(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphEcdf <- function(data, theoMean = mean(data), theoSD = sd(data)) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphEcdf),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(data.frame(data), aes(data)) +
        stat_ecdf(geom = "step") +
        stat_function(fun= pnorm, color="blue", args=list(mean=theoMean,sd=theoSD)) +
        labs(x = NULL, y = "Cumulative Probability")

    }
}

#' Plot Histogramm of data
#'
#' This function plots data as histogramm.
#' @param data a vector, list or data.frame of data values.
#' @param addLine Draw curve of normal distribution with mean and sd calculated from data.
#' @examples
#' asm_graphHist(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphHist <- function(data, addLine = TRUE) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphHist, addLine = addLine),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(data.frame(data), aes(x = data)) +
        geom_histogram(aes(y=..density..), colour="black") +
        stat_function(fun = dnorm, args = list(mean = mean(data), sd = sd(data)),
                                   color = "red",
                                   size = 1.0, show.legend = TRUE) +
        labs(x = NULL) +
        {if (addLine) geom_density(aes(x = data), color = "blue")}

    }

}

#' Plot Cummulative Mean of data
#'
#' This function plots the fluctuation of the mean if its calculated as cummulated mean.
#' @param data a vector, list or data.frame of data values.
#' @param legend logical add legend.
#' @examples
#' # normal data vs. mixed non-id data
#' asm_graphCummean(list(N = rnorm(60), M = c(rnorm(20), rnorm(20, 2), rnorm(20, 10))))
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphCummean <- function(data, legend = T) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphCummean, legend = legend),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        N <- length(data)
        cumMean <- cumSD <- rep(NA, N)
        for (i in 1:N) {
            cumMean[i]  <- mean(data[1:(i)])
            cumSD[i]    <- sd(data[1:(i)])
        }
        data.frame(
            x = rep(1:N, times = 3),
            group = rep(c("mean", "sd+", "sd-"), each = N),
            val = c(
                cumMean,
                cumMean + cumSD,
                cumMean - cumSD
            )
        ) -> dat
        ggplot(dat, aes(x=x, y=val)) +
        geom_line(aes(color=group), na.rm = T) +
        scale_color_manual(values=c(mean="red", "sd+"="blue", "sd-"="blue")) +
        labs(x = NULL, y = NULL) +
        {if (!legend) theme(legend.position = "none")}
    }
}

#' Probability–Probability Plot of data
#'
#' This function plots the Probability–Probability plot of data.
#' @param data a vector, list or data.frame of data values.
#' @param theoMean mean for theoretic distribution.
#' @param theoSD standard deviation for theoretic distribution.
#' @examples
#' asm_graphPPlot(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphPPlot <- function(data, theoMean = mean(data), theoSD = sd(data)) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphPPlot),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        N <- length(data)
        Sample   <- (1 : N) / N- 0.5 / N
        Theoretical <- sort(pnorm(data, theoMean, theoSD))
        ggplot(tibble::tibble(data)) +
        geom_abline(intercept = 0, slope = 1, colour = "blue") +
        geom_point(aes(x = Sample, y = Theoretical)) +
        labs(x = NULL)

    }

}

#' Quantile-Quantile Plot of data
#'
#' This function plots the Quantile-Quantile plot of data.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' asm_graphQQplot(iris[,c("Sepal.Width", "Sepal.Length")])
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphQQplot <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphQQplot),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(tibble::tibble(x = data), aes(sample = x)) +
        stat_qq() +
        stat_qq_line() +
        labs(x = NULL)

    }
}

#' Difference-Sign Plot of data
#'
#' This function plots the relative shares of runs for the differences. Every value is subtracted by
#' its predecessor and counted if above zero. The relative share of a change of directions (run) is
#' plotted. The Difference-Sign plot is a graphical representation of the Difference-Sign Runs Test.
#' See also the \link[randtests]{difference.sign.test} function.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' asm_graphDifferencesign(list(A = rnorm(100), B = (1:100/10)+rnorm(10)))
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphDifferencesign <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphDifferencesign),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        runs <- data.table::frollapply(data, n = 2, FUN = function(x) (x[2] - x[1]) > 0)[-1]
        runsShare <- unlist(sapply(
            split(runs, c(runs[1], cumsum(runs)[-length(runs)])),
            function(x) {
                if (length(x)>1) sum(x) / length(x) else NULL
            }
        ))

        ggplot(data.frame(x = 1:length(runsShare), runs = runsShare), aes(x=x, y=runs)) +
        geom_line() +
        labs(x = NULL)

    }
}

#' Runs Plot of data
#'
#' This function plots the runs of data. A run as change in the trend of a data point series is coded
#' by dashed lines. Red points are above the separateBy function (default is median) and blue points are bellow
#' separateBy function. The Runs Plot is a graphical representation of the Wald-Wolfowitz Runs Test.
#' See also the \link[randtests]{runs.test} function.
#' @param data a vector, list or data.frame of data values.
#' @param separateBy a function to seperate the data points by (default is median).
#' @param legend logical add legend.
#' @examples
#' asm_graphRuns((1:100/10)+rnorm(10))
#' @import ggplot2
#' @import cowplot
#' @import data.table
#' @export
asm_graphRuns <- function(data, separateBy = median(data), legend = T) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphRuns, legend = legend),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        data <- na.omit(data)
        stopifnot(is.numeric(data))

        data <- data[data != separateBy]
        data <- data[-c(length(data))]
        sigs <- sign(data - separateBy)

        data.table(
            x     = data,
            group = ifelse(sigs > 0, "<", ">")
        ) ->
        plotData

        groupLines <- which(cumprod(sigs) == -1) -0.5

        ggplot(plotData, aes(y = x, x = 1:length(x))) +
        geom_point(aes(color = group)) +
        geom_hline(yintercept = separateBy) +
        geom_vline(xintercept = groupLines, linetype="dotted", color = "blue") +
        labs(x = NULL) +
        {if (!legend) theme(legend.position = "none")}

    }
}

#' Scatterplot of data
#'
#' This function plots the scatterplot of data.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' normal data vs. mixed non-id data
#' asm_graphScatterplot(list(N = rnorm(60), M = c(rnorm(20), rnorm(20, 2), rnorm(20, 10))))
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphScatterplot <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphScatterplot),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        ggplot(data.frame(data), aes(y = data, x = 1:length(data))) +
        geom_point() +
        geom_line() +
        labs(x = "index", y = NULL, title = NULL)

    }
}

#' Plot Cummulative Variance of data
#'
#' This function plots the fluctuation of the variance if its calculated as cummulated variance.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' # normal data vs. mixed non-id data
#' asm_graphCumvar(list(N = rnorm(60), M = c(rnorm(20), rnorm(20, sd = 2), rnorm(20, sd = 10))))
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphCumvar <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphCumvar),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        N <- length(data)
        cumVar <- cumSD <- rep(NA, N)
        for (i in 1:N) cumVar[i]  <- sqrt(var(data[1:(i)]))

        data.frame(
            x   = 1:N,
            variance = cumVar
        ) -> dat

        ggplot(dat, aes(x=x, y=variance)) +
        geom_line(na.rm = T) +
        labs(x = NULL)

    }

}

#' Window Graph of data
#'
#' This function draws 'windows' onto a scatterplot of the data as a support to imagen homogeneity.
#' Each window should contain the same pattern if the data points are homogeneous or
#' identically distributed.
#' @param data a vector, list or data.frame of data values.
#' @examples
#' # normal data vs. mixed non-id data
#' asm_graphWindow(list(N = rnorm(60), M = c(rnorm(20), rbeta(20, 2, 5), rt(20, 9))))
#' @import ggplot2
#' @import cowplot
#' @export
asm_graphWindow <- function(data) {

    # check if multiple vars
    if (is.list(data)) {
        if (length(data)>4) stop("Too many vars for plotting")
        if (is.null(names(data))) lab <- "AUTO" else lab <- names(data)
        cowplot::plot_grid(
            plotlist = lapply(data, asm_graphWindow),
            labels=lab
        )
    } else {

        # check and clean data
        data <- asm_intCheckData(data)

        if (length(data) < 5) stop("Not enough values for window graph")

        # find optim windows
        windows <- ifelse(length(data) < 15, 2, 3)
        windowColumn <- (ceiling(length(data) * 0.02) * windows - 1) / (windows - 1)
        windowWidth  <- ceiling(length(data)/windows)

        ggplot(data.frame(data), aes(y = data, x = 1:length(data))) +
        geom_point() +
        geom_line(alpha=0.4) +
        geom_rect(
            xmin = 1,
            xmax = windowWidth - (windowColumn / (windows-1)),
            ymin = min(data) - 0.1, ymax = Inf,
            color="black",
            fill = NA
        ) +
        geom_rect(
            xmin = windowWidth + windowColumn / (windows-1),
            xmax = windowWidth * 2 - windowColumn / (windows-1),
            ymin = min(data) - 0.1, ymax = Inf,
            color="black",
            fill = NA
        ) +
        {if (windows == 3) {
            geom_rect(
                xmin = windowWidth * 2 + windowColumn / 2,
                xmax = length(data),
                ymin = min(data) - 0.1, ymax = Inf,
                color="black",
                fill = NA
            )
        } else NULL} +
        labs(x = "index", y = NULL, title = NULL)

    }

}
