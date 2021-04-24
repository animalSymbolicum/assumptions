#' Apply assumption tests
#'
#' This function applies all tests of \link[assumptions]{asm_library} tests or a defined selection of tests.
#' @param data a vector, list or data.frame of data values.
#' @param tests a selection of tests via its names or index.
#' @param testBatch a subset of \link[assumptions]{asm_library} table for selected tests.
#' @param simulate logical if running in simulation.
#' @examples
#' # test with normal data
#' asm_preTests(rnorm(40))
#' # test with non identically distributed data
#' asm_preTests(c(rnorm(20), rnorm(20, 2, 2)))
#' # test with non independent data
#' asm_preTests(diffinv(rnorm(40)))
#' # with homogeneity tests
#' asm_preTests(list(A = rnorm(20), B = rnorm(20, 2, 2)))
#' @export

asm_preTests <- function(data,
                         tests = NULL,
                         testBatch = asm_intSelT(asm_library(print = F), tests),
                         simulate = FALSE) {

    # select test
    if (!is.null(tests) && is.character(tests)) {
        if (is.character(tests)) tests <- testBatch[, which(Name %in% tests)]
        if (testBatch[tests][!is.na(Assumption), .N == 0]) stop("No test selected")
    } else tests <- 1:testBatch[, .N]

    if (!is.list(data) || length(data) == 1) {
        tests <- setdiff(tests, testBatch[, which(Assumption == "homogeneity")])
    }

    list(
        {if (testBatch[tests, any(Assumption == "distribution")])
            asm_distribution(
                data,
                tests = NULL,
                # intersect(tests, which(Assumption == "distribution"))
                # ],
                testBatch = testBatch[Assumption == "distribution", ],
                simulate = simulate
            )},
        {if (testBatch[tests, any(Assumption == "randomness")])
            asm_randomness(
                data,
                tests = NULL,
                # intersect(tests, which(Assumption == "randomness")) -
                #     (min(which(Assumption == "randomness")) - 1)
                # ],
                testBatch = testBatch[Assumption == "randomness", ],
                simulate = simulate
            )},
        {if (testBatch[tests, any(Assumption == "homogeneity")])
            asm_homogeneity(
                data,
                tests = NULL,
                #     intersect(tests, which(Assumption == "homogeneity")) -
                #     (min(which(Assumption == "homogeneity")) - 1)
                # ],
                testBatch = testBatch[Assumption == "homogeneity", ],
                simulate = simulate
            )},
        {if (testBatch[tests, any(Assumption == "independence")])
            asm_independence(
                data,
                tests = NULL,
                # intersect(tests, which(Assumption == "independence")) -
                #     (min(which(Assumption == "independence")) - 1)
                # ],
                testBatch = testBatch[Assumption == "independence", ],
                simulate = simulate
            )},
        {if (testBatch[tests, any(Assumption == "iid")])
            asm_preJoint(
                data,
                # tests = NULL,
                # intersect(tests, which(Assumption == "joint")) -
                #     (min(which(Assumption == "joint")) - 1)
                # ],
                # testBatch = testBatch[Assumption == "iid", ],
                simulate = simulate
            )}
    ) ->
        result

    if (simulate) {
        unlist(result, recursive = F)
    } else rbindlist(result, fill = T)

}

#' Spanos Auxiliary Regression
#'
#' This function conducts a version of Aris Spanos Auxiliary Regression to jointly test if the variable is
#' independent and identically distributed.
#' @param data a data.frame with at least two columns or a list of length two.
#' @param simulate logical if running in simulation.
#' @examples
#' # test with normal data
#' asm_preJoint(rnorm(40))
#' # test with non identically distributed data
#' asm_preJoint(c(rnorm(20), rnorm(20, 2, 2)))
#' # test with non independent data
#' asm_preJoint(diffinv(rnorm(40)))
#' @references Spanos, A. (2019). \emph{Misspecification (M-S) Testing}. Chapter 15 of \emph{Probability theory and statistical inference: Empirical modeling with observational data.}
#' @export

asm_preJoint <- function(data, simulate = FALSE) {

    # bivariate case
    if (is.list(data) && length(data) == 2) {

        # prepare data
        if (is.list(data)) data <- data[[1]]
        N <- length(data)
        t <- 1:(N-1)
        data <- data.frame(
            x     = data[-1],
            "t_1" = data[-N]
        )

        # F-Test
        meanModel <- summary(lm(x ~ t + I(t^2) + t_1, data = data))$fstatistic
        varModel  <- summary(lm(I(x)^2 ~ t + I(t^2) + t_1, data = data))$fstatistic

    # univariate case
    } else
    if (!is.list(data) || length(data) == 1) {

        # prepare data
        if (is.list(data)) data <- data[[1]]
        N <- length(data)
        t <- 1:(N-1)
        data <- data.frame(
            x     = data[-1],
            "t_1" = data[-N]
        )

        # F-Test
        meanModel <- summary(lm(x ~ t + I(t^2) + t_1, data = data))$fstatistic
        varModel  <- summary(lm(I(x)^2 ~ t + I(t^2) + t_1, data = data))$fstatistic


    } else stop("Only one and two sample cases are implemented")

    if (simulate) {
        list(
            pre_joint_mean_pvalue = pf(meanModel[1], meanModel[2], meanModel[3], lower.tail=FALSE),
            pre_joint_var_pvalue  = pf(varModel[1], varModel[2], varModel[3], lower.tail=FALSE)
        )
    } else {
        data.table(
            Assumption            = "iid",
            Name                  = "Spanos Auxiliary Regression",
            Help                  = list(list(help("asm_preJoint", package = "assumptions"))),
            pre_joint_mean_pvalue = pf(meanModel[1], meanModel[2], meanModel[3], lower.tail=FALSE),
            pre_joint_var_pvalue  = pf(varModel[1], varModel[2], varModel[3], lower.tail=FALSE)
        )
    }

}
