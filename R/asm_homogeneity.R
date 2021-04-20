#' Test the assumption of identical distribution
#'
#' This function applies tests for identically distribution assumptions of two variables.
#' @param data a data.frame with at least two columns or a list of length two.
#' @param tests a selection of tests via its names or index. See also the \link[assumptions]{asm_library} function.
#' @param report which statistics should be reported.
#' @param testBatch a subset of \link[assumptions]{asm_library} table for selected homogeneity tests.
#' @param simulate logical if running in simulation.
#' @param checkData logical if TRUE data is checked before running tests.
#' @examples
#' asm_homogeneity(data.frame(X=rnorm(50), Y=rnorm(50, mean = 2, sd = 2)))
#' @import data.table
#' @export

asm_homogeneity <- function(data = NULL,
                            tests = NULL,
                            report = c("pvalue"),
                            testBatch = NULL,
                            simulate = FALSE,
                            checkData = !simulate) {


    # select tests
    if (is.null(testBatch)) testBatch <- asm_intSelT(asm_library(print = F)[Assumption == "homogeneity"], tests)

    # check data
    if (checkData) sapply(data, asm_intCheckData, return = F)

    # evaluate randomness tests
    testBatch[,
        c(report) := as.numeric(NA)
    ][,
        c(report) := asm_intHomogTest(data, stat)[report],
        by = "index"
    ]

    # transpose for single row entry in simulation
    if (simulate) {

        Reduce(
            cbind,
            lapply(
                report,
                function(x) {
                    data.table::transpose(
                        testBatch[, list(get(x), name = paste0("pre_", stat, "_", x))],
                        make.names ="name"
                    )
                }
            )
        )

    } else {

        testBatch[,":="(alter = NULL, nbparams=NULL)]
        asm_internalLibNames(testBatch)[,list(Name, Help, pvalue)][]

    }

}
