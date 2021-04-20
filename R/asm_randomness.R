#' Test the assumption of randomness
#'
#' This function applies tests for the randomness assumption.
#' @param data a vector, list or data.frame of data values.
#' @param tests a selection of tests via its names or index. See also the \link[assumptions]{asm_library} function.
#' @param report which statistics should be reported.
#' @param testBatch a subset of \link[assumptions]{asm_library} table for selected randomness tests.
#' @param simulate logical if running in simulation.
#' @param checkData logical if TRUE data is checked before running tests.
#' @examples
#' # non random brownian motion
#' asm_randomness(diffinv(rnorm(999)))
#' # random normal data
#' asm_randomness(rnorm(999))
#' @import data.table
#' @import randtests
#' @export

asm_randomness <- function(data = NULL,
                           tests = NULL,
                           report = c("p.value", "statistic")[1],
                           testBatch = NULL,
                           simulate = FALSE,
                           checkData = !simulate) {

    # select tests
    if (is.null(testBatch)) testBatch <- asm_intSelT(asm_library(print = F)[Assumption == "randomness"], tests)

    # if multivar input
    if (is.list(data) && length(data) > 1) {

        # if input data without names
        if (is.null(names(data))) names(data) <- LETTERS[1:length(data)]

        # prepare report columns
        testBatch[,
          c(paste0(rep(names(data), each = length(report)),"_", report)) := as.numeric(NA)
        ]

        # run tests for each variable
        for (var in names(data)) {

            # check data
            if (checkData) asm_intCheckData(data[[var]], return = F)

            testBatch[,
                      c(paste0(var, "_", report)) := eval(
                          as.call(c(
                              list(eval(parse(text = paste0("randtests::", stat, ".test")))),
                              list(data[[var]])
                          ))
                      )[report],
                      by = "index"
                  ]
        }

        report <- c(paste0(rep(names(data), each = length(report)),"_", report))


    # if vector
    } else {

        # check data
        if (checkData) asm_intCheckData(data, return = F)

        # evaluate randomness tests
        testBatch[,
            c(report) := as.numeric(NA)
        ][,
            c(report) := eval(
                as.call(c(
                    list(eval(parse(text = paste0("randtests::", stat, ".test")))),
                    list(data)
                ))
            )[report],
            by = "index"
        ]
    }

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

        testBatch[,":="(alter = NULL, nbparams=NULL, Name=NULL)]
        testBatch <- asm_internalLibNames(testBatch)
        testBatch[,":="(index = NULL, Package=NULL, Assumption = NULL)]
        setnames(testBatch, gsub("p[.]value","pvalue" , names(testBatch)))
        testBatch[]

    }

}
