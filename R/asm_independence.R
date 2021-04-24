#' Test the assumption of independence
#'
#' This function applies tests for the assumption of independence in form of autocorrelations.
#' @param data a vector, list or data.frame of data values.
#' @param tests a selection of tests via its names or index. See also the \link[assumptions]{asm_library} function.
#' @param report which statistics should be reported.
#' @param testBatch a subset of \link[assumptions]{asm_library} table for selected independence tests.
#' @param simulate logical if running in simulation.
#' @param lag logical if running in simulation.
#' @param checkData logical if TRUE data is checked before running tests.
#' @examples
#' asm_independence(list(A = 1:100, B = 1:100))
#' @import data.table
#' @export

asm_independence <- function(data  = NULL,
                             tests = NULL,
                             report = c("p.value", "statistic", "parameter")[1],
                             testBatch = NULL,
                             simulate = FALSE,
                             lag = 1,
                             checkData = !simulate) {

    # select tests
    if (is.null(testBatch)) testBatch <- asm_intSelT(asm_library(print = F)[Assumption == "independence"], tests)

    # if multivar input
    if (is.list(data) && length(data) > 1) {

        # if input data without names
        if (is.null(names(data))) names(data) <- LETTERS[1:length(data)]

        # prepare report columns
        testBatch[,c(paste0(rep(names(data), each = length(report)),"_", report)) := as.numeric(NA)]

        # run tests for each variable
        for (var in names(data)) {

            # check data
            if (checkData) asm_intCheckData(data[[var]], return = F)

            testBatch[,
              c(paste0(var, "_", report)) := switch(
                  stat,
                  Box.Pierce = Box.test(data[[var]], lag = lag),
                  Ljung.Box  = Box.test(data[[var]], lag = lag, type = "Ljung-Box")
              )[report],
              by = "index"
          ]
        }

        report <- c(paste0(rep(names(data), each = length(report)),"_", report))


        # if vector
    } else {

        # check data
        if (checkData) asm_intCheckData(data, return = F)

        testBatch[,
          c(report) := as.numeric(NA)
          ][,
            c(report) := switch(
                stat,
                Box.Pierce = Box.test(data, lag = lag),
                Ljung.Box  = Box.test(data, lag = lag, type = "Ljung-Box")
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

        testBatch[,":="(alter = NULL, nbparams=NULL, Name=NULL,Assumption=NULL)]
        testBatch <- asm_internalLibNames(testBatch)
        testBatch[,":="(index = NULL, Package=NULL, stat = NULL)]
        setnames(testBatch, gsub("p[.]value","pvalue" , names(testBatch)))
        testBatch[]

    }

}
