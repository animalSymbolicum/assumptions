#' Test the assumption of a normal distribution
#'
#' This function applies tests for the normal distribution assumption.
#' @param data a vector, list or data.frame of data values.
#' @param tests a selection of tests via its names or index. See also the \link[assumptions]{asm_library} function.
#' @param report which statistics should be reported.
#' @param testBatch a subset of \link[assumptions]{asm_library} table for selected distribution tests.
#' @param simulate logical if running in simulation.
#' @param checkData logical if TRUE data is checked before running tests.
#' @examples
#' # normal distribution
#' asm_distribution(rnorm(100))
#' # gamma distribution
#' asm_distribution(rgamma(100, 2))
#' @import data.table
#' @import PoweR
#' @export

asm_distribution <- function(data  = NULL,
                             tests = NULL,
                             report = c("pvalue", "statistic")[1],
                             simulate  = FALSE,
                             checkData = !simulate,
                             testBatch = NULL) {

    # select tests
    if (is.null(testBatch)) testBatch <- asm_intSelT(asm_library(print=F)[Assumption == "distribution"], tests)

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
          # c(paste0(var, "_", report)) := PoweR::statcompute(index, data[[var]], levels = alpha)[report],
          c(paste0(var, "_", report)) := {
            .C(
              paste0("stat", index), data = data[[var]], n = length(data[[var]]), levels = 0.05, nblevels = 1L,
              name = rep(" ", length(data[[var]])), getname = 0L, statistic = 0, pvalcomp = 1L, pvalue = 0,
              cL = 0.0, cR = 0.0, usecrit = 0L, alter = alter, decision = 0L, stat.pars = 0.0,
              nbparstat = 0L
            )[report]
          },
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
        # c(report) := PoweR::statcompute(index, data, levels = alpha)[report],
        c(report) := {
          .C(
            paste0("stat", index), data = data, n = length(data), levels = alpha, nblevels = 1L,
            name = rep(" ", length(data)), getname = 0L, statistic = 0, pvalcomp = 1L, pvalue = 0,
            cL = 0.0, cR = 0.0, usecrit = 0L, alter = alter, decision = 0L, stat.pars = 0.0,
            nbparstat = 0L
          )[report]
        },
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
      testBatch[,":="(index = NULL, Package=NULL, stat=NULL)]
      testBatch[]

    }

}
