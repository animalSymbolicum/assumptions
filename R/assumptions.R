#' Print assumptions for commmon statistical tests
#'
#' This function prints assumptions of common statistical methods.
#' @param call a function call for a hypothesis test like t.test.
#' @param color logical. If TRUE the assumption print is colored.
#' @examples
#' assumptions(t.test)
#' assumptions(wilcox.test)
#' @export
#' @references Conover, W. J. (1998). \emph{SOME METHODS BASED ON RANKS}. Chapter 5 of \emph{Practical nonparametric statistics}
#' @references Spanos, A. (2019). \emph{Misspecification (M-S) Testing}. Chapter 15 of \emph{Probability theory and statistical inference: Empirical modeling with observational data.}

assumptions <- function(call, color = T) {

    stopifnot(is.function(call))

    # is it possible do select assumption depending on call?
    # assumptions(t.test(x,y, var.equal=T))
    colStart <- ifelse(color, "\u001b[31m", "")
    colEnd   <- ifelse(color, "\u001b[0m", "")

    cat(paste0(switch(
        as.character(substitute(call)),
        "t.test" = {
            switch(
                environmentName(environment(call)),
                "stats" = {
                    cat(
                        paste0(
                            c(
                                paste0("The t.test makes ",colStart,"three", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) the sample distribution is ",colStart,"normal", colEnd, " or the sample size is big enough that "),
                                "        the distribution of the sample means likely converge to normal.",
                                paste0(" II ) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                "        As two variables are used, the two variables can have two different distributions if ",
                                "        the Welch Test is used (var.equal=F).",
                                paste0(" III) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value. "),
                                "        As two paired variables are used (paired=T) the pairs are dependent but ",
                                "        values between pairs still must be independent."
                            ),
                            collapse = "\n"
                        )
                    )
                },
                "Function is not knwon to assumptions package."
            )
        },
        "wilcox.test" = {
            switch(
                environmentName(environment(call)),
                "stats" = {
                    cat(
                        paste0(
                            c(
                                "If paired = FALSE - ",
                                paste0("the wilcox.test makes ",colStart,"three", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) are at least ",colStart,"ordinal", colEnd, " scaled. "),
                                paste0(" II ) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                paste0(" III) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value. "),
                                "        As two paired variables are used (paired=T) the pairs are dependent but ",
                                "        values between pairs still must be independent.",
                                "If paired = TRUE - ",
                                paste0("the wilcox.test makes ",colStart,"four", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) are at least ",colStart,"interval", colEnd, " scaled. "),
                                paste0(" II ) the distribution of the calculated differences is ",colStart,"symmetric", colEnd, "."),
                                paste0(" III) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                paste0(" IV ) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value.")
                            ),
                            collapse = "\n"
                        )
                    )
                },
                "Function is not knwon to assumptions package."
            )
        },
        "asm_ttest" = {
            switch(
                environmentName(environment(call)),
                "assumptions" = {
                    cat(
                        paste0(
                            c(
                                paste0("The asm_ttest makes ",colStart,"three", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) the sample distribution is ",colStart,"normal", colEnd, " or the sample size is big enough that "),
                                "        the distribution of the sample means likely converge to normal.",
                                paste0(" II ) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                paste0(" III) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value. "),
                                "        As two paired variables are used (paired=T) the pairs are dependent but ",
                                "        values between pairs still must be independent."
                            ),
                            collapse = "\n"
                        )
                    )
                },
                "Function is not knwon to assumptions package."
            )
        },
        "asm_wilcox" = {
            switch(
                environmentName(environment(call)),
                "assumptions" = {
                    cat(
                        paste0(
                            c(
                                "If paired = FALSE - ",
                                paste0("the asm_wilcox makes ",colStart,"three", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) are at least ",colStart,"ordinal", colEnd, " scaled. "),
                                paste0(" II ) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                paste0(" III) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value. "),
                                "        As two paired variables are used (paired=T) the pairs are dependent but ",
                                "        values between pairs still must be independent.",
                                "If paired = TRUE - ",
                                paste0("the asm_wilcox makes ",colStart,"four", colEnd, " assumptions for the input variable(s) x (and y):"),
                                paste0(" I  ) are at least ",colStart,"interval", colEnd, " scaled. "),
                                paste0(" II ) the distribution of the calculated differences is ",colStart,"symmetric", colEnd, "."),
                                paste0(" III) are ",colStart,"identically distributed", colEnd, ", which means each value comes from the same distribution."),
                                paste0(" IV ) are ",colStart,"independent", colEnd, ", which means each value is not influenced by any other value.")
                            ),
                            collapse = "\n"
                        )
                    )
                },
                "Function is not knwon to assumptions package."
            )
        },
        "Function is not knwon to assumptions package."
    ), collapse ="\n"))

}
