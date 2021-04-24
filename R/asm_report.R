#' Describe simulated data
#'
#' This function is used in simulation to calculate descriptive parameters of simulated data.
#' @param data a vector, list or data.frame of data values.
#' @param with which descriptive parameters to calculate.
#' @examples
#' asm_reportData(rnorm(10))
#' @import data.table
#' @import moments
#' @export

asm_reportData <- function(data, with = c("mean", "median", "sd", "skew", "kurtosis")) {

    # if input is list
    if (is.list(data)) {

        # map
        unlist(
            mapply(
                function(x, nam) {
                    res <- c(
                        {if ("mean" %in% with)     list(descr_mean     = mean(x))},
                        {if ("median" %in% with)   list(descr_median   = median(x))},
                        {if ("skew" %in% with)     list(descr_skew     = moments::skewness(x))},
                        {if ("kurtosis" %in% with) list(descr_kurtosis = moments::kurtosis(x))},
                        {if ("sd" %in% with)       list(descr_sd       = sd(x))}
                    )
                    setNames(res, paste0(names(res), "_", nam))
                },
                data,
                names(data),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            ),
            recursive  = FALSE
        )

    } else {

        c(
            {if ("mean" %in% with)     list(descr_mean     = mean(data))},
            {if ("median" %in% with)   list(descr_median   = median(data))},
            {if ("skew" %in% with)     list(descr_skew     = moments::skewness(data))},
            {if ("kurtosis" %in% with) list(descr_kurtosis = moments::kurtosis(data))},
            {if ("sd" %in% with)       list(descr_sd       = sd(data))}
        )
    }

}

#' Describe simulation result
#'
#' This function can be used to aggregate a simulation result of the \link[assumptions]{asm_simulate} function.
#' @param simResult simulation result of \link[assumptions]{asm_simulate} function.
#' @param digits integer indicating the number of decimal places for rounding the results.
#' @param report which aggregations should be reported. "result" gives aggregation for all pre- and post-tests and
#' strategies. "cross" gives a cross tabulation for all tests. "description" gives aggregation for all descritpive parameters.
#' @examples
#' asm_reportSim(asm_simulate())
#' @import data.table
#' @export

asm_reportSim <- function(simResult, digits = 4, report = c("result", "cross", "description")) {

    stopifnot(is.data.table(simResult))
    stopifnot(digits > 0 & digits < 100)

    # if any result columns
    if (any(grepl("^(pre|post)_", names(simResult)))) {

        # primary result table
        if ("result" %in% report) {
        resultTable <- merge.data.table(
            # single test performance
             simResult[,
                  lapply(.SD, function(x) round(sum(x, na.rm = T) / sum(complete.cases(x)), digits)),
                  by = setdiff(grep("^(sim)_", names(simResult), value = T), "sim_index"),
                  .SDcols = grep("^(pre|post)_", names(simResult)[sapply(simResult, is.logical)], value = T)
            ],
            # decision strategies
            simResult[,
                  lapply(.SD, function(x) round(sum(x, na.rm = T) / sum(complete.cases(x)), digits)),
                  by = setdiff(grep("^(sim)_", names(simResult), value = T), "sim_index"),
                  .SDcols = grep("^strat_", names(simResult), value = T)
            ],
            by = setdiff(grep("^(sim)_", names(simResult), value = T), "sim_index"),
            all = T
        )} else resultTable <- NULL

        # performance cross table
        if ("cross" %in% report) {
            crossTable <- simResult[,
                {

                    # Wie viele unter var1
                    pp <- data.table(NULL)
                    for (var in names(.SD)) {

                        pp <- rbind(
                            pp,
                            subset(data.table(
                                var1 = c(rep(paste0(var, "_acc"), ncol(.SD)), rep(paste0(var, "_rej"), ncol(.SD))),
                                var2 = c(rep(paste0(names(.SD), "_acc"), 2), rep(paste0(names(.SD), "_rej"), 2)),
                                value = round(c(
                                    .SD[, sapply(.SD, sum, na.rm=T) / .N, by=var, .SDcols = names(.SD)][order(get(var),decreasing = T), V1],
                                    .SD[, sapply(.SD, function(x) sum(!x, na.rm=T)) / .N, by=var, .SDcols = names(.SD)][order(get(var),decreasing = T), V1]
                                    # .SD[get(var) ==T, .N / sapply(.SD, sum, na.rm=T)],
                                    # .SD[get(var) ==T, .N / sapply(.SD, function(x) sum(!x, na.rm=T))],
                                    # .SD[get(var) ==F, .N / sapply(.SD, sum, na.rm=T)],
                                    # .SD[get(var) ==F, .N / sapply(.SD, function(x) sum(!x, na.rm=T))]
                            ), digits)
                            ), value != 0)
                        )


                    }

                    pp


                },
                by = setdiff(grep("^(sim)_", names(simResult), value = T), "sim_index"),
                .SDcols = grep("^(pre|post)_", names(simResult)[sapply(simResult, is.logical)], value = T)
            ]
        } else crossTable <- NULL

        # ggplot(
        #     crossTable[
        #         sim_n == 10 & sim_skew == -3 & sim_kurt == 14 &
        #             var1 %like% "(post_|pre_W_(rej|acc))" &
        #             var2 %like% "(post_|pre_W_(rej|acc))",
        #         list(var1,var2,value = value * 100)
        #         ], aes(var1, var2, fill= value)) +
        #     geom_tile() +
        #     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    } else crossTable <- resultTable <- NULL

    # descriptives
    if ("description" %in% report && any(grepl("^descr_", names(simResult)))) {

        simParams <- setdiff(
            grep("^sim_", names(simResult), value = T),
            grep("^sim_(fun|cell|index)", names(simResult), value = T)
        )
        descCols  <- grep("^(descr)_", names(simResult), value = T)

        descr <- simResult[,
            c(list(
                Distribution = sim_func,
                Parameter    = paste0(
                    .SD[1, grep("^sim_n",simParams, value = T, invert = T), with = F],
                    collapse = ","
                )
            ),
            setnames(
                .SD[1, grep("^sim_n",simParams, value = T), with = F],
                gsub("sim_n", "N", grep("^sim_n",simParams, value = T))
            ),
            rbindlist(mapply(
                function(x, v) data.table(
                    descr = paste0(gsub("^descr_","", v), "_", c("min", "mean", "max")),
                    value = c(min(x), mean(x), max(x))
                ),
                .SD[, descCols, with = F],
                descCols,
                SIMPLIFY=F
            ))
            ),
            by = c("sim_func", "sim_cell"),
            .SDcols = c(simParams, descCols)
        ][,":="(sim_func=NULL, sim_cell=NULL)]


    } else descr <- NULL

    list(
        result      = resultTable,
        cross       = crossTable,
        description = descr
    )

}
