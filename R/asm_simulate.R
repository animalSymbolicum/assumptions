#' Simulate data for asm_ functions
#'
#' This function simulates the impact of the assessment of statistical assumptions (pre-tests) to pick
#' a statistical hypothesis test (post-test).
#' @param simulations integer indicating the number of simulations per configuration.
#' @param sim_n integer or list of integers for samplesize(s) to draw.
#' @param sim_func character a function name as character to use for drawing sample.
#' @param sim_args list arguments for sim_func to define different simulations.
#' @param describe function to use for descriptive parameters of each simulated sample.
#' @param pre_test function to use for conducting pre-tests.
#' @param pre_selection character vector containing the 'Names' of pre-tests given by \link[assumptions]{asm_library} to select subset of pre-tests.
#' @param post_test function to conduct post-test with.
#' @param strategies list of quotes to define decision strategies for selecting the post-test depending on pre-test (if certain
#' assumptions are not met use a different post-test). Quotes must contain the 'SimulationName' of \link[assumptions]{asm_library}.
#' @param showSimPlan logical. Set TRUE to preview all configurations of simulation.
#' @param alpha numeric indicating the global alpha-level for pre- and post-tests.
#' @param useParallel logical. If TRUE running on multiple cores using \link[parallel]{mclapply}.
#' @param dryRun logical. If TRUE test a dry run by simulating each configuration ones before running all simulations.
#' @return
#' Return value is a data.table containing the results for each simulation. Each simulation is corresponding to a row
#' in the result table.
#' \describe{
#'      \item{'sim_'}{All columns beginning with sim_ prefix are describing the configuration of the simulation.}
#'      \item{'descr_'}{All columns beginning with descr_ prefix are the result of the describe function of the drawn sample (default is \link[assumptions]{asm_reportData}).}
#'      \item{'pre_'}{All columns beginning with 'pre_' prefix are containing the results of the selected pre-tests with the selected report statistics (default is the p-value). Columns with 'SimulationName' of \link[assumptions]{asm_library} are a logical value indicating if p-value of pre-test is smaller than global alpha.}
#'      \item{'post_'}{All columns beginning with post_ prefix are containing the results of the selected post-tests with the selected report statistics (default is the p-value). Columns with only the name of the test (like post_ttest) are a logical value indicating if p-value of post-test is smaller than global alpha.}
#'      \item{'strat_'}{All columns beginning with strat_ prefix are the evaluated decision strategies.}
#' }
#' @examples
#' asm_simulate()
#' @import data.table
#' @import parallel
#' @export

asm_simulate <- function(
    simulations   = 1,
    sim_n         = c(10,30),
    sim_func      = "asm_simData", # simulation func
    sim_args      = list(),        # per sim_func list with named args inf nested if necessary
    describe      = asm_reportData,
    pre_test      = asm_preTests,
    pre_selection = NULL,
    post_test     = asm_simPostTest,
    strategies    = list(),
    # cache         = 10,
    showSimPlan   = FALSE,
    alpha         = 0.05,
    useParallel   = FALSE,
    dryRun        = TRUE
) {

    # debugging
    # sapply(formalArgs(asm_simulate), function(x) assign(x, get(x), envir = globalenv()))
    # stop("mo")

    # test input funcs
    if (length(sim_args)) dataTryArgs <- sim_args[1] else dataTryArgs <- NULL
    if (is.list(unlist(dataTryArgs, recursive = F))) dataTryArgs <- dataTryArgs[[1]]
    dataTryN <- if (is.numeric(sim_n)) sim_n[1] else list(rep(unlist(sim_n)[1], 2))
    dataTry <- try({eval(as.call(c(get(sim_func[1]), dataTryN , dataTryArgs)))})
    if ("try-error" %in% class(dataTry)) stop("sim_func can not be evaluated with sim_args")

    # skip if check simulation plan
    if (!showSimPlan) {

        # check sim functions / exclude if not defined
        if (!is.null(pre_test)) {
            preTry  <- try({pre_test(dataTry, tests = pre_selection, simulate = T)})
            if ("try-error" %in% class(preTry))  preTry  <- NULL
        } else {
            preTry <- NULL
            pre_test <- function(data, simulate) NULL
        }
        if (!is.null(post_test)) {
            postTry <- try({post_test(dataTry)})
            if ("try-error" %in% class(postTry)) postTry <- NULL
        } else {
            postTry <- NULL
            post_test <- function(data, simulate) NULL
        }
        if (!is.null(describe)) {
            descTry <- try({describe(dataTry)})
            if ("try-error" %in% class(descTry)) descTry <- NULL
        } else {
            descTry <- NULL
            describe <- function(data, simulate) NULL
        }

        # check if data and at least one test function
        stopifnot(
            !is.null(dataTry),
            sum(
                !is.null(preTry),
                !is.null(postTry),
                !is.null(descTry)
            ) > 0
        )
        # check if args given fo
        # if (length(sim_func) > 1 && length(sim_args) != length(sim_func)) stop("")

        # prepare different inputs
        if (!is.null(descTry) && is.null(names(descTry))) names(descTry) <- as.character(substitute(describe))
        if (!is.null(preTry)) preTry[] <- as.numeric(NA)

    # if simulation plan
    } else preTry <- postTry <- descTry <- NULL

    # build cell table - if any arguments
    if (length(sim_args)) {

        # expand simulation args to a preview table
        instructionPreview <- rbindlist(
            lapply(
              sim_args,
              asm_intListToTab,
              suffix = names(dataTry),
              dim = ifelse(is.list(dataTry), length(dataTry), 1)
            ),
            fill = T
        )
        instructionPreview[, sim_cell := 1:(.N)]
        instructionPreview[, sim_func := sim_func]

    # if no simulation args
    } else instructionPreview <- data.table(sim_func = sim_func, sim_cell = 1)

    # if multi var simulation
    if (length(dataTry) == 2) {

        ## sample size
        # if sample size given as vector
        if (!is.list(sim_n)) {
            # if cross join intended ()
            if (length(sim_n) != length(dataTry)) {
                sim_n <- CJ(sim_n, sim_n, unique=TRUE)
            # if sample as given
            } else {
                sim_n <- as.data.table(
                  as.list(sim_n),
                  value.name = paste0(names(sim_n), "_",names(dataTry))
                )
            }
        } else
        if (is.list(sim_n) && length(sim_n) == 2) {
            if (length(sim_n[[1]]) != length(sim_n[[2]])) {
                sim_n <- rbindlist(lapply(sim_n[[1]], CJ, sim_n[[2]]))
            } else {
                sim_n <- as.data.table(sim_n)
            }
        } else stop("")
        setnames(sim_n, paste0("sim_n_", names(dataTry)))

    } else {
        sim_n <- data.table(sim_n = sim_n)
    }

    # expand with sim func(s)
    simFuncVec <- rep(sim_func, each = nrow(sim_n))
    workingTable <- unique(
        merge.data.table(
            transform(
                rbindlist(rep(list(sim_n), length(sim_func))),
                sim_func = simFuncVec
            ),
            instructionPreview,
            by = "sim_func",
            allow.cartesian = T
        )
    )

    # if showSimPlan - return preview table
    if (showSimPlan) return(
      workingTable[,
        -c("sim_cell"), with = F
      ][,simulations := paste0("1:",simulations)
      ][]
    )

    # identify result columns
    statisticTable <- data.table::as.data.table(c(list(sim_index = 1), preTry, postTry, descTry))
    statCols       <- setdiff(names(statisticTable), "sim_index")

    ## check simulation size
    # if ((object.size(workingTable) * simulations)) stop("")


    countCells   <- workingTable[,.N]
    NParam   <- names(sim_n)

    # select test batch
    testBatch <- asm_intSelT(asm_library(print = F), pre_selection)
    preFunc   <- as.character(substitute(pre_test))
    if (preFunc %in% c("asm_distribution", "asm_homogeneity",
                                "asm_independence", "asm_randomness")) {
      testBatch <- testBatch[
        Assumption == gsub("^asm_", "", preFunc)
        ]
    }

    ### test dry run
    if (dryRun) {

      # copy table and run simulation
      dryRunTable <- copy(workingTable)
      dryRunTable[, sim_index := 1:(.N)]
      dryRunTable[,
         c(statCols) := try({

           # simulate data
           data <- eval(
             as.call(c(
               get(sim_func),
               list(unlist(.SD[, NParam, with = F], use.names =F)),
               unlist(sim_args[.SD$sim_cell], recursive = F)
             ))
           )

           # run tests
           c(
             pre_test(data, testBatch = testBatch, tests = pre_selection, simulate = T),
             post_test(data),
             describe(data)
           )

         }),
         by = names(dryRunTable),
         .SDcols =  names(dryRunTable)
      ]

      # check result
      ErrSum <- sum(sapply(dryRunTable[[statCols[1]]], function(x) grepl("^Error in", x)))
      if (ErrSum) stop(
        paste0(
          ErrSum,
          " simulation errors detected in dryRun - disable dry run or fix error simulations")
      )

      rm("dryRunTable")

    }

    # expand simulation table
    workingTable <- workingTable[rep(1:.N, simulations),]
    workingTable[, sim_index:=rep(1:simulations, each = countCells)]

    # prepare cache table
    # cacheTable <- data.table(
    #   index = 1:cache,
    # )

    # save threads -
    dtThreads <- getDTthreads()
    setDTthreads(1)

    ### RUN SIMULATION - parallel or single core
    if (useParallel) {

      # # open cluster
      # cl <- makeClusterPSOCK(availableCores())
      # on.exit(parallel::stopCluster(cl))
      # plan(cluster, workers = cl)
      # progressr::handlers(progressr::handler_progress(format="[:bar] :percent :eta :message"))

      # calculate chznk size
      chunkSize <- ceiling(nrow(workingTable) /  parallel::detectCores())

      ### run simulation
      # progressr::with_progress({
        # p <- progressr::progressor(100)
        # workingTable <- rbindlist(future.apply::future_lapply(
          workingTable <- rbindlist(mclapply(
          split(workingTable, rep(1:ceiling(nrow(workingTable) / chunkSize), each = chunkSize)[1:nrow(workingTable)]),
          function(x, pmax = chunkSize / 100) {

            setDTthreads(1)
            iterCount <- 0

            x[,
              c(statCols) := try({

                iterCount <<- iterCount + 1
                if (iterCount%%chunkSize==0) progressr::p(iterCount/chunkSize)

                # simulate data
                data <- eval(
                  as.call(c(
                    get(sim_func),
                    list(unlist(.SD[, NParam, with = F], use.names =F)),
                    unlist(sim_args[.SD$sim_cell], recursive = F)
                  ))
                )

                # run tests
                c(
                  pre_test(data, testBatch = testBatch, tests = pre_selection, simulate = T),
                  post_test(data),
                  describe(data)
                )

              }),
              by = names(x),
              .SDcols =  names(x)
            ][]

          },
          # future.seed	= TRUE,
          # future.globals = c("statCols", "sim_func", "NParam", "sim_args", "testBatch",
          #                    "pre_test", "pre_selection", "post_test", "describe"),
          # future.packages	= c("assumptions", "data.table", "progressr"),
          # future.chunk.size = 1
          mc.cores = parallel::detectCores()#,
        ))
      # })

      # stop cluster
      # parallel::stopCluster(cl)

    # single core simulation
    } else {
        workingTable[,
          c(statCols) := try({

              # simulate data
              data <- eval(
                  as.call(c(
                      get(sim_func),
                      list(unlist(.SD[, NParam, with = F], use.names =F)),
                      unlist(sim_args[.SD$sim_cell], recursive = F)
                  ))
              )

              # run tests
              c(
                  pre_test(data, testBatch = testBatch, tests = pre_selection, simulate = T),
                  post_test(data),
                  describe(data)
              )

          }),
          by = names(workingTable),
          .SDcols =  names(workingTable)
      ]
    }

    # if any error - number of errors
    ErrSum <- sum(sapply(workingTable[[statCols[1]]], function(x) grepl("^Error in", x)))
    if (ErrSum) cat(paste0(ErrSum, " simulation errors detected"))

    # evaluate alpha level as logical variables
    for (testVar in grep("^(pre|post)_.+value$", names(workingTable), value = T)) {
        workingTable[,
           c(gsub("_p[.]*value$", "", testVar)) := get(testVar) <= alpha
        ]
    }

    # apply strategy
    asm_simStrategy(workingTable, strategies)

    # reset DT threads
    setDTthreads(dtThreads)

    # return simulation result
    workingTable[]

}

#' Apply pre-test strategy to simulation result
#'
#' This function can be used to apply a pre-test strategy to simulation result.
#' @param simulationTable asm_simulate() result table.
#' @param strategies a list with quoted expression to reference on columns of asm_simulate() result table.
#' @examples
#' asm_simStrategy(
#'   asm_simulate(10),
#'   list(example = quote(ifelse(pre_W_X & pre_W_Y, post_ttest, post_wilcox)))
#' )[, list(pre_W_X, pre_W_Y, post_ttest, post_wilcox, strat_example)]
#' @import data.table
#' @export

asm_simStrategy <- function(simulationTable, strategies = NULL) {

  # add decision strategies
  if (length(unlist(strategies)))
  for (strat in names(strategies))
  simulationTable[, c(paste0("strat_", strat)) := eval(strategies[[strat]])]

  simulationTable
}

#' Simulate a single variable
#'
#' This function simulates contaminated variables as a mix of different distributions.
#' @param n integer samplesize to draw.
#' @param distr distribution function to draw sample with.
#' @param args args passed to distribution function.
#' @param share share of distributions.
#' @param sorting how to sort the simulated variable. 'grouped' means its sorted by the different distributions its sampled from.
#' @examples
#' asm_simVar()
#' @export

asm_simVar <- function(n     = 30,
                       distr = c("rnorm", "rnorm"),
                       args  = list(
                         list(sd = 1, mean = 0),
                         list(sd = 1, mean = 0)
                       ),
                       share = c(0.3, 0.7),
                       sorting = c("random", "grouped")[1]) {

  #
  if (is.list(distr)) distr <- unlist(distr)
  if (is.list(sorting)) sorting <- unlist(sorting)
  if (is.list(share)) share <- unlist(share)

  # draw sample
  mapply(
    function(func, arg, samplesize) {
      eval(
        as.call(c(
          list(eval(parse(text = func))),
          samplesize,
          unlist(arg)
        ))
      )
    },
    distr,
    args,
    n * share,
    SIMPLIFY = FALSE
  ) ->
  sampleList

  # if any dependence
  # if (any(dependence != 0) && length(unique(share)) == 1) {
  #     # simcor <- function (x, ymean=0, ysd=1, correlation=0) {
  #     #     n <- length(x)
  #     #
  #     #     y <- rnorm(n)
  #     #     z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
  #     #         scale(resid(lm(y ~ x)))[,1]
  #     #     yresult <- ymean + ysd * z
  #     #     yresult
  #     # }
  # }

  # sort result depending on mode
  switch(
    sorting,
    random   = unlist(sampleList, use.names =FALSE)[sample(1:n, n)],
    grouped  = unlist(sampleList, use.names =FALSE),
    ordered  = sort(unlist(sampleList, use.names =FALSE)),
    coxorder = {
      sort(unlist(sampleList, use.names =FALSE))[
        c(
          seq(1, n, by = 2),
          sample(seq(2, n, by = 2), n / 2)
        )[
          unlist(sapply(1:(n/2), function(x) c(x, x+(n/2)), simplify=F))
          ]

        ]
    }
  )

}

#' Simulate multiple variables
#'
#' This function simulates multiple variables with given distribution functions.
#' @param N integer sample size to draw.
#' @param distr distribution function to draw sample with.
#' @param dots args passed to distribution function.
#' @param suffix how to name simulated variables.
#' @examples
#' asm_simData()
#' @export

asm_simData <- function(
  N     = c(30, 40),
  distr = c("rnorm", "rnorm"),
  dots  = rep(list(list()), length(distr)),
  suffix = c("X", "Y")
) {

  setNames(mapply(
    function(func, arg, samplesize) {
      eval(
        as.call(c(
          list(eval(parse(text = func))),
          samplesize,
          unlist(arg)
        ))
      )
    },
    distr,
    dots,
    N,
    SIMPLIFY = FALSE
  ), suffix)

}

#' Conduct hypothesis test in simulation
#'
#' This function is used in asm_simulate() to calculate hypothesis test like t-test, welch or wilcoxon.
#' @param data a vector, list or data.frame of data values.
#' @param paired logical - conduct a paired hypothesis test.
#' @param report which statistics should be saved in simulation.
#' @examples
#' asm_simPostTest(rnorm(10))
#' asm_simPostTest(iris[, c("Sepal.Width", "Sepal.Length")])
#' @import data.table
#' @export

asm_simPostTest <- function(data, paired = FALSE, report = c("p.value", "conf.int", "statistic")[1]) {

  # format function
  formatTest <- function(result, suffix) {
    # flaten results and rename
    result <- as.list(unlist(result))
    setNames(result, paste0("post_", suffix, "_", names(result)))
  }

  # two sample case
  if (is.list(data) && length(data) == 2) {

    if (paired) {

      c(
        formatTest(t.test(data[[1]], data[[2]], paired = T)[report], "ttest"),
        formatTest(wilcox.test(data[[1]], data[[2]], paired = T)[report], "wilcox")
      )

    } else {

      c(
        formatTest(t.test(data[[1]], data[[2]])[report], "welch"),
        formatTest(t.test(data[[1]], data[[2]], var.equal = T)[report], "ttest"),
        formatTest(wilcox.test(data[[1]], data[[2]])[report], "wilcox")
      )

    }

  # if on sample case
  } else
  if (!is.list(data) || length(data) == 1) {

    if (is.list(data)) data <- data[[1]]

    c(
      formatTest(t.test(data)[report], "ttest"),
      formatTest(wilcox.test(data)[report], "wilcox")
    )

  # multivar not implemented yet..
  } else stop("Only one and two sample cases are implemented")


}

