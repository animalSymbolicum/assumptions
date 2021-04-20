#' Convert list to data.table
#'
#' This function converts a list of arguments for sim_func of asm_simulate to a data.table.
#' @param li integer samplesize to draw.
#' @param suffix distribution function to draw sample with.
#' @keywords internal
#' @examples
#' asm_intListToTab(list(list(mean = 1, sd = 1)))
#' @export

asm_intListToTab <- function(li, suffix = LETTERS[1:length(li)], dim = length(li)) {


    if (dim == 1) {

      res <- as.data.table(as.list(unlist(li)))
      setnames(res, paste0("sim_", names(res)))
      names(res)[duplicated(names(res))] <- paste0(names(res)[duplicated(names(res))], seq_along(sum(duplicated(names(res)))))
      res

    } else {

      # unnested list
      if (all(!sapply(li, is.list))) {
          res <- as.data.table(li)
          if (names(res) == "li") setnames(res, "arg")
      } else {
          res <- rbindlist(list(li), fill = T)
      }

      # nested list level one
      nesOne <- Reduce(cbind, mapply(
          function(x, nam) {
              tmp <- rbindlist(x, fill = T)
              names(tmp) <- paste0(nam, "_", names(tmp))
              tmp
          },
          res[, sapply(res, is.list), with = F],
          names(res)[sapply(res, is.list)],
          SIMPLIFY = FALSE
      ))

      if (length(suffix) && sum(dim(res))>2) {
          Reduce(cbind, mapply(
              function(x, nam) {
                  setnames(x, names(x), paste0("sim_", names(x), "_", nam))
              },
              split(cbind(res[, !sapply(res, is.list), with = F], nesOne), 1:nrow(res)),
              suffix,
              SIMPLIFY = FALSE
          ))
      } else setnames(res, paste0("sim_", names(res)))

    }

}

#' Appends pretty names to asm_library()
#'
#' This function appends full names and links to their documentation of tests in asm_library() table.
#' @param libTable integer samplesize to draw.
#' @keywords internal
#' @examples
#' asm_library(print = T)
#' @import data.table
#' @export

asm_internalLibNames <- function(libTable) {

  stopifnot("asm_internalLibNames should only be applied in context of asm_library()"=is.data.table(libTable))
  stopifnot("asm_internalLibNames should only be applied in context of asm_library()"=c("stat", "index") %in% names(libTable))

  data.table(
      index = as.character(c(1:37, 41, 50:54, 60:65, 70:71, 80:92)),
      Name  = c(
          "Lilliefors",
          "Anderson-Darling",
          "1st Zhang-Wu",
          "2nd Zhang-Wu",
          "Glen-Leemis-Barr",
          "D'Agostino-Pearson",
          "Jarque-Bera",
          "Doornik-Hansen",
          "Gel-Gastwirth",
          "1st Hosking",
          "2nd Hosking",
          "3rd Hosking",
          "4th Hosking",
          "1st Bontemps-Meddahi",
          "2nd Bontemps-Meddahi",
          "Brys-Hubert-Struyf",
          "Bonett-Seier",
          "Brys-Hubert-Struyf & Bonett-Seier",
          "1st Cabana-Cabana",
          "2nd Cabana-Cabana",
          "Shapiro-Wilk",
          "Shapiro-Francia",
          "Shapiro-Wilk modified by Rahman-Govindarajulu",
          "D'Agostino",
          "Filliben",
          "Chen-Shapiro",
          "1st Zhang",
          "3rd Zhang",
          "Barrio-Cuesta-Matran-Rodriguez",
          "Coin",
          "Epps-Pulley",
          "Martinez-Iglewicz",
          "Gel-Miao-Gastwirth",
          "2nd Zhang",
          "R_n",
          "X_{APD}",
          "Z_{EPD}",
          "Spiegelhalter",
          "Bartels Rank",
          "Cox Stuart",
          "Difference Sign",
          "Rank",
          "Runs",
          "F-Test",
          "Bartlett",
          "Layard",
          "Levene",
          "ONeill and Mathews",
          "Samiuddin",
          "Box-Pierce",
          "Ljung-Pierce",
          "Spanos Auxiliary Regression",
          "Graph Boxplot", "Graph Dotplot","Graph ecdf","Graph Histogram",
          "Graph Mean Increase","Graph P-P-Plot","Graph Q-Q-plot","Graph Randruns",
          "Graph Differencesign","Graph t-Plot","Graph Var Increase","Graph Window"
    ),
    Package = c(
      rep("PoweR", 38), rep("randtests", 5), "stats" ,rep("ExpDes", 5), rep("stats", 2),
      rep("assumptions", 13)
    ),
    Help    = c(
            list(list(help("stat0001", package = "PoweR"))),
            list(list(help("stat0002", package = "PoweR"))),
            list(list(help("stat0003", package = "PoweR"))),
            list(list(help("stat0004", package = "PoweR"))),
            list(list(help("stat0005", package = "PoweR"))),
            list(list(help("stat0006", package = "PoweR"))),
            list(list(help("stat0007", package = "PoweR"))),
            list(list(help("stat0008", package = "PoweR"))),
            list(list(help("stat0009", package = "PoweR"))),
            list(list(help("stat0010", package = "PoweR"))),
            list(list(help("stat0011", package = "PoweR"))),
            list(list(help("stat0012", package = "PoweR"))),
            list(list(help("stat0013", package = "PoweR"))),
            list(list(help("stat0014", package = "PoweR"))),
            list(list(help("stat0015", package = "PoweR"))),
            list(list(help("stat0016", package = "PoweR"))),
            list(list(help("stat0017", package = "PoweR"))),
            list(list(help("stat0018", package = "PoweR"))),
            list(list(help("stat0019", package = "PoweR"))),
            list(list(help("stat0020", package = "PoweR"))),
            list(list(help("stat0021", package = "PoweR"))),
            list(list(help("stat0022", package = "PoweR"))),
            list(list(help("stat0023", package = "PoweR"))),
            list(list(help("stat0024", package = "PoweR"))),
            list(list(help("stat0025", package = "PoweR"))),
            list(list(help("stat0026", package = "PoweR"))),
            list(list(help("stat0027", package = "PoweR"))),
            list(list(help("stat0028", package = "PoweR"))),
            list(list(help("stat0029", package = "PoweR"))),
            list(list(help("stat0030", package = "PoweR"))),
            list(list(help("stat0031", package = "PoweR"))),
            list(list(help("stat0032", package = "PoweR"))),
            list(list(help("stat0033", package = "PoweR"))),
            list(list(help("stat0034", package = "PoweR"))),
            list(list(help("stat0035", package = "PoweR"))),
            list(list(help("stat0036", package = "PoweR"))),
            list(list(help("stat0037", package = "PoweR"))),
            list(list(help("stat0041", package = "PoweR"))),
            list(list(help("bartels.rank.test", package = "randtests"))),
            list(list(help("cox.stuart.test", package = "randtests"))),
            list(list(help("difference.sign.test", package = "randtests"))),
            list(list(help("rank.test", package = "randtests"))),
            list(list(help("runs.test", package = "randtests"))),
            list(list(help("var.test", package = "stats"))),
            list(list(help("bartlett", package = "ExpDes"))),
            list(list(help("layard", package = "ExpDes"))),
            list(list(help("levene", package = "ExpDes"))),
            list(list(help("oneillmathews", package = "ExpDes"))),
            list(list(help("samiuddin", package = "ExpDes"))),
            list(list(help("Box.test", package = "stats"))),
            list(list(help("Box.test", package = "stats"))),
            list(list(help("asm_preJoint", package = "assumptions"))),
            list(list(help("asm_graphBoxplot", package = "assumptions"))),
            list(list(help("asm_graphDotplot", package = "assumptions"))),
            list(list(help("asm_graphEcdf", package = "assumptions"))),
            list(list(help("asm_graphHist", package = "assumptions"))),
            list(list(help("asm_graphCummean", package = "assumptions"))),
            list(list(help("asm_graphPPlot", package = "assumptions"))),
            list(list(help("asm_graphQQplot", package = "assumptions"))),
            list(list(help("asm_graphRuns", package = "assumptions"))),
            list(list(help("asm_graphDifferencesign", package = "assumptions"))),
            list(list(help("asm_graphScatterplot", package = "assumptions"))),
            list(list(help("asm_graphCumvar", package = "assumptions"))),
            list(list(help("asm_graphWindow", package = "assumptions")))
        )
  )[libTable, on = "index"][]

}

#' Select pre-test
#'
#' This function selects a pre-test from asm_library() table for usage in asm_ functions.
#' @param testTable asm_library() table.
#' @param testSelection name given in 'Name' column or index of asm_library() table.
#' @keywords internal
#' @examples
#' asm_intSelT(asm_library(), "Runs")
#' asm_intSelT(asm_library(), c(3, 33))
#' @import data.table
#' @export

asm_intSelT <- function(testTable, testSelection) {

    # select test
    if (!is.null(testSelection)) {
        if (is.character(testSelection)) testTable <- testTable[Name %in% testSelection]
        if (is.numeric(testSelection)) testTable   <- testTable[testSelection]
        if (testTable[!is.na(Assumption), .N == 0]) stop("No test selected")
    }

    testTable[!is.na(Assumption)]

}

#' Apply homogeneity test in asm_homogeneity
#'
#' This function is used in asm_homogeneity to conduct homogeneity tests.
#' @param data a data.frame with at least two columns or a list of length two.
#' @param tests vector of tests.
#' @keywords internal
#' @examples
#' asm_intHomogTest(list(A = rnorm(30), B = rnorm(40)), "levene")
#' @import ExpDes
#' @export

asm_intHomogTest <- function(data,
                               test = c("ftest", "bartlett", "levene", "oneillmathews")[1]) { #"layard", "samiuddin"

  # check inputs
  match.arg(test, c("ftest", "bartlett", "layard", "levene", "oneillmathews", "samiuddin"))

  if (any(c("data.frame", "data.table", "tibble") %in% class(data))) data <- as.list(data)

  # perform ExpDes homogeneity test
  try({list(pvalue = switch(
    test,
    ftest = stats::var.test(
      data[[1]],
      data[[2]]
    )$p.value,
    bartlett = ExpDes::bartlett(
      rep(1:length(data), times = sapply(data, length)),
      unlist(data),
      length(data),
      sapply(data, length)
    ),
    # layard = ExpDes::layard(
    #     rep(1:length(data), times = sapply(data, length)),
    #     unlist(data),
    #     length(data),
    #     sapply(data, length)
    # ),
    levene = ExpDes::levene(
      rep(1:length(data), times = sapply(data, length)),
      unlist(data),
      length(data),
      sapply(data, length)
    ),
    oneillmathews = ExpDes::oneillmathews(
      rep(1:length(data), times = sapply(data, length)),
      unlist(data),
      length(data),
      sapply(data, length)
    )#,
    # samiuddin = ExpDes::samiuddin(
    #     rep(1:length(data), times = sapply(data, length)),
    #     unlist(data),
    #     length(data),
    #     sapply(data, length)
    # )
  ))})


}

#' Check input data for asm_graph functions
#'
#' This function tests a vector if it containts numeric data with at least two not NA datapoints.
#' @param data a vector, list or data.frame of data values.
#' @param return logical. Return data.
#' @keywords internal
#' @examples
#' asm_intCheckData()
#' @export
asm_intCheckData <- function(data, return = T) {

  # checks for data to be plottable
  stopifnot("Function needs numeric data"=is.numeric(data))
  stopifnot("Function needs at least two non NA values"=length(na.omit(data)) > 2)

  if (return) na.omit(data) else NULL

}
