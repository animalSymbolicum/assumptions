#' List of all assumptions tests
#'
#' This function returns a data.table with all pre-tests and graphs to assess statistical assumptions.
#' @param index a integer indicating the row number of data.table to open documentation for indexed function.
#' @param print logical. If TRUE print also the Name, Package, Link to documentation and the name of the
#' reference column for \link[assumptions]{asm_simulate} results.
#' @examples
#' asm_library()
#' # shortcut to open documantation for 33rd test
#' asm_library(33)
#' @import PoweR
#' @import data.table
#' @import PoweR
#' @export

asm_library <- function(index = NULL, print = TRUE) {

    # load dependency functions
    powerLib <- as.data.table(PoweR::getindex()[["mat.stats"]][c(1:37,41), ][-c(3:5,  19:20, 23:32, 34, 38), ])# , 1:15,, 38), ])
    powerLib[, Alter := sapply(strsplit(Alter, ","), function(x) max(as.numeric(x)))]
    names(powerLib) <- tolower(names(powerLib))

    # clean names to be usable in decision rules
    powerLib$stat <- gsub("[(){}]","", powerLib$stat)
    powerLib$stat <- gsub("[^a-zA-Z0-9]","_", powerLib$stat)
    powerLib$stat <- gsub("__","_", powerLib$stat)
    powerLib$stat[powerLib$stat=="Q_"]<-"Q_Asterics"
    powerLib$stat[powerLib$stat=="W_"]<-"W_Quote"
    powerLib$stat <- gsub("_$","", powerLib$stat)

    # minimal technical table indexing all functions/graphs
    libTable <- rbind(
        # distribution 'PoweR' package
        data.table::data.table(
            Assumption = "distribution",
            powerLib
        ),
        # randomness 'randtests' package
        data.table::data.table(
            Assumption = "randomness",
            index    = 50:54,
            stat     = c("bartels.rank", "cox.stuart", "difference.sign",
                         "rank", "runs"),
            alter    = as.numeric(NA),
            nbparams = as.numeric(NA)
        ),
        # homogeneity 'ExpDes' package
        data.table::data.table(
            Assumption = "homogeneity",
            index    = 60:63,
            stat     = c("ftest", "bartlett", "levene", "oneillmathews"), #, "samiuddin", "layard"
            alter    = as.numeric(NA),
            nbparams = as.numeric(NA)
        ),
        # independence
        data.table::data.table(
            Assumption = "independence",
            index    = 70:71,
            stat     = c("Box.Pierce", "Ljung.Box"),
            alter    = as.numeric(NA),
            nbparams = as.numeric(NA)
        ),
        # joint assumption testing
        data.table::data.table(
            Assumption = "iid",
            index    = 80,
            stat     = c("asm_preJoint"),
            alter    = as.numeric(NA),
            nbparams = as.numeric(NA)
        )
    )

    # pre test descriptions
    if (print) {

        libTable[,":="(alter = NULL, nbparams=NULL)]

        # add graphs
        libTable <- rbind(
            libTable,
            data.table(
                Assumption =  c(
                    "homogeneity", "distribution","distribution","distribution",
                    "distribution","distribution","distribution","randomness",
                    "randomness", "independence","distribution","homogeneity"
                ),
                index = 81:92,
                stat = c(
                    "asm_graphBoxplot", "asm_graphDotplot","asm_graphEcdf","asm_graphHist",
                    "asm_graphCummean","asm_graphPPlot","asm_graphQQplot","asm_graphRuns",
                    "asm_graphDifferencesign","asm_graphScatterplot","asm_graphCumvar","asm_graphWindow"
                )
            )
        )

        libTable <- asm_internalLibNames(libTable)
        libTable <- libTable[,
            list(Assumption, Name, Package, Help, SimulationName = ifelse(
                grepl("^Graph ", Name),
                "-",
                paste0("pre_", stat)
            ) )
        ]

    } else {
        libTable <- asm_internalLibNames(libTable)[,-c("Package", "Help"), with = F]
    }

    # if help file indexed
    if (!is.null(index) && print) {
        libTable[index, Help]
    } else libTable

}
