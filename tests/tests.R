require(assumptions)
library(testthat)

### dictionaries ###################################################################################
test_that("dictionaries", {

    expect_output(assumptions(t.test), "^The t.test makes")
    expect_output(assumptions(wilcox.test), "^If paired")

    expect_is(asm_library(), "data.table")
    expect_named(
        asm_library(),
        c("Assumption", "Name", "Package", "Help", "SimulationName")
    )

})
### graphs #########################################################################################
errorData <- list(list(NA), list(1), list(Inf), list(c(1,NA)), list(c(1:2)))

for (i in errorData) {
    i <- i[[1]]
    test_that("Throw an error", {
        for (graphFunc in ls("package:assumptions", pattern = "asm_graph")) {
            expect_error(get(graphFunc)(i))
        }
    })
}

minData <- list(list(rnorm(10)), list(rep(1, 10)), list(rep(rnorm(100))))
for (i in minData) {
    i <- i[[1]]
    test_that("plotting is succesfull", {
        for (graphFunc in ls("package:assumptions", pattern = "asm_graph")) {
            expect_is(get(graphFunc)(i), "ggplot")
        }
    })
}

### pre tests ######################################################################################
# asm_(dist|rand|ind)
test_that("asm_(dist|rand|ind) var tests", {
    for (graphFunc in c("asm_distribution", "asm_randomness", "asm_independence"))
    expect_is(get(graphFunc)(rnorm(10)), "data.table")
    expect_is(get(graphFunc)(list(rnorm(10), rnorm(10))), "data.table")
    expect_is(get(graphFunc)(list(X = rnorm(10), Y = rnorm(10))), "data.table")
    expect_is(get(graphFunc)(data.frame(rnorm(10), rnorm(10))), "data.table")
    expect_is(get(graphFunc)(data.frame(X=rnorm(10), Y=rnorm(10))), "data.table")
    expect_is(get(graphFunc)(rnorm(10), simulate = T), "data.table")
    expect_is(get(graphFunc)(list(X = rnorm(10), Y = rnorm(10)), simulate = T), "data.table")
})

# add asm_homogeneity, add error cases - only NA..

# ### simulation #####################################################################################
test_that("asm_simulate standards", {

    # test different levels of data functions
    expect_is(
        asm_simulate(1, 10, "rnorm"),
        "data.table"
    )
    expect_is(
        asm_simulate(1, 10, "rnorm", list(mean = 0, sd = 2)),
        "data.table"
    )
    expect_is(
        asm_simulate(1, 10, c("rnorm", "rexp"), list(list(mean = 0, sd = 2), list(rate = 1))),
        "data.table"
    )
    expect_is(
        asm_simulate(1, c(10,20,30), c("rnorm", "rexp"), list(list(mean = 0, sd = 2), list(rate = 1))),
        "data.table"
    )
    expect_is(
        asm_simulate(1, c(10,20,30), "rnorm"),
        "data.table"
    )
    expect_is(
        asm_simulate(1, c(10,20,30), "rnorm", list(mean = 0, sd = 2)),
        "data.table"
    )
    expect_is(
        asm_simulate(1, c(10,20,30), c("rnorm", "rexp"), list(list(mean = 0, sd = 2), list(rate = 1))),
        "data.table"
    )
    expect_is(
        asm_simulate(
            sim_n = list(c(10, 30, 10, 30, 30), c(10, 30,30,10,100)),
            sim_args = list(list(
                distr = c("rnorm",  "rnorm"),
                dots = list(list(), list(mean = 1, sd = 2))
            )),
            pre_selection = c(1,46)
        ),
        "data.table"
    )
})

test_that("asm_simData ", {

    # test different levels of data functions
    expect_is(
        asm_simData(),
        "list"
    )
    expect_is(
        asm_simData(c(10, 20), rep("asm_simVar", 2)),
        "list"
    )
})

### thesis simulations #############################################################################

# load required packages
require(MASS)
require(miceadds)
require(magrittr)
require(purrr)
require(data.table)

test_that("Thesis Example 1", {
    example <- asm_simulate(
        simulations = 1,
        sim_n = list(c(10,30,10,30,30), c(10,30,30,10,100)),
        pre_selection = c("Shapiro-Wilk", "Levene"),
        sim_func = rep("asm_simData", 24),
        sim_args = mapply(function(distr, sig, delta) {
            list(
                distr = c("fleishman_sim",  "fleishman_sim"),
                dots  = list(
                    list(mean = delta, sd = sig, skew = distr[1], kurt = distr[2]),
                    list(skew = distr[3], kurt = distr[4])
                )
            )
        },
        rep(list(c(0,0,0,0), c(0,15,0,15),
                 c(0.5,15,0.5,15), c(1,15,1,15),
                 c(3,15,3,15), c(0,0,3,15)),
            4
        ),
        as.list(rep(rep(1:2, each = 6), 2)),
        as.list(rep(c(0, 1), each = 12)),
        SIMPLIFY=FALSE
        )
    )

    expect_equal(
        dim(example),
        c(120, 37)
    )
})

test_that("Thesis Example 2", {

     # zufällige Ziehung aus Normalverteilung oder t3 Verteilung
    mySim <- function(n, share = 0.5) {
        if (rbinom(1,1,share)) list(X=rnorm(n[1],0), Y=rnorm(n[2],0.5)) else
            list(X=rt(n[1], 3), Y= rt(n[2], 3)+1)
    }

    assign("mySim",mySim,envir = globalenv())

    # Simulation der Shamsudheen (2015) Studie [mit bekannten Parametern]
    result <- asm_simulate(
        simulations   = 1,
        sim_n         = list(20, 20),
        sim_func      = "mySim",
        sim_args      = seq(0, 1, 0.1),
        pre_test      = asm_distribution,
        pre_selection = "Shapiro-Wilk",
        showSimPlan   = F,
        strategies    = list(
            shamsudheen=quote(ifelse(pre_W_X | pre_W_Y, post_welch, post_wilcox))
        )
    )

    expect_equal(
        dim(result),
        c(11, 27)
    )
})

test_that("Thesis Simulation 1", {

        # Schiefe & Wölbungs Parameterspanne
        kurt <- -2:15
        skew <- -3:3

        # Mögliche Kombinationen
        combins <- rbindlist(map(skew, ~ data.table(skew = .x, kurt = kurt[kurt >= (.x^2) - 2])))

        # Identifizierung möglicher Kombination (andernfalls - keine Konvergenz)
        for (i in 1:nrow(combins)) {
            combins[
                i,
                succ := !"try-error" %in%
                    class(try(fleishman_sim(10, skew = combins[i, skew], kurt = combins[i, kurt]), silent = T))
                ]
        }

        # Liste mit möglicher Parameterkombinationen
        combins[
        succ == T,
        list(skew, kurt)
        ] %>%
        split(1:nrow(.)) %>%
        map(as.list) %>%
        setNames(NULL) ->
        simArgs

        sim_distr_1 <- asm_simulate(
            simulations = 1,
            sim_n = c(10,20,30),
            pre_test = asm_distribution,
            sim_func = rep("fleishman_sim", length(simArgs)),
            sim_args = simArgs
        )

        # Zusammenfassung der Simulation
        sim_distr_1_report <- asm_reportSim(sim_distr_1, digits = 2)

        # check for all parameters
        expect_named(
            sim_distr_1,
            c(
                'sim_func', 'sim_n', 'sim_skew', 'sim_kurt', 'sim_cell', 'sim_index', 'pre_K_S_pvalue',
                'pre_AD_pvalue', 'pre_K_2_pvalue', 'pre_JB_pvalue', 'pre_DH_pvalue', 'pre_RJB_pvalue',
                'pre_T_Lmom_pvalue', 'pre_T_Lmom_1_pvalue', 'pre_T_Lmom_2_pvalue', 'pre_T_Lmom_3_pvalue',
                'pre_BM_3_4_pvalue', 'pre_BM_3_6_pvalue', 'pre_T_MC_LR_pvalue', 'pre_T_w_pvalue',
                'pre_T_MC_LR_T_w_pvalue', 'pre_W_pvalue', 'pre_W_Quote_pvalue', 'pre_R_sJ_pvalue',
                'pre_R_n_pvalue', 'pre_X_APD_pvalue', 'pre_Z_EPD_pvalue', 'post_ttest_p.value',
                'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew', 'descr_kurtosis',
                'descr_sd', 'pre_K_S', 'pre_AD', 'pre_K_2', 'pre_JB', 'pre_DH', 'pre_RJB', 'pre_T_Lmom',
                'pre_T_Lmom_1', 'pre_T_Lmom_2', 'pre_T_Lmom_3', 'pre_BM_3_4', 'pre_BM_3_6', 'pre_T_MC_LR',
                'pre_T_w', 'pre_T_MC_LR_T_w', 'pre_W', 'pre_W_Quote', 'pre_R_sJ', 'pre_R_n', 'pre_X_APD',
                'pre_Z_EPD', 'post_ttest', 'post_wilcox'
            )
        )
        expect_equal(dim(sim_distr_1), c(219, 57))
        expect_named(
            sim_distr_1_report,
            c('result', 'cross', 'description')
        )
        expect_equal(dim(sim_distr_1_report$result), c(219, 28))
        expect_equal(dim(sim_distr_1_report$description), c(1095, 5))

        # Anwendung verschiedener Strategien zur Identifikation relevanter NV-Abweichungen
        #   FALSE Wert als Ausschluss
        sim_distr_1 <- asm_simStrategy(sim_distr_1, strategies = list(
            W_only   = quote(fifelse(!pre_W, post_ttest, F)),
            W_high   = quote(fifelse(pre_W_pvalue >= 0.001, post_ttest, F)),
            W_higher = quote(fifelse(pre_W_pvalue >= 0.00005, post_ttest, F))
        ))
        strategyResult <- asm_reportSim(sim_distr_1, 2, "result")$result


        expect(
            all(c('strat_W_only', 'strat_W_high', 'strat_W_higher') %in% names(sim_distr_1)),
           "asm_simStrategy fails"
        )
        expect(
            all(c('strat_W_only', 'strat_W_high', 'strat_W_higher') %in% names(strategyResult)),
            "asm_reportSim fails"
        )

    })

test_that("Thesis Simulation 2", {

        kurt <- -2:15
        skew <- -3:3

        # Mögliche Kombinationen
        combins <- rbindlist(map(skew, ~ data.table(skew = .x, kurt = kurt[kurt >= (.x^2) - 2])))

        # Identifizierung möglicher Kombination (andernfalls - keine Konvergenz)
        for (i in 1:nrow(combins)) {
            combins[
                i,
                succ := !"try-error" %in%
                    class(try(fleishman_sim(10, skew = combins[i, skew], kurt = combins[i, kurt]), silent = T))
                ]
        }

        ### Power Analyse
        # Ergänzung des wahren MW von 1
        combins[
            succ == T,
            list(skew, kurt, mean = 1)
        ] %>%
        split(1:nrow(.)) %>%
        map(as.list) %>%
        setNames(NULL) ->
        simArgs2

        # Simulation aller Fleishman Parameter hinsichtlich der Power
        sim_distr_Power <- asm_simulate(
            simulations = 1,
            sim_n = c(10,20,30),
            pre_test = asm_distribution,
            sim_func = rep("fleishman_sim", length(simArgs2)),
            sim_args = simArgs2
        )

        # check for all parameters
        expect_named(
            sim_distr_Power,
            c(
                'sim_func', 'sim_n', 'sim_skew', 'sim_kurt', 'sim_mean', 'sim_cell', 'sim_index', 'pre_K_S_pvalue', 'pre_AD_pvalue', 'pre_K_2_pvalue', 'pre_JB_pvalue', 'pre_DH_pvalue', 'pre_RJB_pvalue', 'pre_T_Lmom_pvalue', 'pre_T_Lmom_1_pvalue', 'pre_T_Lmom_2_pvalue', 'pre_T_Lmom_3_pvalue', 'pre_BM_3_4_pvalue', 'pre_BM_3_6_pvalue', 'pre_T_MC_LR_pvalue', 'pre_T_w_pvalue', 'pre_T_MC_LR_T_w_pvalue', 'pre_W_pvalue', 'pre_W_Quote_pvalue', 'pre_R_sJ_pvalue', 'pre_R_n_pvalue', 'pre_X_APD_pvalue', 'pre_Z_EPD_pvalue', 'post_ttest_p.value', 'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew', 'descr_kurtosis', 'descr_sd', 'pre_K_S', 'pre_AD', 'pre_K_2', 'pre_JB', 'pre_DH', 'pre_RJB', 'pre_T_Lmom', 'pre_T_Lmom_1', 'pre_T_Lmom_2', 'pre_T_Lmom_3', 'pre_BM_3_4', 'pre_BM_3_6', 'pre_T_MC_LR', 'pre_T_w', 'pre_T_MC_LR_T_w', 'pre_W', 'pre_W_Quote', 'pre_R_sJ', 'pre_R_n', 'pre_X_APD', 'pre_Z_EPD', 'post_ttest', 'post_wilcox'
            )
        )
        expect_equal(
            dim(sim_distr_Power),
            c(219, 58)
        )

        # Anwendung der Strategie bei Power Simulation
        sim_distr_Power <- asm_simStrategy(sim_distr_Power, strategies = list(
            W_only   = quote(fifelse(!pre_W, post_ttest, F)),
            W_high   = quote(fifelse(pre_W_pvalue >= 0.001, post_ttest, F)),
            W_higher = quote(fifelse(pre_W_pvalue >= 0.00005, post_ttest, F))
        ))

        strategyResultPower <- asm_reportSim(sim_distr_Power, 2, "result")$result


        expect(
            all(c('strat_W_only', 'strat_W_high', 'strat_W_higher') %in% names(sim_distr_Power)),
            "asm_simStrategy fails"
        )
        expect(
            all(c('strat_W_only', 'strat_W_high', 'strat_W_higher') %in% names(strategyResultPower)),
            "asm_reportSim fails"
        )

})

test_that("Thesis Simulation 3", {

    # Variation der problematischen Verteilungen über die Gruppengröße
    sim_distr_2 <- asm_simulate(
        simulations = 1,
        sim_n = c(40,50,60,70,80,90,100,150,200),
        pre_test = asm_distribution,
        sim_func = rep("fleishman_sim", 4),
        sim_args = c(
            list(list(skew = 2, kurt = 5)),
            list(list(skew = -2, kurt = 5)),
            list(list(skew = 3, kurt = 14)),
            list(list(skew = -3,kurt = 14))
        )
    )

    # Zusammenfassung der Simulation
    sim_distr_2_report <- asm_reportSim(sim_distr_2, digits = 2)

    expect_named(
        sim_distr_2,
        c(
           'sim_func', 'sim_n', 'sim_skew', 'sim_kurt', 'sim_cell', 'sim_index', 'pre_K_S_pvalue',
           'pre_AD_pvalue', 'pre_K_2_pvalue', 'pre_JB_pvalue', 'pre_DH_pvalue', 'pre_RJB_pvalue',
           'pre_T_Lmom_pvalue', 'pre_T_Lmom_1_pvalue', 'pre_T_Lmom_2_pvalue', 'pre_T_Lmom_3_pvalue',
           'pre_BM_3_4_pvalue', 'pre_BM_3_6_pvalue', 'pre_T_MC_LR_pvalue', 'pre_T_w_pvalue',
           'pre_T_MC_LR_T_w_pvalue', 'pre_W_pvalue', 'pre_W_Quote_pvalue', 'pre_R_sJ_pvalue',
           'pre_R_n_pvalue', 'pre_X_APD_pvalue', 'pre_Z_EPD_pvalue', 'post_ttest_p.value',
           'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew', 'descr_kurtosis',
           'descr_sd', 'pre_K_S', 'pre_AD', 'pre_K_2', 'pre_JB', 'pre_DH', 'pre_RJB',
           'pre_T_Lmom', 'pre_T_Lmom_1', 'pre_T_Lmom_2', 'pre_T_Lmom_3', 'pre_BM_3_4',
           'pre_BM_3_6', 'pre_T_MC_LR', 'pre_T_w', 'pre_T_MC_LR_T_w', 'pre_W', 'pre_W_Quote',
           'pre_R_sJ', 'pre_R_n', 'pre_X_APD', 'pre_Z_EPD', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(+dim(sim_distr_2), c(36, 57))
    expect_named(
        sim_distr_2_report,
        c('result', 'cross', 'description')
    )
    expect_equal(dim(sim_distr_2_report$result), c(36, 28))
    expect_equal(dim(sim_distr_2_report$description), c(60, 5))

})

test_that("Thesis Simulation 4", {

    # Simulation des Zwei-Stichprobenfalls
    sim_distr_3 <- asm_simulate(
        simulations = 1,
        sim_n = list(
            c(10, 20, 30, 40, 50, 100, 150),
            c(10, 20, 30, 40, 50, 100, 150)
        ),
        pre_test = asm_distribution,
        sim_func = rep("asm_simData", 7),
        sim_args = c(
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = -2, kurt = 5))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -2, kurt = 5)),
                    list(list(skew = 2, kurt = 5))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = 2, kurt = 5))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = -3, kurt = 15))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -3, kurt = 15)),
                    list(list(skew = 3, kurt = 15))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = 3, kurt = 15))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0)),
                    list(list(skew = 0, kurt = 0))
                )
            ))
        )
    )

    # Zusammenfassung der Simulation
    sim_distr_3_report <- asm_reportSim(sim_distr_3, digits = 2)

    expect_named(
        sim_distr_3,
        c(
            'sim_func', 'sim_n_X', 'sim_n_Y', 'sim_distr_X', 'sim_dots_skew_X', 'sim_dots_kurt_X',
            'sim_distr_Y', 'sim_dots_skew_Y', 'sim_dots_kurt_Y', 'sim_cell', 'sim_index',
            'pre_K_S_X_pvalue', 'pre_AD_X_pvalue', 'pre_K_2_X_pvalue', 'pre_JB_X_pvalue',
            'pre_DH_X_pvalue', 'pre_RJB_X_pvalue', 'pre_T_Lmom_X_pvalue', 'pre_T_Lmom_1_X_pvalue',
            'pre_T_Lmom_2_X_pvalue', 'pre_T_Lmom_3_X_pvalue', 'pre_BM_3_4_X_pvalue',
            'pre_BM_3_6_X_pvalue', 'pre_T_MC_LR_X_pvalue', 'pre_T_w_X_pvalue',
            'pre_T_MC_LR_T_w_X_pvalue', 'pre_W_X_pvalue', 'pre_W_Quote_X_pvalue',
            'pre_R_sJ_X_pvalue', 'pre_R_n_X_pvalue', 'pre_X_APD_X_pvalue', 'pre_Z_EPD_X_pvalue',
            'pre_K_S_Y_pvalue', 'pre_AD_Y_pvalue', 'pre_K_2_Y_pvalue', 'pre_JB_Y_pvalue',
            'pre_DH_Y_pvalue', 'pre_RJB_Y_pvalue', 'pre_T_Lmom_Y_pvalue', 'pre_T_Lmom_1_Y_pvalue',
            'pre_T_Lmom_2_Y_pvalue', 'pre_T_Lmom_3_Y_pvalue', 'pre_BM_3_4_Y_pvalue',
            'pre_BM_3_6_Y_pvalue', 'pre_T_MC_LR_Y_pvalue', 'pre_T_w_Y_pvalue',
            'pre_T_MC_LR_T_w_Y_pvalue', 'pre_W_Y_pvalue', 'pre_W_Quote_Y_pvalue',
            'pre_R_sJ_Y_pvalue', 'pre_R_n_Y_pvalue', 'pre_X_APD_Y_pvalue',
            'pre_Z_EPD_Y_pvalue', 'post_welch_p.value', 'post_ttest_p.value', 'post_wilcox_p.value',
            'descr_mean_X', 'descr_median_X', 'descr_skew_X', 'descr_kurtosis_X', 'descr_sd_X',
            'descr_mean_Y', 'descr_median_Y', 'descr_skew_Y', 'descr_kurtosis_Y', 'descr_sd_Y',
            'pre_K_S_X', 'pre_AD_X', 'pre_K_2_X', 'pre_JB_X', 'pre_DH_X', 'pre_RJB_X',
            'pre_T_Lmom_X', 'pre_T_Lmom_1_X', 'pre_T_Lmom_2_X', 'pre_T_Lmom_3_X', 'pre_BM_3_4_X',
            'pre_BM_3_6_X', 'pre_T_MC_LR_X', 'pre_T_w_X', 'pre_T_MC_LR_T_w_X', 'pre_W_X',
            'pre_W_Quote_X', 'pre_R_sJ_X', 'pre_R_n_X', 'pre_X_APD_X', 'pre_Z_EPD_X',
            'pre_K_S_Y', 'pre_AD_Y', 'pre_K_2_Y', 'pre_JB_Y', 'pre_DH_Y', 'pre_RJB_Y',
            'pre_T_Lmom_Y', 'pre_T_Lmom_1_Y', 'pre_T_Lmom_2_Y', 'pre_T_Lmom_3_Y',
            'pre_BM_3_4_Y', 'pre_BM_3_6_Y', 'pre_T_MC_LR_Y', 'pre_T_w_Y', 'pre_T_MC_LR_T_w_Y',
            'pre_W_Y', 'pre_W_Quote_Y', 'pre_R_sJ_Y', 'pre_R_n_Y', 'pre_X_APD_Y', 'pre_Z_EPD_Y',
            'post_welch', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(sim_distr_3), c(49, 111))
    expect_equal(dim(sim_distr_3_report$result), c(49, 55))
    expect_equal(dim(sim_distr_3_report$description), c(210, 6))

})

test_that("Thesis Simulation 5", {

    # Simulation zur identischen Verteilung hinsichtlich Gruppengröße
    result_ID <- asm_simulate(
        simulations = 1,
        sim_n = list(c(rep(30, 6)), seq(10, 20, by = 2)),
        pre_test = asm_homogeneity,
        sim_func = rep("asm_simData", 8),
        sim_args = c(
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = -2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -2, kurt = 5)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = -3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -3, kurt = 15)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0)),
                    list(list(skew = 0, kurt = 0, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0)),
                    list(list(skew = 0, kurt = 0, sd = 1))
                )
            ))
        )
    )

    # Zusammenfassung der Simulation
    result_ID_report <- asm_reportSim(result_ID, digits = 2)

    expect_named(
        result_ID,
        c(
            'sim_func', 'sim_n_X', 'sim_n_Y', 'sim_distr_X', 'sim_dots_skew_X', 'sim_dots_kurt_X',
            'sim_dots_sd_X', 'sim_distr_Y', 'sim_dots_skew_Y', 'sim_dots_kurt_Y', 'sim_dots_sd_Y',
            'sim_cell', 'sim_index', 'pre_ftest_pvalue', 'pre_bartlett_pvalue', 'pre_levene_pvalue',
            'pre_oneillmathews_pvalue', 'post_welch_p.value', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean_X', 'descr_median_X', 'descr_skew_X',
            'descr_kurtosis_X', 'descr_sd_X', 'descr_mean_Y', 'descr_median_Y',
            'descr_skew_Y', 'descr_kurtosis_Y', 'descr_sd_Y', 'pre_ftest', 'pre_bartlett',
            'pre_levene', 'pre_oneillmathews', 'post_welch', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID), c(48, 37))
    expect_equal(dim(result_ID_report$result), c(48, 19))
    expect_equal(dim(result_ID_report$description), c(240, 6))

})

test_that("Thesis Simulation 6", {


    # Varianzen
    varianceVector <- sqrt(c(0.25, 0.5, 1:10))

    # Simulation zur identischen Verteilung hinsichtlich Gruppengröße
    result_ID2 <- asm_simulate(
        simulations = 1,
        sim_n = list(c(rep(30, 6)), c(10, 12)),
        pre_test = asm_homogeneity,
        sim_func = rep("asm_simData", 36),
        sim_args = c(
            lapply(varianceVector, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = -2, kurt = 5)),
                        list(list(skew = 2, kurt = 5, sd = x))
                    )
                )
            }),
            lapply(varianceVector, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = -3, kurt = 15)),
                        list(list(skew = 3, kurt = 15, sd = x))
                    )
                )
            }),
            lapply(varianceVector, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = 0, kurt = 0)),
                        list(list(skew = 0, kurt = 0, sd = x))
                    )
                )
            })
        )
    )

    # Zusammenfassung der Simulation
    result_ID2_report <- asm_reportSim(result_ID2, digits = 2)


    expect_named(
        result_ID2,
        c(
            'sim_func', 'sim_n_X', 'sim_n_Y', 'sim_distr_X', 'sim_dots_skew_X', 'sim_dots_kurt_X',
            'sim_dots_sd_X', 'sim_distr_Y', 'sim_dots_skew_Y', 'sim_dots_kurt_Y', 'sim_dots_sd_Y',
            'sim_cell', 'sim_index', 'pre_ftest_pvalue', 'pre_bartlett_pvalue', 'pre_levene_pvalue',
            'pre_oneillmathews_pvalue', 'post_welch_p.value', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean_X', 'descr_median_X', 'descr_skew_X',
            'descr_kurtosis_X', 'descr_sd_X', 'descr_mean_Y', 'descr_median_Y',
            'descr_skew_Y', 'descr_kurtosis_Y', 'descr_sd_Y', 'pre_ftest', 'pre_bartlett',
            'pre_levene', 'pre_oneillmathews', 'post_welch', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID2), c(72, 37))
    expect_equal(dim(result_ID2_report$result), c(72, 19))
    expect_equal(dim(result_ID2_report$description), c(1080, 6))

})

test_that("Thesis Simulation 7", {

    # Varianzen
    varianceVector2 <- sqrt(c(0.25, 1, 2, 4, 9))

    # Simulation zur identischen Verteilung hinsichtlich Gruppengröße
    result_ID3 <- asm_simulate(
        simulations = 1,
        sim_n = list(c(15, 45, 60, 75, 150, 300), c(5, 15, 20, 25, 50, 100)),
        pre_test = asm_homogeneity,
        sim_func = rep("asm_simData", 15),
        sim_args = c(
            lapply(varianceVector2, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = -2, kurt = 5)),
                        list(list(skew = 2, kurt = 5, sd = x))
                    )
                )
            }),
            lapply(varianceVector2, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = -3, kurt = 15)),
                        list(list(skew = 3, kurt = 15, sd = x))
                    )
                )
            }),
            lapply(varianceVector2, function(x) {
                list(
                    distr = rep("fleishman_sim", 2),
                    dots = c(
                        list(list(skew = 0, kurt = 0)),
                        list(list(skew = 0, kurt = 0, sd = x))
                    )
                )
            })
        )
    )

    # Zusammenfassung der Simulation
    result_ID3_report <- asm_reportSim(result_ID3, digits = 2)

    expect_named(
        result_ID3,
        c(
            'sim_func', 'sim_n_X', 'sim_n_Y', 'sim_distr_X', 'sim_dots_skew_X', 'sim_dots_kurt_X',
            'sim_dots_sd_X', 'sim_distr_Y', 'sim_dots_skew_Y', 'sim_dots_kurt_Y', 'sim_dots_sd_Y',
            'sim_cell', 'sim_index', 'pre_ftest_pvalue', 'pre_bartlett_pvalue', 'pre_levene_pvalue',
            'pre_oneillmathews_pvalue', 'post_welch_p.value', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean_X', 'descr_median_X', 'descr_skew_X',
            'descr_kurtosis_X', 'descr_sd_X', 'descr_mean_Y', 'descr_median_Y',
            'descr_skew_Y', 'descr_kurtosis_Y', 'descr_sd_Y', 'pre_ftest', 'pre_bartlett',
            'pre_levene', 'pre_oneillmathews', 'post_welch', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID3), c(90, 37))
    expect_equal(dim(result_ID3_report$result), c(90, 19))
    expect_equal(dim(result_ID3_report$description), c(450, 6))

})

test_that("Thesis Simulation 8", {

    # Simulation Vortest mit NV Test
    result_ID4 <- asm_simulate(
        simulations = 1,
        sim_n = list(30, 10),
        pre_test = asm_preTests,
        pre_selection = c("Shapiro-Wilk", "F-Test", "Bartlett", "Layard", "Levene"),
        sim_func = rep("asm_simData", 8),
        sim_args = c(
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = -2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -2, kurt = 5)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = -3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -3, kurt = 15)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0)),
                    list(list(skew = 0, kurt = 0, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0)),
                    list(list(skew = 0, kurt = 0, sd = 1))
                )
            ))
        )
    )

    # SW Test als Pre-Verteilungstets + SW
    result_ID4 <- asm_simStrategy(result_ID4, strategies = list(
        W_only = quote(fifelse(!pre_W_Y, post_welch, F))
    ))

    # Zusammenfassung der Simulation
    result_ID4_report <- asm_reportSim(result_ID4, 2, "result")$result

    expect_named(
        result_ID4,
        c(
            'sim_func','sim_n_X','sim_n_Y','sim_distr_X','sim_dots_skew_X','sim_dots_kurt_X',
            'sim_dots_sd_X','sim_distr_Y','sim_dots_skew_Y','sim_dots_kurt_Y','sim_dots_sd_Y',
            'sim_cell','sim_index','pre_W_X_pvalue','pre_W_Y_pvalue','pre_ftest_pvalue',
            'pre_bartlett_pvalue','pre_levene_pvalue','pre_oneillmathews_pvalue',
            'post_welch_p.value','post_ttest_p.value','post_wilcox_p.value','descr_mean_X',
            'descr_median_X','descr_skew_X','descr_kurtosis_X','descr_sd_X','descr_mean_Y',
            'descr_median_Y','descr_skew_Y','descr_kurtosis_Y','descr_sd_Y','pre_W_X','pre_W_Y',
            'pre_ftest','pre_bartlett','pre_levene','pre_oneillmathews','post_welch','post_ttest',
            'post_wilcox','strat_W_only'
        )
    )
    expect_equal(dim(result_ID4), c(8, 42))
    expect_equal(dim(result_ID4_report), c(8, 22))

})

test_that("Thesis Simulation 9", {

    # Simulation id - SW pre-test strategy - power
    result_ID5 <- asm_simulate(
        simulations = 1,
        sim_n = list(30, 10),
        pre_test = asm_preTests,
        pre_selection = c("Shapiro-Wilk", "F-Test", "Bartlett", "Layard", "Levene"),
        sim_func = rep("asm_simData", 8),
        sim_args = c(
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5, mean = 1)),
                    list(list(skew = -2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -2, kurt = 5, mean = 1)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 2, kurt = 5, mean = 1)),
                    list(list(skew = 2, kurt = 5, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15, mean = 1)),
                    list(list(skew = -3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = -3, kurt = 15, mean = 1)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 3, kurt = 15, mean = 1)),
                    list(list(skew = 3, kurt = 15, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0, mean = 1)),
                    list(list(skew = 0, kurt = 0, sd = 4))
                )
            )),
            list(list(
                distr = rep("fleishman_sim", 2),
                dots = c(
                    list(list(skew = 0, kurt = 0, mean = 1)),
                    list(list(skew = 0, kurt = 0, sd = 1))
                )
            ))
        )
    )

    # SW Test als Pre-Verteilungstets + SW
    result_ID5 <- asm_simStrategy(result_ID5, strategies = list(
        W_only = quote(fifelse(!pre_W_Y, post_welch, F))
    ))

    # Zusammenfassung der Simulation
    result_ID5_report <- asm_reportSim(result_ID5, 2, "result")$result

    expect_named(
        result_ID5,
        c(
            'sim_func', 'sim_n_X', 'sim_n_Y', 'sim_distr_X', 'sim_dots_skew_X', 'sim_dots_kurt_X',
            'sim_dots_mean_X', 'sim_dots_sd_X', 'sim_distr_Y', 'sim_dots_skew_Y', 'sim_dots_kurt_Y',
            'sim_dots_mean_Y', 'sim_dots_sd_Y', 'sim_cell', 'sim_index', 'pre_W_X_pvalue',
            'pre_W_Y_pvalue', 'pre_ftest_pvalue', 'pre_bartlett_pvalue', 'pre_levene_pvalue',
            'pre_oneillmathews_pvalue', 'post_welch_p.value', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean_X', 'descr_median_X', 'descr_skew_X',
            'descr_kurtosis_X', 'descr_sd_X', 'descr_mean_Y', 'descr_median_Y', 'descr_skew_Y',
            'descr_kurtosis_Y', 'descr_sd_Y', 'pre_W_X', 'pre_W_Y', 'pre_ftest', 'pre_bartlett',
            'pre_levene', 'pre_oneillmathews', 'post_welch', 'post_ttest', 'post_wilcox', 'strat_W_only'
        )
    )
    expect_equal(dim(result_ID5), c(8, 44))
    expect_equal(dim(result_ID5_report), c(8, 24))

})

test_that("Thesis Simulation 10", {

    # Normalverteilungs-Simulationen
    result_ID7 <- asm_simulate(
        simulations = 1,
        sim_n = c(10, 20, 30, 50),
        pre_test = asm_preTests,
        pre_selection = c("Bartels Rank", "Cox Stuart", "Difference Sign","Rank", "Runs",
                          "Spanos Auxiliary Regression"),
        sim_func = c(rep("rnorm", 2)),
        sim_args = c(
            list(list(mean = 0, sd = 1)),
            list(list(mean = 0, sd = 3))
        )
    )
    result_ID7_report <- asm_reportSim(result_ID7, 2)

    expect_named(
        result_ID7,
        c(
            'sim_func', 'sim_n', 'sim_mean', 'sim_sd', 'sim_cell', 'sim_index',
            'pre_bartels.rank_p.value', 'pre_cox.stuart_p.value', 'pre_difference.sign_p.value',
            'pre_rank_p.value', 'pre_runs_p.value', 'pre_joint_mean_pvalue', 'pre_joint_var_pvalue',
            'post_ttest_p.value', 'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew',
            'descr_kurtosis', 'descr_sd', 'pre_bartels.rank', 'pre_cox.stuart', 'pre_difference.sign',
            'pre_rank', 'pre_runs', 'pre_joint_mean', 'pre_joint_var', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID7), c(8, 29))
    expect_equal(dim(result_ID7_report$result), c(8, 14))

})

test_that("Thesis Simulation 11", {

    # Fleishman-Simulationen
    result_ID8 <- asm_simulate(
        simulations = 1,
        sim_n = c(10, 20, 30, 50),
        pre_test = asm_preTests,
        pre_selection = c("Bartels Rank", "Cox Stuart", "Difference Sign","Rank", "Runs",
                          "Spanos Auxiliary Regression"),
        sim_func = c(rep("fleishman_sim", 4)),
        sim_args = c(
            list(list(skew = 2, kurt = 5)),
            list(list(skew = -2, kurt = 5)),
            list(list(skew = 3, kurt = 14)),
            list(list(skew = -3,kurt = 14))
        )
    )
    result_ID8_report <- asm_reportSim(result_ID8, 2)

    expect_named(
        result_ID8,
        c(
            'sim_func', 'sim_n', 'sim_skew', 'sim_kurt', 'sim_cell', 'sim_index',
            'pre_bartels.rank_p.value', 'pre_cox.stuart_p.value', 'pre_difference.sign_p.value',
            'pre_rank_p.value', 'pre_runs_p.value', 'pre_joint_mean_pvalue', 'pre_joint_var_pvalue',
            'post_ttest_p.value', 'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew',
            'descr_kurtosis', 'descr_sd', 'pre_bartels.rank', 'pre_cox.stuart',
            'pre_difference.sign', 'pre_rank', 'pre_runs', 'pre_joint_mean', 'pre_joint_var',
            'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID8), c(16, 29))
    expect_equal(dim(result_ID8_report$result), c(16, 14))

})

test_that("Thesis Simulation 12", {

    # Simulation zu kontaminierten Verteilungen
    result_ID9 <- asm_simulate(
        simulations = 1,
        sim_n = c(10, 30, 100),
        pre_test = asm_preTests,
        pre_selection = c("Bartels Rank", "Cox Stuart", "Difference Sign","Rank", "Runs",
                          "Spanos Auxiliary Regression"),
        sim_func = rep("asm_simVar", 36),
        sim_args = c(
            mapply(
                function(v, m, s, sor) {
                    list(
                        distr = list(rep("rnorm", 2)),
                        args  = list(list(sd = 1, mean = 0), list(sd = v, mean = m)),
                        share = list(c(1-s, s)),
                        sorting = sor
                    )
                },
                v   = as.list(rep(sqrt(c(1, 9)), each = 36 / 2)),
                m   = as.list(rep(c(0, 1, 3), each = 36 / 6)),
                s   = as.list(rep(c(0.1, 0.2, 0.5), each =  36 / 18)),
                sor = as.list(c("grouped", "random")),
                SIMPLIFY = F
            )
        )
    )
    result_ID9_report <- asm_reportSim(result_ID9, 2)

    expect_named(
        result_ID9,
        c(
            'sim_func', 'sim_n', 'sim_distr1', 'sim_distr2', 'sim_args.sd', 'sim_args.mean',
            'sim_args.sd1', 'sim_args.mean1', 'sim_share1', 'sim_share2', 'sim_sorting', 'sim_cell',
            'sim_index', 'pre_bartels.rank_p.value', 'pre_cox.stuart_p.value',
            'pre_difference.sign_p.value', 'pre_rank_p.value', 'pre_runs_p.value',
            'pre_joint_mean_pvalue', 'pre_joint_var_pvalue', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew', 'descr_kurtosis',
            'descr_sd', 'pre_bartels.rank', 'pre_cox.stuart', 'pre_difference.sign', 'pre_rank',
            'pre_runs', 'pre_joint_mean', 'pre_joint_var', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID9), c(108, 36))
    expect_equal(dim(result_ID9_report$result), c(108, 21))

})

test_that("Thesis Simulation 13", {

    # eigene Funktion zur Simulation der Autoregression erster Ordnung
    myAuto <- function(N, m = 0, cor = 0, sort = c("grouped", "random")[1]) {

        sigmaMatrix <- matrix(cor, N, N)
        sigmaMatrix <- sigmaMatrix^abs(row(sigmaMatrix)-col(sigmaMatrix))
        if (sort == "grouped") {
            mvrnorm(1, rep(m, N), sigmaMatrix)
        } else mvrnorm(1, rep(m, N), sigmaMatrix)[sample(1:N, N)]

    }
    assign("myAuto",myAuto,envir = globalenv())

    # Simulation mit Autokorrelationen von 0-0.9
    result_ID10 <- asm_simulate(
        simulations = 1,
        sim_n = c(10, 20, 30, 50),
        pre_test = asm_preTests,
        pre_selection = c("Bartels Rank", "Cox Stuart", "Difference Sign","Rank", "Runs",
                          "Box-Pierce", "Ljung-Pierce", "Spanos Auxiliary Regression"),
        sim_func = rep("myAuto", 20),
        sim_args = c(
            mapply(
                function(cor, sor) {
                    list(
                        cor = cor,
                        sort = sor
                    )
                },
                cor = as.list(rep(seq(0, 0.9, by = 0.1), 2)),
                sor = as.list(rep(c("grouped", "random"), each = 10)),
                SIMPLIFY = F
            )
        )
    )

    # Simualtionsergebnis
    result_ID10_report <- asm_reportSim(result_ID10, 2, "result")

    expect_named(
        result_ID10,
        c(
            'sim_func', 'sim_n', 'sim_cor', 'sim_sort', 'sim_cell', 'sim_index',
            'pre_bartels.rank_p.value', 'pre_cox.stuart_p.value',
            'pre_difference.sign_p.value', 'pre_rank_p.value',
            'pre_runs_p.value', 'pre_Box.Pierce_p.value', 'pre_Ljung.Box_p.value',
            'pre_joint_mean_pvalue', 'pre_joint_var_pvalue', 'post_ttest_p.value',
            'post_wilcox_p.value', 'descr_mean', 'descr_median', 'descr_skew',
            'descr_kurtosis', 'descr_sd', 'pre_bartels.rank', 'pre_cox.stuart',
            'pre_difference.sign', 'pre_rank', 'pre_runs', 'pre_Box.Pierce',
            'pre_Ljung.Box', 'pre_joint_mean', 'pre_joint_var', 'post_ttest', 'post_wilcox'
        )
    )
    expect_equal(dim(result_ID10), c(80, 33))
    expect_equal(dim(result_ID10_report$result), c(80, 16))

})
