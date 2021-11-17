
#' R6 class describing log-linear inactivation
#'
#' @details
#' A risk module describing log-linear inactivation. It has 3 inputs: logN0, t
#' and D.
#'
#' @export
#'
LogLinInactivation <- R6::R6Class(
  classname = "LogLinInactivation",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "D", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        D = self$depends_on$D$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - t/D
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

#' R6 class describing the Weibull inactivation
#'
#' @details
#' A risk module describing Weibullian inactivation. It has 4 inputs: logN0, t
#' delta, beta.
#'
#' @export
#'
WeibullInactivation <- R6::R6Class(
  classname = "WeibullInactivation",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "delta", "beta", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        delta = self$depends_on$delta$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        beta = self$depends_on$beta$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - (t/delta)^beta
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

#' R6 class describing the Weibull inactivation
#'
#' @details
#' A risk module describing Weibullian inactivation using the parameterization by
#' Peleg. It has 4 inputs: logN0, t, b and n.
#'
#' @export
#'
PelegInactivation <- R6::R6Class(
  classname = "PelegInactivation",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "b", "n", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        b = self$depends_on$b$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        n = self$depends_on$n$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - b*t^n
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)


#' R6 class describing Tri-linear inactivation
#'
#' @details
#' A risk module describing Tri-linear inactivation. It has 5 inputs: logN0, t, D, SL and Nres.
#'
#' @export
#'
TrilinearInactivation <- R6::R6Class(
  classname = "TrilinearInactivation",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "logN0", "D", "SL", "logNres"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        D = self$depends_on$D$simulate(niter),
        SL = self$depends_on$SL$simulate(niter),
        logNres = self$depends_on$logNres$simulate(niter),
      ) %>%
        dplyr::mutate(
          logN = logN0 - (t-SL)/D
        ) %>%
        dplyr::mutate(
          logN = ifelse(t < SL, logN0, logN),
          logN = ifelse(logN < logNres, logNres, logN)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

## tests
#
# library(biorisk)
# library(tidyverse)
#
# time <- Constant$new("Time", 30)
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", 1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000) %>%
#   hist()
# inact_model$simulations

#
# plot_model(inact_model)

## test peleg

#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN", 3))$
#   map_input("sigma", Constant$new("sigma_logN", 0.5))
#
# b <- Normal$new("b")$
#   map_input("mu", Constant$new("mu", .1))$
#   map_input("sigma", Constant$new("sigma", 0.02))
#
# t <- Constant$new("t", 7)
# n = Normal$new("n")$
#   map_input("mu", Constant$new("mu_n", 1))$
#   map_input("sigma", Constant$new("sigma_n", .1))
#
#
# PelegInactivation$new("Peleg")$
#   map_input("n", n)$
#   map_input("b", b)$
#   map_input("t", t)$
#   map_input("logN0", logN0)$
#   simulate(10000) %>%
#   hist()




## test weibull
#
# WeibullInactivation$new("Weibull")
#
# logN0 <- Normal$new("logN0")$
#     map_input("mu", Constant$new("mu_logN", 3))$
#     map_input("sigma", Constant$new("sigma_logN", 0.5))
#
# delta <- LogNormal$new("log_delta")$
#     map_input("mu_log10", Constant$new("mu", 1))$
#     map_input("sigma_log10", Constant$new("sigma", 0.2))
#
# t <- Constant$new("t", 7)
#
# beta <- Constant$new("beta", 0.8)
#
#
# weibull_model <- WeibullInactivation$new("Weibull")$
#   map_input("beta", beta)$
#   map_input("t", t)$
#   map_input("logN0", logN0)$
#   map_input("delta", delta)
#
#
#
# weibull_model$depends_on
#
# weibull_model$simulate(1000) %>% hist()
# weibull_model$simulations
# plot_model(weibull_model)


## test tri-linear
#
# library(biorisk)
# library(tidyverse)
#
# time <- Normal$new("t")$
#   map_input("mu", Constant$new("mu_t", 5))$
#   map_input("sigma", Constant$new("sigma_logN0", 2))
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", .1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.02))
#
# logN0 <- Constant$new("logN0", 6)
# logNres <- Constant$new("logNres", 3)
# SL <- Constant$new("SL", 5)
#
# inact_model <- TrilinearInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)$
#   map_input("logNres", logNres)$
#   map_input("SL", SL)
#
# plot_model(inact_model)
# inact_model$simulate(1000) %>%
#   hist()
# inact_model$simulations

#
# plot_model(inact_model)






