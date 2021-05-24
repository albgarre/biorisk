
#' R6 class describing log-linear inactivation
#'
#' @details
#' A risk module describing log-linear inactivation. It has 3 inputs: logN0, t
#' and logD.
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
                       input_names = c("t", "logD", "logN0"),
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
        logD = self$depends_on$logD$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - t/(10^logD)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

## tests

# time <- Constant$new("Time", 30)
#
# logD <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000) %>% hist()
# inact_model$simulations









