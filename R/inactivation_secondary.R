
#' R6 class describing the secondary model for the D/z model
#'
#' @details
#' A risk module describing the D/z model. It has 4 inputs: Dref, temperature, z and Tref.
#'
#' @export
#'
Dz_model <- R6::R6Class(
  classname = "Dz_model",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("Dref", "temperature", "z", "Tref"),
                       units = units,
                       module_type = "secondary",
                       output_var = "D",
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
        Dref = self$depends_on$Dref$simulate(niter),
        temperature = self$depends_on$temperature$simulate(niter),
        z = self$depends_on$z$simulate(niter),
        Tref = self$depends_on$Tref$simulate(niter)
      ) %>%
        dplyr::mutate(
          logD = log10(Dref) - (temperature - Tref)/z,
          D = 10^logD
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

## tests
#
## tests
#
# library(biorisk)
# library(tidyverse)
#
# Dref <- LogNormal$new("Dref")$
#   map_input("mu_log10", Constant$new("mu_log10", .1))$
#   map_input("sigma_log10", Constant$new("simga_log10", .01))
#
# temperature <- Normal$new("temp")$
#   map_input("mu", Constant$new("mu_temp", 60))$
#   map_input("sigma", Constant$new("sigma_temp", 2))
#
# z <- Normal$new("z")$
#   map_input("mu", Constant$new("mu_z", 5))$
#   map_input("sigma", Constant$new("sigma_z", .2))
#
# Tref <- Constant$new("Tref", 60)
#
# model_D <- Dz_model$new("secondary_D")$
#   map_input("Dref", Dref)$
#   map_input("temperature", temperature)$
#   map_input("z", z)$
#   map_input("Tref", Tref)
#
# model_D$simulate(1000)
# plot_model(model_D)
#
#
# time <- Constant$new("Time", 30)
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", model_D)$
#   map_input("logN0", logN0)
#
# plot_model(inact_model)
#
#
# inact_model$simulate(1000) %>%
#   hist()
# inact_model$simulations





