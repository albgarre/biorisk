
#' R6 class describing the secondary model for the D/z model
#'
#' @details
#' A risk module describing the D/z model. It has 4 inputs: Dref, temperature, z and Tref.
#'
#' @export
#'
Dz_model <- R6::R6Class(
  classname = "Dz_model",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("Dref", "temperature", "z", "Tref"),
                       units = units,
                       module_type = "secondary",
                       output_var = "D",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      Dref <- self$depends_on$Dref$discrete_prediction()
      temperature <- self$depends_on$temperature$discrete_prediction()
      z <- self$depends_on$z$discrete_prediction()
      Tref <- self$depends_on$Tref$discrete_prediction()

      logD <- log10(Dref) - (temperature - Tref)/z
      10^logD

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logD = log10(Dref) - (temperature - Tref)/z,
          D = 10^logD
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logD = log10(Dref) - (temperature - Tref)/z,
          D = 10^logD
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the secondary model for the Peleg model
#'
#' @details
#' A risk module describing the secondary model of the Peleg model. It has 3 inputs:
#' k_b, temperature, temp_crit.
#'
#' @export
#'
Peleg_secondary <- R6::R6Class(
  classname = "Peleg_secondary",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("temperature", "k_b", "temp_crit"),
                       units = units,
                       module_type = "secondary",
                       output_var = "b",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      k_b <- self$depends_on$k_b$discrete_prediction()
      temperature <- self$depends_on$temperature$discrete_prediction()
      temp_crit <- self$depends_on$temp_crit$discrete_prediction()

      log(1 + exp(k_b * (temperature - temp_crit)))

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          b = log(1 + exp(k_b * (temperature - temp_crit)))
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          b = log(1 + exp(k_b * (temperature - temp_crit)))
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
# model_D$histogram()
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
# inact_model$simulate(1000)
# inact_model$histogram()
#
#
#
# ## Test of the Peleg model
#
# library(biorisk)
# library(tidyverse)
#
# k_b <- LogNormal$new("k_b")$
#   map_input("mu_log10", Constant$new("mu_log10", 0))$
#   map_input("sigma_log10", Constant$new("simga_log10", .01))
#
# temperature <- Normal$new("temp")$
#   map_input("mu", Constant$new("mu_temp", 60))$
#   map_input("sigma", Constant$new("sigma_temp", 2))
#
# temp_crit <- Normal$new("temp_crit")$
#   map_input("mu", Constant$new("mu_Tcr", 55))$
#   map_input("sigma", Constant$new("sigma_Tcr", 1))
#
#
# model_b <- Peleg_secondary$new("secondary_b")$
#   map_input("k_b", k_b)$
#   map_input("temperature", temperature)$
#   map_input("temp_crit", temp_crit)
#
# model_b$simulate(1000)
# model_b$density_plot()
#
#
# time <- Constant$new("Time", 30)
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# n <- Normal$new("n")$
#   map_input("mu", Constant$new("mu_n", 1))$
#   map_input("sigma", Constant$new("sigma_n", .1))
#
#
# inact_model <- PelegInactivation$new("Peleg")$
#   map_input("n", n)$
#   map_input("b", model_b)$
#   map_input("t", time)$
#   map_input("logN0", logN0)
#
#
# inact_model$simulate(10000)
# inact_model$histogram()
