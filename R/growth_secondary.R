
#' R6 class describing the secondary Ratkowsky model (suboptimal)
#'
#' @details
#' A risk module describing the (suboptimal) Ratkowsky model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
Ratkowsky_model <- R6::R6Class(
  classname = "Ratkowsky_model",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
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
        temperature = self$depends_on$temperature$simulate(niter),
        b = self$depends_on$b$simulate(niter),
        Tmin = self$depends_on$Tmin$simulate(niter),
      ) %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = sq_mu^2
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' R6 class describing the secondary Ratkowsky model (suboptimal) including error term
#'
#' @details
#' A risk module describing the (suboptimal) Ratkowsky model including an error term.
#' It has 4 inputs: b, Tmin, temperature, sigma.
#'
#' @export
#'
Ratkowsky_model_error <- R6::R6Class(
  classname = "Ratkowsky_model_error",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature", "sigma"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
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
        temperature = self$depends_on$temperature$simulate(niter),
        b = self$depends_on$b$simulate(niter),
        Tmin = self$depends_on$Tmin$simulate(niter),
        sigma = self$depends_on$sigma$simulate(niter)
      ) %>%
        dplyr::mutate(.,
          sq_mu_mean = b*(temperature - Tmin),
          sq_mu = rnorm(n = nrow(.), mean = sq_mu_mean, sd = sigma),
          mu = sq_mu^2
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

####### Tests -----------------------

# library(tidyverse)
#
# mu <- Ratkowsky_model$new("aa")$
#   map_input("b",
#             Uniform$new("b")$
#               map_input("min", Constant$new("min_b", .1))$
#               map_input("max", Constant$new("max_b", .2))
#             )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#             )
#
# plot_model(mu)
# mu$simulate(1000) %>% hist()
#
# (.15*(15-0))^2
#
# mu_e <- Ratkowsky_model_error$new("aa")$
#   map_input("b",
#             Constant$new("b", .15)
#   )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Constant$new("T", 15)
#   )$
#   map_input("sigma", Constant$new("sigma", .5))
#
# plot_model(mu_e)
# mu_e$simulate(100) %>% hist()



