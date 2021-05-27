
#' A module simulating a Poisson distribution
#'
#' @export
#'
Poisson <- R6::R6Class(
  classname = "Poisson",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("lambda"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        lambda = self$depends_on$lambda$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rpois(niter, lambda)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Binomial distribution
#'
#' @export
#'
Binomial <- R6::R6Class(
  classname = "Binomial",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("size", "prob"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        size = self$depends_on$size$simulate(niter),
        prob = self$depends_on$prob$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rbinom(niter, size, prob)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a zero-inflated Poisson distribution
#'
#' @export
#'
ZIPoisson <- R6::R6Class(
  classname = "ZIPoisson",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("lambda", "pi"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        lambda = self$depends_on$lambda$simulate(niter),
        pi = self$depends_on$pi$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rzip(niter, lambda, pi)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Beta-Binomial distribution
#'
#' @export
#'
BetaBinomial <- R6::R6Class(
  classname = "BetaBinomial",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("size", "alpha", "beta"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        size = self$depends_on$size$simulate(niter),
        alpha = self$depends_on$alpha$simulate(niter),
        beta = self$depends_on$beta$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rbbinom(niter, size, alpha, beta)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Gamma-Poisson distribution
#'
#' @export
#'
GammaPoisson <- R6::R6Class(
  classname = "GammaPoisson",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        shape = self$depends_on$shape$simulate(niter),
        scale = self$depends_on$scale$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rgpois(niter, shape, scale=scale)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

## test

# Poisson$new("aa")$
#   map_input("lambda", Constant$new("a", 30))$
#   simulate(1000) %>% hist()
#
# Binomial$new("aa")$
#   map_input("size",
#             Poisson$new("a")$
#               map_input("lambda", Constant$new("b", 30))
#             )$
#   map_input("prob", Constant$new(".5", .5))$
#   simulate(1000) %>% hist()
#
# ZIPoisson$new("aa")$
#   map_input("lambda", Constant$new("a", 30))$
#   map_input("pi", Constant$new(".3", .3))$
#   simulate(1000) %>%
#   hist()
#
# BetaBinomial$new("")$
#   map_input("size", Constant$new("a", 30))$
#   map_input("alpha", Constant$new("b", 3))$
#   map_input("beta", Constant$new("c", 4))$
#   simulate(1000) %>% hist()
#
# GammaPoisson$new("")$
#   map_input("shape", Constant$new("", 2))$
#   map_input("scale", Constant$new("", 3))$
#   simulate(1000) %>%
#   hist()







