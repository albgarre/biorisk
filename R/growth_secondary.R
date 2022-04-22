
#' R6 class describing the secondary Ratkowsky model (suboptimal)
#'
#' @details
#' A risk module describing the (suboptimal) Ratkowsky model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
Ratkowsky_model <- R6::R6Class(
  classname = "Ratkowsky_model",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      b <- self$depends_on$b$discrete_prediction()
      Tmin <- self$depends_on$Tmin$discrete_prediction()
      temperature <- self$depends_on$temperature$discrete_prediction()

      sq_mu <- b*(temperature - Tmin)

      ifelse(temperature < Tmin,
             0,
             sq_mu^2
             )
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = sq_mu^2
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = sq_mu^2
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature", "sigma"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      b <- self$depends_on$b$discrete_prediction()
      Tmin <- self$depends_on$Tmin$discrete_prediction()
      temperature <- self$depends_on$temperature$discrete_prediction()

      sq_mu <- b*(temperature - Tmin)

      ifelse(temperature < Tmin,
             0,
             sq_mu^2
      )
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin),
          sq_mu = rnorm(n = nrow(.), mean = sq_mu_mean, sd = sigma),
          mu = sq_mu^2
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin),
          sq_mu = rnorm(n = niter0, mean = sq_mu_mean, sd = sigma),
          mu = sq_mu^2
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
# mu$simulate(1000)
# mu$density_plot()
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
# mu_e$simulate(100)
# mu_e$density_plot()


