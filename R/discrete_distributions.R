
#' R6 Class that re-samples a vector
#'
#'
#' @export
#'
EmpiricalDistr <- R6::R6Class(
  classname = "EmpiricalDistr",
  inherit = DiscreteModule,
  public = list(

    #' @field values A vector to resample
    values = NULL,

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)
      self$values <- values

    },


    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      median(self$values)
    }

  ),

  private = list(

    update_output = function(niter) {

      x <- sample(self$values, niter, replace = TRUE)

      self$simulations <- tibble(x = x)

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      x <- sample(self$values, niter0, replace = TRUE)

      ## Save it

      self$simulations_multi[[iter1]] <- tibble(x = x)

      ## Return the output

      invisible(self$simulations_multi[[iter1]][[self$output]])

    }

  )

)


#' A module simulating a Poisson distribution
#'
#' @export
#'
Poisson <- R6::R6Class(
  classname = "Poisson",
  inherit = DiscreteModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("lambda"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$lambda$discrete_prediction()
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rpois(niter, lambda)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rpois(niter0, lambda)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Binomial distribution
#'
#' @export
#'
Binomial <- R6::R6Class(
  classname = "Binomial",
  inherit = DiscreteModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("size", "prob"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$size$discrete_prediction() * self$depends_on$prob$discrete_prediction()
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rbinom(niter, size, prob)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rbinom(niter0, size, prob)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a zero-inflated Poisson distribution
#'
#' @export
#'
ZIPoisson <- R6::R6Class(
  classname = "ZIPoisson",
  inherit = DiscreteModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("lambda", "pi"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      (1 - self$depends_on$pi$discrete_prediction())*self$depends_on$lambda$discrete_prediction()
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rzip(niter, lambda, pi)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rzip(niter0, lambda, pi)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Beta-Binomial distribution
#'
#' @export
#'
BetaBinomial <- R6::R6Class(
  classname = "BetaBinomial",
  inherit = DiscreteModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("size", "alpha", "beta"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      n <- self$depends_on$size$discrete_prediction()
      a <- self$depends_on$alpha$discrete_prediction()
      b <- self$depends_on$beta$discrete_prediction()

      n*a/(a + b)
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rbbinom(niter, size, alpha, beta)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rbbinom(niter0, size, alpha, beta)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Gamma-Poisson distribution
#'
#' @export
#'
GammaPoisson <- R6::R6Class(
  classname = "GammaPoisson",
  inherit = DiscreteModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      self$depends_on$shape$discrete_prediction() * self$depends_on$scale$discrete_prediction()

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rgpois(niter, shape, scale=scale)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rgpois(niter0, shape, scale=scale)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

## test

# aa <- Poisson$new("aa")$
#   map_input("lambda", Constant$new("a", 30))
# aa$simulate(1000)
# aa$histogram()
#
# aa <- Binomial$new("aa")$
#   map_input("size",
#             Poisson$new("a")$
#               map_input("lambda", Constant$new("b", 30))
#             )$
#   map_input("prob", Constant$new(".5", .5))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <-ZIPoisson$new("aa")$
#   map_input("lambda", Constant$new("a", 30))$
#   map_input("pi", Constant$new(".3", .3))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- BetaBinomial$new("")$
#   map_input("size", Constant$new("a", 30))$
#   map_input("alpha", Constant$new("b", 3))$
#   map_input("beta", Constant$new("c", 4))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- GammaPoisson$new("")$
#   map_input("shape", Constant$new("", 2))$
#   map_input("scale", Constant$new("", 3))
#
# aa$simulate(1000)
# aa$histogram()








