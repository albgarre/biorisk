
#' @title EmpiricalDistr Class
#'
#' @description
#' An element that describes an empirical distribution
#'
#'
#' @export
#'
EmpiricalDistr <- R6::R6Class(
  classname = "EmpiricalDistr",
  inherit = DiscreteElement,
  public = list(

    #' @field values A vector to resample
    values = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param values Values of the empirical distribution for resampling
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          values,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

      self$values <- values

    },


    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
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

#' @title Poisson Class
#'
#' @description
#' An element that describes a Poisson distribution
#'
#'
#' @export
#'
Poisson <- R6::R6Class(
  classname = "Poisson",
  inherit = DiscreteElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("lambda"),
                       input_types = list(lambda = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$lambda$point_estimate()
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

#' @title Binomial Class
#'
#' @description
#' An element that describes a binomial distribution
#'
#'
#' @export
#'
Binomial <- R6::R6Class(
  classname = "Binomial",
  inherit = DiscreteElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("size", "prob"),
                       input_types = list(size = "discrete",
                                          prob = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$size$point_estimate() * self$depends_on$prob$point_estimate()
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

#' @title ZIPoisson Class
#'
#' @description
#' An element that describes a zero-inflated Poisson distribution
#'
#'
#' @export
#'
ZIPoisson <- R6::R6Class(
  classname = "ZIPoisson",
  inherit = DiscreteElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("lambda", "pi"),
                       input_types = list(lambda = "continuous",
                                          pi = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      (1 - self$depends_on$pi$point_estimate())*self$depends_on$lambda$point_estimate()
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

#' @title BetaBinomial Class
#'
#' @description
#' An element that describes a beta-binomial Poisson distribution
#'
#'
#' @export
#'
BetaBinomial <- R6::R6Class(
  classname = "BetaBinomial",
  inherit = DiscreteElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("size", "alpha", "beta"),
                       input_types = list(size = "continuous",
                                          alpha = "continuous",
                                          beta = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      n <- self$depends_on$size$point_estimate()
      a <- self$depends_on$alpha$point_estimate()
      b <- self$depends_on$beta$point_estimate()

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

#' @title GammaPoisson Class
#'
#' @description
#' An element that describes a gamma-Poisson distribution
#'
#'
#' @export
#'
GammaPoisson <- R6::R6Class(
  classname = "GammaPoisson",
  inherit = DiscreteElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       input_types = list(shape = "continuous",
                                          scale = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      self$depends_on$shape$point_estimate() * self$depends_on$scale$point_estimate()

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

