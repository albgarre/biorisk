
#' R6 Class Representing a Constant
#'
#' @description
#' A constant that can be used as input by other methods
#'
#' @export
#'
Constant <- R6::R6Class(
  classname = "Constant",
  inherit = RiskModule,
  public = list(

    #' @field value The value of the constant
    value = NULL,

    initialize = function(name,
                          value,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       module_type = "constant",
                       output_var = "x",
                       output_unit = output_unit)
      self$value <- value

    },

    #' @description
    #' Make simulation. Returns a vector of length niter with self.value.
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignore.
    #'
    simulate = function(niter, check_units = FALSE) {

      x <- rep(self$value, niter)
      self$simulations <- tibble(x = x)
      # x
      invisible(self)

    },

    #' @description
    #' Simulation at some level for X-D Monte Carlo
    #' @param niter0 Number of iterations at the lower level
    #' @param iter1 Iteration at the upper level
    #' @param level Level of the calculation (ignored, it always returns a vector
    #' to facilitate the mutates)
    simulate_level = function(niter0, iter1 = 1, level = 0) {
      out <- rep(self$value, niter0)

      self$simulations_multi[[iter1]] <- tibble(x = out)

      out
    },

    #' @description
    #' Returns the value of the constant
    #'
    discrete_prediction = function() {
      self$value
    }

  )

)

#' R6 Class Representing a Constant vector
#'
#' @description
#' A constant vector that can be used as input by other methods
#'
#' @export
#'
Vector <- R6::R6Class(
  classname = "Vector",
  inherit = RiskModule,
  public = list(

    #' @field value The value of the constant
    values = NULL,

    #' @field n Length of the vector
    n = NULL,

    initialize = function(name,
                          values,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       module_type = "constant",
                       output_var = "x",
                       output_unit = output_unit)

      self$simulations <- tibble::tibble(x = values)
      self$n <- length(values)

    },

    #' @description
    #' Make simulation. Returns the vector saved in the class. If the length
    #' is different from niter, it raises an error.
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignored.
    #'
    simulate = function(niter, check_units = FALSE) {

      if (niter != self$n) {
        stop("niter must be equal to the length of the saved vector")
      }

      # self$simulations$x
      invisible(self)

    },

    #' @description
    #' Simulation at some level for X-D Monte Carlo
    #' @param niter0 Number of iterations at the lower level
    #' @param iter1 Iteration at the upper level
    #' @param level Level of the calculation (ignored, it always returns a vector
    #' to facilitate the mutates)
    simulate_level = function(niter0, iter1 = 1, level = 0) {

      if (niter0 != self$n) {
        stop("niter must be equal to the length of the saved vector")
      }

      out <- self$simulations$x

      self$simulations_multi[[iter1]] <- tibble(x = out)

      out
    }

  )

)





