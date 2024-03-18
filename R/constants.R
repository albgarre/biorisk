
#' @title Constant Class
#'
#' @description
#' An element that defines a constant
#'
#'
#' @export
#'
Constant <- R6::R6Class(
  classname = "Constant",
  inherit = RiskElement,
  public = list(

    #' @field value The value of the constant
    value = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param value The value of the constant
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          value,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       element_type = "constant",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)
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
    point_estimate = function() {
      self$value
    }

  )

)

#' @title Vector Class
#'
#' @description
#' An element that defines a constant vector
#'
#'
#' @export
#'
Vector <- R6::R6Class(
  classname = "Vector",
  inherit = RiskElement,
  public = list(

    #' @field values The values of the vector
    values = NULL,

    #' @field n Length of the vector
    n = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param values A vector of values for the vector
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          values,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       element_type = "constant",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

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





