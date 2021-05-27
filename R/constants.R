
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
      x

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

      self$simulations$x

    }

  )

)

#' R6 Class that re-samples a vector
#'
#'
#' @export
#'
EmpiricalDistr <- R6::R6Class(
  classname = "EmpiricalDistr",
  inherit = RiskModule,
  public = list(

    #' @field values A vector to resample
    values = NULL,

    initialize = function(name,
                          values,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = NULL,
                       units = NULL,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)
      self$values <- values

    },

    #' @description
    #' Make simulation. Returns a vector of length niter which is a resample of
    #' self$values.
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignore.
    #'
    simulate = function(niter, check_units = FALSE) {

      x <- sample(self$values, niter, replace = TRUE)

      self$simulations <- tibble(x = x)

      x

    }

  )

)

## tests

# a <- Constant$new("a", 5)
# a$simulate(10)
# a$get_output()
# a$name
# a$unit
#
# b <- Constant$new("b", 6)
# b$simulate(20)
# b$name

# aa <- EmpiricalDistr$new("aa", rnorm(5))
# aa$simulate(10)
# aa$simulate(10)

# aa <- Vector$new("aa", rnorm(100))
# aa$get_output()
# aa$simulate(100)
