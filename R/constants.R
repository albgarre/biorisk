
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







