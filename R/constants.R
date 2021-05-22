
#' R6 Class Representing a Constant
#'
#' @description
#' A constant that can be used as input by other methods
#'
#' @export
#'
Constant <- R6::R6Class(
  classname = "Constant",
  # inherit = Module,
  public = list(

    ## Usual attributes

    #' @field name A character giving a name to this constant
    name = "",

    #' @field inputs Ignored (empty tibble)
    inputs = tibble::tibble(),

    #' @field depends_on Ignored (empty list)
    depends_on = list(),

    #' @field depended_by A list of modules that use this instance as input
    depended_by = c(),

    #' @field output Ignored
    output = "",

    #' @field simulations Ignored
    simulations = tibble::tibble(),

    #' @field type A character defining it as a constant
    type = "constant",

    #' @field unit The unit of the constant
    unit = NULL,

    ## Specific attributes

    #' @field value The value of the constant
    value = NULL,

    ## Methods

    #' @description
    #' Create a new constant
    #' @param name Name
    #' @param value Value of the constant
    #' @param unit Unit of the constant (NA by default").
    #' @return A new Constant object
    #'
    initialize = function(name, value, unit = NA) {

      self$name <- name
      self$value <- value
      self$unit <- unit

    },

    #' @description
    #' Make simulation. Returns a vector of length niter with self.value.
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignore.
    #'
    simulate = function(niter, check_units = FALSE) {

      rep(self$value, niter)

    }

  )

)

## tests

# a <- ModuleConstant$new("a", 5)
# a$simulate(10)
# a$name
# a$unit
#
# b <- ModuleConstant$new("b", 6)
# b$simulate(20)
# b$name







