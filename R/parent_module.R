

#' Parent class for modules
#'
#'
RiskModule <- R6::R6Class(
  classname = "RiskModule",
  # inherit = Module,
  public = list(

    ## Fields ------------------------------------------------------------------

    #' @field name A character giving a name to this constant
    name = "",

    #' @field inputs A tibble with the inputs of the module and its units
    inputs = NULL,

    #' @field depends_on List describing the dependencies for each input
    depends_on = NULL,

    #' @field depended_by A list of modules that use this instance as input
    depended_by = c(),

    #' @field output Name of the output variable
    output = "",

    #' @field output_unit Unit of the output variable
    output_unit = "",

    #' @field simulations A tibble with the results of the simulations
    simulations = tibble::tibble(),

    #' @field type A character describing the type of the module
    type = NULL,


    ## Methods -----------------------------------------------------------------

    #' @description
    #' Create a new module
    #' @param name Name
    #' @param input_names A character vector with the names of the inputs
    #' @param units A character vector of units for each input
    #' @param module_type A character with the type of module
    #' @param output_var A character with the name of the output variable
    #' @param output_unit A character with the unit of the output
    #' @return A new instance of the module
    #'
    initialize = function(name,
                          input_names = NA,
                          units = NA,
                          module_type = "",
                          output_var = "",
                          output_unit = "") {

      self$name <- name
      self$type <- module_type
      self$inputs <- tibble(var = input_names, units = units)
      self$depends_on <-rep(NA, length(input_names)) %>%
        set_names(., input_names) %>%
        as.list()
      self$output <- output_var
      self$output_unit <- output_unit

    },

    #' @description
    #' Map an input to a module
    #' @param input A character identifying the input variable
    #' @param module An instance of a module to use as input
    #' @param check_units Ignored
    #' @return Self (invisible)
    #'
    map_input = function(input, module, check_units = FALSE) {

      if (! (input %in% names(self$depends_on))) {
        stop("Unkonwn input: ", input)
      }

      ## Add the dependency

      self$depends_on[[input]] <- module

      ## Reflect it on the other module

      module$depended_by <- c(module$depended_by, self)

      ## Return self

      invisible(self)

    },

    #' @description
    #' Make simulation. Returns a vector of length niter with self.value.
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignored.
    #' @return A vector with the output variable
    #'
    simulate = function(niter, check_units = FALSE) {

      stop("Simulate not implemented for this class")

      ## Do the simulations (recursively)

      # sims <- tibble::tibble(
      #   t = self$depends_on$t$simulate(niter),
      #   mu = self$depends_on$mu$simulate(niter),
      #   logN0 = self$depends_on$logN0$simulate(niter)
      # ) %>%
      #   dplyr::mutate(
      #     logN = logN0 + t*mu
      #   )
      #
      # ## Save the results of the simulations
      #
      # self$simulations <- sims
      #
      # ## Return
      #
      # sims$logN

    },

    #' @description
    #' Get the output
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignored.
    #' @return A vector with the output variable
    get_output = function() {

      if (nrow(self$simulations) == 0) {
        stop("Run the simulation first")
      }

      self$simulations[[self$output]]

    },

    #' @description
    #' Saves the output of the simulation as a vector module
    #' @param name Name of the new module (vctr_from_+self_name by default)
    #' @return An instance of Vector
    save_as_vector = function(name = NULL) {

      if (nrow(self$simulations) == 0) {
        stop("Run the simulation first")
      }

      if (is.null(name)) {
        name <- paste0("vctr_from_", self$name)
      }

      Vector$new(name, self$get_output())

    },

    #' @description
    #' Saves the output of the simulation as an empirical distribution.
    #' @param name Name of the new module (distr_from_+self_name by default)
    #' @return An instance of EmpiricalDistr
    save_as_distribution = function(name = NULL) {

      if (nrow(self$simulations) == 0) {
        stop("Run the simulation first")
      }

      if (is.null(name)) {
        name <- paste0("distr_from_", self$name)
      }

      EmpiricalDistr$new(name, self$get_output())

    }

  )

)

# aa <- biorisk:::RiskModule$new("aa", c("t", "T"))
# bb <- biorisk:::RiskModule$new("bb")
# aa$depends_on
# #
# aa$map_input("t", bb)
# #
# aa$depends_on



# library(biorisk)
# library(tidyverse)
#
# treat_time <- Constant$new("Treat time", 30)
#
# treat_time <- Normal$new("Treat time")$
#   map_input("mu", Constant$new("mu_t", 30))$
#   map_input("sigma", Constant$new("sigma_t", 3))
#
# logD <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu_logD", 1))$
#   map_input("sigma", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Treatment")$
#   map_input("t", treat_time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000)
# aa <- inact_model$save_as_vector()
# aa$simulate(1000)
#
# bb <- inact_model$save_as_distribution("s")
# bb$name
# bb$get_output()



