
#' R6 class describing log-linear inactivation
#'
#' @details
#' A risk module describing log-linear inactivation. It has 3 inputs: logN0, t
#' and logD.
#'
#' @export
#'
LogLinInactivation <- R6::R6Class(
  classname = "LogLinInactivation",
  # inherit = Module,
  public = list(

    #' @field name A character giving a name to this constant
    name = "",

    #' @field inputs A tibble describing the inputs and their units.
    inputs = tibble::tibble(var = c("logN0", "t", "logD"),
                    units = c(NA, NA, NA)
    ),

    #' @field depends_on A list with the mapping of each input
    depends_on = list(logN0 = NA,
                      t = NA,
                      logD = NA
                      ),

    #' @field depended_by A list of modules that use this instance as input
    depended_by = c(),

    #' @field simulations A tibble with the results of the simulations
    simulations = tibble::tibble(),

    #' @field type A character defining it as an inactivation module
    type = "inactivation",


    ## Methods

    #' @description
    #' Create a new inactivation module
    #' @param name Name
    #' @param units units of the parameter (NULL by default)
    #' @return A new Inactivation Module
    #'
    initialize = function(name, units = NULL) {

      self$name <- name

      if (!is.null(units)) {
        self$inputs$units = units
      }

    },

    #' @description
    #' Maps an input variable to a module/constant.
    #' @param input Character identifying the input
    #' @param module A module/constant whose output is used as input.
    #' @return self (silently)
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
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        logD = self$depends_on$logD$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - t/(10^logD)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    },

    #' @description
    #' Get the output of the simulations
    #' @return the output of the module (logN)
    #'
    get_output = function() {
      self$simulations$logN
    }

  )

)

## tests

# time <- Constant$new("Time", 30)
#
# logD <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000) %>% hist()
# inact_model$simulations









