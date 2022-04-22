


#' Parent class for modules
#'
#'
RiskModule <- R6::R6Class(
  classname = "RiskModule",

  ## Public methods ------------------------------------------------------------

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

    #' @field output_type Type of output variable (discrete/continuous)
    output_type = "",

    #' @field simulations A tibble with the results of the simulations
    simulations = tibble::tibble(),

    #' @field type A character describing the type of the module
    type = NULL,

    #' @field level Description of the level for X-D Monte Carlo
    level = NULL,

    #' @field simulations_multi For multidimensional MC
    simulations_multi = NULL,

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
                          output_unit = "",
                          output_type = "",
                          level = 0) {

      self$name <- name
      self$type <- module_type
      self$inputs <- tibble(var = input_names, units = units)
      self$depends_on <-rep(NA, length(input_names)) %>%
        set_names(input_names) %>%
        as.list()
      self$output <- output_var
      self$output_unit <- output_unit
      self$output_type <- output_type
      self$level <- level
      self$simulations_multi <- list()

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
    simulate = function(niter
                        # check_units = FALSE, force = TRUE  # TODO
                        ) {

      private$update_inputs(niter)
      private$update_output(niter)
      invisible(self$get_output())

    },

    simulate_level = function(niter0, iter1 = 1, level = 0) {

      private$update_inputs_level(niter0, iter1 = iter1, level = level)
      private$update_output_level(niter0, iter1 = iter1, level = level)
      invisible(self$get_output(iter1 = iter1))

    },

    #' @description
    #' Gets a discrete (fast and simple) prediction
    #'
    discrete_prediction = function() {
      stop("Discrete prediction not available for this module")
    },

    #' @description
    #' Get the output
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignored.
    #' @return A vector with the output variable
    get_output = function(iter1 = NULL) {

      # if (nrow(self$simulations) == 0) {
      #   stop("Run the simulation first")
      # }

      if (is.null(iter1)) {

        self$simulations[[self$output]]

      } else {
        self$simulations_multi[[iter1]][[self$output]]
      }

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

    },

    #' @description
    #' Makes a density plot of the model output
    #'
    density_plot = function() {

      ggplot() + geom_density(aes(x = self$get_output()))

    },

    #' @description
    #' Makes a histogram of the model output
    #'
    histogram = function() {

      ggplot() + geom_histogram(aes(x = self$get_output()))

    },

    #' @description
    #' Makes a boxplot of the model output
    #'
    boxplot = function() {
      ggplot() + geom_boxplot(aes(y = self$get_output()))
    },

    #' @description
    #' Makes a summary table of the model output
    #'
    quantiles = function(probs = c(.01, .1, .5, .9, .99)) {

      quantile(self$get_output(), probs = probs)

    }

  ),
  private = list(

    #' @description
    #' Calls the simulate method for the dependencies
    #'
    update_inputs = function(niter) {

      sims <- self$depends_on %>% map_dfc(~ .$simulate(niter))

      self$simulations <- sims

    },

    #' @description
    #' Calculates the output based on the
    #'
    update_output = function(niter) {
      stop("update_output not implemented for this class")

    },

    update_inputs_level = function(niter0, iter1 = 1, level = 0) {

      sims <- self$depends_on %>%
        map_dfc(~ .$simulate_level(niter0, iter1, level))

      self$simulations_multi[[iter1]] <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      stop("update_output_level not implemented for this class")

    }

  )

)




#' Parent for modules with discrete outputs
#'
DiscreteModule <- R6::R6Class(
  inherit = RiskModule,

  public = list(
    initialize = function(name,
                          input_names = NA,
                          units = NA,
                          module_type = "",
                          output_var = "",
                          output_unit = "",
                          output_type = "",
                          level = 0) {

      super$initialize(name,
                       input_names = input_names,
                       units = units,
                       module_type = module_type,
                       output_var = output_var,
                       output_unit = output_unit,
                       output_type = "discrete",
                       level = level)

    }
  )

)

#' Parent for modules with continuous outputs
#'
ContinuousModule <- R6::R6Class(
  inherit = RiskModule,

  public = list(
    initialize = function(name,
                          input_names = NA,
                          units = NA,
                          module_type = "",
                          output_var = "",
                          output_unit = "",
                          output_type = "",
                          level = 0) {

      super$initialize(name,
                       input_names = input_names,
                       units = units,
                       module_type = module_type,
                       output_var = output_var,
                       output_unit = output_unit,
                       output_type = "continuous",
                       level = level)

    }
  )

)







