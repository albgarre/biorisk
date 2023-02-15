


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

    #' @field depends_index List with the indexes for each independency
    depends_index = NULL,

    #' @field depended_by A list of modules that use this instance as input
    depended_by = c(),

    #' @field input_types A list with the preferred data type for each input (discrete/continuous/any)

    input_types = list(),

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
                          input_types = list(),
                          output_var = "",
                          output_unit = "",
                          output_type = "",
                          level = 0) {

      self$name <- name
      self$type <- module_type
      self$inputs <- tibble(var = input_names, units = units)
      self$depends_on <- rep(NA, length(input_names)) %>%
        set_names(input_names) %>%
        as.list()

      self$depends_index <- rep(NA, length(input_names)) %>%
        set_names(input_names) %>%
        as.list()

      self$output <- output_var
      self$input_types <- input_types
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
    map_input = function(input, module, check_units = FALSE, index = 1) {

      if (! (input %in% names(self$depends_on))) {
        stop("Unkonwn input: ", input)
      }

      ## Add the dependency

      self$depends_on[[input]] <- module

      ## Add the index

      self$depends_index[[input]] <- index

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
    simulate = function(niter,
                        seed = NULL
                        # check_units = FALSE, force = TRUE  # TODO
                        ) {

      if (!is.null(seed)) {
        set.seed(seed)
      }

      private$update_inputs(niter)
      private$update_output(niter)
      # invisible(self$get_output())  # TODO: Maybe change this to invisible(self)... maybe
      invisible(self)

    },

    #' @description
    #' 2D Monte Carlo simulation
    #'
    #' @param niter0 number of iterations on the lower level
    #' @param niter1 number of iterations on the upper level
    #'
    simulate_2D = function(niter0, niter1,
                           seed = NULL) {

      if (!is.null(seed)) {
        set.seed(seed)
      }

      sims <- 1:niter1 %>%
        map(.,
            ~ self$simulate_level(niter0, iter1=., level=0)
        )

      invisible(self)

    },

    simulate_level = function(niter0, iter1 = 1, level = 0) {

      private$update_inputs_level(niter0, iter1 = iter1, level = level)
      private$update_output_level(niter0, iter1 = iter1, level = level)
      invisible(self$get_output(iter1 = iter1))

    },

    #' @description
    #' Gets a discrete (fast and simple) prediction
    #'
    point_estimate = function() {
      stop("Discrete prediction not available for this module")
    },

    #' @description
    #' Get the output
    #' @param niter Number of iterations (length of the vector).
    #' @param check_units Ignored.
    #' @return A vector with the output variable
    get_output = function(iter1 = NULL, index = 1) {

      # if (nrow(self$simulations) == 0) {
      #   stop("Run the simulation first")
      # }

      column <- self$output[[index]]

      if (is.null(iter1)) {

        self$simulations[[column]]

      } else {
        self$simulations_multi[[iter1]][[column]]
      }

    },

    #' @description
    #' Get the output of a 2D simulation
    #'
    get_output_2D = function(index = 1) {

      column <- self$output[[index]]

      c(1:length(self$simulations_multi)) %>%
        map( ~ tibble(x = self$simulations_multi[[.]][[column]])) %>%
        imap_dfr(~ mutate(.x, sim = .y, node = self$name))

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
    #' Makes density plot of a 2D Monte Carlo simulation
    #'
    density_plot_2D = function() {

      my_sims <- self$get_output_2D()

      p_unc <- geom_density(aes(x), data = my_sims,
                            fill = "grey", alpha = .5)

      p_var <- my_sims %>%
        group_by(sim) %>%
        summarize(x = median(x, na.rm = TRUE)) %>%
        geom_density(aes(x), data = .,
                     fill = "steelblue", alpha = .5)

      ggplot() + p_var + p_unc

    },

    #' @description
    #' Plots the empirical density function for 2D simulations
    #'
    cummulative_plot_2D = function() {

      r <- range(self$get_output_2D()$x)

      self$get_output_2D() %>%
        split(.$sim) %>%
        map(.,
            ~ ecdf(.$x)
            ) %>%
        map_dfr(.,
                ~ tibble(x = seq(r[[1]], r[[2]], length = 100),
                         y = .(x))
                ) %>%
        group_by(x) %>%
        summarize(m_y = median(y, na.rm = TRUE),
                  up = quantile(y, .95, na.rm = TRUE),
                  down = quantile(y, .05, na.rm = TRUE)) %>%
        ggplot(aes(x = x, y = m_y)) +
        geom_line() +
        geom_ribbon(aes(ymin = down, ymax = up), alpha = .5)

    },

    #' @description
    #' Plots the empirical density function
    #'
    cummulative_plot = function(add_discrete = FALSE) {

      c <- ecdf(self$get_output())
      r <- range(self$get_output())

      p <- tibble(x = seq(r[[1]], r[[2]], length = 1000),
             y = c(x)) %>%
        ggplot() +
        geom_line(aes(x, y))

      if (add_discrete) {
        p <- p + geom_vline(xintercept = self$point_estimate(),
                            linetype = 2)
      }

      p

    },

    #' @description
    #' Makes a density plot of the model output
    #'
    density_plot = function(add_discrete = FALSE) {

      p <- ggplot() + geom_density(aes(x = self$get_output()))

      if (add_discrete) {
        p <- p + geom_vline(xintercept = self$point_estimate(),
                            linetype = 2)
      }

      p

    },

    #' @description
    #' Makes a histogram of the model output
    #'
    histogram = function(add_discrete = FALSE) {

      p <- ggplot() + geom_histogram(aes(x = self$get_output()))

      if (add_discrete) {
        p <- p + geom_vline(xintercept = self$point_estimate(),
                            linetype = 2)
      }

      p

    },

    #' @description
    #' Makes a boxplot of the model output
    #'
    boxplot = function(add_discrete = FALSE) {

      p <- ggplot() + geom_boxplot(aes(y = self$get_output()))

      if (add_discrete) {
        p <- p + geom_hline(yintercept = self$point_estimate(),
                            linetype = 2)
      }

      p
    },

    #' @description
    #' Quantiles of the output variable
    #'
    quantiles = function(probs = c(.01, .1, .5, .9, .99)) {

      quantile(self$get_output(), probs = probs)

    },

    #' @description
    #' Quantiles of a 2D Monte Carlo simulation
    #'
    quantiles_2D = function(probs = c(.01, .1, .5, .9, .99)) {

      my_sims <- self$get_output_2D()

      out <- quantile(my_sims$x, probs) %>%
        set_names(., paste0(probs, "_level0"))

      out2 <- my_sims %>%
        group_by(sim) %>%
        summarize(x = median(x, na.rm = TRUE)) %>%
        pull(x) %>%
        quantile(., probs) %>%
        set_names(., paste0(probs, "_level1"))

      c(out, out2)

    },

    #' @description
    #' Get the data type of the output.
    #' This is a default implementation (just return the output type). For other modules,
    #' (e.g., sum) the type depends on the type of the inputs. So, they have their
    #' own implementation.
    #'
    get_output_type = function() {
      self$output_type
    },

    #' @description
    #' Checks that the type of the inputs is consistent
    #'
    check_input_types = function(recursive = FALSE) {

      for (each_par in names(self$input_types)) {

        this_module <- self$depends_on[[each_par]]

        # browser()

        if (!R6::is.R6(this_module)) {  # The dependency has not been defined

          stop("In module ", self$name,
               ": input ", each_par,
               " not defined.")

        }

        if (this_module$type == "constant") {  # Nothing to check for constants
          next
        }

        up_type <- this_module$get_output_type()
        this_type <- self$input_types[[each_par]]

        if (this_type == "any") {  # Nothing to check
          next
        }

        if (this_type != up_type) {
          warning("In module ", self$name,
                  ": the module expects ", this_type,
                  " for input ", each_par,
                  ". Got ", up_type,
                  " instead from ", this_module$name
          )
        }

        ## Call the dependency if it is recursive

        if (recursive) {
          this_module$check_input_types()
        }

      }

    }

  ),
  private = list(

    #' @description
    #' Calls the simulate method for the dependencies
    #'
    update_inputs = function(niter) {

      # sims <- self$depends_on %>% map_dfc(~ .$simulate(niter)$get_output())

      sims <- map2_dfc(self$depends_on, self$depends_index,
                       ~ .x$simulate(niter)$get_output(index = .y)
                       )

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
                          input_types = list(),
                          level = 0) {

      super$initialize(name,
                       input_names = input_names,
                       units = units,
                       module_type = module_type,
                       output_var = output_var,
                       output_unit = output_unit,
                       output_type = "discrete",
                       input_types = input_types,
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
                          input_types = list(),
                          level = 0) {

      super$initialize(name,
                       input_names = input_names,
                       units = units,
                       module_type = module_type,
                       output_var = output_var,
                       output_unit = output_unit,
                       output_type = "continuous",
                       input_types = input_types,
                       level = level)

    }
  )

)







