
#' Boolean operator >
#'
#' TRUE if variable x is greater than a
#'
#' @export
#'
ModuleGreaterThan <- R6::R6Class(
  classname = "ModuleGreaterThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$x$discrete_prediction() > self$depends_on$a$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x > a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x > a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Boolean operator <
#'
#' TRUE if variable x is lower than a
#'
#' @export
#'
ModuleLowerThan <- R6::R6Class(
  classname = "ModuleLowerThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$x$discrete_prediction() < self$depends_on$a$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x < a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x < a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Boolean operator >=
#'
#' TRUE if variable x is greater than a
#'
#' @export
#'
ModuleGreaterEqualThan <- R6::R6Class(
  classname = "ModuleGreaterEqualThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$x$discrete_prediction() >= self$depends_on$a$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x >= a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x >= a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Boolean operator <=
#'
#' TRUE if variable x is greater than a
#'
#' @export
#'
ModuleLowerEqualThan <- R6::R6Class(
  classname = "ModuleLowerEqualThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$x$discrete_prediction() <= self$depends_on$a$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x <= a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x <= a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Boolean operator ==
#'
#' TRUE if variable x is greater than a
#'
#' @export
#'
ModuleEqualTo <- R6::R6Class(
  classname = "ModuleEqualTo",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$x$discrete_prediction() == self$depends_on$a$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x == a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x == a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

## tests

# N1 <- Normal$new("N1")$
#   map_input("mu", Constant$new("mu_1", 1))$
#   map_input("sigma", Constant$new("sd_1", 1))
#
# N2 <- Normal$new("N2")$
#   map_input("mu", Constant$new("mu_2", 2))$
#   map_input("sigma", Constant$new("sd_2", 1))
#
# aa <- ModuleGreaterThan$new("aa")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# plot_model(aa)
# aa$simulate(1000) %>% mean()
# aa$simulations
#
# bb <- ModuleLowerThan$new("bb")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# bb$simulate(10)
# bb$simulations
#
# cc <- ModuleGreaterEqualThan$new("cc")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# cc$simulate(10)
# cc$simulations
#
# dd <- ModuleLowerEqualThan$new("dd")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# dd$simulate(10)
# dd$simulations
#
# xx <- Poisson$new("xx")$
#   map_input("lambda", Constant$new("lambda", 2))
#
# aa <- ModuleEqualTo$new("aa")$
#   map_input("x", xx)$
#   map_input("a", Constant$new("a", 2))
#
# aa$simulate(10)
# aa$simulations
#
