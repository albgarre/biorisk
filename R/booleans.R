
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
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = x > a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

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
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = x < a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Boolean operator >=
#'
#' TRUE if variable x is greater or equal than a
#'
#' @export
#'
ModuleGreaterEqualThan <- R6::R6Class(
  classname = "ModuleGreaterEqualThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = x >= a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Boolean operator <=
#'
#' TRUE if variable x is lower or equal than a
#'
#' @export
#'
ModuleLowerEqualThan <- R6::R6Class(
  classname = "ModuleLowerEqualThan",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = x <= a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Boolean operator ==
#'
#' TRUE if variable x is equal to a
#'
#' @export
#'
ModuleEqualTo <- R6::R6Class(
  classname = "ModuleEqualTo",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = x == a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Boolean operator between
#'
#' TRUE if variable x is between a and b
#'
#' @export
#'
ModuleBetween <- R6::R6Class(
  classname = "ModuleBetween",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b", "x"),
                       units = units,
                       module_type = "boolean",
                       output_var = "y",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter),
        x = self$depends_on$x$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = between(x, a, b)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

# ## tests
#
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
# aa <- ModuleBetween$new("aa")$
#   map_input("x", xx)$
#   map_input("a", Constant$new("a", 1))$
#   map_input("b", Constant$new("b", 3))
#
# plot_model(aa)
# aa$simulate(10)
# aa$simulations
