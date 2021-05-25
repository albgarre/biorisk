

#' Sum of the output of two modules
#'
#' @export
#'
ModulePlus <- R6::R6Class(
  classname = "ModulePlus",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = a + b
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Substraction of the output of two modules
#'
#' @export
#'
ModuleMinus <- R6::R6Class(
  classname = "ModuleMinus",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = a - b
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Multiplication of the output of two modules
#'
#' @export
#'
ModuleTimes <- R6::R6Class(
  classname = "ModuleTimes",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = a * b
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Division of the output of two modules
#'
#' @export
#'
ModuleDivision <- R6::R6Class(
  classname = "ModuleDivision",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = a/b
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Calculation of the output of one module to the power of a different one
#'
#' @export
#'
ModulePower <- R6::R6Class(
  classname = "ModulePower",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = a^b
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Square root of the output of a module
#'
#' @export
#'
ModuleSqrt <- R6::R6Class(
  classname = "ModuleSqrt",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = sqrt(a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Decimal logarithm of the output of a module
#'
#' @export
#'
ModuleLog <- R6::R6Class(
  classname = "ModuleLog",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = log10(a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Natural logarithm of the output of a module
#'
#' @export
#'
ModuleLn <- R6::R6Class(
  classname = "ModuleLn",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = log(a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Exponential of the output of a module
#'
#' @export
#'
ModuleExp <- R6::R6Class(
  classname = "ModuleExp",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = exp(a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Ten to the power of the output of a module
#'
#' @export
#'
ModulePow10 <- R6::R6Class(
  classname = "ModulePow10",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = 10^a
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

## tests

# N1 <- Normal$new("N1")$
#   map_input("mu", Constant$new("mu_1", 1))$
#   map_input("sigma", Constant$new("sd_1", 1))
#
# N2 <- Normal$new("N2")$
#   map_input("mu", Constant$new("mu_2", 5))$
#   map_input("sigma", Constant$new("sd_2", 4))
#
# ModulePlus$new("sum")$
#   map_input("a", N1)$
#   map_input("b", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleMinus$new("-")$
#   map_input("a", N1)$
#   map_input("b", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleTimes$new("-")$
#   map_input("a", N1)$
#   map_input("b", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleDivision$new("-")$
#   map_input("a", N2)$
#   map_input("b", N1)$
#   simulate(1000) %>%
#   hist()
#
# ModulePower$new("-")$
#   map_input("a", N1)$
#   map_input("b", Constant$new("a", 2))$
#   simulate(1000) %>%
#   hist()
#
# ModuleSqrt$new("-")$
#   map_input("a", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleLog$new("-")$
#   map_input("a", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleLn$new("")$
#   map_input("a", N2)$
#   simulate(1000) %>%
#   hist()
#
# ModuleExp$new("")$
#   map_input("a", N1)$
#   simulate(1000) %>%
#   hist()
#
# ModulePow10$new("")$
#   map_input("a", N2)$
#   simulate(1000) %>%
#   hist()




