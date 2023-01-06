
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
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$a$discrete_prediction() + self$depends_on$b$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a + b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a + b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$a$discrete_prediction() - self$depends_on$b$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a - b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a - b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$a$discrete_prediction() * self$depends_on$b$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a * b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a * b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Division of the output of two modules (a/b)
#'
#' @export
#'
ModuleDivision <- R6::R6Class(
  classname = "ModuleDivision",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$a$discrete_prediction() / self$depends_on$b$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a / b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a / b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Power of the output of two modules (a^b)
#'
#' @export
#'
ModulePower <- R6::R6Class(
  classname = "ModulePower",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$a$discrete_prediction() ^ self$depends_on$b$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a ^ b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a ^ b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Square root of the output of a modules
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
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      sqrt(self$depends_on$a$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = sqrt(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = sqrt(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      log10(self$depends_on$a$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = log10(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = log10(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      log(self$depends_on$a$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = log(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = log(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      exp(self$depends_on$a$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = exp(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = exp(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      10^(self$depends_on$a$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = 10^(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = 10^(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

# N1 <- Normal$new("N1")$
#   map_input("mu", Constant$new("mu_1", 1))$
#   map_input("sigma", Constant$new("sd_1", 1))
#
# N2 <- Normal$new("N2")$
#   map_input("mu", Constant$new("mu_2", 5))$
#   map_input("sigma", Constant$new("sd_2", 4))
#
# aa <- ModulePlus$new("sum")$
#   map_input("a", N1)$
#   map_input("b", N2)
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- ModuleMinus$new("sum")$
#   map_input("a", N1)$
#   map_input("b", N2)
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- ModuleTimes$new("sum")$
#   map_input("a", N1)$
#   map_input("b", N2)
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- ModuleDivision$new("sum")$
#   map_input("a", N1)$
#   map_input("b", N2)
#
# aa$simulate(1000)
# aa$density_plot()
#
#
# aa <- ModulePower$new("-")$
#   map_input("a", N1)$
#   map_input("b", Constant$new("a", 2))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <-ModuleSqrt$new("-")$
#   map_input("a", N2)
# aa$simulate(1000)
# aa$histogram()
#
# aa <- ModuleLog$new("-")$
#   map_input("a", N2)
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- ModuleLn$new("")$
#   map_input("a", N2)
#
# aa$simulate(100)
# aa$density_plot()
#
# aa <- ModuleExp$new("")$
#   map_input("a", N1)
# aa$simulate(100)
# aa$histogram()
#
# aa <- ModulePow10$new("")$
#   map_input("a", N2)
# aa$simulate(52)
# aa$histogram()


