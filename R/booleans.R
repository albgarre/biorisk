
#' Boolean operator >
#'
#' TRUE if variable x is greater than a
#'
#' @export
#'
ElementGreaterThan <- R6::R6Class(
  classname = "ElementGreaterThan",
  inherit = RiskElement,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() > self$depends_on$a$point_estimate()
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
ElementLowerThan <- R6::R6Class(
  classname = "ElementLowerThan",
  inherit = RiskElement,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() < self$depends_on$a$point_estimate()
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
ElementGreaterEqualThan <- R6::R6Class(
  classname = "ElementGreaterEqualThan",
  inherit = RiskElement,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() >= self$depends_on$a$point_estimate()
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
ElementLowerEqualThan <- R6::R6Class(
  classname = "ElementLowerEqualThan",
  inherit = RiskElement,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() <= self$depends_on$a$point_estimate()
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
ElementEqualTo <- R6::R6Class(
  classname = "ElementEqualTo",
  inherit = RiskElement,
  public = list(

    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() == self$depends_on$a$point_estimate()
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
# aa <- ElementGreaterThan$new("aa")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# plot_model(aa)
# aa$simulate(1000) %>% mean()
# aa$simulations
#
# bb <- ElementLowerThan$new("bb")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# bb$simulate(10)
# bb$simulations
#
# cc <- ElementGreaterEqualThan$new("cc")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# cc$simulate(10)
# cc$simulations
#
# dd <- ElementLowerEqualThan$new("dd")$
#   map_input("a", N1)$
#   map_input("x", N2)
#
# dd$simulate(10)
# dd$simulations
#
# xx <- Poisson$new("xx")$
#   map_input("lambda", Constant$new("lambda", 2))
#
# aa <- ElementEqualTo$new("aa")$
#   map_input("x", xx)$
#   map_input("a", Constant$new("a", 2))
#
# aa$simulate(10)
# aa$simulations
#
