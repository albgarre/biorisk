
#' Filters the output of a element according to a maximum value
#'
#' @export
#'
MaxFilter <- R6::R6Class(
  classname = "MaxFilter",
  inherit = RiskElement,
  public = list(

    max_value = NULL,
    rule = NULL,

    initialize = function(name,
                          max_value,
                          units = NA,
                          output_unit = NA,
                          rule = 1
    ) {

      self$rule = rule
      self$max_value = max_value

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("point_estimate not implemented for filters")

    }

  ),

  private = list(

    update_output = function(niter) {

      ## Get the substitute value

      substitute <- switch(as.character(self$rule),
                           `1` = NA,
                           `2` = self$max_value,
                           stop("Unknown rule:", self$rule)
      )

      sims <- self$simulations %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute, a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      substitute <- switch(as.character(self$rule),
                           `1` = NA,
                           `2` = self$max_value,
                           stop("Unknown rule:", self$rule)
      )

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute, a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Filters the output of a element according to a minimum value
#'
#' @export
#'
MinFilter <- R6::R6Class(
  classname = "MinFilter",
  inherit = RiskElement,
  public = list(

    min_value = NULL,
    rule = NULL,

    initialize = function(name,
                          min_value,
                          units = NA,
                          output_unit = NA,
                          rule = 1
    ) {

      self$rule = rule
      self$min_value = min_value

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("point_estimate not implemented for filters")

    }

  ),

  private = list(

    update_output = function(niter) {

      ## Get the substitute value

      substitute <- switch(as.character(self$rule),
                           `1` = NA,
                           `2` = self$min_value,
                           stop("Unknown rule:", self$rule)
      )

      sims <- self$simulations %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute, a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      substitute <- switch(as.character(self$rule),
                           `1` = NA,
                           `2` = self$min_value,
                           stop("Unknown rule:", self$rule)
      )

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute, a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Filters the output of a element according to both a minimum and maximum value
#'
#' @export
#'
MinMaxFilter <- R6::R6Class(
  classname = "MinMaxFilter",
  inherit = RiskElement,
  public = list(

    max_value = NULL,
    min_value = NULL,
    rule = NULL,

    initialize = function(name,
                          min_value,
                          max_value,
                          units = NA,
                          output_unit = NA,
                          rule = 1
    ) {

      self$rule = rule
      self$max_value = max_value
      self$min_value = min_value

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("point_estimate not implemented for filters")

    }

  ),

  private = list(

    update_output = function(niter) {

      ## Get the substitute value

      substitute_left <- switch(as.character(self$rule),
                                `1` = NA,
                                `2` = self$min_value,
                                stop("Unknown rule:", self$rule)
      )

      substitute_right <- switch(as.character(self$rule),
                                 `1` = NA,
                                 `2` = self$max_value,
                                 stop("Unknown rule:", self$rule)
      )

      sims <- self$simulations %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute_left, a)
        ) %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute_right, x)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      substitute_left <- switch(as.character(self$rule),
                                `1` = NA,
                                `2` = self$min_value,
                                stop("Unknown rule:", self$rule)
      )

      substitute_right <- switch(as.character(self$rule),
                                 `1` = NA,
                                 `2` = self$max_value,
                                 stop("Unknown rule:", self$rule)
      )

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute_left, a)
        ) %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute_right, x)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

## tests
#
# N1 <- Normal$new("")$
#   map_input("mu", Constant$new("", 0))$
#   map_input("sigma", Constant$new("", .5))
#
# aa <- MaxFilter$new("aa", .6, rule = 2)$
#   map_input("a", N1)
# aa$simulate(100)
# aa$histogram()

# aa <- MinFilter$new("aa", -.5, rule = 2)$
#   map_input("a", N1)
#
# aa$simulate(100)
# aa$histogram()
#
# aa <- MinMaxFilter$new("", -.9, 0.9, rule = 2)$
#   map_input("a", N1)
#
# aa$simulate(1000)
# aa$histogram()

