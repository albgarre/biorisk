
#' Filters the output of a module according to a maximum value
#'
#' @export
#'
MaxFilter <- R6::R6Class(
  classname = "MaxFilter",
  inherit = RiskModule,
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
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Get the substitute value

      substitute <- switch(as.character(self$rule),
             `1` = NA,
             `2` = self$max_value,
             stop("Unknown rule:", self$rule)
             )

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute, a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Filters the output of a module according to a minimum value
#'
#' @export
#'
MinFilter <- R6::R6Class(
  classname = "MinFilter",
  inherit = RiskModule,
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
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Get the substitute value

      substitute <- switch(as.character(self$rule),
                           `1` = NA,
                           `2` = self$min_value,
                           stop("Unknown rule:", self$rule)
      )

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute, a)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

#' Filters the output of a module according to both a minimum and maximum value
#'
#' @export
#'
MinMaxFilter <- R6::R6Class(
  classname = "MinMaxFilter",
  inherit = RiskModule,
  public = list(

    min_value = NULL,
    max_value = NULL,
    rule = NULL,

    initialize = function(name,
                          min_value,
                          max_value,
                          units = NA,
                          output_unit = NA,
                          rule = 1
    ) {

      if (max_value < min_value) {
        stop("max_value must be higher than min_value")
      }

      self$rule = rule
      self$min_value = min_value
      self$max_value = max_value

      super$initialize(name,
                       input_names = c("a"),
                       units = units,
                       module_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

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

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = ifelse(a < self$min_value, substitute_left, a)
        ) %>%
        dplyr::mutate(
          x = ifelse(a > self$max_value, substitute_right, x)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x

    }

  )

)

## tests
#
# N1 <- Normal$new("")$
#   map_input("mu", Constant$new("", 0))$
#   map_input("sigma", Constant$new("", .5))
#
# # N1$simulate(1000)
# MaxFilter$new("aa", .6, rule = 2)$
#   map_input("a", N1)$
#   simulate(1000) %>% hist()
#
#
# MinFilter$new("aa", -1, rule = 2)$
#   map_input("a", N1)$
#   simulate(1000) %>% hist()
#
# MinMaxFilter$new("", -1, 0, rule = 2)$
#   map_input("a", N1)$
#   simulate(1000) %>%
#   hist()









