
#' Filters the output of a module according to a maximum value
#'
#' @export
#'
MaxFilter <- R6::R6Class(
  classname = "RightFilter",
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

## tests
#
# N1 <- Normal$new("")$
#   map_input("mu", Constant$new("", 0))$
#   map_input("sigma", Constant$new("", .5))
#
# # N1$simulate(1000)
# aa <- MaxFilter$new("aa", .6, rule = 2)$
#   map_input("a", N1)
#
#
# aa$simulate(1000) %>% hist()








