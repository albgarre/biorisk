
#' Lineal model
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
LinealModel <- R6::R6Class(
  classname = "LinealModel",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("x", "a", "b"),
                       input_types = list(x = "any",
                                          a = "continuous",
                                          b = "continuous"),
                       units = units,
                       module_type = "simple",
                       output_var = "y",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      self$depends_on$a$point_estimate() + self$depends_on$b$point_estimate() * self$depends_on$x$point_estimate()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = a + b*x
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = a + b*x
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)


# x <- Constant$new("x", 3)
#
# b <- Normal$new("b")$
#   map_input("mu", Constant$new("b_mu", 1))$
#   map_input("sigma", Constant$new("b_sigma", 0.2))
#
# a <- Normal$new("a")$
#   map_input("mu", Constant$new("mu_a", 2))$
#   map_input("sigma", Constant$new("sigma_a", 0.5))
#
#
# growth_model <- LinealModel$new("aa")$
#   map_input("x", x)$
#   map_input("b", b)$
#   map_input("a", a)
#
# plot_model(growth_model)
# growth_model$simulate(1000)
# growth_model$density_plot()


