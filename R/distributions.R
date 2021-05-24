
#' A module simulating a normal distribution
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
Normal <- R6::R6Class(
  classname = "Normal",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("mu", "sigma"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        mu = self$depends_on$mu$simulate(niter),
        sd = self$depends_on$sigma$simulate(niter)
        ) %>%
        dplyr::mutate(
          x = rnorm(niter, mu, sd)
          )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

# ## tests
#
# my_mean <- Constant$new("mean", 3)
# my_sd <- Constant$new("sd", .3)
#
# # my_mean$simulate(10)
# # my_sd$simulate(20)
#
# logN0 <- Normal$new("Concentracion inicial")$
#   map_input("mu", my_mean)$
#   map_input("sigma", my_sd)
# # aa$map_input("mu", my_mean)
# # aa$map_input("sigma", my_sd)
#
# # aa$depends_on
# # my_mean$depended_by
#
# logN0$simulate(2000) %>% hist()
# logN0$simulations
# logN0$get_output()
#
# ## test 2
#
# big_mean <- Constant$new("big mean", 4)
# big_sd <- Constant$new("big sd", 1)
#
# norm1 <- Normal$new("Normal 1")$
#   map_input("mu", big_mean)$
#   map_input("sigma", big_sd)
#
# norm1$simulate(100) %>%
#   hist()
#
# small_sd <- Constant$new("small sd", 0.3)
#
# norm2 <- Normal$new("Normal 2")$
#   map_input("mu", norm1)$
#   map_input("sigma", small_sd)
#
# norm2$simulate(1000) %>% hist()
# norm2$simulations
# norm2$get_output()


