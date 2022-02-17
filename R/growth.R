
#' Growth module based on exponential growth
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
ExponentialGrowth <- R6::R6Class(
  classname = "ExponentialGrowth",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        mu = self$depends_on$mu$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 + t*mu
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Growth module based on exponential growth considering a stationary phase
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
ExponentialGrowthNmax <- R6::R6Class(
  classname = "ExponentialGrowthNmax",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "logNmax"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        mu = self$depends_on$mu$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        logNmax = self$depends_on$logNmax$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN_calc = logN0 + t*mu,
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)



#' Growth model with exponential phase + lag
#'
#' @export
#'
LagExponentialGrowth <- R6::R6Class(
  "LagExponentialGrowth",
  inherit = RiskModule,

  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "lambda", "mu", "logN0"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit)

    },

    simulate = function(niter, check_units = FALSE) {

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        mu = self$depends_on$mu$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        lambda = self$depends_on$lambda$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = ifelse(t <= lambda,
                        logN0,
                        logN0 + (t-lambda)*mu
          )
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]


    }
  )
)

#' Growth model with exponential phase + lag
#'
#' @export
#'
TrilinealGrowth <- R6::R6Class(
  "TrilinealGrowth",
  inherit = RiskModule,

  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "lambda", "mu", "logN0", "logNmax"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit)

    },

    simulate = function(niter, check_units = FALSE) {

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        mu = self$depends_on$mu$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        lambda = self$depends_on$lambda$simulate(niter),
        logNmax = self$depends_on$logNmax$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN_calc = ifelse(t <= lambda,
                        logN0,
                        logN0 + (t-lambda)*mu
                        ),
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]


    }
  )
)


# ### tests
#
# time <- Constant$new("Time", 3)
#
# mu <- Normal$new("mu")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
# growth_model <- ExponentialGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)
#
# growth_model$simulate(1000) %>% hist()
# growth_model$simulations
#
# growth_model <- ExponentialGrowthNmax$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("logNmax", Constant$new("logNmax", 4))
#
# growth_model <- TrilinealGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("logNmax", Constant$new("logNmax", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))
#
# growth_model$simulate(100)
#
# growth_model$simulate(1000)
#
# ## tests
#
### Inactivation

# library(biorisk)
# library(tidyverse)
#
# treat_time <- Constant$new("Time", 30)
#
# D <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", treat_time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)
#
# ## Storage
#
# stor_time <- Constant$new("Storage time", 3)
#
# mu <- Normal$new("mu")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# growth_model <- ExponentialGrowth$new("Growth")$
#   map_input("t", stor_time)$
#   map_input("mu", mu)$
#   map_input("logN0", inact_model)
#
# growth_model <- ExponentialGrowth$new("Growth")
#
# growth_model$map_input("t", stor_time)
# growth_model$map_input("mu", mu)
# growth_model$map_input("logN0", inact_model)
#
# ### Simulate and plot
#
# growth_model$simulate(1000)
#
# tibble(
#   logN0 = logN0$get_output(),
#   treatment = inact_model$get_output(),
#   consumer = growth_model$get_output()
# ) %>%
#   pivot_longer(everything(), names_to = "step", values_to = "logN") %>%
#   mutate(step = factor(step, levels = c("logN0", "treatment", "consumer"))) %>%
#   ggplot() +
#   geom_boxplot(aes(x = step, y = logN))
#
# inact_model$simulations
# plot_model(inact_model)



