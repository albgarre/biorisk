
#' Growth module based on exponential growth including time/temperature correlation
#'
#' @export
#'
CorrelatedExpGrowth <- R6::R6Class(
  classname = "CorrelatedExpGrowth",
  inherit = RiskModule,
  public = list(

    ## Fields to describe the MVNorm
    mean_time = NULL,
    sd_time = NULL,
    mean_temperature = NULL,
    sd_temperature = NULL,
    correlation = NULL,

    initialize = function(name,
                          mean_time, sd_time,
                          mean_temperature, sd_temperature,
                          correlation,
                          units = NA,
                          output_unit = NA) {

      ## Generic initilization

      super$initialize(name,
                       input_names = c("Tmin", "b", "logN0"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit)

      ## Add the specific fields


      self$mean_time <- mean_time
      self$sd_time <- sd_time
      self$mean_temperature <- mean_temperature
      self$sd_temperature <- sd_temperature
      self$correlation <- correlation

    },

    simulate = function(niter) {

      ## Get the MVNorm sample

      mus <- c(self$mean_time, self$mean_temperature)
      stdevs <- c(self$sd_time, self$sd_temperature)
      b_matrix <- stdevs %*% t(stdevs)
      cov_matrix <- b_matrix * matrix(c(1, self$correlation, self$correlation, 1), nrow = 2)

      sims <- tibble::as_tibble(mvtnorm::rmvnorm(niter,
                                 mus,
                                 cov_matrix),
                .name_repair = ~ c("t", "temperature")
                ) %>%

        ## Call the rest of the dependencies

        dplyr::mutate(
          Tmin = self$depends_on$Tmin$simulate(niter),
          b = self$depends_on$b$simulate(niter),
          logN0 = self$depends_on$logN0$simulate(niter)
        ) %>%

        ## Apply the secondary model

        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = sq_mu^2
        ) %>%

        ## Model prediction

        dplyr::mutate(
          logN = logN0 + t*mu
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

## tests

# Tmin <- Normal$new("Tmin")$
#   map_input("mu", Constant$new("0", 0))$
#   map_input("sigma", Constant$new("1", 1))
#
# b <- LogNormal$new("b")$
#   map_input("mu_log10", Constant$new("-2", -2))$
#   map_input("sigma_log10", Constant$new(".1", .1))
#
# aa <- CorrelatedExpGrowth$new("test",
#                         mean_time = 100, sd_time = 10,
#                         mean_temperature = 5, sd_temperature = 1,
#                         correlation = -.7)$
#   map_input("Tmin", Tmin)$
#   map_input("b", b)$
#   map_input("logN0", Constant$new("logN0", 3))
#
# aa$simulate(1000) %>% hist()
#
# plot_model(aa)



















