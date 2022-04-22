
#' Growth module based on exponential growth
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
ExponentialGrowth <- R6::R6Class(
  classname = "ExponentialGrowth",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      self$depends_on$logN0$discrete_prediction() + self$depends_on$t$discrete_prediction() * self$depends_on$mu$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 + t*mu
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 + t*mu
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "logNmax"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      logN0 <- self$depends_on$logN0$discrete_prediction()
      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      logNmax <- self$depends_on$logNmax$discrete_prediction()

      logN <- logN0 + t*mu

      ifelse(logN > logNmax, logNmax, logN)

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN_calc = logN0 + t*mu,
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN_calc = logN0 + t*mu,
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Growth model with exponential phase + lag
#'
#' @export
#'
LagExponentialGrowth <- R6::R6Class(
  "LagExponentialGrowth",
  inherit = ContinuousModule,

  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "lambda", "mu", "logN0"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      logN0 <- self$depends_on$logN0$discrete_prediction()
      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()

      logN <- logN0 + t*mu

      ifelse(t > lambda, logN, logN0)

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = ifelse(t <= lambda,
                        logN0,
                        logN0 + (t-lambda)*mu
          )
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = ifelse(t <= lambda,
                        logN0,
                        logN0 + (t-lambda)*mu
          )
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Growth model with exponential phase + lag
#'
#' @export
#'
TrilinealGrowth <- R6::R6Class(
  "TrilinealGrowth",
  inherit = ContinuousModule,

  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "lambda", "mu", "logN0", "logNmax"),
                       units = units,
                       module_type = "growth",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      logN0 <- self$depends_on$logN0$discrete_prediction()
      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()
      logNamx <- self$depends_on$logNmax$discrete_prediction()

      if (t < lambda) {

        logN0

      } else {

        logN <- logN0 + (t-lambda)*mu

        ifelse(logN > logNmax, logNmax, logN)

      }

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN_calc = ifelse(t <= lambda,
                             logN0,
                             logN0 + (t-lambda)*mu
                             ),
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN_calc = ifelse(t <= lambda,
                             logN0,
                             logN0 + (t-lambda)*mu
                             ),
          logN = ifelse(logN_calc > logNmax, logNmax, logN_calc)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

### tests

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
#
# growth_model <- ExponentialGrowthNmax$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("logNmax", Constant$new("logNmax", 4))
#
# growth_model$simulate(1000)
# growth_model$density_plot()
#
# growth_model <- TrilinealGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("logNmax", Constant$new("logNmax", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))
#
# growth_model$simulate(1000)
# growth_model$density_plot()



