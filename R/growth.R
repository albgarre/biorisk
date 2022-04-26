
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

#' Growth module based on the Baranyi model
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
BaranyiGrowth <- R6::R6Class(
  classname = "BaranyiGrowth",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "lambda", "logNmax"),
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

      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()
      logNmax <- self$depends_on$logNmax$discrete_prediction()

      num <- 1 + exp(log(10)*mu*(t - lambda)) - exp(-log(10)*mu*lambda)
      den <- exp(log(10)*mu*(t-lambda)) - exp(-log(10)*mu*lambda) + 10^(logNmax - logN0)
      logN <- logNmax + log10(num/den)

      logN


    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          num = 1 + exp(log(10)*mu*(t - lambda)) - exp(-log(10)*mu*lambda),
          den = exp(log(10)*mu*(t-lambda)) - exp(-log(10)*mu*lambda) + 10^(logNmax - logN0),
          logN = logNmax + log10(num/den)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          num = 1 + exp(log(10)*mu*(t - lambda)) - exp(-log(10)*mu*lambda),
          den = exp(log(10)*mu*(t-lambda)) - exp(-log(10)*mu*lambda) + 10^(logNmax - logN0),
          logN = logNmax + log10(num/den)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Growth module based on the modified Gompertz model
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
modGompertzGrowth <- R6::R6Class(
  classname = "modGompertzGrowth",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "lambda", "C"),
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

      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()
      C <- self$depends_on$C$discrete_prediction()

      exponent <- (mu/C)*exp(1)*(lambda - t) +1

      logN <- logN0 + C*exp( -exp( exponent ) )

      logN


    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 + C*(exp(-exp( exp(1)*(mu/C)*(lambda-t)+1 )))
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 + C*(exp(-exp( exp(1)*(mu/C)*(lambda-t)+1 )))
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Growth module based on the logistic growth model
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
LogisticGrowth <- R6::R6Class(
  classname = "LogisticGrowth",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "lambda", "C"),
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

      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()
      C <- self$depends_on$C$discrete_prediction()

      logN0 + C/(1 + exp(4*mu/C*(lambda-t) + 2))

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 + C/(1 + exp(4*mu/C*(lambda-t) + 2))
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 + C/(1 + exp(4*mu/C*(lambda-t) + 2))
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Growth module based on the Richards growth model
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
RichardsGrowth <- R6::R6Class(
  classname = "RichardsGrowth",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "mu", "logN0", "lambda", "C", "nu"),
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

      t <- self$depends_on$t$discrete_prediction()
      mu <- self$depends_on$mu$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      lambda <- self$depends_on$lambda$discrete_prediction()
      C <- self$depends_on$C$discrete_prediction()
      nu <- self$depends_on$nu$discrete_prediction()

      exp_part <- 1 + nu + mu/C*(1+nu)^(1 + 1/nu)*(lambda-t)

      logN <- logN0 + C*(1 + nu*exp(exp_part))^(-1/nu)

      logN

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          exp_part = 1 + nu + mu/C*(1+nu)^(1 + 1/nu)*(lambda-t),
          logN = logN0 + C*(1 + nu*exp(exp_part))^(-1/nu)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          exp_part = 1 + nu + mu/C*(1+nu)^(1 + 1/nu)*(lambda-t),
          logN = logN0 + C*(1 + nu*exp(exp_part))^(-1/nu)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

### tests
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
#
# growth_model <- BaranyiGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("logNmax", Constant$new("logNmax", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))
#
# growth_model$simulate(100)
# growth_model$density_plot()
# growth_model$discrete_prediction()
#
#
# growth_model <- modGompertzGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("C", Constant$new("C", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))
#
# growth_model$simulate(100)
# growth_model$density_plot()
# growth_model$discrete_prediction()
#
# growth_model <- LogisticGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("C", Constant$new("C", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))
#
# growth_model$simulate(100)
# growth_model$density_plot()
# growth_model$discrete_prediction()
#
# growth_model <- RichardsGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)$
#   map_input("C", Constant$new("C", 4))$
#   map_input("lambda", Constant$new("lambda", 1.5))$
#   map_input("nu", Constant$new("nu", .8))
#
# growth_model$simulate(100)
# growth_model$density_plot()
# growth_model$discrete_prediction()

