
#' R6 class describing log-linear inactivation
#'
#' @details
#' A risk module describing log-linear inactivation. It has 3 inputs: logN0, t
#' and D.
#'
#' @export
#'
LogLinInactivation <- R6::R6Class(
  classname = "LogLinInactivation",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "D", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      D <- self$depends_on$D$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()

      logN0 - t/D

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 - t/D
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 - t/D
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the Weibull inactivation
#'
#' @details
#' A risk module describing Weibullian inactivation. It has 4 inputs: logN0, t
#' delta, beta.
#'
#' @export
#'
WeibullInactivation <- R6::R6Class(
  classname = "WeibullInactivation",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "delta", "beta", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      delta <- self$depends_on$delta$discrete_prediction()
      beta <- self$depends_on$beta$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()

      logN0 - (t/delta)^beta

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 - (t/delta)^beta
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 - (t/delta)^beta
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the Weibull inactivation
#'
#' @details
#' A risk module describing Weibullian inactivation using the parameterization by
#' Peleg. It has 4 inputs: logN0, t, b and n.
#'
#' @export
#'
PelegInactivation <- R6::R6Class(
  classname = "PelegInactivation",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "b", "n", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      b <- self$depends_on$b$discrete_prediction()
      n <- self$depends_on$n$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()

      logN0 - b*t^n

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 - b*t^n
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 - b*t^n
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing Tri-linear inactivation
#'
#' @details
#' A risk module describing Tri-linear inactivation. It has 5 inputs: logN0, t, D, SL and Nres.
#'
#' @export
#'
TrilinearInactivation <- R6::R6Class(
  classname = "TrilinearInactivation",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "logN0", "D", "SL", "logNres"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      D <- self$depends_on$D$discrete_prediction()
      SL <- self$depends_on$SL$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      logNres <- self$depends_on$logNres$discrete_prediction()

      logN0 - b*t^n

      logN <- logN0 - (t-SL)/D
      logN <- ifelse(t < SL, logN0, logN)
      logN <- ifelse(logN < logNres, logNres, logN)

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          logN = logN0 - (t-SL)/D
        ) %>%
        dplyr::mutate(
          logN = ifelse(t < SL, logN0, logN),
          logN = ifelse(logN < logNres, logNres, logN)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          logN = logN0 - (t-SL)/D
        ) %>%
        dplyr::mutate(
          logN = ifelse(t < SL, logN0, logN),
          logN = ifelse(logN < logNres, logNres, logN)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing Tri-linear inactivation
#'
#' @details
#' A risk module describing Tri-linear inactivation. It has 5 inputs: logN0, t, D, SL and Nres.
#'
#' @export
#'
GeeraerdInactivation <- R6::R6Class(
  classname = "GeeraerdInactivation",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "logN0", "D", "SL", "logNres"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      D <- self$depends_on$D$discrete_prediction()
      SL <- self$depends_on$SL$discrete_prediction()
      logN0 <- self$depends_on$logN0$discrete_prediction()
      logNres <- self$depends_on$logNres$discrete_prediction()

      k <- log(10)/D

      logNres + log10(( (10^(logN0-logNres)-1)*exp(k*SL) )/(exp(k*t) + exp(k*SL) - 1) + 1)

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          k = log(10)/D,
          logN = logNres + log10(( (10^(logN0-logNres)-1)*exp(k*SL) )/(exp(k*t) + exp(k*SL) - 1) + 1)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          k = log(10)/D,
          logN = logNres + log10(( (10^(logN0-logNres)-1)*exp(k*SL) )/(exp(k*t) + exp(k*SL) - 1) + 1)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing Tri-linear inactivation
#'
#' @details
#' A risk module describing Tri-linear inactivation. It has 5 inputs: logN0, t, D, SL and Nres.
#'
#' @export
#'
GeeraerdInactivation_noSL <- R6::R6Class(
  classname = "GeeraerdInactivation_noSL",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("t", "logN0", "D", "logNres"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      t <- self$depends_on$t$discrete_prediction()
      D <- self$depends_on$D$discrete_prediction()
      SL <- 0
      logN0 <- self$depends_on$logN0$discrete_prediction()
      logNres <- self$depends_on$logNres$discrete_prediction()

      k <- log(10)/D

      logNres + log10(( (10^(logN0-logNres)-1)*exp(k*SL) )/(exp(k*t) + exp(k*SL) - 1) + 1)

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          k = log(10)/D,
          logN = logNres + log10(( (10^(logN0-logNres)-1) )/(exp(k*t)) + 1)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          k = log(10)/D,
          logN = logNres + log10(( (10^(logN0-logNres)-1) )/(exp(k*t)) + 1)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)



# ## tests
#
# library(biorisk)
# library(tidyverse)
#
# time <- Constant$new("Time", 30)
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", 1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000)
# inact_model$histogram()
#
# ##
#
# # Dref <- LogNormal$new("Dref")$
# #   map_input("mu_log10", Constant$new("mu_log10", .1))$
# #   map_input("sigma_log10", Constant$new("simga_log10", .01))
# #
# # temperature <- Normal$new("temp")$
# #   map_input("mu", Constant$new("mu_temp", 60))$
# #   map_input("sigma", Constant$new("sigma_temp", 2))
# #
# # z <- Normal$new("z")$
# #   map_input("mu", Constant$new("mu_z", 5))$
# #   map_input("sigma", Constant$new("sigma_z", .2))
# #
# # Tref <- Constant$new("Tref", 60)
# #
# # model_D <- Dz_model$new("secondary_D")$
# #   map_input("Dref", Dref)$
# #   map_input("temperature", temperature)$
# #   map_input("z", z)$
# #   map_input("Tref", Tref)
# #
# # inact_model <- LogLinInactivation$new("Inactivation")$
# #   map_input("t", time)$
# #   map_input("D", model_D)$
# #   map_input("logN0", logN0)
# #
# # plot_model(inact_model)
# #
# # inact_model$simulate(100) %>% hist()
# #
# # model_D$simulations$logD %>% hist()
#
# #
# # plot_model(inact_model)
#
# ## test peleg
#
# #
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN", 3))$
#   map_input("sigma", Constant$new("sigma_logN", 0.5))
#
# b <- Normal$new("b")$
#   map_input("mu", Constant$new("mu", .1))$
#   map_input("sigma", Constant$new("sigma", 0.02))
#
# t <- Constant$new("t", 7)
# n <- Normal$new("n")$
#   map_input("mu", Constant$new("mu_n", 1))$
#   map_input("sigma", Constant$new("sigma_n", .1))
#
#
# aa <- PelegInactivation$new("Peleg")$
#   map_input("n", n)$
#   map_input("b", b)$
#   map_input("t", t)$
#   map_input("logN0", logN0)
#
# aa$simulate(10000)
# aa$density_plot()
#
#
#
#
# ## test weibull
#
# logN0 <- Normal$new("logN0")$
#     map_input("mu", Constant$new("mu_logN", 3))$
#     map_input("sigma", Constant$new("sigma_logN", 0.5))
#
# delta <- LogNormal$new("log_delta")$
#     map_input("mu_log10", Constant$new("mu", 1))$
#     map_input("sigma_log10", Constant$new("sigma", 0.2))
#
# t <- Constant$new("t", 7)
#
# beta <- Constant$new("beta", 0.8)
#
#
# weibull_model <- WeibullInactivation$new("Weibull")$
#   map_input("beta", beta)$
#   map_input("t", t)$
#   map_input("logN0", logN0)$
#   map_input("delta", delta)
#
# weibull_model$simulate(1000)
# weibull_model$boxplot()
#

## test tri-linear
#
# library(biorisk)
# library(tidyverse)
#
# time <- Normal$new("t")$
#   map_input("mu", Constant$new("mu_t", 5))$
#   map_input("sigma", Constant$new("sigma_logN0", 2))
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", .1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.02))
#
# logN0 <- Constant$new("logN0", 6)
# logNres <- Constant$new("logNres", 3)
# SL <- Constant$new("SL", 5)
#
# inact_model <- TrilinearInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)$
#   map_input("logNres", logNres)$
#   map_input("SL", SL)
#
# plot_model(inact_model)
# inact_model$simulate(1000)
# inact_model$histogram()
#
#
# plot_model(inact_model)
#
# time <- Normal$new("t")$
#   map_input("mu", Constant$new("mu_t", 5))$
#   map_input("sigma", Constant$new("sigma_logN0", 2))
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", .1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.02))
#
# logN0 <- Constant$new("logN0", 6)
# logNres <- Constant$new("logNres", 3)
# SL <- Constant$new("SL", 5)
#
# inact_model <- GeeraerdInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)$
#   map_input("logNres", logNres)$
#   map_input("SL", SL)
#
# plot_model(inact_model)
# inact_model$simulate(1000)
# inact_model$histogram()
#
#
# inact_model$discrete_prediction()
#
# time <- Normal$new("t")$
#   map_input("mu", Constant$new("mu_t", 5))$
#   map_input("sigma", Constant$new("sigma_logN0", 2))
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", .1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.02))
#
# logN0 <- Constant$new("logN0", 6)
# logNres <- Constant$new("logNres", 3)
# SL <- Constant$new("SL", 5)
#
# inact_model <- GeeraerdInactivation_noSL$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)$
#   map_input("logNres", logNres)
#
# plot_model(inact_model)
# inact_model$simulate(1000)
# inact_model$histogram()
#
#
# inact_model$discrete_prediction()









