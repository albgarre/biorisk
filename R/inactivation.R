
#' R6 class describing log-linear inactivation
#'
#' @details
#' A risk module describing log-linear inactivation. It has 3 inputs: logN0, t
#' and logD.
#'
#' @export
#'
LogLinInactivation <- R6::R6Class(
  classname = "LogLinInactivation",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t", "logD", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        logD = self$depends_on$logD$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - t/(10^logD)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    }

  )

)

#' R6 class describing the Weibull inactivation
#'
#' @details
#' A risk module describing Weibullian inactivation. It has 4 inputs: logN0, t
#' log_delta, beta.
#'
#' @export
#'
WeibullInactivation <- R6::R6Class(
  classname = "WeibullInactivation",
  inherit = RiskModule,
  public = list(
    
    initialize = function(name,
                          units = NA,
                          output_unit = NA) {
      
      super$initialize(name,
                       input_names = c("t", "log_delta", "beta", "logN0"),
                       units = units,
                       module_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit)
      
    },
    
    
    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {
      
      ## Do the simulations (recursively)
      
      sims <- tibble::tibble(
        t = self$depends_on$t$simulate(niter),
        log_delta = self$depends_on$log_delta$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter),
        beta = self$depends_on$beta$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 - (t/(10^log_delta))^beta
        )
      
      ## Save the results of the simulations
      
      self$simulations <- sims
      
      ## Return
      
      sims$logN
      
    }
    
  )
  
)


## tests

# time <- Constant$new("Time", 30)
# time2 <- Constant$new("Time", 30, output_unit = "CFU")
#
# time$output_unit
# time2$output_unit


# logD <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# inact_model$simulate(1000) %>% hist()
# inact_model$simulations

## test weibull
# 
# WeibullInactivation$new("Weibull")
# 
# logN0 <- Normal$new("logN0")$
#     map_input("mu", Constant$new("mu", 3))$
#     map_input("sigma", Constant$new("sigma", 0.5))
# 
# log_delta <- Normal$new("log_delta")$
#     map_input("mu", Constant$new("mu", 1))$
#     map_input("sigma", Constant$new("sigma", 0.2))
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
#   map_input("log_delta", log_delta)
# 
# 
# 
# weibull_model$depends_on
# 
# weibull_model$simulate(1000)%>%hist()
# weibull_model$simulations
# plot_model(weibull_model)
