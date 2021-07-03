
#' Dynamic microbial inactivation for the Bigelow model with a ramp profile
#'
predict_Bigelow_ramp <- function(max_time, D_R, z, temp_ref, logN0,
                                 temp_start, temp_end) {

  model_parms <- c(D_R = D_R,
                   z = z,
                   temp_ref = temp_ref,
                   logN0 = logN0
                   )

  times <- seq(0, max_time, length=100)

  ## Define the temperature profile for the prediction
  temperature_profile <- data.frame(time = c(0, max_time),
                                    temperature = c(temp_start, temp_end))

  ## Call the prediction function
  predict_inactivation("Bigelow", times, model_parms, temperature_profile)$simulation$logN %>%
    tail(1)
}

#' R6 class describing log-linear inactivation under dynamic conditions for a ramp
#'
#' @details
#' A risk module describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 7 inputs: treat_time, D_R, z, temp_ref, logN0, temp_start,
#' temp_end
#'
#' @export
#'
DynamicBigelow_1phase <- R6::R6Class(
  classname = "DynamicBigelow_1phase",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("treat_time", "D_R", "z", "temp_ref",
                                       "logN0", "temp_start", "temp_end"),
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
        logN0 = self$depends_on$logN0$simulate(niter),
        max_time = self$depends_on$treat_time$simulate(niter),
        D_R = self$depends_on$D_R$simulate(niter),
        z = self$depends_on$z$simulate(niter),
        temp_ref = self$depends_on$temp_ref$simulate(niter),
        temp_start = self$depends_on$temp_start$simulate(niter),
        temp_end = self$depends_on$temp_end$simulate(niter),
      ) %>%
        mutate(logN = purrr::pmap_dbl(., predict_Bigelow_ramp))

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' Dynamic microbial inactivation for the Bigelow model with a biphasic profile
#'
predict_Bigelow_biphasic <- function(t1, t2, D_R, z, temp_ref, logN0,
                                 temp0, temp1, temp2) {

  model_parms <- c(D_R = D_R,
                   z = z,
                   temp_ref = temp_ref,
                   logN0 = logN0
  )

  times <- seq(0, max_time, length=100)

  ## Define the temperature profile for the prediction
  temperature_profile <- data.frame(time = c(0, t1, t2),
                                    temperature = c(temp0, temp1, temp2))

  ## Call the prediction function
  predict_inactivation("Bigelow", times, model_parms, temperature_profile)$simulation$logN %>%
    tail(1)
}

#' R6 class describing log-linear inactivation under dynamic conditions for a ramp
#'
#' @details
#' A risk module describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 9 inputs: t1, t2, D_R, z, temp_ref, logN0, temp0, temp1, temp2,
#'
#' @export
#'
DynamicBigelow_2phase <- R6::R6Class(
  classname = "DynamicBigelow_2phase",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t1", "t2", "D_R", "z", "temp_ref",
                                       "logN0", "temp0", "temp1", "temp2"),
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
        logN0 = self$depends_on$logN0$simulate(niter),
        t1 = self$depends_on$t1$simulate(niter),
        t2 = self$depends_on$t2$simulate(niter),
        D_R = self$depends_on$D_R$simulate(niter),
        z = self$depends_on$z$simulate(niter),
        temp_ref = self$depends_on$temp_ref$simulate(niter),
        temp0 = self$depends_on$temp0$simulate(niter),
        temp1 = self$depends_on$temp1$simulate(niter),
        temp2 = self$depends_on$temp2$simulate(niter)
      ) %>%
        mutate(logN = purrr::pmap_dbl(., predict_Bigelow_biphasic))

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)


#' Dynamic microbial inactivation for the Bigelow model with a triphasic profile
#'
predict_Bigelow_triphasic <- function(t1, t2, t3, D_R, z, temp_ref, logN0,
                                     temp0, temp1, temp2, temp3) {

  model_parms <- c(D_R = D_R,
                   z = z,
                   temp_ref = temp_ref,
                   logN0 = logN0
  )

  times <- seq(0, max_time, length=100)

  ## Define the temperature profile for the prediction
  temperature_profile <- data.frame(time = c(0, t1, t2, t3),
                                    temperature = c(temp0, temp1, temp2, temp3))

  ## Call the prediction function
  predict_inactivation("Bigelow", times, model_parms, temperature_profile)$simulation$logN %>%
    tail(1)
}

#' R6 class describing log-linear inactivation under dynamic conditions for a ramp
#'
#' @details
#' A risk module describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 11 inputs: t1, t2, t3, D_R, z, temp_ref, logN0,
#' temp0, temp1, temp2, temp3.
#'
#' @export
#'
DynamicBigelow_3phase <- R6::R6Class(
  classname = "DynamicBigelow_3phase",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("t1", "t2", "t3", "D_R", "z", "temp_ref",
                                       "logN0", "temp0", "temp1", "temp2", "temp3"),
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
        logN0 = self$depends_on$logN0$simulate(niter),
        t1 = self$depends_on$t1$simulate(niter),
        t2 = self$depends_on$t2$simulate(niter),
        t3 = self$depends_on$t3$simulate(niter),
        D_R = self$depends_on$D_R$simulate(niter),
        z = self$depends_on$z$simulate(niter),
        temp_ref = self$depends_on$temp_ref$simulate(niter),
        temp0 = self$depends_on$temp0$simulate(niter),
        temp1 = self$depends_on$temp1$simulate(niter),
        temp2 = self$depends_on$temp2$simulate(niter),
        temp3 = self$depends_on$temp3$simulate(niter)
      ) %>%
        mutate(logN = purrr::pmap_dbl(., predict_Bigelow_triphasic))

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#-------------------------------------------------------------------------------
# ## Test Bigelow ramp
#
# D_R <- LogNormal$new("D_R")$
#   map_input("mu_log10", Constant$new("mu_logD", 1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 8))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
#
# treat_time <- Normal$new("treat_time")$
#   map_input("mu", Constant$new("mu_t", 5))$
#   map_input("sigma", Constant$new("sigma_t", 0.1))
#
# z <- Normal$new("z")$
#   map_input("mu", Constant$new("mu_z", 5))$
#   map_input("sigma", Constant$new("sigma_z", 0.2))
#
# temp_ref <- Constant$new("Tref", 60)
#
# temp_start <- Constant$new("T0", 45)
# temp_end  <- Normal$new("Tend")$
#   map_input("mu", Constant$new("mu_Tend", 65))$
#   map_input("sigma", Constant$new("sigma_Tend", 1))
#
#
# inact_model <- DynamicBigelow_1phase$new("inact_model")$
#   map_input("D_R", D_R)$
#   map_input("logN0", logN0)$
#   map_input("treat_time", treat_time)$
#   map_input("z", z)$
#   map_input("temp_ref", temp_ref)$
#   map_input("temp_start", temp_start)$
#   map_input("temp_end", temp_end)
#
# plot_model(inact_model)
# inact_model$simulate(1000) %>% hist()
# inact_model$simulations


## Test Bigelow biphasic

# D_R <- LogNormal$new("D_R")$
#   map_input("mu_log10", Constant$new("mu_logD", 1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 8))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
#
# t1 <- Normal$new("t1")$
#   map_input("mu", Constant$new("mu_t1", 5))$
#   map_input("sigma", Constant$new("sigma_t1", 0.1))
#
# t2 <- Normal$new("t2")$
#   map_input("mu", Constant$new("mu_t2", 5))$
#   map_input("sigma", Constant$new("sigma_t2", 0.1))
#
# z <- Normal$new("z")$
#   map_input("mu", Constant$new("mu_z", 5))$
#   map_input("sigma", Constant$new("sigma_z", 0.2))
#
# temp_ref <- Constant$new("Tref", 60)
#
# temp0 <- Constant$new("T0", 45)
# temp1  <- Normal$new("T1")$
#   map_input("mu", Constant$new("mu_T1", 65))$
#   map_input("sigma", Constant$new("sigma_T1", 1))
#
# temp2  <- Normal$new("T2")$
#   map_input("mu", Constant$new("mu_T2", 55))$
#   map_input("sigma", Constant$new("sigma_T2", 1))
#
#
# inact_model <- DynamicBigelow_2phase$new("inact_model")$
#   map_input("D_R", D_R)$
#   map_input("logN0", logN0)$
#   map_input("t1", t1)$
#   map_input("t2", t2)$
#   map_input("z", z)$
#   map_input("temp_ref", temp_ref)$
#   map_input("temp0", temp0)$
#   map_input("temp1", temp1)$
#   map_input("temp2", temp2)
#
# plot_model(inact_model)
# inact_model$simulate(50) %>% hist()
# inact_model$simulations

## Test Bigelow triphasic

D_R <- LogNormal$new("D_R")$
  map_input("mu_log10", Constant$new("mu_logD", 1))$
  map_input("sigma_log10", Constant$new("sigma_logD", 0.2))

logN0 <- Normal$new("logN0")$
  map_input("mu", Constant$new("mu_logN0", 8))$
  map_input("sigma", Constant$new("sigma_logN0", 0.5))


t1 <- Normal$new("t1")$
  map_input("mu", Constant$new("mu_t1", 5))$
  map_input("sigma", Constant$new("sigma_t1", 0.1))

t2 <- Normal$new("t2")$
  map_input("mu", Constant$new("mu_t2", 5))$
  map_input("sigma", Constant$new("sigma_t2", 0.1))

t3 <- Normal$new("t3")$
  map_input("mu", Constant$new("mu_t3", 5))$
  map_input("sigma", Constant$new("sigma_t3", 0.1))

z <- Normal$new("z")$
  map_input("mu", Constant$new("mu_z", 5))$
  map_input("sigma", Constant$new("sigma_z", 0.2))

temp_ref <- Constant$new("Tref", 60)

temp0 <- Constant$new("T0", 45)
temp1  <- Normal$new("T1")$
  map_input("mu", Constant$new("mu_T1", 65))$
  map_input("sigma", Constant$new("sigma_T1", 1))

temp2  <- Normal$new("T2")$
  map_input("mu", Constant$new("mu_T2", 55))$
  map_input("sigma", Constant$new("sigma_T2", 1))

temp3  <- Normal$new("T3")$
  map_input("mu", Constant$new("mu_T3", 55))$
  map_input("sigma", Constant$new("sigma_T3", 1))


inact_model <- DynamicBigelow_3phase$new("inact_model")$
  map_input("D_R", D_R)$
  map_input("logN0", logN0)$
  map_input("t1", t1)$
  map_input("t2", t2)$
  map_input("t3", t3)$
  map_input("z", z)$
  map_input("temp_ref", temp_ref)$
  map_input("temp0", temp0)$
  map_input("temp1", temp1)$
  map_input("temp2", temp2)$
  map_input("temp3", temp3)

plot_model(inact_model)
inact_model$simulate(50) %>% hist()
inact_model$simulations

























