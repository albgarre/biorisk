
#' Dynamic microbial inactivation for the Bigelow model with a ramp profile
#'
#' @importFrom bioinactivation predict_inactivation
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
#' A risk element describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 7 inputs: treat_time, D_R, z, temp_ref, logN0, temp_start,
#' temp_end
#'
#' @export
#'
DynamicBigelow_1phase <- R6::R6Class(
  classname = "DynamicBigelow_1phase",
  inherit = ContinuousElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("max_time", "D_R", "z", "temp_ref",
                                       "logN0", "temp_start", "temp_end"),
                       input_types = list(max_time = "continuous",
                                          D_R = "continuous",
                                          z = "continuous",
                                          temp_ref = "continuous",
                                          logN0 = "continuous",
                                          temp_start = "continuous",
                                          temp_end = "continuous"
                                          ),
                       units = units,
                       element_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("Discrete predictions not (yet) implemented for dynamic elements")

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_ramp))

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
          dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_ramp))

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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

  times <- seq(0, t2, length=100)

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
#' A risk element describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 9 inputs: t1, t2, D_R, z, temp_ref, logN0, temp0, temp1, temp2,
#'
#' @export
#'
DynamicBigelow_2phase <- R6::R6Class(
  classname = "DynamicBigelow_2phase",
  inherit = ContinuousElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("t1", "t2", "D_R", "z", "temp_ref",
                                       "logN0", "temp0", "temp1", "temp2"),
                       input_types = list(t1 = "continuous",
                                          t2 = "continuous",
                                          D_R = "continuous",
                                          z = "continuous",
                                          temp_ref = "continuous",
                                          logN0 = "continuous",
                                          temp0 = "continuous",
                                          temp1 = "continuous",
                                          temp2 = "continuous"
                       ),
                       units = units,
                       element_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("Discrete predictions not (yet) implemented for dynamic elements")

    }

  ),

  private = list(

    update_output = function(niter) {

      # browser()

      sims <- self$simulations %>%
          dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_biphasic))

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
          dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_biphasic))

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

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

  times <- seq(0, t3, length=100)

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
#' A risk element describing log-linear inactivation under dynamic conditions considering
#' a heating (or cooling) profile. It has 11 inputs: t1, t2, t3, D_R, z, temp_ref, logN0,
#' temp0, temp1, temp2, temp3.
#'
#' @export
#'
DynamicBigelow_3phase <- R6::R6Class(
  classname = "DynamicBigelow_3phase",
  inherit = ContinuousElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("t1", "t2", "t3", "D_R", "z", "temp_ref",
                                       "logN0", "temp0", "temp1", "temp2", "temp3"),
                       input_types = list(t1 = "continuous",
                                          t2 = "continuous",
                                          t3 = "continuous",
                                          D_R = "continuous",
                                          z = "continuous",
                                          temp_ref = "continuous",
                                          logN0 = "continuous",
                                          temp0 = "continuous",
                                          temp1 = "continuous",
                                          temp2 = "continuous",
                                          temp3 = "continuous"
                       ),
                       units = units,
                       element_type = "inactivation",
                       output_var = "logN",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      stop("Discrete predictions not (yet) implemented for dynamic elements")

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_triphasic))

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(logN = purrr::pmap_dbl(., predict_Bigelow_triphasic))

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)


