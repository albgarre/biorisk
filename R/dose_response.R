
#' R6 class describing the exponential dose-response model
#'
#' @details
#' A risk module describing the exponential dose-response model. It has 2 inputs: r, dose
#'
#' @export
#'
DoseResponse_Exponential <- R6::R6Class(
  classname = "DoseResponse_Exponential",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("r", "dose"),
                       units = units,
                       module_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      r <- self$depends_on$r$discrete_prediction()
      dose <- self$depends_on$dose$discrete_prediction()

      1 - exp(-r*dose)

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          P_ill = 1 - exp(-r*dose)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          P_ill = 1 - exp(-r*dose)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the beta-Poisson dose-response model
#'
#' @details
#' A risk module describing the beta-Poisson dose-response model. It has 3 inputs: beta, alpha, dose
#'
#' @export
#'
DoseResponse_BetaPoisson <- R6::R6Class(
  classname = "DoseResponse_BetaPoisson",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("beta", "alpha", "dose"),
                       units = units,
                       module_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      dose <- self$depends_on$dose$discrete_prediction()
      alpha <- self$depends_on$alpha$discrete_prediction()
      beta <- self$depends_on$beta$discrete_prediction()

      1 - (1 + dose/beta)^(-alpha)

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          P_ill = 1 - (1 + dose/beta)^(-alpha)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          P_ill = 1 - (1 + dose/beta)^(-alpha)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)












