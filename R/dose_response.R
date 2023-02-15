
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
                       input_types = list(r = "continuous",
                                          dose = "discrete"),
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
                       input_types = list(beta = "continuous",
                                          alpha = "continuous",
                                          dose = "discrete"),
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


#' Helper-module to convert from concentration to dose
#'
#'
#' @export
#'
Concentration2Dose <- R6::R6Class(
  classname = "Concentration2Dose",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("logN", "size"),
                       input_types = list(logN = "continuous",
                                          size = "continuous"),
                       units = units,
                       module_type = "simple",
                       output_var = "dose",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      10^self$depends_on$logN$discrete_prediction() * self$depends_on$size$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(.,
                      expected_dose = 10^logN * size,
                      dose = rpois(nrow(.), expected_dose)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          expected_dose = 10^logN * size,
          dose = rpois(nrow(.), expected_dose)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)


#' Helper-module to convert from concentration to dose
#'
#'
#' @export
#'
Concentration2Dose_continuous <- R6::R6Class(
  classname = "Concentration2Dose_continuous",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("logN", "size"),
                       input_types = list(logN = "continuous",
                                          size = "continuous"),
                       units = units,
                       module_type = "simple",
                       output_var = "dose",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      10^self$depends_on$logN$discrete_prediction() * self$depends_on$size$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(.,
                      dose = 10^logN * size
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          dose = 10^logN * size
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)


#' Helper-module to convert from Pill to number of cases (1 per simulation)
#'
#'
#' @export
#'
Pill2Cases_1 <- R6::R6Class(
  classname = "Pill2Cases_1",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("Pill"),
                       input_types = list(Pill = "continuous"),
                       units = units,
                       module_type = "simple",
                       output_var = "case",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      self$depends_on$Pill$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(.,
          case = rbinom(nrow(.), 1, Pill)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(.,
          case = rbinom(nrow(.), 1, Pill)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' Helper-module to convert from Pill to number of cases (N per simulation)
#'
#'
#' @export
#'
Pill2Cases_N <- R6::R6Class(
  classname = "Pill2Cases_N",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("Pill", "servings"),
                       input_types = list(Pill = "continuous",
                                          servings = "discrete"),
                       units = units,
                       module_type = "simple",
                       output_var = "case",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      self$depends_on$Pill$discrete_prediction() * self$depends_on$servings$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(.,
          case = rbinom(nrow(.), servings, Pill)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(.,
          case = rbinom(nrow(.), servings, Pill)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)









