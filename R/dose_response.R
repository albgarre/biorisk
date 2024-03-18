
#' @title DoseResponse_Exponential Class
#'
#' @description
#' An element for the exponential dose-response model
#'
#'
#' @export
#'
DoseResponse_Exponential <- R6::R6Class(
  classname = "DoseResponse_Exponential",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("r", "dose"),
                       input_types = list(r = "continuous",
                                          dose = "discrete"),
                       units = units,
                       element_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      r <- self$depends_on$r$point_estimate()
      dose <- self$depends_on$dose$point_estimate()

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

#' @title DoseResponse_BetaPoisson Class
#'
#' @description
#' An element for the beta-Poisson dose-response model
#'
#'
#' @export
#'
DoseResponse_BetaPoisson <- R6::R6Class(
  classname = "DoseResponse_BetaPoisson",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      #' @description
      #' Creates a new instance of this [R6][R6::R6Class] class.
      #'
      #' @param name A character defining the name for the element
      #' @param units A character vector of units for each input
      #' @param output_unit A character with the unit of the output
      #'
      #' @return A new instance of the element
      #'
      super$initialize(name,
                       input_names = c("beta", "alpha", "dose"),
                       input_types = list(beta = "continuous",
                                          alpha = "continuous",
                                          dose = "discrete"),
                       units = units,
                       element_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      dose <- self$depends_on$dose$point_estimate()
      alpha <- self$depends_on$alpha$point_estimate()
      beta <- self$depends_on$beta$point_estimate()

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

#' @title Concentration2Dose Class
#'
#' @description
#' An element that acts as a helper to convert from concentration to dose
#'
#'
#' @export
#'
Concentration2Dose <- R6::R6Class(
  classname = "Concentration2Dose",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("logN", "size"),
                       input_types = list(logN = "continuous",
                                          size = "continuous"),
                       units = units,
                       element_type = "simple",
                       output_var = "dose",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      10^self$depends_on$logN$point_estimate() * self$depends_on$size$point_estimate()

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

#' @title Concentration2Dose_continuous Class
#'
#' @description
#' An element that acts as a helper to convert from concentration to dose
#'
#'
#' @export
#'
Concentration2Dose_continuous <- R6::R6Class(
  classname = "Concentration2Dose_continuous",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("logN", "size"),
                       input_types = list(logN = "continuous",
                                          size = "continuous"),
                       units = units,
                       element_type = "simple",
                       output_var = "dose",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      10^self$depends_on$logN$point_estimate() * self$depends_on$size$point_estimate()

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

#' @title Pill2Cases_1 Class
#'
#' @description
#' An element that acts as a helper to convert from probability of illness to number of cases
#'
#'
#' @export
#'
Pill2Cases_1 <- R6::R6Class(
  classname = "Pill2Cases_1",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("Pill"),
                       input_types = list(Pill = "continuous"),
                       units = units,
                       element_type = "simple",
                       output_var = "case",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      self$depends_on$Pill$point_estimate()

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

#' @title Pill2Cases_N Class
#'
#' @description
#' An element that acts as a helper to convert from probability of illness to number of cases
#'
#'
#' @export
#'
Pill2Cases_N <- R6::R6Class(
  classname = "Pill2Cases_N",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("Pill", "servings"),
                       input_types = list(Pill = "continuous",
                                          servings = "discrete"),
                       units = units,
                       element_type = "simple",
                       output_var = "case",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      self$depends_on$Pill$point_estimate() * self$depends_on$servings$point_estimate()

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









