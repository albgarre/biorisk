
#' A element to describe partition as a Poisson process
#'
#' @export
#'
PartitionPoisson <- R6::R6Class(
  classname = "PartitionPoisson",
  inherit = DiscreteElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("logN", "volume"),
                       input_types = list(logN = "continuous",
                                          volume = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "N",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      10^self$depends_on$logN$point_estimate() * self$depends_on$volume$point_estimate()
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          lambda = 10^logN*volume,
          N = rpois(niter, lambda)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          lambda = 10^logN*volume,
          N = rpois(niter, lambda)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A element to describe partition as a binomial process
#'
#' @export
#'
PartitionBinomial <- R6::R6Class(
  classname = "PartitionBinomial",
  inherit = DiscreteElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("N0", "volume0", "volume1"),
                       input_types = list(N0 = "discrete",
                                          volume0 = "discrete",
                                          volume1 = "discrete"),
                       units = units,
                       element_type = "distribution",
                       output_var = "N",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$N0$point_estimate() * self$depends_on$volume1$point_estimate() / self$depends_on$volume0$point_estimate()
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          p = volume1/volume0,
          N = rbinom(niter, N0, p)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          p = volume1/volume0,
          N = rbinom(niter, N0, p)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)
