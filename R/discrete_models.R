
#' A element describing exponential growth as a discrete (negbinomial) process
#'
#' This describes a birth process (Yule's process)
#'
#' @export
#'
DiscreteGrowth <- R6::R6Class(
  classname = "DiscreteGrowth",
  inherit = DiscreteElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("N0", "time", "mu"),
                       input_types = list(N0 = "discrete",
                                          time = "continuous",
                                          mu = "continuous"),
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
      increase <- self$depends_on$time$point_estimate() * self$depends_on$mu$point_estimate()
      self$depends_on$N0$point_estimate() + increase
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          p = 10^(-mu*time),
          N = N0 + rnbinom(niter, N0, p)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          p = 10^(-mu*time),
          N = N0 + rnbinom(niter, N0, p)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A element describing exponential growth as a discrete (negbinomial) process with Nmax
#'
#' This describes a birth process (Yule's process) with Nmax
#'
#' @export
#'
DiscreteGrowthNmax <- R6::R6Class(
  classname = "DiscreteGrowthNmax",
  inherit = DiscreteElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("N0", "time", "mu", "Nmax"),
                       input_types = list(N0 = "discrete",
                                          time = "continuous",
                                          mu = "continuous",
                                          Nmax = "discrete"),
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
      increase <- self$depends_on$time$point_estimate() * self$depends_on$mu$point_estimate()
      N <- self$depends_on$N0$point_estimate() + increase
      min(c(self$depends_on$Nmax$point_estimate(), N))
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          p = 10^(-mu*time),
          N = N0 + rnbinom(niter, N0, p)
        ) %>%
        dplyr::mutate(
          N = ifelse(N > Nmax, Nmax, N)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          p = 10^(-mu*time),
          N = N0 + rnbinom(niter, N0, p)
        )  %>%
        dplyr::mutate(
          N = ifelse(N > Nmax, Nmax, N)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)
