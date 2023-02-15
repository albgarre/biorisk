
#' A module describing exponential growth as a discrete (negbinomial) process
#'
#' This describes a birth process (Yule's process)
#'
#' @export
#'
DiscreteGrowth <- R6::R6Class(
  classname = "DiscreteGrowth",
  inherit = DiscreteModule,
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
                       module_type = "distribution",
                       output_var = "N",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      increase <- self$depends_on$time$discrete_prediction() * self$depends_on$mu$discrete_prediction()
      self$depends_on$N0$discrete_prediction() + increase
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

#' A module describing exponential growth as a discrete (negbinomial) process with Nmax
#'
#' This describes a birth process (Yule's process) with Nmax
#'
#' @export
#'
DiscreteGrowthNmax <- R6::R6Class(
  classname = "DiscreteGrowthNmax",
  inherit = DiscreteModule,
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
                       module_type = "distribution",
                       output_var = "N",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      increase <- self$depends_on$time$discrete_prediction() * self$depends_on$mu$discrete_prediction()
      N <- self$depends_on$N0$discrete_prediction() + increase
      min(c(self$depends_on$Nmax$discrete_prediction(), N))
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
