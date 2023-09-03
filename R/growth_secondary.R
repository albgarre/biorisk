
#' R6 class describing the secondary Ratkowsky model (suboptimal)
#'
#' @details
#' A risk module describing the (suboptimal) Ratkowsky model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
Ratkowsky_model <- R6::R6Class(
  classname = "Ratkowsky_model",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature"),
                       input_types = list(b = "continuous",
                                          Tmin = "continuous",
                                          temperature = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      b <- self$depends_on$b$point_estimate()
      Tmin <- self$depends_on$Tmin$point_estimate()
      temperature <- self$depends_on$temperature$point_estimate()

      sq_mu <- b*(temperature - Tmin)

      ifelse(temperature < Tmin,
             0,
             sq_mu^2
             )
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = ifelse(temperature < Tmin, 0, sq_mu^2)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin),
          mu = ifelse(temperature < Tmin, 0, sq_mu^2)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the secondary Ratkowsky model (suboptimal) including error term
#'
#' @details
#' A risk module describing the (suboptimal) Ratkowsky model including an error term.
#' It has 4 inputs: b, Tmin, temperature, sigma.
#'
#' @export
#'
Ratkowsky_model_error <- R6::R6Class(
  classname = "Ratkowsky_model_error",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature", "sigma"),
                       input_types = list(b = "continuous",
                                          Tmin = "continuous",
                                          temperature = "continuous",
                                          sigma = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      b <- self$depends_on$b$point_estimate()
      Tmin <- self$depends_on$Tmin$point_estimate()
      temperature <- self$depends_on$temperature$point_estimate()

      sq_mu <- b*(temperature - Tmin)

      ifelse(temperature < Tmin,
             0,
             sq_mu^2
      )
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin),
          sq_mu = rnorm(n = nrow(.), mean = sq_mu_mean, sd = sigma),
          mu = ifelse(sq_mu < 0, 0, sq_mu^2)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin),
          sq_mu = rnorm(n = niter0, mean = sq_mu_mean, sd = sigma),
          mu = ifelse(sq_mu < 0, 0, sq_mu^2)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the secondary Full Ratkowsky model
#'
#' @details
#' A risk module describing the fullRatkowsky model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
FullRatkowsky_model <- R6::R6Class(
  classname = "FullRatkowsky_model",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature", "Tmax", "c"),
                       input_types = list(b = "continuous",
                                          Tmin = "continuous",
                                          temperature = "continuous",
                                          c = "continuous",
                                          Tmax = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      b <- self$depends_on$b$point_estimate()
      Tmin <- self$depends_on$Tmin$point_estimate()
      temperature <- self$depends_on$temperature$point_estimate()
      Tmax <- self$depends_on$Tmax$point_estimate()
      c <- self$depends_on$c$point_estimate()

      sq_mu <- b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax)))

      ifelse(
        between(temperature, Tmin, Tmax),
        sq_mu^2,
        0
      )

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))),
          mu = ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu^2,
            0
          )
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))),
          mu = ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu^2,
            0
          )
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)


#' R6 class describing the secondary Full Ratkowsky model
#'
#' @details
#' A risk module describing the fullRatkowsky model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
FullRatkowsky_model_error <- R6::R6Class(
  classname = "FullRatkowsky_model_error",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("b", "Tmin", "temperature", "Tmax", "c", "sigma"),
                       input_types = list(b = "continuous",
                                          Tmin = "continuous",
                                          temperature = "continuous",
                                          c = "continuous",
                                          Tmax = "continuous",
                                          sigma = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      b <- self$depends_on$b$point_estimate()
      Tmin <- self$depends_on$Tmin$point_estimate()
      temperature <- self$depends_on$temperature$point_estimate()
      Tmax <- self$depends_on$Tmax$point_estimate()
      c <- self$depends_on$c$point_estimate()

      sq_mu <- b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax)))

      ifelse(
        between(temperature, Tmin, Tmax),
        sq_mu^2,
        0
      )

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))),
          sq_mu = rnorm(n = niter, mean = sq_mu_mean, sd = sigma),
          ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu^2,
            0
          )
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu_mean = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))),
          sq_mu = rnorm(n = niter0, mean = sq_mu_mean, sd = sigma),
          ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu^2,
            0
          )
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the secondary Cardinal Parameter Model
#'
#' @details
#' A risk module describing the Cardinal Parameter Model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
CardinalParameterModel <- R6::R6Class(
  classname = "CardinalParameterModel",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("X", "Xmin", "Xopt", "Xmax", "n"),
                       input_types = list(X = "continuous",
                                          Xmin = "continuous",
                                          Xopt = "continuous",
                                          Xmax = "continuous",
                                          n = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "gamma",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      X <- self$depends_on$X$point_estimate()
      Xmin <- self$depends_on$Xmin$point_estimate()
      Xmax <- self$depends_on$Xmax$point_estimate()
      Xopt <- self$depends_on$Xopt$point_estimate()
      n <- self$depends_on$n$point_estimate()

      num <- (X-Xmax)*(X-Xmin)^n
      den <- (Xopt-Xmin)^(n-1)*( (Xopt-Xmin)*(X-Xopt) - (Xopt-Xmax)*((n-1)*Xopt + Xmin-n*X) )
      gamma <- num/den

      ifelse(
        between(X, Xmin, Xmax),
        gamma,
        0
      )

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          num = (X-Xmax)*(X-Xmin)^n,
          den = (Xopt-Xmin)^(n-1)*( (Xopt-Xmin)*(X-Xopt) - (Xopt-Xmax)*((n-1)*Xopt + Xmin-n*X) ),
          gamma = num/den
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          num = (X-Xmax)*(X-Xmin)^n,
          den = (Xopt-Xmin)^(n-1)*( (Xopt-Xmin)*(X-Xopt) - (Xopt-Xmax)*((n-1)*Xopt + Xmin-n*X) ),
          gamma = num/den
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the gamma coefficient using the Zwietering parameterization
#'
#' @details
#' A risk module describing the Cardinal Parameter Model. It has 3 inputs: b, Tmin, temperature.
#'
#' @export
#'
ZwieteringGamma <- R6::R6Class(
  classname = "ZwieteringGamma",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
                          ) {

      super$initialize(name,
                       input_names = c("X", "Xmin", "Xopt", "n"),
                       input_types = list(X = "continuous",
                                          Xmin = "continuous",
                                          Xopt = "continuous",
                                          n = "continuous"),
                       units = units,
                       module_type = "secondary",
                       output_var = "gamma",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      X <- self$depends_on$X$point_estimate()
      Xmin <- self$depends_on$Xmin$point_estimate()
      Xopt <- self$depends_on$Xopt$point_estimate()
      n <- self$depends_on$n$point_estimate()

      gamma <- ((X-Xmin)/(Xopt-Xmin))^n

      ifelse(
        X > Xmin,
        gamma,
        0
      )

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          gamma = ((X-Xmin)/(Xopt-Xmin))^n
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          gamma = ((X-Xmin)/(Xopt-Xmin))^n
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' R6 class describing the secondary Full Ratkowsky model
#'
#' @details
#' A risk module describing the fullRatkowsky model. It has 3 inputs: b, Tmin, Tmax, c,
#' pHmax, pHmin, temperature and pH.
#'
#' @export
#'
FullRatkowsky_pH_model <- R6::R6Class(
  classname = "FullRatkowsky_pH_model",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA
    ) {

      super$initialize(name,
                       input_names = c("b",
                                       "Tmin", "temperature", "Tmax", "c",
                                       "pHmax", "pHmin", "pH"
                                       ),
                       input_types = list(b = "continuous",
                                          Tmin = "continuous",
                                          temperature = "continuous",
                                          c = "continuous",
                                          Tmax = "continuous",
                                          pHmax = "continuous",
                                          pHmin = "continuous",
                                          pH = "continuous"
                                          ),
                       units = units,
                       module_type = "secondary",
                       output_var = "mu",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      b <- self$depends_on$b$point_estimate()

      Tmin <- self$depends_on$Tmin$point_estimate()
      temperature <- self$depends_on$temperature$point_estimate()
      Tmax <- self$depends_on$Tmax$point_estimate()
      c <- self$depends_on$c$point_estimate()

      pHmax <- self$depends_on$pHmax$point_estimate()
      pHmin <- self$depends_on$pHmin$point_estimate()
      pH <- self$depends_on$pH$point_estimate()

      sq_mu <- b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))) * (1-10^(pHmin - pH))*(1-10^(pH - pHmax))

      sq_mu <- ifelse(
        between(temperature, Tmin, Tmax),
        sq_mu,
        0
      )

      ifelse(
        between(pH, pHmin, pHmax),
        sq_mu^2,
        0
      )

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))) * (1-10^(pHmin - pH))*(1-10^(pH - pHmax))
        ) %>%
        mutate(
          sq_mu = ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu,
            0
          ),
          mu = ifelse(
            between(pH, pHmin, pHmax),
            sq_mu^2,
            0
          )
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          sq_mu = b*(temperature - Tmin)*(1 - exp(c*(temperature - Tmax))) * (1-10^(pHmin - pH))*(1-10^(pH - pHmax))
        ) %>%
        mutate(
          sq_mu = ifelse(
            between(temperature, Tmin, Tmax),
            sq_mu,
            0
          ),
          mu = ifelse(
            between(pH, pHmin, pHmax),
            sq_mu^2,
            0
          )
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)



####### Tests -----------------------
#
# library(tidyverse)
#
# mu <- Ratkowsky_model$new("aa")$
#   map_input("b",
#             Uniform$new("b")$
#               map_input("min", Constant$new("min_b", .1))$
#               map_input("max", Constant$new("max_b", .2))
#             )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#             )
#
# mu <- FullRatkowsky_model$new("aa")$
#   map_input("b",
#             Uniform$new("b")$
#               map_input("min", Constant$new("min_b", .1))$
#               map_input("max", Constant$new("max_b", .2))
#   )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#   )$
#   map_input("c", Constant$new("c", .1))$
#   map_input("Tmax", Constant$new("Tmax", 20))
#
# plot_model(mu)
# mu$simulate(1000)
# mu$density_plot()
# mu$point_estimate()

#
# mu_e <- Ratkowsky_model_error$new("aa")$
#   map_input("b",
#             Constant$new("b", .15)
#   )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Constant$new("T", 15)
#   )$
#   map_input("sigma", Constant$new("sigma", .5))
#
# plot_model(mu_e)
# mu_e$simulate(100)
# mu_e$density_plot()
#
# mu <- FullRatkowsky_model_error$new("aa")$
#   map_input("b",
#             Uniform$new("b")$
#               map_input("min", Constant$new("min_b", .1))$
#               map_input("max", Constant$new("max_b", .2))
#   )$
#   map_input("Tmin", Constant$new("Tmin", 0))$
#   map_input("temperature",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#   )$
#   map_input("c", Constant$new("c", .1))$
#   map_input("Tmax", Constant$new("Tmax", 20))$
#   map_input("sigma", Constant$new("sigma_mu", .2))
#
# plot_model(mu)
# mu$simulate(1000)
# mu$density_plot()
# mu$point_estimate()
#
#
# mu <- CardinalParameterModel$new("aa")$
#   map_input("Xmin", Constant$new("Tmin", 0))$
#   map_input("X",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#   )$
#   map_input("Xopt", Uniform$new("Xopt")$
#               map_input("min",
#                         Constant$new("Xopt_min", 20)
#                         )$
#               map_input("max",
#                         Constant$new("Xopt_max", 30)
#                         )
#             )$
#   map_input("Xmax", Constant$new("Xmax", 40))$
#   map_input("n", Constant$new("n", 2))
#
# plot_model(mu)
# mu$simulate(1000)
# mu$density_plot()
# mu$point_estimate()
#
# mu <- ZwieteringGamma$new("aa")$
#   map_input("Xmin", Constant$new("Tmin", 0))$
#   map_input("X",
#             Normal$new("temp")$
#               map_input("mu", Constant$new("mu", 15))$
#               map_input("sigma", Constant$new("sigma", 1))
#   )$
#   map_input("Xopt", Uniform$new("Xopt")$
#               map_input("min",
#                         Constant$new("Xopt_min", 20)
#                         )$
#               map_input("max",
#                         Constant$new("Xopt_max", 30)
#                         )
#             )$
#   map_input("n", Constant$new("n", 2))
#
# plot_model(mu)
# mu$simulate(1000)
# mu$density_plot()
# mu$point_estimate()


