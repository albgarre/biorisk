
#' A module simulating a normal distribution
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
Normal <- R6::R6Class(
  classname = "Normal",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu", "sigma"),
                       input_types = list(mu = "continuous",
                                          sigma = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      self$depends_on$mu$discrete_prediction()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rnorm(niter, mu, sigma)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rnorm(niter0, mu, sigma)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a log-normal distribution (in e-scale)
#'
#' @export
#'
LnNormal <- R6::R6Class(
  classname = "LnNormal",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu_ln", "sigma_ln"),
                       input_types = list(mu_ln = "continuous",
                                          sigma_ln = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    discrete_prediction = function() {
      exp(self$depends_on$mu_ln$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rlnorm(niter, mu_ln, sigma_ln)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rlnorm(niter0, mu_ln, sigma_ln)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a log-normal distribution (in 10-scale)
#'
#' @export
#'
LogNormal <- R6::R6Class(
  classname = "LogNormal",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu_log10", "sigma_log10"),
                       input_types = list(mu_log10 = "continuous",
                                          sigma_log10 = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    discrete_prediction = function() {
      10^(self$depends_on$mu_log10$discrete_prediction())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = rnorm(niter, mu_log10, sigma_log10),
          x = 10^y
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = rnorm(niter0, mu_log10, sigma_log10),
          x = 10^y
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Weibull distribution
#'
#' @export
#'
Weibull <- R6::R6Class(
  classname = "Weibull",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       input_types = list(shape = "continuous",
                                          scale = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    discrete_prediction = function() {
      self$depends_on$scale$discrete_prediction()*log(2)^(1/self$depends_on$shape$discrete_prediction())
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rweibull(niter, shape, scale)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rweibull(niter0, shape, scale)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Beta distribution
#'
#' @export
#'
Beta <- R6::R6Class(
  classname = "Beta",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape1", "shape2"),
                       input_types = list(shape1 = "continuous",
                                          shape2 = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {
      a <- self$depends_on$shape1$discrete_prediction()
      b <- self$depends_on$shape2$discrete_prediction()

      a/(a+b)
    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rbeta(niter, shape1, shape2)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rbeta(niter0, shape1, shape2)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Gamma distribution
#'
#' @export
#'
Gamma <- R6::R6Class(
  classname = "Gamma",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       input_types = list(shape = "continuous",
                                          scale = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      self$depends_on$shape$discrete_prediction() * self$depends_on$scale$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rgamma(niter, shape, scale = scale)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rgamma(niter0, shape, scale = scale)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating an exponential distribution
#'
#' @export
#'
Exponential <- R6::R6Class(
  classname = "Exponential",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("rate"),
                       input_types = list(rate = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    discrete_prediction = function() {

      log(2)/self$depends_on$rate$discrete_prediction()

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rexp(niter, rate)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rexp(niter0, rate)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a (continuous) Uniform distribution
#'
#' @export
#'
Uniform <- R6::R6Class(
  classname = "Uniform",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("min", "max"),
                       input_types = list(min = "continuous",
                                          max = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      (self$depends_on$min$discrete_prediction() * self$depends_on$max$discrete_prediction())/2

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = runif(niter, min, max)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = runif(niter0, min, max)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Triangular distribution
#'
#' @export
#'
Triangular <- R6::R6Class(
  classname = "Triangular",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a", "b", "c"),
                       input_types = list(a = "continuous",
                                          b = "continuous",
                                          c = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      (self$depends_on$a$discrete_prediction() + self$depends_on$b$discrete_prediction() + self$depends_on$c$discrete_prediction() )/3

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter, a, b, c)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter0, a, b, c)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a symmetrical Triangular distribution
#'
#' @export
#'
TriangularSym <- R6::R6Class(
  classname = "TriangularSym",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       input_types = list(a = "continuous",
                                          b = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    discrete_prediction = function() {

      (self$depends_on$a$discrete_prediction() * self$depends_on$b$discrete_prediction())/2

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter, a, b)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter0, a, b)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' A module simulating a Pareto distribution
#'
#' @export
#'
Pareto <- R6::R6Class(
  classname = "Pareto",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       input_types = list(a = "continuous",
                                          b = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the model
    #'
    discrete_prediction = function() {

      self$depends_on$b$discrete_prediction()

    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = extraDistr::rpareto(niter, a, b)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = extraDistr::rpareto(niter0, a, b)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }
  )
)

#' A module simulating a Pert distribution
#'
#' @export
#'
Pert <- R6::R6Class(
  classname = "Pert",
  inherit = ContinuousModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("min", "mode", "max", "shape"),
                       input_types = list(min = "continuous",
                                          mode = "continuous",
                                          max = "continuous",
                                          shape = "continuous"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level =  level)

    },

    #' @description
    #' Returns the mode
    #'
    discrete_prediction = function() {

      self$depends_on$mode$discrete_prediction()

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = mc2d::rpert(niter, min, mode, max, shape)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = mc2d::rpert(niter, min, mode, max, shape)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

# ## tests
#
# my_mean <- Constant$new("mean", 3)
# my_sd <- Constant$new("sd", .3)
#
# # my_mean$simulate(10)
# # my_sd$simulate(20)
#
# logN0 <- Normal$new("Concentracion inicial")$
#   map_input("mu", my_mean)$
#   map_input("sigma", my_sd)
# # aa$map_input("mu", my_mean)
# # aa$map_input("sigma", my_sd)
#
# # aa$depends_on
# # my_mean$depended_by
#
# logN0$simulate(2000)
# logN0$histogram()
#
# ## test 2
#
# big_mean <- Constant$new("big mean", 4)
# big_sd <- Constant$new("big sd", 1)
#
# norm1 <- Normal$new("Normal 1")$
#   map_input("mu", big_mean)$
#   map_input("sigma", big_sd)
#
# norm1$simulate(100)
# norm1$histogram()
#
# small_sd <- Constant$new("small sd", 0.3)
#
# norm2 <- Normal$new("Normal 2")$
#   map_input("mu", norm1)$
#   map_input("sigma", small_sd)
#
# norm2$simulate(1000)
# norm2$density_plot()
#
# aa <- LnNormal$new("test")$
#   map_input("mu_ln", Constant$new("aa", 4))$
#   map_input("sigma_ln", Constant$new("bb", .2))
#
# aa$simulate(100)
# aa$histogram()
#
# aa <- LogNormal$new("test")$
#   map_input("mu_log10", Constant$new("aa", 4))$
#   map_input("sigma_log10", Constant$new("bb", .1))
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- Weibull$new("")$
#   map_input("shape",
#             Normal$new("aa")$
#               map_input("mu", Constant$new("", 3))$
#               map_input("sigma", Constant$new("", .1))
#             )$
#   map_input("scale", Constant$new("bb", 3))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- Beta$new("aa")$
#   map_input("shape1", Constant$new("", 3))$
#   map_input("shape2", Constant$new("", 4))
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- Gamma$new("")$
#   map_input("shape", Constant$new("", 1))$
#   map_input("scale", Constant$new("as", 3))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- Gamma$new("")$
#   map_input("shape", Constant$new("", 1))$
#   map_input("scale", Constant$new("as", 3))
#
# aa <- Exponential$new("")$
#   # map_input("rate", Constant$new("", 3))$
#   # simulate(1000)
#   map_input("rate",aa)
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- Uniform$new("")$
#   map_input("min", Constant$new("", 2))$
#   map_input("max", Constant$new("", 4))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- Triangular$new("")$
#   map_input("a", Constant$new("", 0))$
#   map_input("b", Constant$new("", 2))$
#   map_input("c", Constant$new("", 1.5))
#
# aa$simulate(1000)
# aa$histogram()
#
# aa <- TriangularSym$new("")$
# map_input("a", Constant$new("", 0))$
# map_input("b", Constant$new("", 2))
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- Pareto$new("")$
#   map_input("a", Constant$new("", 2))$
#   map_input("b", Constant$new("", .5))
#
# aa$simulate(1000)
# aa$density_plot()
#
# aa <- Pert$new("")$
#   map_input("min", Constant$new("", 0))$
#   map_input("mode", Constant$new("", 3))$
#   map_input("max", Constant$new("", 4))$
#   map_input("shape", Constant$new("", 8))
#
# aa$simulate(1000)
# aa$histogram()























