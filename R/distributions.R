
#' A module simulating a normal distribution
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
Normal <- R6::R6Class(
  classname = "Normal",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("mu", "sigma"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        mu = self$depends_on$mu$simulate(niter),
        sd = self$depends_on$sigma$simulate(niter)
        ) %>%
        dplyr::mutate(
          x = rnorm(niter, mu, sd)
          )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a log-normal distribution (in e-scale)
#'
#' @export
#'
LnNormal <- R6::R6Class(
  classname = "LnNormal",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("mu_ln", "sigma_ln"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        mu = self$depends_on$mu_ln$simulate(niter),
        sd = self$depends_on$sigma_ln$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rlnorm(niter, mu, sd)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a log-normal distribution (in 10-scale)
#'
#' @export
#'
LogNormal <- R6::R6Class(
  classname = "LogNormal",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("mu_log10", "sigma_log10"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        mu = self$depends_on$mu_log10$simulate(niter),
        sd = self$depends_on$sigma_log10$simulate(niter)
      ) %>%
        dplyr::mutate(
          y = rnorm(niter, mu, sd),
          x = 10^y
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Weibull distribution
#'
#' @export
#'
Weibull <- R6::R6Class(
  classname = "Weibull",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        shape = self$depends_on$shape$simulate(niter),
        scale = self$depends_on$scale$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rweibull(niter, shape, scale)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Beta distribution
#'
#' @export
#'
Beta <- R6::R6Class(
  classname = "Beta",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("shape1", "shape2"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        shape1 = self$depends_on$shape1$simulate(niter),
        shape2 = self$depends_on$shape2$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rbeta(niter, shape1, shape2)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Gamma distribution
#'
#' @export
#'
Gamma <- R6::R6Class(
  classname = "Gamma",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        shape = self$depends_on$shape$simulate(niter),
        scale = self$depends_on$scale$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rgamma(niter, shape, scale = scale)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating an exponential distribution
#'
#' @export
#'
Exponential <- R6::R6Class(
  classname = "Exponential",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("rate"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        rate = self$depends_on$rate$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = rexp(niter, rate)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a (continuous) Uniform distribution
#'
#' @export
#'
Uniform <- R6::R6Class(
  classname = "Uniform",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("min", "max"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        min = self$depends_on$min$simulate(niter),
        max = self$depends_on$max$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = runif(niter, min, max)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Triangular distribution
#'
#' @export
#'
Triangular <- R6::R6Class(
  classname = "Triangular",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b", "c"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter),
        c = self$depends_on$c$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter, a, b, c)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a symmetrical Triangular distribution
#'
#' @export
#'
TriangularSym <- R6::R6Class(
  classname = "TriangularSym",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rtriang(niter, a, b)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Pareto distribution
#'
#' @export
#'
Pareto <- R6::R6Class(
  classname = "Pareto",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        a = self$depends_on$a$simulate(niter),
        b = self$depends_on$b$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = extraDistr::rpareto(niter, a, b)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
    }
  )

)

#' A module simulating a Pert distribution
#'
#' @export
#'
Pert <- R6::R6Class(
  classname = "Pert",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("min", "mode", "max", "shape"),
                       units = units,
                       module_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit)

    },

    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        min = self$depends_on$min$simulate(niter),
        mode = self$depends_on$mode$simulate(niter),
        max = self$depends_on$max$simulate(niter),
        shape = self$depends_on$shape$simulate(niter)
      ) %>%
        dplyr::mutate(
          x = mc2d::rpert(niter, min, mode, max, shape)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$x
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
# logN0$simulate(2000) %>% hist()
# logN0$simulations
# logN0$get_output()
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
# norm1$simulate(100) %>%
#   hist()
#
# small_sd <- Constant$new("small sd", 0.3)
#
# norm2 <- Normal$new("Normal 2")$
#   map_input("mu", norm1)$
#   map_input("sigma", small_sd)
#
# norm2$simulate(1000) %>% hist()
# norm2$simulations
# norm2$get_output()
#
# LnNormal$new("test")$
#   map_input("mu_ln", Constant$new("aa", 4))$
#   map_input("sigma_ln", Constant$new("bb", .2))$
#   simulate(1000) %>% hist()
#
# LogNormal$new("test")$
#   map_input("mu_log10", Constant$new("aa", 4))$
#   map_input("sigma_log10", Constant$new("bb", .1))$
#   simulate(1000) %>% {log10(.)} %>% hist()
#
# Weibull$new("")$
#   map_input("shape",
#             Normal$new("aa")$
#               map_input("mu", Constant$new("", 3))$
#               map_input("sigma", Constant$new("", .1))
#             )$
#   map_input("scale", Constant$new("bb", 3))$
#   simulate(1000) %>% hist()
#
# Beta$new("")$
#   map_input("shape1", Constant$new("", 3))$
#   map_input("shape2", Constant$new("", 4))$
#   simulate(1000) %>% hist()
#
# Gamma$new("")$
#   map_input("shape", Constant$new("", 1))$
#   map_input("scale", Constant$new("as", 3))$
#   simulate(1000) %>% hist()
#
# aa <- Gamma$new("")$
#   map_input("shape", Constant$new("", 1))$
#   map_input("scale", Constant$new("as", 3))
#
# Exponential$new("")$
#   # map_input("rate", Constant$new("", 3))$
#   # simulate(1000)
#   map_input("rate",aa)$
#   simulate(1000) %>% hist()
#
# Uniform$new("")$
#   map_input("min", Constant$new("", 2))$
#   map_input("max", Constant$new("", 4))$
#   simulate(1000) %>% hist()
#
# Triangular$new("")$
#   map_input("a", Constant$new("", 0))$
#   map_input("b", Constant$new("", 2))$
#   map_input("c", Constant$new("", 1.5))$
#   simulate(1000) %>%
#   hist()
#
# TriangularSym$new("")$
  # map_input("a", Constant$new("", 0))$
  # map_input("b", Constant$new("", 2))$
  # simulate(1000) %>%
  # hist()
#
# Pareto$new("")$
#   map_input("a", Constant$new("", 2))$
#   map_input("b", Constant$new("", .5))$
#   simulate(1000) %>%
#   hist()
#
# Pert$new("")$
#   map_input("min", Constant$new("", 0))$
#   map_input("mode", Constant$new("", 3))$
#   map_input("max", Constant$new("", 4))$
#   map_input("shape", Constant$new("", 8))$
#   simulate(1000) %>% hist()















