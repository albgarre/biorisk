
#' @title Normal Class
#'
#' @description
#' An element for a normal distribution
#'
#' @importFrom tibble tibble
#'
#' @export
#'
Normal <- R6::R6Class(
  classname = "Normal",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu", "sigma"),
                       input_types = list(mu = "continuous",
                                          sigma = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$mu$point_estimate()
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

#' @title LnNormal Class
#'
#' @description
#' An element for a log-normal distribution (base e)
#'
#' @export
#'
LnNormal <- R6::R6Class(
  classname = "LnNormal",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu_ln", "sigma_ln"),
                       input_types = list(mu_ln = "continuous",
                                          sigma_ln = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    point_estimate = function() {
      exp(self$depends_on$mu_ln$point_estimate())
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

#' @title LogNormal Class
#'
#' @description
#' An element for a log-normal distribution (base 10)
#'
#' @export
#'
LogNormal <- R6::R6Class(
  classname = "LogNormal",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu_log10", "sigma_log10"),
                       input_types = list(mu_log10 = "continuous",
                                          sigma_log10 = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    point_estimate = function() {
      10^(self$depends_on$mu_log10$point_estimate())
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

#' @title Weibull Class
#'
#' @description
#' An element for a Weibull distribution
#'
#' @export
#'
Weibull <- R6::R6Class(
  classname = "Weibull",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       input_types = list(shape = "continuous",
                                          scale = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    point_estimate = function() {
      self$depends_on$scale$point_estimate()*log(2)^(1/self$depends_on$shape$point_estimate())
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

#' @title Beta Class
#'
#' @description
#' An element for a beta distribution
#'
#' @export
#'
Beta <- R6::R6Class(
  classname = "Beta",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape1", "shape2"),
                       input_types = list(shape1 = "continuous",
                                          shape2 = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      a <- self$depends_on$shape1$point_estimate()
      b <- self$depends_on$shape2$point_estimate()

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

#' @title Gamma Class
#'
#' @description
#' An element for a gamma distribution
#'
#' @export
#'
Gamma <- R6::R6Class(
  classname = "Gamma",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("shape", "scale"),
                       input_types = list(shape = "continuous",
                                          scale = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      self$depends_on$shape$point_estimate() * self$depends_on$scale$point_estimate()

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

#' @title Exponential Class
#'
#' @description
#' An element for a exponential distribution
#'
#' @export
#'
Exponential <- R6::R6Class(
  classname = "Exponential",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("rate"),
                       input_types = list(rate = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the median
    #'
    point_estimate = function() {

      log(2)/self$depends_on$rate$point_estimate()

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

#' @title Uniform Class
#'
#' @description
#' An element for a uniform distribution
#'
#' @export
#'
Uniform <- R6::R6Class(
  classname = "Uniform",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("min", "max"),
                       input_types = list(min = "continuous",
                                          max = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      (self$depends_on$min$point_estimate() * self$depends_on$max$point_estimate())/2

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

#' @title Triangular Class
#'
#' @description
#' An element for a triangular distribution
#'
#' @export
#'
Triangular <- R6::R6Class(
  classname = "Triangular",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
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
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      (self$depends_on$a$point_estimate() + self$depends_on$b$point_estimate() + self$depends_on$c$point_estimate() )/3

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

#' @title TriangularSym Class
#'
#' @description
#' An element for a symmetrical triangular distribution
#'
#' @export
#'
TriangularSym <- R6::R6Class(
  classname = "TriangularSym",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       input_types = list(a = "continuous",
                                          b = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {

      (self$depends_on$a$point_estimate() * self$depends_on$b$point_estimate())/2

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

#' @title Pareto Class
#'
#' @description
#' An element for a Pareto distribution
#'
#' @export
#'
Pareto <- R6::R6Class(
  classname = "Pareto",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("a", "b"),
                       input_types = list(a = "continuous",
                                          b = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the model
    #'
    point_estimate = function() {

      self$depends_on$b$point_estimate()

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

#' @title Pert Class
#'
#' @description
#' An element for a Pert distribution
#'
#' @export
#'
Pert <- R6::R6Class(
  classname = "Pert",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
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
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level =  level)

    },

    #' @description
    #' Returns the mode
    #'
    point_estimate = function() {

      self$depends_on$mode$point_estimate()

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

#' @title LogisticDistr Class
#'
#' @description
#' An element for a Logistic distribution
#'
#' @export
#'
LogisticDistr <- R6::R6Class(
  classname = "LogisticDistr",
  inherit = ContinuousElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #' @param output_unit A character with the unit of the output
    #' @param level Level of the distribution (for 2D Monte Carlo). By default, `0`
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("location", "scale"),
                       input_types = list(location = "continuous",
                                          scale = "continuous"),
                       units = units,
                       element_type = "distribution",
                       output_var = "x",
                       output_unit = output_unit,
                       level =  level)

    },

    #' @description
    #' Returns the mode
    #'
    point_estimate = function() {

      self$depends_on$location$point_estimate()

    }
  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = rlogis(niter, location, scale)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = rlogis(niter, location, scale)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)





