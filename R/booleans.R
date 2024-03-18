
#' @title ElementGreaterThan Class
#'
#' @description
#' An element that checks if the output of one element is greater than the output of
#' another ones
#'
#'
#' @export
#'
ElementGreaterThan <- R6::R6Class(
  classname = "ElementGreaterThan",
  inherit = RiskElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() > self$depends_on$a$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x > a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x > a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementLowerThan Class
#'
#' @description
#' An element that checks if the output of one element is lower than the output of
#' another ones
#'
#'
#' @export
#'
ElementLowerThan <- R6::R6Class(
  classname = "ElementLowerThan",
  inherit = RiskElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() < self$depends_on$a$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x < a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x < a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementGreaterEqualThan Class
#'
#' @description
#' An element that checks if the output of one element is greater or equal than the output of
#' another one
#'
#' @export
#'
ElementGreaterEqualThan <- R6::R6Class(
  classname = "ElementGreaterEqualThan",
  inherit = RiskElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() >= self$depends_on$a$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x >= a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x >= a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementLowerEqualThan Class
#'
#' @description
#' An element that checks if the output of one element is lower or equal than the output of
#' another one
#'
#' @export
#'
ElementLowerEqualThan <- R6::R6Class(
  classname = "ElementLowerEqualThan",
  inherit = RiskElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() <= self$depends_on$a$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x <= a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x <= a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementEqualTo Class
#'
#' @description
#' An element that checks if the output of one element is equal to the output of
#' another one
#'
#' @export
#'
ElementEqualTo <- R6::R6Class(
  classname = "ElementEqualTo",
  inherit = RiskElement,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param name A character defining the name for the element
    #' @param units A character vector of units for each input
    #'
    #' @return A new instance of the element
    #'
    initialize = function(name,
                          units = NA
                          ) {

      super$initialize(name,
                       input_names = c("a", "x"),
                       input_types = list(a = "continuous",
                                          x = "continuous"),
                       units = units,
                       element_type = "boolean",
                       output_var = "y",
                       output_unit = NA,
                       level = 0)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$x$point_estimate() == self$depends_on$a$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          y = x == a
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          y = x == a
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

