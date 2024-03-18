
#' @title ElementPlus Class
#'
#' @description
#' An element that sums the output of two elements
#'
#'
#' @export
#'
ElementPlus <- R6::R6Class(
  classname = "ElementPlus",
  inherit = RiskElement,
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
                       input_names = c("a", "b"),
                       input_types = list(a = "any",
                                          b = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      type1 <- self$depends_on$a$get_output_type()
      type2 <- self$depends_on$b$get_output_type()

      if (type1 == "continuous" || type2 == "continuous") {
        "continuous"
      } else {
        "discrete"
      }
    },

    #' @description
    #' Returns the point estimate
    #'
    point_estimate = function() {
      self$depends_on$a$point_estimate() + self$depends_on$b$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a + b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a + b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementMinus Class
#'
#' @description
#' An element that substraction of the output of two elements
#'
#'
#' @export
#'
ElementMinus <- R6::R6Class(
  classname = "ElementMinus",
  inherit = RiskElement,
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
                       input_names = c("a", "b"),
                       input_types = list(a = "any",
                                          b = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      type1 <- self$depends_on$a$get_output_type()
      type2 <- self$depends_on$b$get_output_type()

      if (type1 == "continuous" || type2 == "continuous") {
        "continuous"
      } else {
        "discrete"
      }
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$a$point_estimate() - self$depends_on$b$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a - b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a - b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementTimes Class
#'
#' @description
#' An element that multiplies the output of two elements
#'
#'
#' @export
#'
ElementTimes <- R6::R6Class(
  classname = "ElementTimes",
  inherit = RiskElement,
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
                       input_names = c("a", "b"),
                       input_types = list(a = "any",
                                          b = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      type1 <- self$depends_on$a$get_output_type()
      type2 <- self$depends_on$b$get_output_type()

      if (type1 == "continuous" || type2 == "continuous") {
        "continuous"
      } else {
        "discrete"
      }
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$a$point_estimate() * self$depends_on$b$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a * b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a * b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementDivision Class
#'
#' @description
#' An element that divides the output of two elements
#'
#'
#' @export
#'
ElementDivision <- R6::R6Class(
  classname = "ElementDivision",
  inherit = RiskElement,
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
                       input_names = c("a", "b"),
                       input_types = list(a = "any",
                                          b = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      type1 <- self$depends_on$a$get_output_type()
      type2 <- self$depends_on$b$get_output_type()

      if (type1 == "continuous" || type2 == "continuous") {
        "continuous"
      } else {
        "discrete"
      }
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$a$point_estimate() / self$depends_on$b$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a / b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a / b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementPower Class
#'
#' @description
#' An element that powers the output of one element to the output of a second one
#'
#'
#' @export
#'
ElementPower <- R6::R6Class(
  classname = "ElementPower",
  inherit = RiskElement,
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
                       input_names = c("a", "b"),
                       input_types = list(a = "any",
                                          b = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      type1 <- self$depends_on$a$get_output_type()
      type2 <- self$depends_on$b$get_output_type()

      if (type1 == "continuous" || type2 == "continuous") {
        "continuous"
      } else {
        "discrete"
      }
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      self$depends_on$a$point_estimate() ^ self$depends_on$b$point_estimate()
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = a ^ b
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = a ^ b
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementSqrt Class
#'
#' @description
#' An element that calculates the square root of the output of one element
#'
#'
#' @export
#'
ElementSqrt <- R6::R6Class(
  classname = "ElementSqrt",
  inherit = RiskElement,
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
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      sqrt(self$depends_on$a$point_estimate())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = sqrt(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = sqrt(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementLog Class
#'
#' @description
#' An element that calculates the decimal logarithm of the output of one element
#'
#'
#' @export
#'
ElementLog <- R6::R6Class(
  classname = "ElementLog",
  inherit = RiskElement,
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
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      log10(self$depends_on$a$point_estimate())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = log10(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = log10(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementLn Class
#'
#' @description
#' An element that calculates the decimal logarithm of the output of one element
#'
#'
#' @export
#'
ElementLn <- R6::R6Class(
  classname = "ElementLn",
  inherit = RiskElement,
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
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      log(self$depends_on$a$point_estimate())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = log(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = log(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementExp Class
#'
#' @description
#' An element that calculates e to the power of the output of one element
#'
#'
#' @export
#'
ElementExp <- R6::R6Class(
  classname = "ElementExp",
  inherit = RiskElement,
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
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = 0)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      exp(self$depends_on$a$point_estimate())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = exp(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = exp(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)

#' @title ElementPow10 Class
#'
#' @description
#' An element that calculates 10 to the power of the output of one element
#'
#'
#' @export
#'
ElementPow10 <- R6::R6Class(
  classname = "ElementPow10",
  inherit = RiskElement,
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
                       input_names = c("a"),
                       input_types = list(a = "any"),
                       units = units,
                       element_type = "algebra",
                       output_var = "x",
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Get the data type of the output.
    #'
    get_output_type = function() {
      self$depends_on$a$get_output_type()
    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      10^(self$depends_on$a$point_estimate())
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- self$simulations %>%
        dplyr::mutate(
          x = 10^(a)
        )

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      if (self$level > level) {
        niter0 <- 1
      }

      sims <- self$simulations_multi[[iter1]] %>%
        dplyr::mutate(
          x = 10^(a)
        )

      ## Save it

      self$simulations_multi[[iter1]] <- sims

      ## Return the output

      invisible(sims[[self$output]])

    }

  )

)
