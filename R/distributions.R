
#' A module simulating a normal distribution
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
Normal <- R6::R6Class(
  classname = "Normal",
  # inherit = Module,
  public = list(

    ## Attributes

    name = "",
    inputs = tibble::tibble(var = c("mu", "sigma"),
                    units = c(NA, NA)
                    ),
    depends_on = list(mu = NA,
                      sigma = NA),
    depended_by = c(),
    output = "",
    simulations = tibble::tibble(),
    type = "distribution",


    ## Methods

    initialize = function(name, units = NULL) {

      self$name <- name

      if (!is.null(units)) {
        self$inputs$units = units
      }

    },

    map_input = function(input, module, check_units = FALSE) {

      if (! (input %in% names(self$depends_on))) {
        stop("Unkonwn input: ", input)
      }

      ## Add the dependency

      self$depends_on[[input]] <- module

      ## Reflect it on the other module

      module$depended_by <- c(module$depended_by, self)

      ## Return self

      invisible(self)

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

    },

    get_output = function() {
      self$simulations$x
    }

  )

)

# ## tests
#
# my_mean <- ModuleConstant$new("mean", 3)
# my_sd <- ModuleConstant$new("sd", .3)
#
# # my_mean$simulate(10)
# # my_sd$simulate(20)
#
# logN0 <- ModuleNormal$new("Concentracion inicial")$
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
#
# ## test 2
#
# big_mean <- ModuleConstant$new("big mean", 4)
# big_sd <- ModuleConstant$new("big sd", 1)
#
# norm1 <- ModuleNormal$new("Normal 1")$
#   map_input("mu", big_mean)$
#   map_input("sigma", big_sd)
#
# norm1$simulate(100) %>%
#   hist()
#
# small_sd <- ModuleConstant$new("small sd", 0.3)
#
# norm2 <- ModuleNormal$new("Normal 2")$
#   map_input("mu", norm1)$
#   map_input("sigma", small_sd)
#
# norm2$simulate(1000) %>% hist()
# norm2$simulations



