
#' Growth module based on exponential growth
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
ExponentialGrowth <- R6::R6Class(
  classname = "ExponentialGrowth",
  # inherit = Module,
  public = list(

    ## Attributes

    name = "",
    inputs = tibble::tibble(var = c("logN0", "t", "mu"),
                    units = c(NA, NA, NA)
    ),
    depends_on = list(logN0 = NA,
                      t = NA,
                      mu = NA
    ),

    depended_by = c(),

    output = "",
    simulations = tibble::tibble(),
    type = "growth",


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
        t = self$depends_on$t$simulate(niter),
        mu = self$depends_on$mu$simulate(niter),
        logN0 = self$depends_on$logN0$simulate(niter)
      ) %>%
        dplyr::mutate(
          logN = logN0 + t*mu
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims$logN

    },

    get_output = function() {
      self$simulations$logN
    }

  )

)

# ### tests
#
# time <- ModuleConstant$new("Time", 3)
#
# mu <- ModuleNormal$new("mu")$
#   map_input("mu", ModuleConstant$new("mu", 1))$
#   map_input("sigma", ModuleConstant$new("sigma", 0.2))
#
# logN0 <- ModuleNormal$new("logD")$
#   map_input("mu", ModuleConstant$new("mu", 2))$
#   map_input("sigma", ModuleConstant$new("sigma", 0.5))
#
# growth_model <- ExponentialGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)
#
# growth_model$simulate(1000) %>% hist()
# growth_model$simulations
#
# ## tests
#
# ### Inactivation
#
# treat_time <- ModuleConstant$new("Time", 30)
#
# logD <- ModuleNormal$new("logD")$
#   map_input("mu", ModuleConstant$new("mu", 1))$
#   map_input("sigma", ModuleConstant$new("sigma", 0.2))
#
# logN0 <- ModuleNormal$new("logD")$
#   map_input("mu", ModuleConstant$new("mu", 2))$
#   map_input("sigma", ModuleConstant$new("sigma", 0.5))
#
# inact_model <- ModuleLogLinInactivation$new("Inactivation")$
#   map_input("t", treat_time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# ## Storage
#
# stor_time <- Constant$new("Storage time", 3)
#
# mu <- Normal$new("mu")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# growth_model <- ExponentialGrowth$new("Growth")$
#   map_input("t", stor_time)$
#   map_input("mu", mu)$
#   map_input("logN0", inact_model)
#
# growth_model <- ExponentialGrowth$new("Growth")
#
# growth_model$map_input("t", stor_time)
# growth_model$map_input("mu", mu)
# growth_model$map_input("logN0", inact_model)
#
# ### Simulate and plot
#
# growth_model$simulate(1000)
#
# tibble(
#   logN0 = logN0$get_output(),
#   treatment = inact_model$get_output(),
#   consumer = growth_model$get_output()
# ) %>%
#   pivot_longer(everything(), names_to = "step", values_to = "logN") %>%
#   mutate(step = factor(step, levels = c("logN0", "treatment", "consumer"))) %>%
#   ggplot() +
#   geom_boxplot(aes(x = step, y = logN))
#
# inact_model$simulations



