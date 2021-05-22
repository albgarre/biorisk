#
# ##
# # RiskModel <- R6::R6Class(
# #   "RiskModel",
# #   public = list(
# #     modules = NULL,
# #
# #     initialize = function() {},
# #     add_modul
# #   ),
# #   private = NULL
# # )
#
# library(R6)
# library(tidyverse)
#
# ## Parent class ----------------------------------------------------------------
#
# Module <- R6Class(
#   classname = "Module",
#
#   public = list(
#
#     name = "",
#     inputs = tibble(),
#     depends_on = list(),
#     depended_by = c(),
#     output = "",
#     simulations = tibble(),
#     type = "",
#
#     initialize = function() {
#       self$name = ""
#       self$inputs = tibble()
#       self$depends_on = list()
#       self$depended_by = c()
#       self$output = ""
#       self$simulations = tibble()
#       self$type = ""
#     },
#
#     ## Attributes
#     ## Methods
#
#     map_input = function(input, module) {
#       stop("map_input not defined for this class")
#     },
#
#     simulate = function(niter, check_units = FALSE) {
#       stop("simulate not defined for this class")
#     }
#
#   ),
#
#   private = list(
#     simulate_self = function(niter) {
#       stop("simulate_self not defined for this class")
#     }
#
#   )
# )
#
# a <- Module$new()
# a$output
#
# #####
#
# ModuleConstant <- R6Class(
#   classname = "ModuleConstant",
#   inherit = Module,
#   public = list(
#
#     ## Attributes
#
#     name = NULL,
#     inputs = tibble(),
#     depends_on = list(),
#
#     value = NULL,
#     unit = NULL,
#     type = "constant",
#
#
#     ## Methods
#
#     initialize = function(name, value, unit = NA) {
#
#       self$name <- name
#       self$value <- value
#       self$unit <- unit
#
#     },
#
#     simulate = function(niter) {
#
#       rep(self$value, niter)
#
#     }
#
#   )
#
# )
#
#
#
#
#
#
# ##
#
#
#
#
# ##
#
# DistributionModule <- R6Class(
#   "DistributionModule",
#   public = list(),
#   private = NULL
# )
#
# NormalModule <- R6Class(
#   "NormalModule",
#   inherit = DistributionModule,
#   public = list(),
#   private = NULL
# )
#
# ##
#
# RiskModule <- R6::R6Class(
#   "RiskModule",
#   public = list(
#
#     ## Attributes
#
#     inputs = tibble(input = NA, unit = NA),
#     outputs = NULL,
#     module_type = "",
#
#     ## Methods
#
#     # set_previous_module = function() {},
#     simulate = function() {},
#     map_input = function(input, module) {}
#   ),
#   private = list(
#     simulate_self = function() {},
#     check_units = function() {}
#   )
# )
#
#
# ##
#
# LoglinearInactivation <- R6::R6Class(
#   "InactivationModule",
#   inherit = RiskModule,
#   public = list(
#     module_type = "Inactivation",
#     inputs = tribble(
#       ~input, ~unit,
#       "logN0", "log CFU",
#       "logD", "log min",
#       "t", "min"
#     )
#
#   ),
#   private = list(
#     simulate_self = function(niter) {
#
#       tibble()
#
#     }
#   )
# )
#
# LoglinearInactivationTemperature<- R6::R6Class(
#   "LoglinearInactivationTemperature",
#   inherit = RiskModule,
#   public = list(
#     module_type = "Inactivation",
#     inputs = tribble(
#       ~input, ~unit,
#       "logN0", "log CFU",
#       "logD", "log min",
#       "z", "ºC",
#       "t", "min",
#       "T", "ºC"
#     )
#
#   ),
#   private = list(
#     simulate_self = function(niter) {
#
#       tibble()
#
#     }
#   )
# )
#
#
# ########################################################
#
# module1 <- LoglinearInactivation$new()
# module2 <- LoglinearInactivation$new()
# normal_var <- NormalModule$new()
#
# module1$map_input("logN0", module2)
# module1$map_input("t", normal_var)
# module1$simulate()












