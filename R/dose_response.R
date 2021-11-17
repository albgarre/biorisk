
#' R6 class describing the exponential dose-response model
#'
#' @details
#' A risk module describing the exponential dose-response model. It has 2 inputs: r, dose
#'
#' @export
#'
DoseResponse_Exponential <- R6::R6Class(
  classname = "DoseResponse_Exponential",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("r", "dose"),
                       units = units,
                       module_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        r = self$depends_on$r$simulate(niter),
        dose = self$depends_on$dose$simulate(niter)
      ) %>%
        dplyr::mutate(
          P_ill = 1 - exp(-r*dose)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

#' R6 class describing the beta-Poisson dose-response model
#'
#' @details
#' A risk module describing the beta-Poisson dose-response model. It has 3 inputs: beta, alpha, dose
#'
#' @export
#'
DoseResponse_BetaPoisson <- R6::R6Class(
  classname = "DoseResponse_BetaPoisson",
  inherit = RiskModule,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA) {

      super$initialize(name,
                       input_names = c("beta", "alpha", "dose"),
                       units = units,
                       module_type = "dose_response",
                       output_var = "P_ill",
                       output_unit = output_unit)

    },


    #' @description
    #' Simulates the module
    #' @param niter Number of Monte Carlo simulations.
    #' @return the output of the module
    #'
    simulate = function(niter) {

      ## Do the simulations (recursively)

      sims <- tibble::tibble(
        beta = self$depends_on$beta$simulate(niter),
        alpha = self$depends_on$alpha$simulate(niter),
        dose = self$depends_on$dose$simulate(niter)
      ) %>%
        dplyr::mutate(
          P_ill = 1 - (1 + dose/beta)^(-alpha)
        )

      ## Save the results of the simulations

      self$simulations <- sims

      ## Return

      sims[[self$output]]

    }

  )

)

## Tests ----------------------------------------------------------------------

# aa <- DoseResponse_Exponential$new("aa")$
#   map_input("r",
#             LogNormal$new("r")$
#               map_input("mu_log10", Constant$new("mu_r", -8))$
#               map_input("sigma_log10", Constant$new("sigma_r", 1e-3))
#             )$
#   map_input("dose",
#             Normal$new("dose")$
#               map_input("mu", Constant$new("mu_dose", 200))$
#               map_input("sigma", Constant$new("sigma_dose", 5))
#             )
#
# plot_model(aa)
# aa$simulate(1000) %>% hist()

# aa <- DoseResponse_BetaPoisson$new("aa")$
#   map_input("beta",
#             LogNormal$new("beta")$
#               map_input("mu_log10", Constant$new("mu_r", 8))$
#               map_input("sigma_log10", Constant$new("sigma_r", .5))
#             )$
#   map_input("alpha", Constant$new("alpha", 1.5))$
#   map_input("dose",
#             Normal$new("dose")$
#               map_input("mu", Constant$new("mu_dose", 200))$
#               map_input("sigma", Constant$new("sigma_dose", 5))
#             )
#
# plot_model(aa)
# aa$simulate(1000) %>% hist()

