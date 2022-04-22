
#' 2D Monte Carlo simulation
#'
#' @param node The base node for the simulations
#' @param niter0 Number of iterations on the base level
#' @param niter1 Number of iterations on the upper level
#'
#' @export
#'
simulate_2D <- function(node, niter0, niter1) {

  sims <- 1:niter1 %>%
    map(.,
        ~ node$simulate_level(niter0, iter1=., level=0)
        )

  invisible(NULL)

}

# bb <- Normal$new("mu_mu", level = 1)$
#   map_input("mu", Constant$new("mu_mu_mu", 3))$
#   map_input("sigma", Constant$new("mu_sigma_mu", .1))
#
# aa <- Normal$new("aa", level =  0)$
#   map_input("mu", bb
#   )$
#   map_input("sigma", Constant$new("sigma", .2))
#
# bb$simulate_level(100, 1, 0)
#
# simulate_2D(aa, 50, 3)
#
# bb$get_output(1)
#
# bb$simulations_multi
# aa$simulations_multi





