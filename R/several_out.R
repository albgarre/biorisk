
#' A element simulating a 2D normal distribution
#'
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @export
#'
BiNormal <- R6::R6Class(
  classname = "Normal",
  inherit = ContinuousElement,
  public = list(

    initialize = function(name,
                          units = NA,
                          output_unit = NA,
                          level = 0) {

      super$initialize(name,
                       input_names = c("mu1", "sigma1", "mu2", "sigma2", "rho"),
                       input_types = list(mu1 = "continuous",
                                          sigma1 = "continuous",
                                          mu2 = "continuous",
                                          sigma2 = "continuous",
                                          rho = "continuous"
                                          ),
                       units = units,
                       element_type = "distribution",
                       output_var = c("x1", "x2"),
                       output_unit = output_unit,
                       level = level)

    },

    #' @description
    #' Returns the expected value
    #'
    point_estimate = function() {
      # self$depends_on$mu$point_estimate()

      stop("point_estimate not defined for BiNormal element")
    }

  ),

  private = list(

    update_output = function(niter) {

      sims <- lapply(1:nrow(self$simulations), function(i) {

        # browser()

        this_data <- self$simulations[i,]

        mus <- c(this_data$mu1, this_data$mu2)
        stdevs <- c(this_data$sigma1, this_data$sigma2)
        b_matrix <- stdevs %*% t(stdevs)
        cov_matrix <- b_matrix * matrix(c(1, this_data$rho, this_data$rho, 1), nrow = 2)


        c(this_data, x = mvtnorm::rmvnorm(1, mus, cov_matrix))

      }) %>%
        bind_rows()

      self$simulations <- sims

    },

    update_output_level = function(niter0, iter1 = 1, level = 0) {

      # if (self$level > level) {
      #   niter0 <- 1
      # }
      #
      # sims <- self$simulations_multi[[iter1]] %>%
      #   dplyr::mutate(
      #     x = rnorm(niter0, mu, sigma)
      #   )
      #
      # ## Save it
      #
      # self$simulations_multi[[iter1]] <- sims
      #
      # ## Return the output
      #
      # invisible(sims[[self$output]])

    }

  )

)




#############################


# aa <- BiNormal$new("aa")$
#   map_input("mu1", Constant$new("mu1", 0))$
#   map_input("mu2", Constant$new("mu2", 1))$
#   map_input("sigma1", Constant$new("sigma1", 1))$
#   map_input("sigma2", Constant$new("sigma2", .5))$
#   map_input("rho", Constant$new("rho", .8))
#
# aa$simulate(100)
# aa$simulations
#
# aa$get_output(index = 2)
#
# bb <- ElementPlus$new("bb")$
#   map_input("a", aa, index = 1)$
#   map_input("b", aa, index = 2)
#
# bb$simulate(100)
# bb$simulations
# plot_model(bb)
# bb$histogram()
# bb$density_plot()
# plot_outputs(bb)
# aa$histogram()
