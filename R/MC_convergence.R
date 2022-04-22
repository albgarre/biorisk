

#' Resampling a vector with replacement several times
#'
resample_vector_median <- function(x, size, samples) {
  1:samples %>%
    map_dbl(., ~ median(sample(x, size, replace = TRUE)))
}

#' Analysis of MC convergence
#'
#' @export
#'
MC_convergence <- function(node,
                           sizes = 10^(1:4),
                           n_samples = 100) {

  node_output <- node$get_output()

  sizes %>%
    set_names(., .) %>%
    map_dfc(.,
            ~ resample_vector_median(node_output, ., n_samples)
    ) %>%
    pivot_longer(everything(), names_to = "MC iterations", values_to = "median_output") %>%
    mutate(`MC iterations` = as.numeric(`MC iterations`)) %>%
    group_by(`MC iterations`) %>%
    summarize(`Mean median` = mean(median_output),
              `Median median` = median(median_output),
              `Q90 median` = quantile(median_output, .9),
              `Q10 median` = quantile(median_output, .1))

}

#' Plot of the MC convergence analysis
#'
#' @export
#'
plot_MC_analysis <- function(MC_analysis) {

  ggplot(MC_analysis, aes(x = `MC iterations`)) +
    geom_line(aes(y = `Mean median`)) +
    geom_point(aes(y = `Mean median`)) +
    geom_line(aes(y = `Median median`), linetype = 2) +
    geom_ribbon(aes(ymin = `Q10 median`, ymax = `Q90 median`), fill = "steelblue", alpha = .5)

}

# ## tests
#
# time <- Constant$new("Time", 3)
#
# mu <- Normal$new("mu")$
#   map_input("mu", Constant$new("mu", 1))$
#   map_input("sigma", Constant$new("sigma", 0.2))
#
# logN0 <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu", 2))$
#   map_input("sigma", Constant$new("sigma", 0.5))
#
#
# growth_model <- ExponentialGrowth$new("Growth")$
#   map_input("t", time)$
#   map_input("mu", mu)$
#   map_input("logN0", logN0)
#
# growth_model$simulate(100000)
#
# aa <- MC_convergence(growth_model, sizes = c(10, 100, 500, 1000, 5000), n_samples = 500)
# aa
# plot_MC_analysis(aa)


