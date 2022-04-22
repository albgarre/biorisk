
#' Extract data for the tornado plot
#'
#' @importFrom purrr map_dfc
#'
get_data_tornado <- function(node) {

  # if (node$type == "constant") {  # Constants have no variation and no inputs
  #   return(NULL)
  # }

  this_output <- tibble(node$get_output()) %>%
    set_names(., node$name)

  if (length(node$depends_on) > 0) {

    other_output <- node$depends_on %>%
      map_dfc(~ get_data_tornado(.))

  } else {
    other_output <- NULL
  }

  bind_cols(this_output, other_output)

}

#' Tornado plot
#'
#' @importFrom purrr %>%
#'
#' @export
tornado_plot <- function(node,
                         chosen = NULL,
                         method = "spearman",
                         use = "everything") {

  ## Get the outputs

  d <- get_data_tornado(node)

  ## Remove constant columns (have 0 variance)

  my_names <- d %>%
    summarize(across(everything(), var)) %>%
    pivot_longer(everything()) %>%
    filter(value != 0) %>%
    pull(name)

  d <- d %>%
    select_at(my_names)

  ## Calculate correlations

  my_cor <- cor(d, method = method, use = use)

  ## Make the plot

  # browser()

  d <- tibble(var = names(my_cor[node$name,]),  # We take out 1 bcs self-correlation
         rho = my_cor[node$name,]) %>%
    filter(var != node$name)

  if (!is.null(chosen)) {
    d <- d %>%
      filter(var %in% chosen)
  }

  d %>%
    arrange(abs(rho)) %>%
    mutate(var = factor(var, levels = var)) %>%
    ggplot() +
    geom_col(aes(x = var, y = rho)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    ylab(paste(method, "correlation")) +
    xlab("") +
    ylim(-1, 1)

}

# ### tests
#
# library(biorisk)
# library(tidyverse)
#
# time <- Constant$new("Time", 30)
#
# D <- LogNormal$new("D")$
#   map_input("mu_log10", Constant$new("mu_logD", 1))$
#   map_input("sigma_log10", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Inactivation")$
#   map_input("t", time)$
#   map_input("D", D)$
#   map_input("logN0", logN0)
#
# stor_time <- Constant$new("Stor_Time", 3)
#
# mu <- Normal$new("growth_rate")$
#   map_input("mu", Constant$new("mu_growthrate", 1))$
#   map_input("sigma", Constant$new("sigma_growthrate", 0.2))
#
# growth_model <- ExponentialGrowthNmax$new("Growth")$
#   map_input("t", stor_time)$
#   map_input("mu", mu)$
#   map_input("logN0", inact_model)$
#   map_input("logNmax", Constant$new("logNmax", 4))
#
# # ## Get as graph
#
# plot_model(growth_model, layout = "tree")
#
# growth_model$simulate(1000)
#
# tornado_plot(growth_model)
# tornado_plot(growth_model, chosen = c("logN0", "D"))
#









