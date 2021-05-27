
#' Extract data for the tornado plot
#'
#' @importFrom purrr map_dfc
#'
get_data_tornado <- function(node, which = "output") {

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
tornado_plot <- function(node, which = "output",
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

  tibble(var = names(my_cor[inact_model$name,]),  # We take out 1 bcs self-correlation
         rho = my_cor[inact_model$name,]) %>%
    filter(var != node$name) %>%
    # mutate(abs_rho = abs(rho))
    arrange(abs(rho)) %>%
    mutate(var = factor(var, levels = var)) %>%
    ggplot() +
    geom_col(aes(x = var, y = rho)) +
    coord_flip() +
    geom_hline(yintercept = 0) +
    ylab(paste(method, "correlation")) +
    xlab("")

}



# ### tests
#
#
# library(biorisk)
# library(tidyverse)
#
# treat_time <- Constant$new("Treat time", 30)
#
# treat_time <- Normal$new("Treat time")$
#   map_input("mu", Constant$new("mu_t", 30))$
#   map_input("sigma", Constant$new("sigma_t", 3))
#
# logD <- Normal$new("logD")$
#   map_input("mu", Constant$new("mu_logD", 1))$
#   map_input("sigma", Constant$new("sigma_logD", 0.2))
#
# logN0 <- Normal$new("logN0")$
#   map_input("mu", Constant$new("mu_logN0", 2))$
#   map_input("sigma", Constant$new("sigma_logN0", 0.5))
#
# inact_model <- LogLinInactivation$new("Treatment")$
#   map_input("t", treat_time)$
#   map_input("logD", logD)$
#   map_input("logN0", logN0)
#
# ## Storage
#
# stor_time <- Constant$new("Storage time", 3)
#
# mu <- Normal$new("mu")$
#   map_input("mu", Constant$new("mu_mu", 1))$
#   map_input("sigma", Constant$new("sigma_mu", 0.2))
#
# growth_model <- ExponentialGrowth$new("Storage")$
#   map_input("t", stor_time)$
#   map_input("mu", mu)$
#   map_input("logN0", inact_model)
#
# growth_model$simulate(1000) %>% hist()
#
# # ## Get as graph
#
# plot_model(inact_model)
# plot_model(growth_model, layout = "tree")
#
# tornado_plot(growth_model)
# tornado_plot(inact_model)

















