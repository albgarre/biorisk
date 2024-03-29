
#' Gets the output of all the nodes of a model
#'
#' @param node base node
#'
#' @export
#'
retrieve_outputs <- function(node) {

  if (length(node$depends_on) > 0) {
    other_outs <- node$depends_on %>%
      map_dfc(.,
              ~ retrieve_outputs(.)
      )
  } else {
    other_outs <- NULL
  }

  # browser()

  x <- tibble(node$get_output())
  names(x) <- node$name

  bind_cols(other_outs, x)

}

#' Makes a plot comparing the output of the chosen nodes
#'
#' @param node base node
#' @param chosen A character vector of node names to include (`NULL` by default;
#' i.e. all)
#' @param type Type of plot. Either "boxplot" (default), "density" or "violin"
#'
#' @export
#'
plot_outputs <- function(node, chosen = NULL, type = "boxplot") {

  my_table <- retrieve_outputs(node) %>%
    pivot_longer(everything())

  if (! is.null(chosen)) {
    my_table <- my_table %>%
      filter(name %in% chosen) %>%
      mutate(name = factor(name, levels = chosen))
  }

  if (type == "boxplot") {

    my_table %>%
      ggplot() +
      geom_boxplot(aes(x = name, y = value)) +
      xlab("Node") + ylab("Output")

  } else if (type == "density") {

    my_table %>%
      ggplot() +
      geom_density(aes(x = value, colour = name)) +
      xlab("Output")

  } else if (type == "violin") {
    my_table %>%
      ggplot() +
      geom_violin(aes(x = name, y = value)) +
      xlab("Node") + ylab("Output")
  }

}

#' @description
#' Gets the outputs of all the nodes in a 2D model
#'
retrieve_outputs_2D <- function(node) {

  # browser()

  if (length(node$depends_on) > 0) {
    other_outs <- node$depends_on %>%
      map_dfr(.,
              ~ retrieve_outputs_2D(.)
      )
  } else {
    other_outs <- NULL
  }

  x <- node$get_output_2D()

  bind_rows(other_outs, x)

}

#' @description
#' Makes a plot of the outputs of nodes of a 2D simulation
#'
#' @export
#'
#' @importFrom ggridges geom_density_ridges
#'
plot_outputs_2D <- function(node, chosen = NULL) {

  my_table <- retrieve_outputs_2D(node)

  if (!is.null(chosen)) {
    my_table <- my_table %>%
      filter(node %in% chosen) %>%
      mutate(node = factor(node, levels = chosen))
  }

  p_unc <- geom_density_ridges(aes(x = x, y = node), data = my_table,
                               fill = "darkgrey", alpha = .5)

  p_var <- my_table %>%
    group_by(sim, node) %>%
    summarize(x = median(x, na.rm = TRUE)) %>%
    geom_density_ridges(aes(x = x, y = node), data = .,
                        fill = "steelblue", alpha = .5)

  ggplot() + p_var + p_unc

}


#' Compares the output of several nodes
#'
#' Different from plot_outputs() because it does not check dependencies. Instead,
#' nodes are passed explicitly. Therefore, it can be used to compare between different
#' models.
#'
#' @export
#'
compare_nodes <- function(node_list, type = "density") {

  my_table <- node_list %>%
    map_dfc(~ .$get_output())

  if (type == "boxplot") {

    my_table %>%
      pivot_longer(everything()) %>%
      ggplot() +
      geom_boxplot(aes(x = name, y = value)) +
      xlab("Node") + ylab("Output")

  } else if (type == "density") {

    my_table %>%
      pivot_longer(everything()) %>%
      ggplot() +
      geom_density(aes(x = value, colour = name)) +
      xlab("Output")

  } else if (type == "violin") {

    my_table %>%
      pivot_longer(everything()) %>%
      ggplot() +
      geom_violin(aes(x = name, y = value)) +
      xlab("Node") + ylab("Output")

  }

}

# # ################
# #
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
# inact_model$simulate(1000)
# inact_model$histogram()
#
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
# plot_model(growth_model)
# growth_model$simulate(1000)
# growth_model$density_plot()
#
# plot_outputs(growth_model)
# plot_outputs(growth_model, chosen = c("logN0", "Inactivation", "Growth"))
#
# plot_outputs(growth_model, chosen = c("logN0", "Inactivation", "Growth"),
#              type = "boxplot")
#
# compare_nodes(list(logN0 = logN0, growth = growth_model, inact = inact_model), type = "boxplot")




