
#' @description
#' Builds a table with quantiles of the model outputs
#'
#'
retrieve_table <- function(node, probs=seq(0, 1, 0.25)) {

  if (length(node$depends_on) > 0) {
    other_qs <- node$depends_on %>%
      map_dfr(.,
              ~ retrieve_table(., probs = probs)
      )
  } else {
    other_qs <- NULL
  }

  new_row <- node$quantiles(probs) %>%
    as.list() %>%
    as_tibble() %>%
    mutate(node = node$name)

  bind_rows(other_qs, new_row)
}

#' @description
#' Returns a table of the model quantiles
#'
#' @export
#'
quantile_table <- function(node, probs = seq(0, 1, 0.25),
                           chosen = NULL) {

  my_table <- retrieve_table(node, probs) %>%
    select(node, everything())

  if (!is.null(chosen)) {
    my_table <- my_table %>%
      filter(node %in% chosen)
  }

  my_table

}

#' @description
#' Builds a table with quantiles of the model outputs
#'
#'
retrieve_table_2D <- function(node, probs=seq(0, 1, 0.25)) {

  if (length(node$depends_on) > 0) {
    other_qs <- node$depends_on %>%
      map_dfr(.,
              ~ retrieve_table_2D(., probs = probs)
      )
  } else {
    other_qs <- NULL
  }

  new_row <- node$quantiles_2D(probs) %>%
    as.list() %>%
    as_tibble() %>%
    mutate(node = node$name)

  bind_rows(other_qs, new_row)
}

#' @description
#' Returns a table of the model quantiles
#'
#' @export
#'
quantile_table_2D <- function(node, probs = seq(0, 1, 0.25),
                              chosen = NULL) {

  my_table <- retrieve_table_2D(node, probs) %>%
    select(node, everything())

  if (!is.null(chosen)) {
    my_table <- my_table %>%
      filter(node %in% chosen)
  }

  my_table

}


# ################

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
# plot_model(growth_model)
# growth_model$simulate(1000)
#
# quantile_table(growth_model)
# quantile_table(growth_model, probs = c(.2, .6))
# quantile_table(growth_model, probs = c(.2, .6),
#                chosen = c("logN0", "Inactivation", "Growth"))
#
#
# plot_outputs(growth_model)
# plot_outputs(growth_model, chosen = c("logN0", "Inactivation", "Growth"))
#
# plot_outputs(growth_model, chosen = c("logN0", "Inactivation", "Growth"),
#              type = "violin")
#
# tornado_plot(growth_model)
# tornado_plot(growth_model, chosen = c("logN0", "D"))






