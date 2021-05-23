
#' Getting the names of the nodes
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_rows
#'
get_names <- function(node) {

  if (length(node$depends_on) > 0) {
    other_names <- node$depends_on %>%
      map_dfr(.,
              ~ get_names(.)
      )
  } else {
    other_names <- NULL
  }

  tibble(name = node$name, type = node$type) %>%
    bind_rows(., other_names)


}

#' Get edges for building a graph
#'
#' @importFrom purrr map_chr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
get_edges <- function(node) {

  if (length(node$depends_on) > 0) {

    parent_names <- map_chr(node$depends_on,
                            ~ .$name)

    my_e <- tibble(from = parent_names,
                   to = node$name)

    parent_edges <- map_dfr(node$depends_on,
                            ~ get_edges(.))

    bind_rows(my_e, parent_edges)

  } else {
    NULL
  }

}

#' Convert a model to a graph
#'
#' @param node A node of the model
#'
#' @importFrom tibble tribble
#' @importFrom dplyr left_join pull select
#' @immportFrom DiagrammeR create_graph
#'
#' @export
#'
model_to_graph <- function(node) {

  ## Map of attributes

  aes_map <- tribble(
    ~type, ~shape, ~color,
    "growth", "rectangle", "green",
    "inactivation", "rectangle", "orange",
    "constant", "triangle", "grey",
    "distribution", "circle", "blue"
  )

  ## Get the node attributes

  node_names <- get_names(node) %>%
    left_join(., aes_map)

  ndf <- create_node_df(n = nrow(node_names),
                        label = node_names$name,
                        type = node_names$type,
                        shape = node_names$shape,
                        color = node_names$color)

  ## Get the edge attributes

  my_edges <- get_edges(node)

  from_edge <- my_edges %>%
    select(label = from) %>%
    left_join(., ndf) %>%
    pull(id)

  to_edge <- my_edges %>%
    select(label = to) %>%
    left_join(., ndf) %>%
    pull(id)

  edf <- create_edge_df(from = from_edge,
                        to = to_edge)

  ## Make the graph

  create_graph(nodes_df = ndf,
               edges_df = edf)

}

#' Plotting a model
#'
#' @importFrom DiagrammeR render_graph
#'
#' @export
#'
plot_model <- function(node, layout = "tree") {

  model_to_graph(node) %>%
    # get_node_df()
    render_graph(layout = layout)
}


# ### tests
#
#
# library(biorisk)
# library(tidyverse)
#
# treat_time <- Constant$new("Treat time", 30)
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
# ## Get as graph
#
# # model_to_graph(growth_model)
# plot_model(inact_model)
# plot_model(growth_model)


