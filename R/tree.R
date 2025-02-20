
#' Regression tree of the output
#'
#' Useful for sensitivity analysis
#'
#' @importFrom rpart rpart
#' @importFrom maptree draw.tree
#'
#' @param node The output node.
#' @param chosen A character vector of node names to include in the tree. By default, `NULL` (every node).
#' @param make_plot Whether to plot the tree. `FALSE` by default.
#'
#' @export
#'
regression_tree <- function(node,
                            chosen = NULL,
                            make_plot = FALSE
                            ) {

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

  ## Take only the 'chosen' variables

  # browser()

  if (!is.null(chosen)) {

    d <- d %>% select_at(c(node$name, chosen))

  }

  ## Build the tree

  my_formula <- as.formula(paste0(node$name, " ~ ."))

  out <- rpart(
    formula = my_formula,
    data    = d,
    method  = "anova"
  )

  ## Plot if required

  if (make_plot) {
    draw.tree(out, cases = "sims")
  }

  ## Return

  out

}


