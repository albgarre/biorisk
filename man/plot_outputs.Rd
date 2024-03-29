% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_plot.R
\name{plot_outputs}
\alias{plot_outputs}
\title{Makes a plot comparing the output of the chosen nodes}
\usage{
plot_outputs(node, chosen = NULL, type = "boxplot")
}
\arguments{
\item{node}{base node}

\item{chosen}{A character vector of node names to include (\code{NULL} by default;
i.e. all)}

\item{type}{Type of plot. Either "boxplot" (default), "density" or "violin"}
}
\description{
Makes a plot comparing the output of the chosen nodes
}
