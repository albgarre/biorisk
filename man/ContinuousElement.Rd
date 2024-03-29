% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parent_module.R
\name{ContinuousElement}
\alias{ContinuousElement}
\title{ContinuousElement Class}
\description{
This is the parent class for Elements that have a continuous output
}
\section{Super class}{
\code{\link[biorisk:RiskElement]{biorisk::RiskElement}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NA-new}{\code{ContinuousElement$new()}}
\item \href{#method-unknown-clone}{\code{ContinuousElement$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="boxplot"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-boxplot'><code>biorisk::RiskElement$boxplot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="check_input_types"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-check_input_types'><code>biorisk::RiskElement$check_input_types()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="cummulative_plot"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-cummulative_plot'><code>biorisk::RiskElement$cummulative_plot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="cummulative_plot_2D"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-cummulative_plot_2D'><code>biorisk::RiskElement$cummulative_plot_2D()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="density_plot"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-density_plot'><code>biorisk::RiskElement$density_plot()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="density_plot_2D"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-density_plot_2D'><code>biorisk::RiskElement$density_plot_2D()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="get_output"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-get_output'><code>biorisk::RiskElement$get_output()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="get_output_2D"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-get_output_2D'><code>biorisk::RiskElement$get_output_2D()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="get_output_type"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-get_output_type'><code>biorisk::RiskElement$get_output_type()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="histogram"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-histogram'><code>biorisk::RiskElement$histogram()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="map_input"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-map_input'><code>biorisk::RiskElement$map_input()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="point_estimate"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-point_estimate'><code>biorisk::RiskElement$point_estimate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="quantiles"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-quantiles'><code>biorisk::RiskElement$quantiles()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="quantiles_2D"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-quantiles_2D'><code>biorisk::RiskElement$quantiles_2D()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="save_as_distribution"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-save_as_distribution'><code>biorisk::RiskElement$save_as_distribution()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="save_as_vector"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-save_as_vector'><code>biorisk::RiskElement$save_as_vector()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="simulate"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-simulate'><code>biorisk::RiskElement$simulate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="simulate_2D"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-simulate_2D'><code>biorisk::RiskElement$simulate_2D()</code></a></span></li>
<li><span class="pkg-link" data-pkg="biorisk" data-topic="RiskElement" data-id="simulate_level"><a href='../../biorisk/html/RiskElement.html#method-RiskElement-simulate_level'><code>biorisk::RiskElement$simulate_level()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-new"></a>}}
\if{latex}{\out{\hypertarget{method-NA-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContinuousElement$new(
  name,
  input_names = NA,
  units = NA,
  element_type = "",
  output_var = "",
  output_unit = "",
  input_types = list(),
  level = 0
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{A character defining the name for the element}

\item{\code{input_names}}{A character vector with the names of the inputs}

\item{\code{units}}{A character vector of units for each input}

\item{\code{element_type}}{A character with the type of element}

\item{\code{output_var}}{A character with the name of the output variable}

\item{\code{output_unit}}{A character with the unit of the output}

\item{\code{input_types}}{A list defining types of each input}

\item{\code{level}}{Level of the module for 2D Monte Carlo. By default, \code{0}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new instance
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-unknown-clone"></a>}}
\if{latex}{\out{\hypertarget{method-unknown-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContinuousElement$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
