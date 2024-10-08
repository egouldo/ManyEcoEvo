% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_model_means_orchard.R
\name{plot_model_means_orchard}
\alias{plot_model_means_orchard}
\title{Plot orchard-plot style model means}
\usage{
plot_model_means_orchard(dat, variable, predictor_means, new_order, title)
}
\arguments{
\item{dat}{A tibble with the data to plot}

\item{variable}{A character string of the predictor variable to plot}

\item{predictor_means}{A tibble with the means of the model}

\item{new_order}{A character vector of the new order of the \code{variable}}

\item{title}{A character string of the plot title}
}
\value{
A ggplot object
}
\description{
Plot the means of a model with a predictor variable
}
\seealso{
Other Plotting functions: 
\code{\link{get_forest_plot_data}()},
\code{\link{plot_cont_rating_effects}()},
\code{\link{plot_effects_diversity}()},
\code{\link{plot_forest}()},
\code{\link{plot_model_means_box_cox_cat}()}
}
\concept{Plotting functions}
