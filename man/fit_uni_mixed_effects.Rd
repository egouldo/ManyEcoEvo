% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_uni_mixed_effects.R
\name{fit_uni_mixed_effects}
\alias{fit_uni_mixed_effects}
\title{Fit univariate glm of deviation scores on random effects inclusion}
\usage{
fit_uni_mixed_effects(data)
}
\arguments{
\item{data}{Dataframe containing box-cox transformed absolute deviation scores and binary column called \code{mixed_model} describing whether or not the analysis used a mixed-effects model.}
}
\value{
A fitted model object of class \code{glm} and \code{parsnip}
}
\description{
Fits a univariate glm of box-cox transformed absolute deviation from the meta-analytic mean scores as a function of whether the analysis was a mixed effects model \(i.e. included random effects\) or not.
}
\examples{
# library(tidyverse);library(targets);library(metafor);library(tidymodels)
# tar_load(meta_analysis_outputs)
# fit_uni_mixed_effects(meta_analysis_results$data[[1]])
# Note: used tidymodels approach for dynamic outcome var selection
# base R approach will be more succinct.
}
