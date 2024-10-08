% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_uni_mixed_effects.R
\name{fit_uni_mixed_effects}
\alias{fit_uni_mixed_effects}
\title{Fit model of Box-Cox transformed deviation scores as a function random-effects inclusion in analyses}
\usage{
fit_uni_mixed_effects(data, N = 5)
}
\arguments{
\item{data}{Dataframe containing Box-Cox transformed absolute deviation scores and binary column called \code{mixed_model} describing whether or not the analysis used a mixed-effects model.}

\item{N}{threshold number of analyses in each predictor category for fitting model}
}
\value{
A fitted model object of class \code{glm} and \code{parsnip}
}
\description{
Fits a univariate glm of Box-Cox transformed absolute deviation from the meta-analytic mean scores as a function of whether the analysis was a mixed effects model (i.e. included random effects) or not.
}
\examples{
# library(tidyverse);library(targets);library(metafor);library(tidymodels)
# tar_load(meta_analysis_outputs)
# fit_uni_mixed_effects(meta_analysis_results$data[[1]])
}
\seealso{
\link[parsnip:details_linear_reg_glm]{parsnip::details_linear_reg_glm} for details on the \link[parsnip:linear_reg]{parsnip::linear_reg} engine.

Other Model fitting and meta-analysis: 
\code{\link{fit_MA_mv}()},
\code{\link{fit_boxcox_ratings_cat}()},
\code{\link{fit_boxcox_ratings_cont}()},
\code{\link{fit_boxcox_ratings_ord}()},
\code{\link{fit_metafor_mv}()},
\code{\link{fit_metafor_mv_reduced}()},
\code{\link{fit_metafor_uni}()},
\code{\link{fit_multivar_MA}()},
\code{\link{fit_sorensen_glm}()},
\code{\link{poss_fit_boxcox_ratings_cat}()},
\code{\link{poss_fit_boxcox_ratings_cont}()},
\code{\link{poss_fit_boxcox_ratings_ord}()},
\code{\link{poss_fit_metafor_mv}()},
\code{\link{poss_fit_uni_mixed_effects}()}
}
\concept{Model fitting and meta-analysis}
