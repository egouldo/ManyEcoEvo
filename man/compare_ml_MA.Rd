% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_level_mod_calcs.R
\name{compare_ml_MA}
\alias{compare_ml_MA}
\title{Compare two fitted multi-level models}
\usage{
compare_ml_MA(object1, object2)
}
\arguments{
\item{object1}{A fitted model of class \code{mra.mv}}

\item{object2}{Another fitted model of class \code{mra.mv}}
}
\value{
A tibble with descriptive statistics of model fit
}
\description{
Compares two fitted multi-level models and tidies the results
}
\seealso{
Other Meta-analysis statistics: 
\code{\link{apportion_heterogeneity_ml}()},
\code{\link{calc_I2_ml}()}
}
\concept{Meta-analysis statistics}
