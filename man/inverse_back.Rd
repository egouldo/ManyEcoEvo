% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/back_transformations.R
\name{inverse_back}
\alias{inverse_back}
\title{Back transform beta estimates for models with $1/x$ link}
\usage{
inverse_back(beta, se, sim)
}
\arguments{
\item{beta}{Analyst beta estimate}

\item{se}{Standard error of analyst's beta estimate.}

\item{sim}{numeric vector of length 1. number of simulations.}
}
\value{
data frame containing the mean estimate, its standard error, and quantiles
}
\description{
Back transform beta estimates for models with $1/x$ link
}
\seealso{
Other back transformation: 
\code{\link{conversion}()},
\code{\link{conversion_2}()},
\code{\link{cube_back}()},
\code{\link{divide_back}()},
\code{\link{identity_back}()},
\code{\link{log_back}()},
\code{\link{logit_back}()},
\code{\link{power_back}()},
\code{\link{probit_back}()},
\code{\link{square_back}()},
\code{\link{square_root_back}()}
}
\concept{back transformation}
