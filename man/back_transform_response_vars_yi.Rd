% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_response_variables_yi.R
\name{back_transform_response_vars_yi}
\alias{back_transform_response_vars_yi}
\title{Back Transform Response Variables - yi}
\usage{
back_transform_response_vars_yi(
  dat,
  estimate_type = character(1L),
  param_table = NULL,
  dataset = character(1L)
)
}
\arguments{
\item{estimate_type}{The type of estimate to be standardised. Character vector of length 1, whose value may be  "yi", "y25", "y50", "y75".}

\item{param_table}{A table of estimated 'population' parameters for each variable in the analysis datasets.}

\item{dataset}{One of either "blue tit" or "eucalyptus"}

\item{data}{}
}
\value{
A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
}
\description{
Back Transform Response Variables - yi
}
\seealso{
Other analyst-data: 
\code{\link{standardise_response}()}
}
\concept{analyst-data}
