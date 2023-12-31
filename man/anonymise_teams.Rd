% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anonymise_teams.R
\name{anonymise_teams}
\alias{anonymise_teams}
\title{Anonymise ManyEcoEvo Data}
\usage{
anonymise_teams(df, lookup)
}
\arguments{
\item{df}{A dataframe containing the column \code{id_col}}

\item{lookup}{A dataframe containing the columns \code{TeamIdentifier} and \code{New_Identifier}}
}
\value{
A \code{df} with anonymised values of \code{id_col} based on the \code{New_Identifier} colum of \code{lookup}
}
\description{
Anonymise ManyEcoEvo Data
}
