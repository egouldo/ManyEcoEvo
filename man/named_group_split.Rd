% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{named_group_split}
\alias{named_group_split}
\title{Split data frame by groups and name elements}
\usage{
named_group_split(.data, grouping_variable)
}
\arguments{
\item{.data}{A tbl.}

\item{grouping_variable}{Unquoted variable name to group columns by}
}
\value{
A named list of tibbles. Each tibble contains the rows of .tbl for the associated group and all the columns, including the grouping variables. Note that this returns a list_of which is slightly stricter than a simple list but is useful for representing lists where every element has the same type.
}
\description{
Split data frame by groups and name elements
}
\details{
Function will fail if character string is provided to \code{grouping_variable} instead of bare variable name.
}
\examples{
named_group_split(ManyEcoEvo::euc_data , Property)
named_group_split(ManyEcoEvo::blue_tit_data, hatch_Area)
}
