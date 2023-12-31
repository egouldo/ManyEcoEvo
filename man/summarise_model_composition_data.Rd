% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_model_composition.R
\name{summarise_model_composition_data}
\alias{summarise_model_composition_data}
\title{Summarise model composition for a single dataframe of out of sample predictions or out or effect sizes}
\usage{
summarise_model_composition_data(data)
}
\arguments{
\item{data}{A dataframe with the variables \code{dataset}, \code{num_fixed_effects}, \code{num_random_effects}, \code{num_interactions}, \code{sample_size}}
}
\value{
A dataframe in tidy format yielding descriptive summary statistics (mean, sd, min and max) for the key variables described in \code{data}, includes the variables \code{subset}, \code{dataset}, \code{variable}, \code{fn}, \code{value}
}
\description{
Summarise model composition for a single dataframe of out of sample predictions or out or effect sizes
}
\examples{
ManyEcoEvo \%>\% 
ungroup \%>\% 
filter(dataset == "blue tit") \%>\% 
select(data) \%>\% 
unnest(data) \%>\% 
summarise_model_composition_data()
}
\author{
Hannah S. Fraser

Elliot Gould
}
