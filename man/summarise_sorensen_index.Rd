% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_sorensen_index.R
\name{summarise_sorensen_index}
\alias{summarise_sorensen_index}
\title{Summarise Mean Sorensen's Index Estimates}
\usage{
summarise_sorensen_index(ManyEcoEvo_results, ManyEcoEvo_yi_results)
}
\arguments{
\item{ManyEcoEvo_results}{A tibble of \code{ManyEcoEvo_results}}

\item{ManyEcoEvo_yi_results}{A tibble of \code{ManyEcoEvo_yi_results}}
}
\value{
A tibble of aggregate summary statistics (\code{mean}, \code{sd}, \code{min}, \code{max}) for mean Sorensen's index estimates across each \code{subset} and \code{dataset}.
}
\description{
Summarise Mean Sorensen's Index Estimates
}
\details{
Generates data summary with \code{\link[=summarise_sorensen_index_data]{summarise_sorensen_index_data()}}.
}
\examples{
summarise_sorensen_index(ManyEcoEvo_results,ManyEcoEvo_yi_results)
}
\seealso{
Other Multi-dataset Wrapper Functions: 
\code{\link{apply_VZ_exclusions}()},
\code{\link{compute_MA_inputs}()},
\code{\link{generate_exclusion_subsets}()},
\code{\link{generate_outlier_subsets}()},
\code{\link{generate_rating_subsets}()},
\code{\link{generate_yi_subsets}()},
\code{\link{make_viz}()},
\code{\link{meta_analyse_datasets}()},
\code{\link{prepare_ManyEcoEvo_yi}()},
\code{\link{prepare_ManyEcoEvo}()},
\code{\link{prepare_response_variables_yi}()},
\code{\link{prepare_response_variables}()},
\code{\link{summarise_analysis_types}()},
\code{\link{summarise_conclusions}()},
\code{\link{summarise_model_composition}()},
\code{\link{summarise_reviews}()},
\code{\link{summarise_variable_counts}()}
}
\author{
Hannah S. Fraser

Elliot Gould
}
\concept{Multi-dataset Wrapper Functions}
