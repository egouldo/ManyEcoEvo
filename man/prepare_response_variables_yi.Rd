% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_response_variables_yi.R
\name{prepare_response_variables_yi}
\alias{prepare_response_variables_yi}
\title{Prepare response variable data for nested ManyEcoEvo dataset - out of sample predictions only}
\usage{
prepare_response_variables_yi(
  ManyEcoEvo,
  estimate_type = character(1L),
  param_table = NULL
)
}
\arguments{
\item{ManyEcoEvo}{Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset}

\item{estimate_type}{A character string of length 1, equal to either "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.}

\item{param_table}{A table of parameters \(mean, sd\) for \emph{most} response variables used by analysts. This tibble is pulled from the named object exported by \verb{ManyEcoEvo::}. but can be overwritten with the users's own \code{param_table} dataset.}
}
\value{
A tibble of nested list-columns
}
\description{
Prepare response variable data for nested ManyEcoEvo dataset - out of sample predictions only
}
\details{
Operates on nested list-columns of data
}
\seealso{
Other targets-pipeline functions: 
\code{\link{generate_outlier_subsets}()},
\code{\link{generate_rating_subsets}()},
\code{\link{make_viz}()},
\code{\link{prepare_ManyEcoEvo_yi}()},
\code{\link{prepare_ManyEcoEvo}()},
\code{\link{prepare_response_variables}()},
\code{\link{prepare_review_data}()}

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
\code{\link{prepare_response_variables}()},
\code{\link{summarise_analysis_types}()},
\code{\link{summarise_conclusions}()},
\code{\link{summarise_model_composition}()},
\code{\link{summarise_reviews}()},
\code{\link{summarise_sorensen_index}()},
\code{\link{summarise_variable_counts}()}
}
\concept{Multi-dataset Wrapper Functions}
\concept{targets-pipeline functions}
