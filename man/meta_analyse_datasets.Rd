% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_analyse_datasets.R
\name{meta_analyse_datasets}
\alias{meta_analyse_datasets}
\title{Meta-analyses multiple datasets or subsets of datasets of analyst data}
\usage{
meta_analyse_datasets(MA_data)
}
\arguments{
\item{MA_data}{A nested-dataframe grouped by \code{dataset} and / or \code{exclusion_set}, \code{estimate_type}, containing the list-column of prepared analyst subset data \code{effects_analysis} ready for meta-analysis.}
}
\value{
A nested dataframe with all columns of object parsed to arg \code{MA_data}, but with additional columns for the results of each analysis: \code{MA_mod}, \code{sorensen_glm}, \code{box_cox_ratings_cont}, \code{box_cox_ratings_cat}, \code{box_cox_rating_cat_no_int}, \code{uni_mixed_effects}
}
\description{
Runs all meta-analyses and regression models for the ManyEcoEvo project analysis, including:
\itemize{
\item Fitting univariate / fixed-effects meta-analysis
\item Calculating the deviation of every effect size / point-estimate from the meta-analytic mean for all data subsetes
\item The absolute, box-cox transformed deviation scores
\item A univariate GLM regression of the transformed deviation scores on the sorensen diversity indices
\item A univariate GLM regression of the transformed deviation scores on the continuous peer-review ratings
\item A univariate GLM regression of the transformed deviation scores on the categorical peer-review ratings
\item A univariate GLM regression of the transformed deviation scores on a binary variable corresponding to whether the analysis was a mixed-effects model (i.e. GLM with random-effects) or not.
\item To be implemented: a multivariate regression #TODO
\item The deviation scores on transformed categorical ratings but with no intercept (for nice plotting / ecploration).
}
}
\examples{
  # meta_analyse_datasets(targets::tar_read(round_2_survey_meta_analysis))
  # remove_problematic_responses <- 
  #   preprocessed_dataset \%>\% 
  #   dplyr::filter(dataset == "eucalyptus",
  #          (max(VZr, na.rm = TRUE) == VZr)) TODO, do we need to include now that INF's removed?
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
\code{\link{prepare_ManyEcoEvo_yi}()},
\code{\link{prepare_ManyEcoEvo}()},
\code{\link{prepare_response_variables_yi}()},
\code{\link{prepare_response_variables}()},
\code{\link{summarise_analysis_types}()},
\code{\link{summarise_conclusions}()},
\code{\link{summarise_model_composition}()},
\code{\link{summarise_reviews}()},
\code{\link{summarise_sorensen_index}()},
\code{\link{summarise_variable_counts}()}
}
\concept{Multi-dataset Wrapper Functions}
