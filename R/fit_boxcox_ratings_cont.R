#' Fit model of boxcox deviation scores as function of continuous ratings
#' @description Fit an lmer model of the box-cox transformed deviation from the meta-analytic mean scores as a function of continuous peer-review ratings
#'
#' @param .data Data for model fitting
#' @param outcome outcome variable, unquoted.
#' @param outcome_var Variance of the `outcome` variable
#'
#' @return An object of class `lme4::lmerMod-class`
#' @export
#' @family Model fitting and meta-analysis
#' @examples
#' # Example Usage:
#' # Note, outcome = the name using NSE of the response variable, otucome_var = the
#' # variance associated with that variable.
#' # library(tidyverse);library(targets);library(metafor);library(tidymodels);library(multilevelmod)
#' # tar_load(meta_analysis_outputs)
#' # meta_analysis_outputs$data[[1]] %>%
#' #   fit_boxcox_ratings_cont(.,
#' #                                   box_cox_abs_deviation_score_estimate,
#' #                                   VZr )
#' @import dplyr
#' @importFrom lme4 lmer
#' @importFrom rlang ensym new_formula inject expr caller_env
#' @importFrom tidyr unnest
#' @importFrom cli cli_h2
fit_boxcox_ratings_cont <- function(.data, outcome, outcome_var, ..., env = rlang::caller_env()) {
  cli::cli_h2(c("Fitting metaregression with continuous ratings predictor on box_cox_transformed outcomes"))

  # TODO @egouldo stopifnot data doesn't contain variables named eval(box_cox_outcome_var), eval(sampling_variance_var), review_data
  # TODO @egouldo unnest and then check stopifnot: RateAnalysis, ReviewerId, study_id.

  data_tbl <- # TODO, consider extracting unnesting outside of this fn.
    .data %>%
    select(
      study_id,
      review_data,
      starts_with("box_cox_"), #TODO - delete this row?
      {{ outcome }},
      {{ outcome_var }}
    ) %>%
    unnest(cols = c(review_data)) %>%
    ungroup() %>%
    mutate(., obs_id = 1:nrow(.)) #TODO Is this correct, or should it be done BEFORE the unnesting??

  f <- rlang::new_formula(rlang::ensym(outcome),
    expr(
      RateAnalysis +
        (1 | study_id) # NOTE: ReviewerId removed due to singularity
    ),
    env = env
  )

  mod <- rlang::inject(lme4::lmer(!!f, data = data_tbl, ...))

  return(mod)
}

#' Possibly fit [fit_boxcox_ratings_cont()] model
#' @description Wrapper for [fit_boxcox_ratings_cont()] that returns `NA` if an error is thrown
#' @keywords internal
#' @family Model fitting and meta-analysis
#' @importFrom purrr possibly
poss_fit_boxcox_ratings_cont <- purrr::possibly(fit_boxcox_ratings_cont,
  otherwise = NA,
  quiet = FALSE
)
