#' Fit model of boxcox deviation scores as function of continuous ratings
#' @description Fit an lmer model of the box-cox transformed deviation from the meta-analytic mean scores as a function of continuous peer-review ratings
#'
#' @param .data Data for model fitting
#' @param outcome outcome variable, unquoted.
#' @param outcome_var Variance of the `outcome` variable
#' @param interceptless A logical relating to whether the model should be interceptless or not. Defaults to `FALSE`.
#'
#' @return An object of class `lme4::lmerMod-class`
#' @export
#' @family Model fitting and meta-analysis
#' @importFrom lme4 lmer
#' @importFrom rlang ensym new_formula expr
#' @import dplyr
#' @importFrom forcats fct_relevel
fit_boxcox_ratings_cat <- function(.data, outcome, outcome_var, interceptless = FALSE) {
  cli::cli_h2(c("Fitting lmer with categorical ratings predictor on box_cox_transformed outcomes"))
  # Example Usage:
  # library(tidyverse);library(targets);library(metafor)
  # tar_load(meta_analysis_outputs)
  # meta_analysis_outputs$data[[1]] %>%
  #   fit_boxcox_ratings_cat(.,
  # outcome = box_cox_abs_deviation_score_estimate,
  #                                   outcome_var = VZr, interceptless = FALSE)

  # TODO @egouldo stopifnot data doesn't contain variables named eval(box_cox_outcome_var), eval(sampling_variance_var), review_data
  # TODO @egouldo unnest and then check stopifnot: RateAnalysis, ReviewerId, study_id.
  data_tbl <-
    .data %>%
    unnest(cols = c(review_data)) %>%
    select(
      study_id,
      ReviewerId,
      PublishableAsIs,
      starts_with("box_cox_"), # @TODO - delete this row?
      {{ outcome }},
      {{ outcome_var }}
    ) %>%
    ungroup() %>%
    mutate(
      PublishableAsIs = forcats::fct_relevel(PublishableAsIs, c(
        "deeply flawed and unpublishable",
        "publishable with major revision",
        "publishable with minor revision",
        "publishable as is"
      )),
      obs_id = 1:n()
    )

  if (interceptless == FALSE) {
    f <- rlang::new_formula(
      rlang::ensym(outcome),
      expr(
        PublishableAsIs +
          (1 | ReviewerId) # + (1 | study_id ) RE ommitted due to convergence issues
      )
    )

    mod <- lme4::lmer(formula = f, data = data_tbl)
  } else {
    ( # interceptless: for plotting

      mod <- lme4::lmer( #TODO implement same inject approach as fit_boxcox_ratings_cont
        rlang::new_formula(
          rlang::ensym(outcome),
          expr(-1 + PublishableAsIs + (1 | ReviewerId))
        ),
        data = data_tbl
      )
    )
  }

  return(mod)
}

#' Possibly [fit_boxcox_ratings_cat()]
#' @description Wrapper for [fit_boxcox_ratings_cat()] that returns `NA` if an error is encountered
#' @keywords internal
#' @family Model fitting and meta-analysis
#' @importFrom purrr possibly
poss_fit_boxcox_ratings_cat <- purrr::possibly(fit_boxcox_ratings_cat,
  otherwise = NA,
  quiet = FALSE
)
