#' Fit model of boxcox deviation scores as function of continuous ratings
#' @description Fit an lmer model of the box-cox transformed deviation from the meta-analytic mean scores as a function of continuous peer-review ratings
#'
#' @param .data Data for model fitting
#' @param outcome outcome variable, unquoted.
#' @param outcome_var Variance of the `outcome` variable
#' @param interceptless A logical relating to whether the model should be interceptless or not. Defaults to `FALSE`.
#'
#' @return An object of class lmer.
#' @export
fit_boxcox_ratings_ord <- function(.data, outcome, outcome_var, interceptless = FALSE) {
  cli::cli_h2(glue::glue("Fitting lmer with ordinal ratings predictor on box_cox_transformed outcomes"))

  # Example Usage:
  # library(tidyverse);library(targets);library(metafor)
  # tar_load(meta_analysis_outputs)
  # meta_analysis_outputs$data[[1]] %>%
  #   fit_boxcox_ratings_ord(.,
  # outcome = box_cox_abs_deviation_score_estimate,
  #                                 outcome_var = VZr, interceptless = FALSE)

  data_tbl <-
    .data %>%
    unnest(cols = c(review_data)) %>%
    select(
      study_id,
      ReviewerId,
      PublishableAsIs,
      starts_with("box_cox_"),
      {{ outcome }},
      {{ outcome_var }}
    ) %>%
    ungroup() %>%
    mutate(PublishableAsIs = as.numeric(
      forcats::fct_relevel(PublishableAsIs, c(
        "deeply flawed and unpublishable",
        "publishable with major revision",
        "publishable with minor revision",
        "publishable as is"
      )),
      obs_id = 1:n()
    ))

  if (interceptless == FALSE) {
    f <- rlang::new_formula(
      rlang::ensym(outcome),
      expr(
        PublishableAsIs +
          (1 | ReviewerId) # + (1 | study_id ) RE ommitted due to convergence issues
      )
    )
    mod <- lme4::lmer(f,
      data = data_tbl # ,
      # weights = I(1/pull(data_tbl,{{outcome_var}}))
    )
  } else {
    ( # interceptless: for plotting
      mod <- lme4::lmer(
        rlang::new_formula(
          rlang::ensym(outcome),
          expr(-1 + PublishableAsIs + (1 | ReviewerId))
        ), #+ (1 | study_id) #problem with the groups
        data = data_tbl # ,
        # weights = I(1/pull(data_tbl,{{outcome_var}}))
      )
    )
  }

  return(mod)
}


poss_fit_boxcox_ratings_ord <- purrr::possibly(fit_boxcox_ratings_ord,
  otherwise = NA,
  quiet = FALSE
)
