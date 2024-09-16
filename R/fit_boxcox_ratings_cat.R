#' Fit model of boxcox deviation scores as function of continuous ratings
#' @description Fit an lmer model of the box-cox transformed deviation from the meta-analytic mean scores as a function of continuous peer-review ratings
#'
#' @param data Data for model fitting
#' @param outcome outcome variable, unquoted.
#' @param outcome_var Variance of the `outcome` variable
#' @param interceptless A logical relating to whether the model should be interceptless or not. Defaults to `FALSE`.
#'
#' @return An object of class `lme4::lmerMod-class`
#' @details The model is fitted using the [lme4::lmer()] function with the outcome variable as the response variable and the categorical ratings predictor as the fixed effect `PublishableAsIs`. The model is fitted with a random effect of `study_id` to account for the repeated observations within analyses due to multiple peer-reviews per analysis.
#' 
#' Note that the variables `study_id`, `outcome`, `outcome_var`, and the list-column `review_data` must be present in `data`. The `review_data` column is unnested (see [tidyr::unnest()]) by the function before fitting the model. Within `review_data`, the variables `PublishableAsIs` and `ReviewerId` must be present.
#' @export
#' @family Model fitting and meta-analysis
#' @importFrom lme4 lmer
#' @importFrom rlang ensym new_formula expr
#' @import dplyr
#' @importFrom forcats fct_relevel
#' @importFrom tidyr unnest
#' @importFrom cli cli_h2
#' @importFrom pointblank expect_col_exists
#' @examples
#'   # Example Usage:
#'   # library(tidyverse);library(targets);library(metafor)
#'   # tar_load(meta_analysis_outputs)
#'   # meta_analysis_outputs$data[[1]] %>%
#'   #   fit_boxcox_ratings_cat(.,
#'   # outcome = box_cox_abs_deviation_score_estimate,
#'   #                                   outcome_var = VZr, interceptless = FALSE)
fit_boxcox_ratings_cat <- function(data, outcome, outcome_var, interceptless = FALSE, ..., env = rlang::caller_env()) {
  
  # ----- Argument Checks -----
  stopifnot(
    is.data.frame(data),
    is.logical(interceptless)
  )
  
  pointblank::expect_col_exists(
    data, 
    columns = c(
                {{outcome}},
                {{outcome_var}},
                study_id,
                review_data)
  )
  
  # ----- Fit model -----
  cli::cli_h2(c("Fitting {.fn lmer} with categorical ratings predictor {.arg PublishableAsIs} on Box-Cox transformed {.arg outcome}:  {.arg {rlang::enexpr(outcome)}}"))
  
  data_tbl <-
    data %>%
    unnest(cols = c(review_data)) %>%
    pointblank::col_exists(
      columns = c("PublishableAsIs", "ReviewerId"),
    ) %>%
    select(
      study_id,
      ReviewerId,
      PublishableAsIs,
      {{ outcome }},
      {{ outcome_var }}
    ) %>%
    ungroup() %>%
    mutate(
      PublishableAsIs = 
        forcats::fct_relevel(
          PublishableAsIs, 
          c("deeply flawed and unpublishable",
            "publishable with major revision",
            "publishable with minor revision",
            "publishable as is")
        ),
      obs_id = 1:n()
    )
  
  if (interceptless == FALSE) {
    f <- rlang::new_formula(
      rlang::ensym(outcome),
      rlang::expr(
        PublishableAsIs +
          (1 | ReviewerId)
      ),
      env = env
    )
    
  } else {
    # interceptless: for plotting
    f <- rlang::new_formula(
      rlang::ensym(outcome),
      rlang::expr(
        -1 + PublishableAsIs + (1 | ReviewerId)
      ),
      env = env
    )
  }
  
  mod <- rlang::inject(lme4::lmer(!!f, data = data_tbl, ...))
  
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
