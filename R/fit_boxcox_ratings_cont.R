#' Fit model of boxcox deviation scores as function of continuous ratings
#' @description Fit an lmer model of the box-cox transformed deviation from the meta-analytic mean scores as a function of continuous peer-review ratings
#'
#' @param data Data for model fitting
#' @param outcome outcome variable, unquoted.
#' @param outcome_var Variance of the `outcome` variable
#'
#' @return An object of class `lme4::lmerMod-class`
#' @export
#' @family Model fitting and meta-analysis
#' @import dplyr
#' @importFrom lme4 lmer
#' @importFrom rlang ensym new_formula inject expr caller_env
#' @importFrom tidyr unnest
#' @importFrom cli cli_h2
fit_boxcox_ratings_cont <- function(data, outcome, outcome_var, ..., env = rlang::caller_env()) {
  
  # ----- Argument Checks -----
  stopifnot(
    is.data.frame(data)
  )
  pointblank::expect_col_exists(
    data, 
    columns = c(starts_with("box_cox_abs_"),
                {{outcome}},
                {{outcome_var}},
                study_id,
                review_data)
  )
  
  # ----- Fit model -----
  cli::cli_h2(c("Fitting {.fn lmer} with continuous ratings predictor {.arg RateAnalysis} on Box-Cox transformed {.arg outcome}: {.arg {rlang::enexpr(outcome)}}"))
  data_tbl <- # TODO, consider extracting unnesting outside of this fn.
    data %>%
    select(
      study_id,
      review_data,
      starts_with("box_cox_"), #TODO - delete this row?
      {{ outcome }},
      {{ outcome_var }}
    ) %>%
    unnest(cols = c(review_data)) %>%
    ungroup() %>%
    pointblank::col_exists(columns = c("RateAnalysis", "ReviewerId")) %>% 
    mutate(., obs_id = 1:nrow(.)) 
  
  f <- rlang::new_formula(
    rlang::ensym(outcome),
    rlang::expr(
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
