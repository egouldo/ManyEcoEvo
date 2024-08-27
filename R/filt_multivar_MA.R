#' Fit a multivariate meta-regression model
#'
#' @description Fit a multivariate meta-regression model that models the effect of peer-review ratings on the deviation from the meta-analytic mean (both continuous and categorical ratings), mean Sorensen's index, and/or whether the analysis uses a mixed effects model, or not.
#'
#' @param data_tbl Data for model fitting
#' @param ... Additional arguments passed to `lmer`
#' @param env Environment in which to evaluate the formula, defaults to the calling environment
#' @param N threshold for the number of analyses that must have been conducted using mixed effects models to include the binary predictor `mixed_model` in the meta-regression. Defaults to 5.
#'
#' @return An object of class lmer.
#'
#' @export
#' @importFrom rlang new_formula caller_env expr inject
#' @importFrom lme4 lmer
#' @importFrom pointblank test_col_vals_gte expect_col_exists
#' @import dplyr
#' @importFrom purrr list_flatten list_c
#' @importFrom tibble enframe unite
#' @importFrom cli cli_alert_info cli_bullets cli_h2 style_italic
#' @importFrom glue glue
#' @details
#' Depending on whether enough analyses in `data_tbl` have been conducted with the `mixed_model` variable, the function will fit a model with or without the predictor `mixed_model`.
#'
#' Expects the following columns in `data_tbl`:
#'
#' - `RateAnalysis`: continuous peer-review ratings
#' - `PublishableAsIs`: categorical peer-review ratings
#' - `mean_diversity_index`: mean Sorensen's index
#' - `box_cox_abs_deviation_score_estimate`: response variable, Box-Cox transformed deviation from the meta-analytic mean effect-size for each analysis
#' - `mixed_model`: binary variable indicating whether the analysis used a mixed effects model or not
#' - `ReviewerId`: reviewer identifier
#' @family Model fitting and meta-analysis
fit_multivar_MA <- function(data_tbl, N = 5, ..., env = rlang::caller_env()) {
  data_tbl %>%
    pointblank::expect_col_exists(columns = c(
      box_cox_abs_deviation_score_estimate,
      RateAnalysis, PublishableAsIs,
      mean_diversity_index,
      ReviewerId,
      mixed_model
    ))
  # Define Models
  f1 <- rlang::new_formula(rlang::expr(box_cox_abs_deviation_score_estimate),
    rlang::expr(RateAnalysis +
      PublishableAsIs +
      mean_diversity_index +
      (1 | ReviewerId)),
    env = env
  )

  f2 <- rlang::new_formula(rlang::expr(box_cox_abs_deviation_score_estimate),
    rlang::expr(RateAnalysis +
      PublishableAsIs +
      mean_diversity_index +
      mixed_model +
      (1 | ReviewerId)),
    env = env
  )

  cli::cli_h2("Fitting multivariate meta-regression model")

  pass_threshold <-
    data_tbl %>%
    count(mixed_model) %>%
    pointblank::test_col_vals_gte(n, N)

  cur_group_bullets <- dplyr::cur_group() %>%
    transpose() %>%
    list_flatten() %>%
    enframe() %>%
    mutate(value = list_c(value)) %>%
    unite(group, everything(),
      sep = ": "
    ) %>%
    pull(group)

  if (pass_threshold == TRUE) {
    cli::cli_alert_info(glue::glue(
      "Presence of random effects in analyses ",
      cli::style_italic("included"),
      " as predictor in model for data subset:"
    ))
    cli::cli_bullets(c(setNames(cur_group_bullets, rep("*", length(cur_group_bullets)))))
  } else {
    cli::cli_alert_info(glue::glue(
      "Presence of random effects in analyses ",
      cli::style_italic("excluded"),
      " as predictor in model for data subset:"
    ))
    cli::cli_bullets(c(setNames(cur_group_bullets, rep("*", length(cur_group_bullets)))))
  }

  # TODO MAKE SURE GIVES CORRECT EX
  f <- if (pass_threshold) f2 else f1 # MAKE SURE RETURNS APPROPIRATELY

  mod <- rlang::inject(lme4::lmer(!!f, data = data_tbl, ...))

  return(mod)
}
