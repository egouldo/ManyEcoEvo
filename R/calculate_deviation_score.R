#' Calculate deviation from meta-analytic mean
#' @description Calculate the absolute deviation of the standardised effect size from the meta-analytic mean
#'
#' @param data Dataset containing the column `"Z"` \(standardised out of sample prediction piont estimates\) or standardised effect-sizes, `"Zr"`.
#' @param meta_analytic_model Fitted meta-analytic model of class `rma`
#' @param outcome_colname Column name in `data` containing outcomes used in meta-analysis
#' @return `data` but with an additional column of deviation score estimates `"abs_deviation_score_estimate"`
#' @export
#'
#' @import dplyr
#' @importFrom cli cli_h2
calculate_deviation_score <- function(data, meta_analytic_model, outcome_colname) {
  
  # ----- Argument Checks -----
  stopifnot(
    "rma" %in% class(meta_analytic_model),
    is.data.frame(data)
  )
  
  pointblank::test_col_exists(data, outcome_colname)
  
  meta_analytic_mean <- meta_analytic_model$beta[[1]]
  
  if (any(rlang::is_null(meta_analytic_mean), 
          rlang::is_na(meta_analytic_mean))) {
    cli::cli_abort(
      text = 
        c("{.arg meta_analytic_mean} could not be calculated ",
          "({.val {meta_analytic_mean}}): ",
          "check the meta-analytic model in {.arg meta_analytic_model}."
        )
    )
  }
  
  # ----- Calculate deviation scores -----
  cli::cli_h2(c("Calculating absolute deviation scores from meta-analytic mean"))
  
  cli::cli_alert_info(
    text = 
      c("Using the meta-analytic mean outcome as the reference point:",
        "{.arg meta_analytic_mean} = {.val {round(meta_analytic_mean, 4)}}")
  )
  
  out <- data %>%
    ungroup() %>%
    mutate(across(
      .cols = {{outcome_colname}},
      .fns = ~ abs(.x - meta_analytic_mean),
      .names = "abs_deviation_score_estimate"
    ), 
    meta_analytic_mean = meta_analytic_mean) 
  
  return(out)
}
