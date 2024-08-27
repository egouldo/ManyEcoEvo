#' Calculate deviation from meta-analytic mean
#' @description Calculate the absolute deviation of the standardised effect size from the meta-analytic mean
#'
#' @param data Dataset containing the column `"Z"` \(standardised out of sample prediction piont estimates\) or standardised effect-sizes, `"Zr"`.
#' @param meta_analytic_model Fitted meta-analytic model of class `rma`
#'
#' @return `data` but with an additional column of deviation score estimates `"abs_deviation_score_estimate"`
#' @export
#'
#' @examples
#' # Example Usage: TODO - this is incorrect bc box-cox transformation is included in meta_analyse_datasets_wrapper
#' # library(tidyverse); library(targets)
#' # tar_load(meta_analysis_outputs)
#' # calculate_deviation_score(meta_analysis_outputs$data[[1]],
#' #                           meta_analysis_outputs$MA_mod[[1]])
#' @import dplyr
#' @importFrom cli cli_h2
calculate_deviation_score <- function(data, meta_analytic_model) {
  cli::cli_h2(c("Calculating absolute deviation scores from standardised effect sizes"))

  stopifnot("rma" %in% class(meta_analytic_model))
  # TODO build in checks:
  # stop if not data is not a dataframe
  # stop if not data does not contain the column Zr/Z


  meta_analytic_mean <- meta_analytic_model$beta[[1]] # TODO should this be the fitted val from the model, or the raw score?
  # meta_analytic_se <- meta_analytic_model$se[[1]]

  out <- data %>%
    ungroup() %>%
    mutate(across(
      .cols = starts_with("Z"),
      .fns = ~ abs(.x - {
        meta_analytic_mean
      }),
      .names = "abs_deviation_score_estimate"
    )) # abs_deviation_score_se = abs({VZr - meta_analytic_se})?
  return(out)
}
