#' Fit Meta-regression with random-effects
#' @description Fit a meta-regression model with random effects using the [metafor::rma.mv()] function from the `metafor` package to a dataframe containing the estimates and variances for the meta-analysis.
#'
#' @param effects_analysis A dataframe containing the estimates and variances for the meta-analysis.
#' @param Z_colname The name of the column containing the estimates.
#' @param VZ_colname The name of the column containing the variances.
#' @param estimate_type The type of estimate to be used in the model. One of `c("Zr", "y50", "y25", "y75", or "yi")`.
#'
#' @return A fitted model of class `c("rma.mv","rma")`.
#' @export
#' @import dplyr
#' @importFrom rlang enquo
#' @import metafor
#' @importFrom pointblank stop_if_not
#' @details
#' This function is a wrapper around the [metafor::rma.mv()] function from the [metafor::] package. It takes a dataframe containing the estimates and variances for the meta-analysis, the name of the column containing the estimates, the name of the column containing the variances, and the type of estimate to be used in the model. It then fits a metaregression model with random-effects using the [metafor::rma.mv()] function called in [fit_metafor_mv()] and returns the fitted model.
#'
#' Nested random effects are included for `TeamIdentifier` and `TeamIdentifier/study_id`.
#' @examples
#' ManyEcoEvo_results$effects_analysis[[1]] %>%
#'   fit_MA_mv(beta_estimate, beta_SE, "Zr")
#' @seealso [fit_metafor_mv()], [metafor::rma.mv()]
#' @family Model fitting and meta-analysis
fit_MA_mv <- function(effects_analysis = data.frame(), Z_colname, VZ_colname, estimate_type = character(1L)) {
  pointblank::stop_if_not(estimate_type %in% c("Zr", "yi", "y25", "y50", "y75"))

  Z <- effects_analysis %>% dplyr::pull({{ Z_colname }})
  VZ <- effects_analysis %>% dplyr::pull({{ VZ_colname }})
  mod <- ManyEcoEvo::fit_metafor_mv(
    estimate = Z,
    variance = VZ,
    estimate_type = estimate_type,
    data = effects_analysis
  )
  return(mod)
}
