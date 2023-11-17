#' Fit multivariate meta-analysis of effect-sizes
#'@description
#' Applies [ManyEcoEvo::fit_metafor_mv()] to fit a multivariate meta-analysis model using the `metafor` package. Nested random effects are included for `TeamIdentifier` and `TeamIdentifier/study_id `.
#'
#' @param effects_analysis A dataframe containing containing effect-sizes  and their variances.
#' @param Z_colname Name of the variable containing standardised effect-sizes.
#' @param VZ_colname Name of the variable containing the variance of standardised effect-sizes.
#' @param estimate_type character string, one of "Zr", "yi", "y25", "y50", "y75".
#'
#' @return A fitted model of class `rma.mv` and `rma`.
#' 
#' @export
#' @importFrom dplyr pull
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @import metafor
#' @importFrom pointblank stopifnot
#'
#' @examples
#' ManyEcoEvo_results$effects_analysis[[1]] %>% 
#' fit_MA_mv(beta_estimate, beta_SE, "Zr")
fit_MA_mv <- function(effects_analysis = data.frame(), Z_colname, VZr_colname, estimate_type = character(1L)){
  stop_if_not(estimate_type %in% c("Zr", "yi", "y25", "y50", "y75"))
  
  Zr <- effects_analysis %>%  pull({{Z_colname}})
  VZr <- effects_analysis %>%  pull({{VZr_colname}})
  mod <- ManyEcoEvo::fit_metafor_mv(estimate = Zr, #TODO why is this a separate function?!
                                    variance = VZr, 
                                    estimate_type = estimate_type, 
                                    data = effects_analysis)
  return(mod)
}
