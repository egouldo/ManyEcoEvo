#' Fit univariate meta-analysis with metafor
#'
#' @param Zr Standardized beta-estimate
#' @param VZr Standardised standard error of the beta estimate
#' @param data Dataframe containing estimates and variances
#' @param slab Vector of case identifiers
#'
#' @return A fitted model of class rma
#' @export
#'
#' @examples
#' # library(tidyverse);library(targets);library(metafor)
#' # source("R/functions.R")
#' # tar_read(round_2_survey_meta_analysis) %>%
#' #   filter(dataset == "eucalyptus") %>%
#' #   filter(!is.na(Zr),
#' #          !is.na(VZr),
#' #          !is.infinite(Zr),
#' #          !is.infinite(VZr)) %>%
#' # fit_metafor_uni(Zr = .$Zr,
#' #                      VZr = .$VZr,
#' #                      data = .,
#' #                      slab = .$study_id)
fit_metafor_uni <- function(Zr, VZr, data, slab) {
  cli::cli_h2(glue::glue("Fitting univariate metaregression"))
  metafor::rma(
    yi = Zr,
    vi = VZr,
    data = data,
    control = list(maxiter = 1000),
    slab = slab
  )
}

poss_fit_metafor_uni <- purrr::possibly(fit_metafor_uni,
  otherwise = NA,
  quiet = FALSE
)
