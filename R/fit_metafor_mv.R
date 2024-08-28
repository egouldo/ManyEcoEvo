#' Fit Multivariate Metaregression using metafoR
#'
#' @param estimate Numeric vector
#' @param variance Numeric vector
#' @param estimate_type Character vector of either "Zr", "y50", "y25", "y75".
#' @param data Dataframe containing estimates and variances with case id column \code{study_id}.
#'
#' @return Object of class \code{rma.mv}
#' @import metafor
#' @importFrom cli cli_h2
#' @importFrom glue glue
#' @importFrom metafor rma.mv
#' @export
#'
#' @family Model fitting and meta-analysis
fit_metafor_mv <- function(estimate, variance, estimate_type = character(1L), data) {
  
  cli::cli_h2(c("Fitting metaregression"))
  
  match.arg(estimate_type, 
            choices = c("Zr", "y50", "y25", "y75", "yi"), 
            several.ok = FALSE)
  
  if (estimate_type != "Zr") { # you need to put SE^2 for y
    variance <- variance^2
  }
  
  mod <- metafor::rma.mv(
    yi = estimate, # of type "Zr" or "ymed", "y25", "y75"
    V = variance,
    random = list(~ 1 | TeamIdentifier / study_id),
    data = data,
    sparse = TRUE,
    # verbose = TRUE,
    control = list(optimizer = "nloptr", maxeval = 1000),
    slab = data$study_id
  )
  return(mod)
}


#' Fit reduced metaregression model
#'
#' @param estimate Numeric vector
#' @param variance Numeric vector
#' @param estimate_type Character vector of either "Zr", "y50", "y25", "y75".
#' @param data Dataframe containing estimates and variances with case id column `study_id`.
#'
#' @return Object of class `rma.mv`
#' @importFrom metafor rma.mv
#' @import dplyr
#' @importFrom cli cli_h2
#' @export
#' @family Model fitting and meta-analysis
fit_metafor_mv_reduced <- function(estimate, variance, estimate_type = character(1L), data) {
  
  cli::cli_h2(c("Fitting multivariate metaregression"))
  
  match.arg(estimate_type, choices = c("Zr", "y50", "y25", "y75"), several.ok = FALSE)
  
  data <- data %>%
    ungroup() %>%
    mutate(obs_id = 1:n()) # metafor idiosyncrasy
  
  if (estimate_type != "Zr") { # you need to put SE^2 for y
    variance <- variance^2
  }
  
  metafor::rma.mv(
    yi = estimate, # of type "Zr" or "ymed", "y25", "y75"
    V = variance,
    random = list(~ 1 | TeamIdentifier / study_id),
    data = data,
    sparse = TRUE,
    sigma2 = c(0, NA)
  )
}

#' Possibly [fit_metafor_mv()]
#' @description Wrapper for [fit_metafor_mv()] that returns `NA` if an error is thrown
#' @importFrom purrr possibly
#' @keywords internal
#' @family Model fitting and meta-analysis
#' @seealso [fit_metafor_mv()]
poss_fit_metafor_mv <- purrr::possibly(fit_metafor_mv,
                                       otherwise = NA,
                                       quiet = FALSE
)
