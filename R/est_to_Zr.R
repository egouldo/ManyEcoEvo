#' Convert estimate to Zr
#' @details Convert beta estimate, standard error and degrees of freedom into `r` and then into `Zr` and its sampling variance
#' @param beta_estimate numeric vector, beta estimate of an analysis of either blue tit or eucalyptus dataset
#' @param beta_SE numeric vector, standard error of the `beta_estimate`
#' @param adjusted_df numeric vector of the adjusted degrees of freedom for the analysis generating `beta_SE` and `beta_estimate`
#' @return A named list of length 2 containing converted Zr and VZr values
#' @export
#' @family analysis-values
est_to_zr <- function(beta_estimate, beta_SE, adjusted_df){
  na_args <- purrr::discard(c(beta_estimate, beta_SE, adjusted_df), is.na) %>% 
    length()
  
  if(na_args < 3){
    cli::cli_alert_danger("Required values for computing standardised effect sizes missing:")
    cli::cli_alert_warning("Returning {.val NA} for tupple:")
    cli::cli_ol(c("beta_estimate {.val {beta_estimate}},",
                  "beta_se {.val {beta_SE}},", 
                  "adjusted_df {.val {adjusted_df}}."))
    return(NA)
  }
  
  t_val <- beta_estimate/beta_SE
  r_val <-  t_val/sqrt((t_val^2 + adjusted_df))
  zr <- atanh(r_val)
  var <- 1/(adjusted_df) # assume var is 1/df
  set <- data.frame(Zr = zr, VZr = var)
  if(purrr::flatten_dbl(set) %>% 
     purrr::map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during conversion of standardised effect sizes for tupple:.")
    cli::cli_ol(c("beta_estimate {.val {beta_estimate}},",
                  "beta_se {.val {beta_SE}},", 
                  "adjusted_df {.val {adjusted_df}}."))
  }
  return(set)
}
