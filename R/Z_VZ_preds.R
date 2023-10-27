#' Calculate Z and VZ of out-of-sample predictions
#'
#' @param yi point-estimate prediction, on response scale
#' @param yi_se standard error of \code{yi}
#' @param mu_p Population mean for variable estimated by \code{yi}
#' @param sd_p Population standard deviation for variable estimated by \code{yi}
#'
#' @return A tibble with columns \code{Z} and \code{VZ}
#' @family Analysis-level functions
#' @export
Z_VZ_preds <- function(yi, yi_se, mu_p, sd_p ){
  #TODO should we pass in whole DF as arg instead of yi + yi_se??
  # We want to be able to keep the values linked to their corresponding
  # scenario_ value!
  na_args <- purrr::discard(c(yi, yi_se, mu_p, sd_p), is.na) %>% 
    length()
  
  if(na_args < 3){
    cli::cli_alert_danger("Required values for computing Z/VZ predictions missing:")
    cli::cli_alert_warning("Returning {.val NA} for quadrupple:")
    cli::cli_ol(c("yi {.val {yi}},",
                  "yi_se {.val {yi_se}},", 
                  "mu_p {.val {mu_p}}",
                  "sd_p {.val {sd_p}}"))
    return(NA) #TODO single NA value OR, tibble with NA's?
  }
  
  
  Z <- (yi-mu_p)/sd_p
  VZ <- yi_se/sd_p
  
  return(tibble(Z, VZ)) 
}