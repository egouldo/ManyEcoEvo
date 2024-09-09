#' log transform response-scale yi estimates
#'
#' @param estimate Point estimates on the original response scale
#' @param std.error Standard error of the point estimates on the original response scale
#' @param sim Number of simulations to run when generating distribution to sample from
#' @return A dataframe containing the `mean_log`, `se_log`, `lower` and `upper` 95% CI on the log scale
#' @export
#' @examples log_transform(1.77, 1.01, 1000)
#' @import dplyr
#' @importFrom cli cli_alert_danger cli_alert_success
#' @importFrom purrr map_lgl flatten_dbl
#' @seealso Equivalent in workflow data hierarchy to [Z_VZ_preds()].
log_transform <- function(estimate = numeric(1L), 
                          std.error = numeric(1L), 
                          sim = 10000L) {
  
  simulated <- rnorm(sim, estimate, std.error)
  log_simulated <- suppressWarnings(log(simulated)) %>%
    na.omit()
  
  m_est <- mean(log_simulated)
  se_est <- sd(log_simulated)
  quantiles <- quantile(log_simulated, c(0.025, 0.975), na.rm = TRUE)
  
  out <- data.frame(mean_log = m_est, 
                    se_log = se_est, 
                    lower = quantiles[[1]], 
                    upper = quantiles[[2]])
  
  if (flatten_dbl(out) %>%
      map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
      any()) {
    
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  
  cli::cli_alert_success("Log-transformed out-of-sample predictions, using {.val {sim}} simulations.")
  
  return(out)
}
