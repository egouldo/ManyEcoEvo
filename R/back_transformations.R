# --- Back-transformation Conversion Functions ---
# convertion functions
# we assume estimates are normally distributed
# TODO Question - all natural log (no log10)

#' Back transform beta estimates for models with log-link
#'
#' @param beta Analyst beta estimate or yi estimate
#' @param se Standard error of analyst's beta estimate or yi estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
log_back <- function(beta, se, sim){
  
  simulated <- rnorm(sim, beta, se)
  original <- exp(simulated) %>% # exponential = inverse of log
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for log-transformed effect sizes or out-of-sample predictions, using {.val {sim}} simulations.")
  return(set)
}

#' Back transform beta estimates for models with logit-link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
logit_back <- function(beta, se, sim){
  
  simulated <- rnorm(sim, beta, se)
  original <- plogis(simulated) %>%  # invlogit
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for logit-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' Back transform beta estimates for models with probit-link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
probit_back <- function(beta, se, sim){
  
  simulated <- rnorm(sim, beta, se)
  original <- pnorm(simulated) %>% # inv-probit
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for probit-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' Back transform beta estimates for models with $1/x$ link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
inverse_back <- function(beta, se, sim){
  
  simulated <- rnorm(sim, beta, se)
  original <- 1/simulated %>% # inverse
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for inverse-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' Back transform beta estimates for models with $x^2$-link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
square_back <- function(beta, se, sim){
  
  simulated <- rnorm(sim, beta, se)
  original <- sqrt(simulated) %>%  # inverse of x^2
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for squared effect sizes or out-of-sample predictions.")
  return(set)
}

#' Back transform beta estimates for models with $x^3$-link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
cube_back <- function(beta, se, sim){
  simulated <- rnorm(sim, beta, se)
  original <- pracma::nthroot(simulated, n = 3) %>% # inverse of x^3, use non-base to allow for -ve numbers
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for cubed effect sizes")
  return(set)
}

#' Back transform beta estimates for models with identity-link
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's beta estimate.
#' @param sim numeric vector of length 1. number of simulations.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
identity_back <- function(beta, se, sim){ #identity (typo) TODO
  simulated <- rnorm(sim, beta, se)
  original <- simulated %>% #  no transformation
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_info("No back-transformation required, identity link used.")
  return(set)
}


#' Back transform beta estimates for models with power-link
#' @param beta Analyst beta estimate. Numeric vector of length 1.
#' @param se Standard error of analyst's beta estimate. Numeric vector of length 1.
#' @param sim Number of simulations. Numeric vector of length 1. 
#' @param n Numeric vector of length 1 describing power which values were raised to in transformation.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
power_back <- function(beta, se, sim, n){ 
  simulated <- rnorm(sim, beta, se)
  original <- pracma::nthroot(simulated, n = n) %>% # inverse of x^n, use non-base to allow for -ve numbers
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for ^{n} effect sizes")
  return(set)
}

#' Back transform beta estimates or out-of-sample predictions from models whose response variable has been divided by some number
#' @param beta Analyst beta estimate. Numeric vector of length 1.
#' @param se Standard error of analyst's beta estimate. Numeric vector of length 1.
#' @param sim Number of simulations. Numeric vector of length 1. 
#' @param n Numeric vector of length 1 describing the value of the divisor.
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
divide_back <- function(beta, se, sim, n){ 
  simulated <- rnorm(sim, beta, se)
  original <- simulated*n %>% 
    na.omit()
  m_est <- mean(original, na.rm = TRUE)
  se_est <- sd(original, na.rm = TRUE)
  quantiles <- quantile(original, 
                        c(0.025, 0.975), 
                        na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, 
                    se_origin =  se_est, 
                    lower = quantiles[[1]], 
                    upper = quantiles[[2]])
  
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  
  cli::cli_alert_success("Applied back-transformation for ^{n} effect sizes or out of sample predictions.")
  
  return(set)
}

#' Back transform beta estimates or out-of-sample predictions from models whose response variable has been transformed by the square root
#' @param beta Analyst beta estimate. Numeric vector of length 1.
#' @param se Standard error of analyst's beta estimate. Numeric vector of length 1.
#' @param sim Number of simulations. Numeric vector of length 1. 
#'
#' @return data frame containing the mean estimate, its standard error, and quantiles
#' @export
#' @family back transformation
square_root_back <- function(beta, se, sim){ 
  simulated <- rnorm(sim, beta, se)
  original <- simulated^2 %>% 
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE) 
  set <- data.frame(mean_origin = m_est, se_origin =  se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if(flatten_dbl(set) %>% 
     map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>% 
     any()){
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for square-root transformed effect sizes or out-of-sample predictions.")
  
  return(set)
}
