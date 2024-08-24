# --- Back-transformation Conversion Functions ---
#' Back-transform effect-sizes to response scale.
#' @description
#' Transforms effect-sizes and their standard errors to the response scale.
#' 
#' @details We assume analysts' estimates are normally distributed. Each function uses a normal distribution to simulate the a distribution of effect-sizes and their standard errors. Next this distribution is back-transformed to the desired response scale. The mean `m_est`, standard error `se_est`, and quantiles (`lower` and `upper`) of the back-transformed distribution are returned within a dataframe.
#' @param beta Analyst beta estimate
#' @param se Standard error of analyst's effect size estimate \eqn{\beta} 
#' or out-of-sample prediction estimate \eqn{y_i}.
#' @param sim numeric vector of length 1. number of simulations.
#' @return data frame containing the mean estimate, its standard error, and quantiles.
#' @family Back-transformation
#' @importFrom purrr map_lgl flatten_dbl
#' @importFrom cli cli_alert_danger cli_alert_success cli_alert_info
#' @name back
NULL
#> NULL

#' @describeIn back Back transform beta estimates for models with log-link
#' @export
#' @family Back-transformation
log_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- exp(simulated) %>% # exponential = inverse of log
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for log-transformed effect sizes or out-of-sample predictions, using {.val {sim}} simulations.")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with logit-link
#' @export
#' @family Back-transformation
logit_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- plogis(simulated) %>% # invlogit
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for logit-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with probit-link
#' @export
#' @family Back-transformation
probit_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- pnorm(simulated) %>% # inv-probit
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for probit-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with \eqn{1/x} link
#' @export
#' @family Back-transformation
inverse_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- 1 / simulated %>% # inverse
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for inverse-transformed effect sizes or out-of-sample predictions")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with \eqn{x^2}-link
#' @export
#' @family Back-transformation
square_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- sqrt(simulated) %>% # inverse of x^2
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for squared effect sizes or out-of-sample predictions.")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with \eqn{x^3}-link
#' @export
#' @family Back-transformation
cube_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- pracma::nthroot(simulated, n = 3) %>% # inverse of x^3, use non-base to allow for -ve numbers
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for cubed effect sizes")
  return(set)
}

#' @describeIn back Back transform beta estimates for models with identity-link
#' @export
#' @family Back-transformation
identity_back <- function(beta, se, sim) { # identity (typo) TODO
  simulated <- rnorm(sim, beta, se)
  original <- simulated %>% #  no transformation
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_info("No back-transformation required, identity link used.")
  return(set)
}


#' @describeIn back Back transform beta estimates for models with power-link
#' @export
#' @family Back-transformation
power_back <- function(beta, se, sim, n) {
  simulated <- rnorm(sim, beta, se)
  original <- pracma::nthroot(simulated, n = n) %>% # inverse of x^n, use non-base to allow for -ve numbers
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for ^{n} effect sizes")
  return(set)
}

#' @describeIn back Back transform beta estimates or out-of-sample predictions from models whose response variable has been divided by some number, `n`.
#' @param n Denominator used by analyst to divide the response variable.
#' @export
#' @family Back-transformation
divide_back <- function(beta, se, sim, n) {
  simulated <- rnorm(sim, beta, se)
  original <- simulated * n %>%
    na.omit()
  m_est <- mean(original, na.rm = TRUE)
  se_est <- sd(original, na.rm = TRUE)
  quantiles <- quantile(original,
    c(0.025, 0.975),
    na.rm = TRUE
  )
  set <- data.frame(
    mean_origin = m_est,
    se_origin = se_est,
    lower = quantiles[[1]],
    upper = quantiles[[2]]
  )

  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }

  cli::cli_alert_success("Applied back-transformation for ^{n} effect sizes or out of sample predictions.")

  return(set)
}

#' @describeIn back Back transform beta estimates or out-of-sample predictions from models whose response variable has been transformed by the square root
#' @export
#' @family Back-transformation
square_root_back <- function(beta, se, sim) {
  simulated <- rnorm(sim, beta, se)
  original <- simulated^2 %>%
    na.omit()
  m_est <- mean(original)
  se_est <- sd(original)
  quantiles <- quantile(original, c(0.025, 0.975), na.rm = TRUE)
  set <- data.frame(mean_origin = m_est, se_origin = se_est, lower = quantiles[[1]], upper = quantiles[[2]])
  if (flatten_dbl(set) %>%
    map_lgl(.f = ~ is.na(.x) | is.nan(.x) | is.infinite(.x)) %>%
    any()) {
    cli::cli_alert_danger("{.val NA}, {.val Inf} or {.val NaN} returned during back-transformation of effect sizes and standard errors.")
  }
  cli::cli_alert_success("Applied back-transformation for square-root transformed effect sizes or out-of-sample predictions.")

  return(set)
}
