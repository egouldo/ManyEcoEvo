# Multi-level models

#' Calculate I2 for a multilevel meta-analytic model
#' @details from [http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate](http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate]) described by Nakagawa & Santos, 2012.
#' @param fitted_model of class rma.mv
#'
#' @return A numeric vector of length 1.
#' @export
#' @family multi-level model calculations
calc_I2_ml <- function(fitted_model) {
  W <- diag(1 / fitted_model$vi)
  X <- model.matrix(fitted_model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  out <- 100 * sum(fitted_model$sigma2) / (sum(fitted_model$sigma2) + (fitted_model$k - fitted_model$p) / sum(diag(P)))
  return(out)
}

#' Apportion heterogeneity of a multi-level meta-analytic model
#' @description Estimates how much the total variance $I^2$ can be attributed to between- and within- cluster heterogeneity separately.
#' @details From [http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate](http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate)
#' @param fitted_model
#'
#' @return A named numeric vector of length 2.
#' @export
#' @family multi-level model calculations
apportion_heterogeneity_ml <- function(fitted_model) {
  # Estiamtes how much of the total variance (I^2) can be attributed
  W <- diag(1 / fitted_model$vi)
  X <- model.matrix(fitted_model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  out <- 100 * fitted_model$sigma2 / (sum(fitted_model$sigma2) + (fitted_model$k - fitted_model$p) / sum(diag(P)))
  names(out) <- c("between_cluster", "within_cluster")
  return(out)
}

#' Compare two fitted multi-level models
#' @description Compares two fitted multi-level models and tidies the results
#'
#' @param object1 A fitted model of class `mra.mv`
#' @param object2 Another fitted model of class `mra.mv`
#'
#' @return A tibble with descriptive statistics of model fit
#' @export
#' @family multi-level model calculations
compare_ml_MA <- function(object1, object2) {
  # see https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/multilevel-ma.html#multilevel-R
  # this fn compares two fitted mra.mv models, and tidies the results

  res <- anova(object1, object2)
  out <- bind_rows(
    "full" = res$fit.stats.f,
    "reduced" = res$fit.stats.r,
    .id = "model_name"
  ) %>%
    inner_join(
      tibble(
        "model_name" = c("full", "reduced"),
        "df" = c(res$parms.f, res$parms.r)
      ),
      .
    ) %>%
    mutate(LRT = c(NA, res$LRT), pval = c(NA, res$pval))
  return(out)
}
