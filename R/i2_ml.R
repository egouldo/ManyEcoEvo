#' @title i2_ml
#' @description I2 (I-squared) for mulilevel meta-analytic models, based on Nakagawa & Santos (2012). Under multilevel models, we can have multiple I2 (see also Senior et al. 2016). Alternatively, the method proposed by Wolfgang Viechtbauer can also be used.
#' @param model Model object of class \code{rma.mv} or \code{rma}. Currently only model objects using the \code{mods} argument work (e.g., \code{mod = ~1}).
#' @param method Method used to calculate I2. Two options exist: a ratio-based calculation proposed by Nakagawa & Santos (\code{"ratio"}), or Wolfgang Viechtbauer's matrix method (\code{"matrix"}).
#' @param data Data frame used to fit the model.
#' @param boot Number of simulations to run to produce 95 percent confidence intervals for I2. Default is \code{NULL}, where only the point estimate is provided.
#' @return A data frame containing all the model results including mean effect size estimate, confidence, and prediction intervals
#' @author Shinichi Nakagawa - s.nakagawa@unsw.edu.au
#' @author Daniel Noble - daniel.noble@anu.edu.au
#' @author Elliot Gould - elliot.gould@unimelb.edu.au
i2_ml <- function(model, method = c("ratio", "matrix"), data, boot = NULL) {
  if (all(class(model) %in% c(
    "robust.rma", "rma.mv", "rma",
    "rma.uni"
  )) == FALSE) {
    stop("Sorry, you need to fit a metafor model of class robust.rma, rma.mv, rma, rma.uni")
  }
  if (any(model$tau2 > 0)) {
    stop("Sorry. At the moment i2_ml cannot take models with heterogeneous variance.")
  }
  method <- match.arg(method)
  if (method == "matrix") {
    I2s <- matrix_i2(model)
  } else {
    I2s <- ratio_i2(model)
  }
  if (!is.null(boot)) {
    sim <- metafor::simulate.rma(model, nsim = boot)
    mods_formula <- metafor::formula.rma(model, type = "mods")
    random_formula <- model$random
    vi <- model$vi
    pb <- progress::progress_bar$new(
      total = boot, format = "Bootstrapping [:bar] :percent ETA: :eta",
      show_after = 0
    )
    if (is.null(mods_formula)) {
      I2_each <- sapply(sim, function(ysim) {
        tmp <- tryCatch(metafor::rma.mv(ysim, vi,
          random = random_formula, data = data
        ))
        pb$tick()
        Sys.sleep(1 / boot)
        if (method == "matrix") {
          I2 <- matrix_i2(tmp)
        } else {
          I2 <- ratio_i2(tmp)
        }
        return(I2)
      })
    } else {
      I2_each <- sapply(sim, function(ysim) {
        tmp <- tryCatch(metafor::rma.mv(ysim, vi,
          mods = mods_formula,
          random = random_formula, data = data
        ))
        pb$tick()
        Sys.sleep(1 / boot)
        if (method == "matrix") {
          I2 <- matrix_i2(tmp)
        } else {
          I2 <- ratio_i2(tmp)
        }
        return(I2)
      })
    }

    I2s_each_95 <- data.frame(t(apply(I2_each, 1, stats::quantile,
      probs = c(0.5, 0.025, 0.975)
    )))
    I2s <- round(I2s_each_95, digits = 3)
    colnames(I2s) <- c("Est.", "2.5%", "97.5%")
  }
  return(I2s)
}
