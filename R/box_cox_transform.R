#' Box-cox transform absolute deviation from the meta-analytic mean scores
#'
#' @param data Dataset for model fitting, must contain columns `"abs_deviation_score_estimate"` and standard error
#' @param dataset character string of length 1 either "blue tit" or "eucalyptus"
#' @family Box-Cox transformation
#' @family Analysis-level functions
#' @return data with additional columns of box-cox transformed deviation scores and variance
#' @export
#' @import dplyr
#' @importFrom rlang is_na is_null
#' @importFrom glue glue
#' @importFrom recipes prep tidy juice recipe
#' @importFrom timetk step_box_cox
#' @importFrom cli cli_alert_info cli_h2 cli_alert_warning
#' @importFrom purrr keep map2
#' @importFrom stringr str_starts
#' @importFrom tidyr hoist
#' @seealso [variance_boxcox()], [folded_params()]
box_cox_transform <- function(data, dataset, outcome_SE_colname) {
  # ----- Argument Checking -----
  if ( any(rlang::is_na(data), rlang::is_null(data))) {
    cli::cli_alert_warning(
      text = 
        glue::glue(
          "Cannot box-cox transform data for",
          paste(names(dplyr::cur_group()),
                dplyr::cur_group(),
                sep = " = ",
                collapse = ", "
          )
        ))
    result <- NA
  } else {
    
    # ---- Compute Box-Cox Transformed absolute deviation scores ----
    cli::cli_h2(
      c("Box-cox transforming absolute deviation scores for ",
        "{.arg dataset} = {.val {dataset}}."
      )
    )
    
    box_cox_recipe <- 
      recipes::recipe( ~ .,
                       data = select(
                         data,
                         starts_with("abs_deviation_score_")
                       )
      ) %>%
      timetk::step_box_cox(everything(), limits = c(0, 10)) %>%
      recipes::prep(training = data, retain = TRUE) # estimate lambda + box cox transform vars
    
    if (box_cox_recipe %>%
        recipes::tidy(number = 1) %>% 
        nrow() > 0) { # TODO pull execution of if/else and check result in if() so not executing twice (next line)
      lambda <- box_cox_recipe %>%
        recipes::tidy(number = 1) %>%
        pull(., lambda) %>%
        `names<-`(., pull(box_cox_recipe %>%
                            recipes::tidy(number = 1), 
                          terms))
      
      if (!is.null(dataset)) {
        cli::cli_alert_info(
          c(
            "Optimised Lambda used in Box-Cox Transformation of ",
            "{dataset} dataset variables ",
            "is {round(lambda, 2)} for `{names(lambda)}`."
          )
        )
      }
      
      result <- recipes::juice(box_cox_recipe) %>%
        rename_with(.fn = ~ paste0("box_cox_", .x)) %>%
        bind_cols(data, .) %>%
        mutate(fold_params = 
                 map2(.x = abs_deviation_score_estimate, 
                      .y = !!as.name(outcome_SE_colname),
                      .f = folded_params)) %>%
        hoist(fold_params,
              folded_mu_val = 1, 
              folded_v_val = 2) %>%
        mutate(
          box_cox_var = variance_box_cox(folded_mu_val, 
                                         folded_v_val, 
                                         lambda[[1]]),
          lambda = lambda[[1]]
        )
    } else {
      result <- NA
      cli::cli_alert_warning(text = c("Lambda cannot be computed."))
    }
  }
  
  return(result)
}

#' Calculate the variance of the Box-Cox transformed absolute deviation scores
#' @param folded_mu The mean of the folded absolute deviation scores
#' @param folded_v The variance of the folded VZr
#' @param lambda The lambda value used in the Box-Cox transformation
#' @return The variance of the Box-Cox transformed absolute deviation scores
#' @export
#' @family Box-Cox transformation
#' @family Analysis-level functions
variance_box_cox <- function(folded_mu, folded_v, lambda) {
  variance_bc <- folded_v * (lambda * folded_mu^(lambda - 1))^2 # delta method
  return(variance_bc)
}

#' Calculate the folded parameters for the Box-Cox transformation
#' @param abs_dev_score The absolute deviation score
#' @param VZr The variance of the standardised effect size
#' @return A named list containing the mean `fold_mu` and variance `fold_v` of the folded parameters
#' @export
#' @family Box-Cox transformation
#' @family Analysis-level functions
folded_params <- function(abs_dev_score, VZr) {
  
  mu <- abs_dev_score
  sigma <- sqrt(VZr)
  fold_mu <- sigma * sqrt(2 / pi) * exp((-mu^2) / (2 * sigma^2)) + mu * (1 - 2 * pnorm(-mu / sigma)) # folded abs_dev_score
  fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
  fold_v <- fold_se^2 # folded VZr
  return(list(fold_mu = fold_mu, fold_v = fold_v))
  
}
