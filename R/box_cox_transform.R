#' Box-cox transform absolute deviation from the meta-analytic mean scores
#'
#' @param data Dataset for model fitting, must contain columns `"abs_deviation_score_estimate"` and standard error
#' @param dataset character string of either "blue tit" or "eucalyptus"
#'
#' @return data with additional columns of box-cox transformed deviation scores and variance
#' @export
#' @import dplyr
#' @importFrom purrr map2
#' @import rlang
#' @importFrom glue glue
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_h2
#' @import recipes
#' @importFrom timetk step_box_cox
#' @importFrom cli cli_alert_info
#' @importFrom purrr keep
#' @importFrom tidyr hoist
#'
box_cox_transform <- function(data, dataset) {
  if (rlang::is_na(data) | rlang::is_null(data)) {
    cli::cli_alert_warning(text = glue::glue(
      "Cannot box-cox transform data for",
      paste(names(dplyr::cur_group()),
        dplyr::cur_group(),
        sep = " = ",
        collapse = ", "
      )
    ))
    result <- NA
  } else {
    cli::cli_h2(glue::glue("Box-cox transforming absolute deviation scores for ", {
      dataset
    }))

    box_cox_recipe <- recipes::recipe(~.,
      data = select(
        data,
        starts_with("abs_deviation_score_")
      )
    ) %>%
      timetk::step_box_cox(everything(), limits = c(0, 10)) %>%
      recipes::prep(training = data, retain = TRUE) # estimate lambda + box cox transform vars

    if (box_cox_recipe %>%
      recipes::tidy(number = 1) %>% nrow() > 0) { # TODO pull execution of if/else and check result in if() so not executing twice (next line)
      lambda <- box_cox_recipe %>%
        recipes::tidy(number = 1) %>%
        pull(., lambda) %>%
        `names<-`(., pull(box_cox_recipe %>%
          recipes::tidy(number = 1), terms))

      if (!is.null(dataset)) {
        cli::cli_alert_info(c(
          "Optimised Lambda used in Box-Cox Transformation of ",
          "{dataset} dataset variables ",
          "is {round(lambda, 4)} for `{names(lambda)}`."
        ))
      }

      variance_box_cox <- function(folded_mu, folded_v, lambda) {
        variance_bc <- folded_v * (lambda * folded_mu^(lambda - 1))^2 # delta method
        return(variance_bc)
      }

      folded_params <- function(abs_dev_score, VZr) {
        mu <- abs_dev_score
        sigma <- sqrt(VZr)
        fold_mu <- sigma * sqrt(2 / pi) * exp((-mu^2) / (2 * sigma^2)) + mu * (1 - 2 * pnorm(-mu / sigma)) # folded abs_dev_score
        fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
        fold_v <- fold_se^2 # folded VZr
        return(list(fold_mu = fold_mu, fold_v = fold_v))
      }

      # Z_colname <- data %>% colnames %>% keep(., str_starts(., "Z"))
      VZ_colname <- data %>%
        colnames() %>%
        keep(., str_starts(., "VZ"))
      result <- recipes::juice(box_cox_recipe) %>%
        rename_with(.fn = ~ paste0("box_cox_", .x)) %>%
        bind_cols(data, .) %>%
        mutate(fold_params = map2(.x = abs_deviation_score_estimate, .y = !!as.name(VZ_colname), .f = folded_params)) %>%
        hoist(fold_params, folded_mu_val = 1, folded_v_val = 2) %>%
        mutate(
          box_cox_var = variance_box_cox(folded_mu_val, folded_v_val, lambda[[1]]),
          lambda = lambda[[1]]
        )
    } else {
      result <- NA
      cli::cli_alert_warning(text = glue::glue("Lambda cannot be computed."))
    }
  }

  return(result)
}
