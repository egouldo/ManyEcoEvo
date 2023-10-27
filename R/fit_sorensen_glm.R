#' Fit univariate glm of deviation scores on sorensen diversity index
#'
#' @param data A dataframe containing box-cox transformed absolute deviation from the meta-analytic mean scores
#'
#' @return An fitted model object of class `glm` and `parsnip`
#' @export
#'
#' @examples
#' # library(tidyverse);library(targets);library(metafor);library(tidymodels)
#' # tar_load(meta_analysis_outputs)
#' # fit_sorensen_glm(meta_analysis_results$data[[1]])
fit_sorensen_glm <- function(data) {
  cli::cli_h2(glue::glue("Fitting glm for box-cox transformed outcome with sorensen diversity index as predictor"))
  cli::cli_alert_info(dplyr::cur_group() %>% purrr::simplify()) #TODO only run when applied within a tibble on a list-col... want fn available on its own

  
  # TODO: stopifnot(does not contain box_cox_abs_ or mean_diversity_index)
  
  data <- data %>% 
    dplyr::select(dplyr::starts_with("box_cox_abs_"), mean_diversity_index)
  
  glm_recipe <- 
    recipes::recipe(~.,
                    data = data) %>% 
    recipes::update_role(starts_with("box_cox_abs_"), new_role = "outcome")
  
  glm_mod <- parsnip::linear_reg(engine = "glm")
  
  fitted_mod <- 
    workflows::workflow() %>% 
    workflows::add_model(glm_mod) %>% 
    workflows::add_recipe(glm_recipe) %>% 
    parsnip::fit(data = data) %>% 
    workflows::extract_fit_parsnip()
  
  return(fitted_mod)
  
}

poss_fit_sorensen_glm <- purrr::possibly(fit_sorensen_glm,
                                         otherwise = NA,
                                         quiet = FALSE) #TODO export
