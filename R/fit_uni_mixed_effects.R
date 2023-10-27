#' Fit univariate glm of deviation scores on random effects inclusion
#' @description Fits a univariate glm of box-cox transformed absolute deviation from the meta-analytic mean scores as a function of whether the analysis was a mixed effects model \(i.e. included random effects\) or not.
#'
#' @param data Dataframe containing box-cox transformed absolute deviation scores and binary column called `mixed_model` describing whether or not the analysis used a mixed-effects model.
#'
#' @return A fitted model object of class `glm` and `parsnip`
#' @export
#'
#' @examples
#' # library(tidyverse);library(targets);library(metafor);library(tidymodels)
#' # tar_load(meta_analysis_outputs)
#' # fit_uni_mixed_effects(meta_analysis_results$data[[1]])
#' # Note: used tidymodels approach for dynamic outcome var selection
#' # base R approach will be more succinct.
fit_uni_mixed_effects <- function(data){
  cli::cli_h2(glue::glue("Fitting glm for box-cox transformed outcome with inclusion of random effects (binary variable) as predictor"))
  
  if(pointblank::test_col_exists(data, 
                                 c("mixed_model", 
                                   data %>% 
                                   select(starts_with("box_cox_abs_")) %>% #TODO will this not fail if missing?? Seems illogical to derive the col name we are testing to look for from the object??
                                   colnames()))){
    
   if (length(unique(data$mixed_model)) == 1) {
     cli::cli_warn(message = "More than 1 unique value of {.var mixed_model} is needed to fit model with {.var mixed_model} as predictor variable.")
     
     return(NA)
     
   } else {
     data <- data %>% 
       dplyr::select(dplyr::starts_with("box_cox_abs_"), mixed_model)
     
     glm_recipe <- 
       recipes::recipe(~.,
                       data = data) %>% 
       recipes::update_role(starts_with("box_cox_abs_"), new_role = "outcome") %>% 
       recipes::step_mutate(mixed_model = as.factor(mixed_model)) %>% 
       recipes::step_naomit()
     
     glm_mod <- parsnip::linear_reg(engine = "glm")
     
     fitted_mod <- 
       workflows::workflow() %>% 
       workflows::add_model(glm_mod) %>% 
       workflows::add_recipe(glm_recipe) %>% 
       parsnip::fit(data = data) %>% 
       workflows::extract_fit_parsnip()
     
     return(fitted_mod)
   }
    
    
  } else {
    cli::cli_alert_warning(c("Columns {.var mixed_model} and ", data %>% 
                               select(starts_with("box_cox_abs_")) %>% 
                               colnames(), " missing. Returning {.val {NA}}."))
    
    return(NA)
  }
}

poss_fit_uni_mixed_effects <- purrr::possibly(fit_uni_mixed_effects, 
                                              otherwise = NA, 
                                              quiet = FALSE) #TODO export and document
