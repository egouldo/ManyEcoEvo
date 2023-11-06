#' Standardize Out-Of-Sample Predictions
#' 
#' @description Standardizes out-of-sample predictions by computing the Fisher's Z transformed Correlation Coefficient from analysts' out-of-sample prediction estimates and corresponding standard error.
#' @note `pred_to_Z` expects estimates to be on the response scale, not the link scale. 
#' @param back_transformed_data a dataframe or tibble with the columns "estimate" and "se.fit", containing yi and SE\(yi\) values respectively
#' @param response_variable_name a character vector
#' @export
pred_to_Z <- function(back_transformed_data, 
                      params, 
                      dataset) {
  #TODO test: str_detect(response_variable_name) in param table
  match.arg(dataset, choices = c("blue tit", "eucalyptus"), several.ok = FALSE)
  if(rlang::is_na(params) | rlang::is_na(back_transformed_data)){
    cli::cli_warn("Argument {.arg params} or {.arg back_transformed_data} is {.val {NA}}. Returning {.val {NA}} for standardized predictions.")
    return(NA)
  }
  
  if(dataset == "blue tit"){
    if(!pointblank::test_col_exists(back_transformed_data, 
                                    columns = c("estimate", "se.fit"))){
      
      cli::cli_warn("Blue tit Dataframe {.arg back_transformed_data} is missing columns {.val estimate} and/or {.val se.fit}. Returning {.val {NA}} for standardized predictions.")
      return(NA)
    }
    sd_p <- params %>% 
      dplyr::filter(parameter == "sd") %>% 
      purrr::pluck("value")
    mu_p <- params %>% 
      dplyr::filter(parameter == "mean") %>% 
      purrr::pluck("value")
    
    standardised_preds <- 
      back_transformed_data %>% 
      mutate(res = map2(estimate, 
                        se.fit, 
                        ~Z_VZ_preds(yi = .x,
                                     yi_se = .y,
                                     sd_p = sd_p,
                                     mu_p = mu_p)), 
             .keep = c("unused")) %>% 
      hoist(res, "Z", "VZ") %>% 
      select(-starts_with("ci."))
  }else{
    if(!pointblank::test_col_exists(back_transformed_data, 
                                    columns = c("fit", "se.fit"))){
      
      cli::cli_warn("Eucalyptus Dataframe {.arg back_transformed_data} is missing columns {.val fit} and/or {.val se.fit}. Returning {.val {NA}} for standardized predictions.") #TODO remove hard-coding, generalise
      return(NA)
    }
    sd_p <- params %>% filter(parameter == "sd") %>% pluck("value")
    mu_p <- params %>% filter(parameter == "mean") %>% pluck("value")
    
    standardised_preds <- 
      back_transformed_data %>% 
      mutate(res = map2(fit, se.fit, ~Z_VZ_preds(.x,.y,sd_p,mu_p)),
             .keep = c("unused")) %>% 
      hoist(res, "Z", "VZ") %>% 
      select(-starts_with("ci."))
  }
  
  return(standardised_preds)
}