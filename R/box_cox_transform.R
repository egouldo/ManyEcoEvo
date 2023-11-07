#' Box-cox transform absolute deviation from the meta-analytic mean scores
#'
#' @param data Dataset for model fitting, must contain columns `"abs_deviation_score_estimate"` and standard error
#' @param dataset character string of either "blue tit" or "eucalyptus"
#'
#' @return data with additional columns of box-cox transformed deviation scores and variance
#' @export
box_cox_transform <- function(data, dataset) {
  if(rlang::is_na(data) | rlang::is_null(data)){
    cli::cli_alert_warning(text =  glue::glue("Cannot box-cox transform data for", 
                                              paste(names(dplyr::cur_group()), 
                                                    dplyr::cur_group(), 
                                                    sep = " = ", 
                                                    collapse = ", ")))
    result <- NA
  } else{
    cli::cli_h2(glue::glue("Box-cox transforming absolute deviation scores for ", {dataset}))

    box_cox_recipe <- recipes::recipe(~., 
                                      data = select(data, 
                                                    starts_with("abs_deviation_score_"))) %>% 
      timetk::step_box_cox(everything(),limits = c(0,10)) %>% 
      recipes::prep(training = data, retain = TRUE) #estimate lambda + box cox transform vars
    
    if(box_cox_recipe %>% 
       recipes::tidy(number = 1) %>% nrow() > 0){
      lambda <- box_cox_recipe %>% 
        recipes::tidy(number = 1) %>% 
        pull(., lambda) %>% 
        `names<-`(., pull(box_cox_recipe %>% 
                            recipes::tidy(number = 1), terms))
      
      if(!is.null(dataset)){
        cli::cli_alert_info(c(
          "Optimised Lambda used in Box-Cox Transformation of ",
          "{dataset} dataset variables ",
          "is {round(lambda, 4)} for `{names(lambda)}`."
        ))
      }
      
      variance_box_cox <- function(folded_mu, folded_v, lambda){
        variance_bc <- folded_v*(lambda*folded_mu^(lambda-1))^2 # delta method
        return(variance_bc)
      }
      
      # folded abs_dev_score
      folded_mu <- function(abs_dev_score, VZr) {
        mu <- abs_dev_score
        sigma <- sqrt(VZr)
        fold_mu <- sigma * sqrt(2/pi) * exp((-mu^2)/(2 * sigma^2)) + mu * (1 - 2 * pnorm(-mu/sigma))
        fold_mu
      }
      
      # folded VZr
      folded_v <- function(abs_dev_score, VZr) {
        mu <- abs_dev_score
        sigma <- sqrt(VZr)
        fold_mu <- sigma * sqrt(2/pi) * exp((-mu^2)/(2 * sigma^2)) + mu * (1 - 2 * pnorm(-mu/sigma))
        fold_se <- sqrt(mu^2 + sigma^2 - fold_mu^2)
        # adding se to make bigger abs_dev_score
        fold_v <- fold_se^2
        fold_v
      }
      
      Z_colname <- data %>% colnames %>% keep(., str_starts(., "Z"))
      VZ_colname <- data %>% colnames %>% keep(., str_starts(., "VZ"))
      result <- recipes::juice(box_cox_recipe) %>% 
        rename_with(.fn = ~ paste0("box_cox_", .x)) %>% 
        bind_cols(data, .) %>% 
        mutate(folded_mu_val = folded_mu(abs_deviation_score_estimate, !!as.name(VZ_colname)),
               folded_v_val = folded_v(abs_deviation_score_estimate, !!as.name(VZ_colname)),
               box_cox_var = variance_box_cox(folded_mu_val, folded_v_val, lambda[[1]]),
               lambda = lambda[[1]])
      
    } else{
      result <- NA
      cli::cli_alert_warning(text =  glue::glue("Lambda cannot be computed."))
    }
    
    
  }
  
  return(result)
  
}