#' Fit model of Box-Cox transformed deviation scores as a function random-effects inclusion in analyses
#' @description Fits a univariate glm of Box-Cox transformed absolute deviation from the meta-analytic mean scores as a function of whether the analysis was a mixed effects model (i.e. included random effects) or not.
#'
#' @param data Dataframe containing Box-Cox transformed absolute deviation scores and binary column called `mixed_model` describing whether or not the analysis used a mixed-effects model.
#' @param N threshold number of analyses in each predictor category for fitting model
#' @return A fitted model object of class `glm` and `parsnip`
#' @export
#' @family Model fitting and meta-analysis
#' @examples
#' # library(tidyverse);library(targets);library(metafor);library(tidymodels)
#' # tar_load(meta_analysis_outputs)
#' # fit_uni_mixed_effects(meta_analysis_results$data[[1]])
#' @import dplyr
#' @importFrom cli cli_h2 cli_warn cli_alert_warning
#' @importFrom pointblank test_col_exists
#' @importFrom recipes recipe update_role step_mutate step_naomit
#' @importFrom parsnip fit linear_reg
#' @importFrom workflows workflow add_model add_recipe extract_fit_parsnip
#' @seealso [parsnip::details_linear_reg_glm] for details on the [parsnip::linear_reg] engine. 
fit_uni_mixed_effects <- function(data, N = 5) {
  
  if (!pointblank::test_col_exists(data,
                                  columns = c("mixed_model", 
                                              starts_with("box_cox_abs_")))) {
    
    cli::cli_alert_warning(
      c("Columns {.var mixed_model} and ", 
        data %>%
          dplyr::select(starts_with("box_cox_abs_")) %>%
          colnames(), 
        " missing. Returning {.val {NA}}.")
    )
    
    return(NA)
    
  }  else if ( length(unique(data$mixed_model)) == 1) {
    
    cli::cli_warn(message = "More than 1 unique value of {.var mixed_model} ",
                  "is needed to fit model with {.var mixed_model} ",
                  "as predictor variable. Returning {.val {NA}}")
    
    return(NA)
    
  } else if (!pointblank::test_col_vals_gte(data, 
                                            columns = n, 
                                            value = N, 
                                            preconditions = \(x) count(x, mixed_model))) {
    
    cli::cli_warn(message = "Less than {.arg N} = {.val {N}} observations in ",
                  "each level of {.var mixed_model}. Returning {.val {NA}}.")
    
    return(NA)
    
  } else{
    
    cli::cli_h2(c("Fitting glm for Box-Cox transformed outcome with inclusion of random effects (binary variable) as predictor"))
    
    data <- data %>%
      dplyr::select(dplyr::starts_with("box_cox_abs_"), 
                    mixed_model)
    
    glm_recipe <-
      recipes::recipe(~.,
                      data = data
      ) %>%
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
  }   
  
  return(fitted_mod)
}

#' Possibly [fit_uni_mixed_effects()] 
#' @description Wrapper for [fit_uni_mixed_effects()] that returns `NA` if an error is thrown.
#' @seealso [fit_uni_mixed_effects()]
#' @importFrom purrr possibly
#' @keywords internal
#' @family Model fitting and meta-analysis
poss_fit_uni_mixed_effects <- purrr::possibly(fit_uni_mixed_effects,
                                              otherwise = NA,
                                              quiet = FALSE
)
