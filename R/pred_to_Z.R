#' Z-standardise a dataframe of back-transformed Out-Of-Sample Predictions
#'
#' @description Standardizes out-of-sample predictions by computing the Fisher's Z transformed Correlation Coefficient from analysts' out-of-sample prediction estimates and corresponding standard error.
#' @param back_transformed_data a dataframe or tibble with the columns "estimate" and "se.fit", containing yi and SE\(yi\) values respectively
#' @param response_variable_name a character vector
#' @return A tibble of standardised-out-of-sample predictions on the Z-scale, with columns `Z`, `VZ`, `lower` and `upper`, and the original columns fro `back_transformed_data` that were not used / updated in the transformation.
#' @details This function is used to standardize out-of-sample predictions on the response scale to the Z-scale. [pred_to_Z()] expects estimates to be on the response scale, not the link scale.
#' 
#' The function computes the Z-score and VZ-score for each out-of-sample prediction estimate and its corresponding standard error using [Z_VZ_preds()].
#' @export
#' @import dplyr
#' @importFrom purrr pluck map
#' @importFrom cli cli_warn
#' @importFrom rlang is_na
#' @importFrom tidyr any_of unnest
#' @importFrom glue glue
#' @seealso Equivalent to[log_transform_yi()] in terms of workflow data hierarchy.
pred_to_Z <- function(back_transformed_data,
                      params) {
  
  if (any(
    rlang::is_na(params), 
    rlang::is_na(back_transformed_data)
  )) {
    cli::cli_warn(
      c("Argument {.arg params} or {.arg back_transformed_data} ",
                 "is {.val {NA}}. Returning {.val {NA}}",
                 "for standardized predictions.")
    )
    return(NA)
  }
  
  sd_p <- params %>%
    dplyr::filter(parameter == "sd") %>%
    purrr::pluck("value")
  
  mu_p <- params %>%
    dplyr::filter(parameter == "mean") %>%
    purrr::pluck("value")
  
  names_lookup <- c(yi = "estimate", #blue tit
                    yi = "fit", #eucalyptus
                    yi_se = "se.fit") # both datasets
  
  standardised_preds <- 
    back_transformed_data %>%
    rename(any_of(names_lookup)) %>% 
    mutate(res = map2(.x = yi,
                      .y = yi_se,
                      .f = Z_VZ_preds,
                      sd_p = sd_p,
                      mu_p = mu_p),
           .keep = c("unused")) %>%
    select(-starts_with("ci.")) %>% 
    unnest(res)
  
  return(standardised_preds)
}
