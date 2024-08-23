#' Log-transform a data-frame of back-transformed out-of-sample estimates
#' 
#' @param back_transformed_data description
#' @param sim Number of simulations to run when generating distribution to sample from
#' @return A tibble of standardised-out-of-sample predictions on the Z-scale, with columns `Z`, `VZ`, `lower` and `upper` and the original columns fro `back_transformed_data` that were not used / updated in the transformation.
#' @seealso Equivalent to [pred_to_Z()] in terms of workflow data hierarchy. Called by [log_transform_response()].
#' @param sim Number of simulations to run when generating distribution to sample from
#' @export
#' @import dplyr
#' @import purrr
#' @import cli
#' @import rlang
log_transform_yi <- function(back_transformed_data, 
                             sim = 10000L, ...){
  
  if (any(rlang::is_na(sim), 
          rlang::is_na(back_transformed_data))) {
    cli::cli_warn("Argument {.arg sim} or {.arg back_transformed_data} is {.val {NA}}. Returning {.val {NA}} for log-transformed predictions.")
    return(NA)
  }
  
  names_lookup <- c(estimate = "estimate", #blue tit
                    estimate = "fit", #eucalyptus
                    std.error = "se.fit") # both datasets  
  
  log_transformed_data <- 
    back_transformed_data %>% 
    rename(., any_of(names_lookup)) %>% 
    mutate(res = map2(.x = estimate, 
                      .y = std.error, 
                      .f = log_transform, 
                      sim = sim), 
           .keep = "unused") %>% 
    select(-starts_with("ci.")) %>% 
    unnest(res)
  
  return(log_transformed_data)
}