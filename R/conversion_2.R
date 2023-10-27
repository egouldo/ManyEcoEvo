#' Conditionally apply back-transformation
#' @description Conditionally apply back-transformation functions depending on the value of `transformation`
#'
#' @param beta Beta estimate, numeric vector of length 1.
#' @param se Standard error of the `beta` estimate, numeric vector of length 1
#' @param response_transformation Character string describing transformation
#' @param link_fun Character string describing link function
#' @param sim Number of simulations to use during back-transformation. Defaults to $10000$.
#'
#' @return The outputs of a back-transformation function, see family back-transformations
#' @family back transformation
#' @export
conversion_2 <-function(beta, se, response_transformation, link_fun, sim = 10000){
  
  na_args <- purrr::discard(c(beta, se, response_transformation, link_fun), is.na) %>% 
    length()
  
  
  if(na_args < 4){
    cli::cli_alert_danger("Required values for back-transformation missing:")
    cli::cli_alert_warning("Returning {.val NA} for quadruple:")
    cli::cli_ol(c("beta_estimate {.val {beta}},",
                  "beta_se {.val {se}},", 
                  "with {.val {response_transformation}} response transformation and",
                  "link function {.val {link_fun}}."))
    return(NA)
  }

  set <- if(link_fun == "log"){
    log_back(set$beta, set$se, sim)
  } else if (link_fun == "logit"){
    logit_back(set$beta, set$se, sim)
  } else if (link_fun == "probit") {
    probit_back(set$beta, set$se, sim)
  } else if (link_fun == "square") {
    square_back(set$beta, set$se, sim)
  } else if (link_fun == "cube") {
    cube_back(set$beta, set$se, sim)
  } else if (link_fun == "inverse") {
    inverse_back(set$beta, set$se, sim)
  } else {
    identity_back(set$beta, set$se, sim) 
  }
    
  set <- if(response_transformation == "log"){
    log_back(set$beta, set$se, sim)
  } else if (response_transformation == "logit"){
    logit_back(set$beta, set$se, sim)
  } else if (response_transformation == "probit") {
    probit_back(set$beta, set$se, sim)
  } else if (response_transformation == "square") {
    square_back(set$beta, set$se, sim)
  } else if (response_transformation == "cube") {
    cube_back(set$beta, set$se, sim)
  } else if (response_transformation == "inverse") {
    inverse_back(set$beta, set$se, sim)
  } else {
    identity_back(set$beta, set$se, sim) 
  }
  
  return(set)
  
}