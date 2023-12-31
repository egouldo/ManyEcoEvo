#' Prepare response variable data for nested ManyEcoEvo dataset - out of sample predictions only
#'
#' @param ManyEcoEvo Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset
#' @param estimate_type A character string of length 1, equal to either "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#'
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of data
#' @family targets-pipeline functions
#' @family Multi-dataset Wrapper Functions
#' @export 
prepare_response_variables_yi <- function(ManyEcoEvo, 
                                       estimate_type =  character(1L), #TODO why do we need an estimate type arg if this is for yi only?!
                                       param_table = NULL){
  stopifnot(is.data.frame(ManyEcoEvo))
  #TODO run checks on ManyEcoEvo
  match.arg(estimate_type, choices = c("yi", "y25", "y50", "y75"), several.ok = FALSE)
  out <- ManyEcoEvo %>% 
    ungroup() %>% 
    # dplyr::group_by(dataset) %>% #NOTE: mapping doesn't work properly when tibble is rowwise!
    dplyr::mutate(data = purrr::map2(.x = data, .y = dataset,
                                     .f = ~ back_transform_response_vars_yi(dat = .x, 
                                                                 estimate_type =  !!{estimate_type}, 
                                                                 param_table, 
                                                                 dataset = .y)),
                  diversity_data = map2(.x = diversity_data, 
                                        .y = data,
                                        .f = ~ semi_join(.x, .y) %>% distinct))
  return(out)
}

#' Back Transform Response Variables - yi
#'
#' @param data 
#' @param estimate_type The type of estimate to be standardised. Character vector of length 1, whose value may be  "yi", "y25", "y50", "y75". 
#' @param param_table A table of estimated 'population' parameters for each variable in the analysis datasets.
#' @param dataset One of either "blue tit" or "eucalyptus"
#'
#' @return A tibble of analyst data with standardised values contained in a list-column called 'back_transformed_data'
#' @export 
#' @family analyst-data
back_transform_response_vars_yi <-  function(dat, 
                                  estimate_type = character(1L), 
                                  param_table = NULL,
                                  dataset = character(1L)) {
  # TODO insert checks that appropriate columns exist
  # TODO apply to data and check that all cases accounted for!
  match.arg(estimate_type, choices = c("yi", "y25", "y50", "y75"), several.ok = FALSE)
  match.arg(dataset, choices = c("eucalyptus", "blue tit"), several.ok = FALSE)
  cli::cli_h1(glue::glue("Computing meta-analysis inputs", "for estimate type ", "{estimate_type}"))
  

    cli::cli_h2(paste0("Transforming out of sample predictions from link to response scale"))
    dat <- dat %>% 
      pointblank::col_exists(columns = 
                               pointblank::vars("TeamIdentifier", 
                                                "submission_id", 
                                                "analysis_id", 
                                                "split_id", 
                                                "augmented_data", 
                                                "transformation",
                                                "response_transformation_status")) %>% # add check for  response transformation
      dplyr::group_by(TeamIdentifier, 
                      submission_id, 
                      analysis_id, 
                      split_id) %>% 
      dplyr::mutate(params = purrr::map(.x = response_variable_name, 
                                        .y = param_table, 
                                        .f = ~ dplyr::filter(.y, variable == .x))) %>% 
      dplyr::mutate(nrow_params = purrr::map_int(params, nrow)) %>% 
      dplyr::mutate(params = purrr::map2(params, 
                                         nrow_params, 
                                         .f = ~ if(.y > 0){.x}else{NA})) %>% 
      dplyr::select(-nrow_params) %>% 
      dplyr::mutate(transformation_type = 
                      assign_transformation_type(response_transformation = response_transformation_status, 
                                                 link_fun = transformation)) %>% 
      dplyr::mutate(back_transformed_data = 
                      purrr::pmap(.l = list(augmented_data, 
                                            transformation_type, #TODO update, gh issue 162
                                            response_transformation_status,
                                            transformation), #TODO update, gh issue 162 #NOTE: see #127 / #38 on GH.
                                  .f = ~ if(!rlang::is_na(..1) | !rlang::is_na(..2)){
                                    convert_predictions(augmented_data = ..1,
                                                        transformation_type = ..2,
                                                        response_transformation = ..3,
                                                        link_fun = ..4)
                                  }else{rlang::na_lgl})) #TODO note that the blue tit and eucalyptus back transformed dat has different names for the out of sample prediction estimates. For BT it's "estimate", for Euc it's "fit" (this is because of the way we asked analysts to submit their data...).
    
  return(dat)
  
}