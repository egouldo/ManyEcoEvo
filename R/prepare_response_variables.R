#' Prepare response variable data for nested ManyEcoEvo dataset
#'
#' @param ManyEcoEvo Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset
#' @param estimate_type A character string of length 1, equal to either "Zr", "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#'
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of data
#' @family targets-pipeline functions
#' @family Multi-dataset Wrapper Functions
#' @export 
prepare_response_variables <- function(ManyEcoEvo, 
                                       estimate_type =  character(1L),
                                       param_table = NULL){
  stopifnot(is.data.frame(ManyEcoEvo))
  #TODO run checks on ManyEcoEvo
  match.arg(estimate_type, choices = c("Zr", "yi", "y25", "y50", "y75"), several.ok = FALSE)
  out <- ManyEcoEvo %>% 
    ungroup() %>% 
    # dplyr::group_by(dataset) %>% #NOTE: mapping doesn't work properly when tibble is rowwise!
    dplyr::mutate(data = purrr::map2(.x = data, .y = dataset,
                       .f = ~ standardise_response(dat = .x, 
                                                   estimate_type =  !!{estimate_type}, 
                                                   param_table, 
                                                   dataset = .y)))
  return(out)
}
