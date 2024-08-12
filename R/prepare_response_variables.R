#' Prepare response variable data for nested ManyEcoEvo dataset
#'
#' @param ManyEcoEvo Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset
#' @param estimate_type A character string of length 1, equal to either "Zr", "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#'
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of dataframes, where each dataframe contains the response variable data for a single analysis. The function standardises the response variable data for each analysis, and returns the modified dataset to the `data` list-column.
#' @family targets-pipeline functions.
#' @family Multi-dataset Wrapper Functions
#' @export
prepare_response_variables <- function(ManyEcoEvo,
                                       estimate_type = character(1L),
                                       param_table = NULL) {
  stopifnot(is.data.frame(ManyEcoEvo))
  # TODO run checks on ManyEcoEvo
  match.arg(estimate_type, choices = c("Zr", "yi", "y25", "y50", "y75"), several.ok = FALSE)
  
  if (estimate_type != "Zr") {
    if (is.null(param_table)) {
      
      cli::cli_abort("{.arg param_table} must be supplied for {.val {estimate_type}} data")
    }
    
    # ------ Back transform if estimate_type is yi only ------
    out <- ManyEcoEvo %>%
      ungroup() %>%
      # dplyr::group_by(dataset) %>% #NOTE: mapping doesn't work properly when tibble is rowwise!
      dplyr::mutate(
        data = purrr::map2(
          .x = data, 
          .y = dataset,
          .f = ~ back_transform_response_vars_yi(
            dat = .x,
            estimate_type = !!{
              estimate_type
            },
            dataset = .y
          )
        ),
        diversity_data = 
          map2(
            .x = diversity_data,
            .y = data,
            .f = ~ semi_join(.x, .y, by = join_by(id_col)) %>% 
              distinct()
          )
      )
    return(out)
  } else{
    if (!is.null(param_table)) {
      cli::cli_abort("{.arg param_table} must be NULL for {.val {estimate_type}} data")
    }
  }
  
  # ------ Standardise Response Variables for Meta-analysis ------
  out <- ManyEcoEvo %>%
    ungroup() %>%
    # dplyr::group_by(dataset) %>% #NOTE: mapping doesn't work properly when tibble is rowwise!
    dplyr::mutate(data = purrr::map2(
      .x = data, 
      .y = dataset,
      .f = ~ standardise_response(
        dat = .x,
        estimate_type = !!{
          estimate_type
        },
        param_table,
        dataset = .y
      )
    ))
  
  return(out)
  
}
