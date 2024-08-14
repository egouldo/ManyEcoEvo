#' Prepare response variable data for nested ManyEcoEvo dataset
#'
#' @param ManyEcoEvo Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset
#' @param estimate_type A character string of length 1, equal to either "Zr", "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#' @param dataset_standardise A character string of length 1, equal to the name of the dataset to standardise the response variables to. If `NULL`, all datasets are standardised.
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of dataframes, where each dataframe contains the response variable data for a single analysis. The function standardises the response variable data for each analysis, and returns the modified dataset to the `data` list-column.
#' @family targets-pipeline functions.
#' @family Multi-dataset Wrapper Functions
#' @export
#' @import cli
#' @import dplyr
#' @import purrr
#' @import rlang
prepare_response_variables <- function(ManyEcoEvo,
                                       estimate_type = character(1L),
                                       param_table = NULL,
                                       dataset_standardise = NULL) {
  match.arg(estimate_type, choices = c("Zr", "yi", "y25", "y50", "y75"), several.ok = FALSE)
  stopifnot(is.data.frame(ManyEcoEvo))
  pointblank::expect_col_exists(object = ManyEcoEvo, columns = c(dataset, data))
  if (!is.null(dataset_standardise)) {
    stopifnot(is.character(dataset_standardise))
    stopifnot(length(dataset_standardise) >= 1)
    stopifnot(length(dataset_standardise) == length(unique(ManyEcoEvo$dataset)))
    match.arg(dataset_standardise, choices = ManyEcoEvo$dataset, several.ok = TRUE)
  }
  
  out <- ManyEcoEvo
  
  if (estimate_type != "Zr") {
    if (is.null(param_table)) {
      
      cli::cli_abort("{.arg param_table} must be supplied for {.val {estimate_type}} data")
    }
    
    # ------ Back transform if estimate_type is yi only ------
    out <- out %>%
      ungroup() %>%
      dplyr::mutate(
        data = purrr::map2( #TODO assign to data or not?
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
    
  } else {
    if (!is.null(param_table)) {
      cli::cli_abort("{.arg param_table} must be NULL for {.val {estimate_type}} data")
    }
  }
  
  # ------ Standardise Response Variables for Meta-analysis ------
  
  if (is.null(dataset_standardise)) {
    out <- out %>%
      ungroup() %>%
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
  } else {
    
    process_response <- function(dat){
      dat #TODO replace dummy function with actual function
    }
    
    datasets_to_standardise <- tibble(
      dataset = dataset_standardise,
      fns = list(standardise_response)
    )
    
    pmap_prepare_response <- function(data, estimate_type, param_table, dataset, fns, ...){
      fns(data, estimate_type, param_table, dataset)
    }
    
    out <- out %>%
      ungroup() %>%
      left_join(datasets_to_standardise, by = "dataset") %>% 
      mutate(fns = coalesce(fns, list(process_response)),
             data = pmap(.l = ., 
                         .f = pmap_prepare_response, 
                         estimate_type = estimate_type, 
                         param_table = param_table)) %>% 
      select(-fns)
  }
  return(out)
}
