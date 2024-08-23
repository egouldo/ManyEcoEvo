#' Prepare response variable data for nested ManyEcoEvo dataset
#'
#' @param ManyEcoEvo Complete ManyEcoEvo dataset containing nested datasets for each different analysis and exclusion set dataset
#' @param estimate_type A character string of length 1, equal to either "Zr", "yi", "y25", "y50", "y75", indicating what type of estimates are being prepared.
#' @param param_table A table of parameters \(mean, sd\) for *most* response variables used by analysts. This tibble is pulled from the named object exported by `ManyEcoEvo::`. but can be overwritten with the users's own `param_table` dataset.
#' @param dataset_standardise A character string of length 1, equal to the name of the dataset to standardise the response variables to. If `NULL` (default), no datasets are standardised.
#' @param dataset_log_transform A character string of length 1, equal to the name of the dataset to log-transform the response variables to. If `NULL` (default), no datasets are log-transformed.
#' @return A tibble of nested list-columns
#' @details Operates on nested list-columns of dataframes, where each dataframe contains the response variable data for a single analysis. The function standardises the response variable data for each analysis, and returns the modified dataset to the `data` list-column.
#' @family targets-pipeline functions.
#' @family Multi-dataset Wrapper Functions
#' @export
#' @import cli
#' @import dplyr
#' @import purrr
#' @import rlang
#' @importFrom pointblank expect_col_exists expect_col_is_numeric expect_col_is_character expect_col_vals_in_set
#' @examples
#' data(ManyEcoEvo)
#' ManyEcoEvo %>% prepare_response_variables(estimate_type = "Zr")
prepare_response_variables <- function(ManyEcoEvo,
                                       estimate_type = character(1L),
                                       param_table = NULL,
                                       dataset_standardise = NULL,
                                       dataset_log_transform = NULL,
                                       ...) {
  # ------ Argument Checks ------
  match.arg(estimate_type, 
            choices = c("Zr", "yi", "y25", "y50", "y75"), 
            several.ok = FALSE)
  
  stopifnot(is.data.frame(ManyEcoEvo))
  pointblank::expect_col_exists(object = ManyEcoEvo, 
                                columns = c(dataset, data))
  
  if (!pointblank::test_col_exists(ManyEcoEvo, "estimate_type")) {
    ManyEcoEvo <- dplyr::mutate(ManyEcoEvo, 
                                estimate_type = estimate_type)
  }
  
  if (!is.null(dataset_standardise)) {
    stopifnot(
      is.character(dataset_standardise), 
      length(dataset_standardise) >= 1, 
      length(dataset_standardise) <= length(unique(ManyEcoEvo$dataset)))
    match.arg(dataset_standardise, 
              choices = ManyEcoEvo$dataset,
              several.ok = TRUE)
  }
  
  if (!is.null(dataset_log_transform)) {
    stopifnot(
      is.character(dataset_log_transform), 
      length(dataset_log_transform) >= 1, 
      length(dataset_log_transform) <= length(unique(ManyEcoEvo$dataset)))
    match.arg(dataset_log_transform, 
              choices = ManyEcoEvo$dataset,
              several.ok = TRUE)
  }
  
  if (!is.null(param_table)) {
    stopifnot(is.data.frame(param_table))
    pointblank::expect_col_exists(object = param_table, 
                                  columns = c("variable", 
                                              "parameter", 
                                              "value"))
    pointblank::expect_col_is_numeric(object = param_table, 
                                      columns = "value")
    pointblank::expect_col_is_character(object = param_table, 
                                        columns = c("variable", 
                                                    "parameter"))
    pointblank::expect_col_vals_in_set(object = param_table, 
                                       columns = "parameter", 
                                       set = c("mean", "sd"))
  }
  
  # ------ Prepare Response Variables ------
  
  out <- ManyEcoEvo
  
  if (estimate_type != "Zr") {
    if (all(is.null(param_table), !is.null(dataset_standardise))) {
      cli::cli_abort("{.arg param_table} must be supplied for {.val {estimate_type}} data when {.arg dataset_standardise} is not {.val NULL}.")
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
    # ------ Allocate Response Variable Transformation Functions ------
    
    # yi: no standardise or log-transform
    if ( all(is.null(dataset_standardise), 
             is.null(dataset_log_transform))) {
      
      cli::cli_alert_info("No standardisation or log-transformation applied to response variables for {.val {estimate_type}} estimates.")
      
      transform_datasets <- 
        tibble(
          dataset = unique(out$dataset),
          fns = list(process_response) %>% 
            set_names("process_response")
        )
      
    } else { #yi + standardise and/or log-transform
      cli::cli_alert_info("Standardising and/or log-transforming response variables for {.val {estimate_type}} estimates.")
      transform_datasets <- 
        bind_rows( 
          tibble(
            dataset = dataset_log_transform,
            fns = list(log_transform_response) %>% 
              set_names("log_transform_response")
          ),
          tibble(
            dataset = dataset_standardise,
            fns = list(standardise_response) %>% 
              set_names("standardise_response")
          )) %>% 
        drop_na() # in case of NULLs
    }
  } else { # Zr
    if (!is.null(param_table)) {
      cli::cli_abort("{.arg param_table} must be {.val NULL} for {.val {estimate_type}} data")
    }
    
    cli::cli_alert_info("Standardising response variables for {.val {estimate_type}} estimates.")
    
    transform_datasets <- 
      tibble(
        dataset = unique(out$dataset),
        fns = list(standardise_response)  %>% 
          set_names("standardise_response")
      )
  } 
  
  # ------ Standardise Response Variables for Meta-analysis ------
  
  out <- out %>%
    ungroup() %>%
    left_join(transform_datasets, by = "dataset") %>% 
    mutate(fns = coalesce(fns, list(process_response))) %>%
    mutate(data = pmap(.l = ., 
                       .f = pmap_wrap,
                       # estimate_type = !!{{estimate_type}},
                       param_table = !!{{param_table}}
    )) %>% 
    select(-fns) 
  
  return(out)
}

#' @rdname process_analyst_data
#' @seealso This internal helper function is called by [prepare_response_variables()]
#' @importFrom rlang caller_env exec
#' @keywords internal
pmap_wrap <- function(..., fns, env = caller_env()){
  fn <- match.fun(fns)
  exec(.fn = fn, ..., .env = env)
}
