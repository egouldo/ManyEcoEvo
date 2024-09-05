#' Exclude extreme estimates above a threshold parameter sd
#' 
#' @param data A dataframe of analyst estimates
#' @param outcome_variable the name of the variable in `data` containing the analyst estimates
#' @param outcome_SE variable in `data` containing analyst SE estimates
#' @param sd_threshold A numeric threshold multiplyer see details
#' @param param_table A dataframe containing population parameters `mean` and `sd` for each `variable` in a given `dataset`
#' @param .fn An optional function that will transform parameter estimates to the same scale as `outcome_variable` in `data`
#' @param ... Arguments supplied to `.fn`
#' @import dplyr
#' @importFrom rlang enquo env as_quosures enquos enquo current_env quo_set_env is_null
#' @importFrom cli cli_h3 cli_alert_success
#' @importFrom purrr map list_c
#' @importFrom tidyr pivot_wider hoist
#' @details
#' This function is used to exclude extreme estimates from a dataset. The function
#' calculates a threshold for exclusion based on the mean and standard deviation of
#' the population parameter estimates in `param_table`. The threshold is calculated
#' as the mean of the population parameter plus `sd_threshold` times the standard
#' deviation of the population parameter. Estimates in `data` that are greater than
#' this threshold are excluded from the output.
#' 
#' If the user chooses to supply `.fn` and `...` arguments, the function will transform
#' the population parameter estimates in `param_table` to the same scale as the
#' `outcome_variable` in `data` using `.fn`, before calculating the threshold for exclusion.
#' 
#' @return A dataframe of analyst estimates with extreme estimates excluded
#' @examples
#' # example code
#' data <-   ManyEcoEvo_yi %>% 
#' mutate(data = 
#'          map_if(data, 
#'                 ~ filter(.x, 
#'                          stringr::str_detect(response_variable_name, 
#'                                              "average.proportion.of.plots.containing",
#'                                              negate = TRUE)),
#'                 .p = dataset == "eucalyptus")) %>%   
#'   mutate(
#'     diversity_data =
#'       map2(
#'         .x = diversity_data,
#'         .y = data,
#'         .f = ~ semi_join(.x, .y, join_by(id_col)) %>% 
#'           distinct()
#'       )
#'   ) %>% 
#'   prepare_response_variables(
#'     estimate_type = "yi",
#'     param_table = 
#'       ManyEcoEvo:::analysis_data_param_tables, 
#'     dataset_standardise = "blue tit",
#'     dataset_log_transform = "eucalyptus") %>%
#'   generate_yi_subsets() %>% #TODO: must be run after prepare_response_variables??
#'   apply_VZ_exclusions(
#'     VZ_colname = list("eucalyptus" = "se_log", 
#'                       "blue tit" = "VZ"), 
#'     VZ_cutoff = 3) %>% 
#'   filter(dataset == "eucalyptus", estimate_type == "y25")  %>% 
#'   pluck("data", 1)
#' sd_threshold = 3
#' param_table <- ManyEcoEvo:::analysis_data_param_tables
#' exclude_extreme_estimates(data, "mean_log", "se_log", 3, param_table, log_transform, estimate = mean, std.error = sd)
exclude_extreme_estimates <- function(data, outcome_variable, outcome_SE, sd_threshold = numeric(1L), param_table, .fn = ..., ...) {
  # FOR NOW: allow transformation here, but in future, we make sure that 
  # `prepare_response_variables()` returns both `back_transformed_data` and the
  # transformed / standardised data to separate list-columns to retain this data
  # Then downstream functions operate off the list-column `analysis_data` or 
  # some other named list-col like `transformed_data` etc.
  dots <- rlang::enquos(...) %>% rlang::as_quosures(env = rlang::env())
  
  param_table <- pivot_wider(param_table, names_from = parameter, values_from = value) 
  
  if (!is_null(.fn)){
    cli::cli_h3("Transforming {.arg param_table} using {.arg .fn}:")
    param_table <- param_table  %>% 
      rowwise() %>%
      mutate(transformed_values = list(.fn(!!!dots))) %>% 
      hoist(transformed_values, 
            param_mean = outcome_variable, 
            param_sd = outcome_SE) %>% 
      select(-transformed_values, -{map(dots, rlang::as_name) %>% list_c()})
  } else {
    param_table <- param_table %>% 
      rename_with(.cols = contains(c("mean", "sd")), ~ paste0("param_", .x))
  }
  cli::cli_h3("Excluding extreme estimates from data:")
  out <- data %>% 
    left_join(param_table, 
              by = join_by(response_variable_name == variable)) %>% 
    mutate(exclusion_threshold = param_mean + sd_threshold * param_sd) %>% 
    filter(if_any(outcome_variable, ~ .x <= exclusion_threshold)) %>% 
    select(-starts_with("param_"))
  
  cli::cli_alert_success("Removed {.val {nrow(data) - nrow(out)}} columns from data with {.arg sd_threshold} = {.val {sd_threshold}}")
  
  return(out)
}

