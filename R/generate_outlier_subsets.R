#' Generate Outlier Subsets for ManyEcoEvo datasets
#' @description Removes top outlier for `yi` datasets and top 2 and bottom 2 outliers for `Zr` datasets
#'
#' @param data a ManyEcoEvo dataframe containing formatted raw `data`, formatted `diversity_data`, the `estimate_type`, and `dataset`
#' @param n_min integer, the number of bottom outliers to remove
#' @param n_max integer, the number of top outliers to remove
#' @param ignore_subsets A list of <[`data-masked`][dplyr::dplyr_data_masking]> expressions passed to [dplyr::filter()] for ignoring specific data subsets when removing outliers
#' @param outcome_variable A named list containing either/and a list of `dataset`s and their corresponding outcome variables for each value of `dataset`, a list of `estimate_type`s and their corresponding outcome variables for each value of `estimate_type`.
#' @return A ManyEcoEvo dataframe with added column `exclusion_set` with new subsets of `data`, `diversity_data` that exclude outliers.
#' @export
#' @details
#' 
#' ## Function Usage
#' 
#' `n_min` and `n_max` are used to specify the number of outliers to remove from the bottom and top of the dataset, respectively. These arguments are passed to the `n` argument within [dplyr::slice_min()] and [slice_max()] respectively. Note that negative values of `n_min` and `n_max` will be removed from the dataset, while positive values of `n` will retain observations in the dataframe. 
#' 
#' `ignore_subsets` is used to specify which subsets of the data should be ignored when removing outliers. This is useful when you want to remove outliers from all datasets except for specific subsets. For example, if you want to remove outliers from all datasets except for the `eucalyptus` dataset, you would pass `ignore_subsets = dataset == "eucalyptus"`.
#' 
#' The function will check that any columns specified in `ignore_subsets` exist in the dataset using [pointblank::test_col_exists()]. If they do not, the function will throw an error.
#' 
#' ## Analysis Pipeline Usage
#' 
#' This function ahould be run after computing response variables for `yi` datasets with [generate_yi_subsets()] and `Zr` datasets with [generate_Zr_subsets()], but it may also be executed on the raw data to remove the top `n_max` and bottom `n_min` observations.
#' 
#' Note that `outcome_variable` 
#' 
#' #TODO: will nolonger work on Zr dataset, because this doesn't contain an estimate_type col?
#' 
#' `collinearity_subset != "collinearity_removed"` for Zr datasets and Don't run with the reduced publishability subset.... some of these already only have 10 data points!!
#' 
#' @examples
#' 
#' analysis_data <-  ManyEcoEvo_yi %>%
#' prepare_response_variables(
#' estimate_type = "yi",
#' param_table = ManyEcoEvo:::analysis_data_param_tables,
#' dataset_standardise = "blue tit",
#' dataset_log_transform = "eucalyptus") %>%
#' generate_yi_subsets() %>% #TODO: must be run after prepare_response_variables??
#' apply_VZ_exclusions(
#' VZ_colname = list("eucalyptus" = "std.error_log",
#' "blue tit" = "VZ"),
#' VZ_cutoff = 3) %>% 
#' generate_exclusion_subsets() %>% 
#' compute_MA_inputs()
#' analysis_data %>% generate_outlier_subsets(outcome_variable = list(dataset = list("eucalyptus" = "mean_log", "blue tit" = "Z"), "estimate_type" = list("Zr" = "Zr")), n_min = -3, n_max = -3, ignore_subsets = list(estimate_type != "y25", dataset != "eucalyptus"))
#' 
#' @family Multi-dataset Wrapper Functions
generate_outlier_subsets <- function(data, outcome_variable = NULL, n_min = NULL, n_max = NULL, ignore_subsets = NULL) {
  # ----- Argument Checks -----
  
  if (length(c(n_max, n_min)) == 0) {
    cli_abort("One of {.arg n_max} or {.arg n_min} must be supplied")
  }
  
  stopifnot(
    is.data.frame(data)
  )
  
  required_columns <- c("data", 
                        "diversity_data", 
                        "estimate_type", 
                        "dataset")
  
  if (!is.null(enexpr(ignore_subsets))) {
    ignore_subsets_columns <- 
      rlang::call_args(enquo(ignore_subsets)) %>% 
      map(rlang::f_lhs) %>% 
      map(rlang::as_string) %>% 
      list_c() %>% 
      append(values = required_columns) %>% 
      unique()
  } else {
    ignore_subsets_columns <- required_columns
  }
  
  pointblank::expect_col_exists(data, 
                                columns = ignore_subsets_columns)
  
  if (is.list(n_min)) {
    map(n_min, ~ {
      stopifnot(
        is.numeric(.x)
      )
    })
  } else {
    stopifnot(
      is.numeric(n_min)
    )
    # ----- Format `n_min` when `n_min` is not list -----
    if (length(n_min) < length(unique(data$dataset))) {
      cli::cli_alert_warning("{.arg n_min} = {.val {n_min}} was recycled to match the number of unique datasets in {.arg data}.")
      n_min <- rep(n_min, length(unique(data$dataset)))
    }
  }
  
  if (is.list(n_max)) {
    map(n_max, ~ {
      stopifnot(
        is.numeric(.x)
      )
    })
  } else {
    stopifnot(
      is.numeric(n_max)
    )
    # ----- Format `n_max` when `n_max` is not list -----
    if (length(n_max) < length(unique(data$dataset))) {
      cli::cli_alert_warning("{.arg n_max} = {.val {n_max}} was recycled to match the number of unique datasets in {.arg data}.")
      n_max <- rep(n_max, length(unique(data$dataset)))
    }
  }
  # ----- Create match formulae for outcome variables and n_min/n_max-----
  
  if (is.list(n_min)) {
    formulae_match_n_min <- formulae_match(names(n_min), n_min)
  } else {
    formulae_match_n_min <- formulae_match(unique(data$dataset), n_min)
  }
  
  if (is.list(n_max)) {
    formulae_match_n_max <- formulae_match(names(n_max), n_max)
  } else {
    formulae_match_n_max <- formulae_match(unique(data$dataset), n_min)
  }
  
  matched_formulae <-  map(outcome_variable, ~ formulae_match(x = names(.x), y = .x))
  
  # ----- Generate Outlier Subsets -----
  if (str_detect(data$estimate_type, "Zr") %>% any(na.rm = TRUE)) {
    
    if (!is.null(enexpr(ignore_subsets))) {
      filter_vars <- quos(estimate_type == "Zr", 
                          !!!rlang::call_args(enquo(ignore_subsets)))
    } else {
      filter_vars <- quo(estimate_type == "Zr")
    }
    
    data_Zr <- data %>%
      filter(estimate_type == "Zr")
    
    data_Zr <- 
      map2(names(matched_formulae), matched_formulae,
           .f = ~ map_match_formulae(data_Zr, .x, .y)) %>% 
      bind_rows() %>% 
      drop_na(outcome_colname) %>% 
      map_match_formulae(variable_name = "dataset", formulae_match_n_min, col_name = "n_min") %>%
      map_match_formulae(variable_name = "dataset", formulae_match_n_max, col_name = "n_max") %>% 
      apply_slice_conditionally(
        x = .,
        filter_vars = filter_vars) %>% 
      select(-outcome_colname, -n_min, -n_max)
  }
  
  if (str_detect(data$estimate_type, "y") %>% 
      any(na.rm = TRUE)) {
    
    if (!is.null(enexpr(ignore_subsets))) {
      filter_vars <- quos(str_detect(estimate_type, "y"), 
                          !!!rlang::call_args(enquo(ignore_subsets)))
    } else {
      filter_vars <- quo(str_detect(estimate_type, "y"))
    }
    
    data_yi <- data %>%
      filter(str_detect(estimate_type, "y"))
    
    data_yi <-  map2(
      names(matched_formulae), 
      matched_formulae,
      .f = ~ map_match_formulae(data_yi, .x, .y)) %>% 
      bind_rows() %>% 
      drop_na(outcome_colname) %>% 
      map_match_formulae(variable_name = "dataset", formulae_match_n_min, col_name = "n_min") %>%
      map_match_formulae(variable_name = "dataset", formulae_match_n_max, col_name = "n_max") %>% 
      apply_slice_conditionally(
        x = .,
        filter_vars = filter_vars) %>% 
      select(-outcome_colname, -n_min, -n_max)
  }
  
  out <- if (exists(x = "data_Zr") & exists(x = "data_yi")) {
    bind_rows(data_Zr, data_yi)
  } else if (exists(x = "data_Zr") & !exists(x = "data_yi")) {
    data_Zr
  } else if (!exists(x = "data_Zr") & exists(x = "data_yi")) {
    data_yi
  } else {
    NULL
  }
  
  return(out)
}


#' Slice Conditionally
#' 
#' @param data A tibble
#' @param n_min integer, the number of bottom outliers to remove
#' @param n_max integer, the number of top outliers to remove
#' @param outcome_variable character string, the name of the outcome variable
#' @keywords internal
slice_conditionally <- function(data, n_min, n_max, outcome_variable) {
  data %>% 
    {if (!is.null({{n_min}})) slice_min(., pick(outcome_variable), n = n_min) else .} %>%
    {if (!is.null({{n_max}})) slice_max(., pick(outcome_variable), n = n_max) else .}
}

#' Apply slice conditionally
#' @param x A tibble, containing the full dataset which will be subset with [slice_conditionally()] and bound back to the original dataset `x`
#' @param filter_vars A list of quosures to be used in [dplyr::filter()] to subset `y`
#' @param n_min integer, the number of bottom outliers to remove
#' @param n_max integer, the number of top outliers to remove
#' @details the tibble `x` must contain the columns `data`, `outcome_colname`, `n_min`, and `n_max`
#' @keywords internal
apply_slice_conditionally <- function(x, filter_vars){
  out <- bind_rows(x, {
    x %>%
      filter(!!!filter_vars) %>%
      mutate(data = 
               pmap(list(data, outcome_colname, n_min, n_max),
                    .f = ~ slice_conditionally(..1, 
                                               n_min = ..3, 
                                               n_max = ..4, 
                                               outcome_variable = ..2
                    ))) %>%
      mutate(
        exclusion_set = paste0(exclusion_set, "-rm_outliers"),
        data =
          map2(
            .x = data,
            .y = data,
            .f = ~ semi_join(.x, .y, 
                             by = join_by(id_col)) %>% 
              distinct()
          ),
        diversity_data =
          map2(
            .x = diversity_data,
            .y = data,
            .f = ~ semi_join(.x, .y, 
                             by = join_by(id_col)) %>% 
              distinct()
          )
      )
  })
  
  return(out)
}

#' Match formulae to outcome variables
#' @param x A character vector
#' @param y A vector
#' @return A named list of formulae
#' @details This function is used to match formulae to variables with a `data.frame`. If `x` and `y` are the same vector, then the formulae will be matched to the same variable.
#' @keywords internal
formulae_match <- function(x,y) {
  map2(
    .x = x, 
    .y = y, 
    rlang::new_formula 
  )
}


#' Map matching formulae to data
#' @param data A tibble
#' @param variable_name character string, the name of the variable to match
#' @param formulae A named list of formulae
#' @return A tibble with a new column `outcome_colname` containing `rhs` of the the matched formula, or `NA` if no match is found
#' @keywords internal 
map_match_formulae <- function(data, variable_name, formulae, col_name = "outcome_colname"){
  data %>% 
    mutate(data, {{col_name}} := case_match(.x = !!sym(variable_name),
                                            !!!formulae,
                                            .default = NA))
}
