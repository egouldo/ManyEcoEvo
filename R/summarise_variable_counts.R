#' Summarise variable usage across analyses
#'
#' @description
#' Using [count_analyses_variables_used()] calculates the counts of analyses in which each variable is used for a given `dataset`. Further summarisation of those counts ( `mean`, `sd`, `min` and `max`) is optional, see details.
#'
#' @details To calculate count values, supply `"count"` to argument `output`. To calculate summary statistics of counts, supply `"aggregate"` to argument `output` instead.
#'
#' @param ManyEcoEvo_results A tibble of `ManyEcoEvo_results`
#' @param ManyEcoEvo_yi_results A tibble of `ManyEcoEvo_yi_results`
#' @param ManyEcoEvo A tibble of `ManyEcoEvo`
#' @param output A length 1 character vector equal to `"count"` or `"aggregate"`
#'
#' @return A dataframe of count values `n` or summary statistic values `n_mean`,`n_sd`,`n_min`,`n_max` of counts depending on the the value supplied to the `output` argument.
#' @export
#' @import dplyr
#' @importFrom broom tidy
#' @importFrom tidyr unnest
#' @importFrom purrr map map_dfr set_names
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @family Multi-dataset Wrapper Functions
#' @examples
#' summarise_variable_counts(ManyEcoEvo, ManyEcoEvo_results, ManyEcoEvo_yi_results, "count")
#'
#' summarise_variable_counts(ManyEcoEvo, ManyEcoEvo_results, ManyEcoEvo_yi_results, "aggregate")
summarise_variable_counts <- function(ManyEcoEvo, ManyEcoEvo_results, ManyEcoEvo_yi_results, output = "count") {
  stopifnot(output == "count" | output == "aggregate")
  
  diversity <- ManyEcoEvo %>%
    select(diversity_data) %>%
    unnest(diversity_data)
  
  effect_ids <- ManyEcoEvo_results %>%
    filter(
      exclusion_set == "complete",
      publishable_subset == "All"
    ) %>%
    select(MA_mod, effects_analysis) %>%
    group_by(estimate_type, dataset) %>%
    mutate(
      tidy_mod = 
        map(
          MA_mod,
          ~ broom::tidy(.x,
                        conf.int = TRUE,
                        include_studies = TRUE
          ) %>%
            rename(study_id = term)
        ),
      .keep = "none"
    ) %>%
    unnest(tidy_mod) %>%
    filter(type == "study") %>%
    ungroup() %>%
    select(study_id) %>%
    rename(id_col = study_id) %>% # TODO duplicates for "Bell-2-2-1" and "Bonalbo-1-1-1 WHY?
    distinct()
  
  prediction_ids <- ManyEcoEvo_yi_results %>% # TODO Euc mod_data_logged not here!
    filter(
      exclusion_set == "complete",
      # dataset == "blue tit"
    ) %>%
    select(MA_mod, effects_analysis, -exclusion_set) %>%
    group_by(estimate_type, dataset) %>%
    mutate(tidy_mod = 
             map(
               MA_mod,
               ~ broom::tidy(.x, conf.int = TRUE, include_studies = TRUE) %>%
                 rename(study_id = term)
             ), .keep = "none") %>%
    unnest(tidy_mod) %>%
    filter(type == "study") %>%
    ungroup() %>%
    select(study_id) %>%
    rename(id_col = study_id) %>%
    distinct()
  
  effects <- diversity %>% # TODO consider generalising, so for each 'estimate_type' group: repeat.
    right_join(effect_ids, by = c("id_col")) # repeat for each
  
  predictions <- diversity %>%
    right_join(prediction_ids, by = c("id_col"))
  
  # repeat for all, predictions, effects
  summarised_data <-
    map_dfr(
      .x = list(effects, predictions, diversity) %>%
        purrr::set_names("effects", "predictions", "all"),
      .f = count_analyses_variables_used, .id = "subset"
    )
  
  if (output == "count") {
    
    return(summarised_data)
    
  } else {
    
    summarised_data <- summarised_data %>%
      group_by(dataset, subset) %>%
      summarise(across(n,
                       .fns = list(
                         mean = ~ mean(.x, na.rm = TRUE),
                         sd = ~ sd(.x, na.rm = TRUE),
                         min = ~ min(.x, na.rm = TRUE), # TODO is value of 0 correct?
                         max = ~ max(.x, na.rm = TRUE)
                       )
      ))
  }
  
  return(summarised_data)
  
}

#' Count number of analyses each variable is used
#'
#' @param data A dataframe containing the variables `dataset`, `id_col` and columns filled with the name of the variable indicating variable usage.
#' @details
#' Values in variable columns are character strings or `NA`. `NA` values are treated as absence of variable in a given analysis (observation / row), while character strings are treated as presence of that variable in a given analysis.
#'
#' Variable presence in an analysis is converted to numeric 1's/0's and then summed to calculate total times the analysis is used across all analyses (`n`).
#' @return A dataframe of counts `n` each `variable` is used across all analyses within a given `dataset`
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @examples
#' library(ManyEcoEvo)
#' ManyEcoEvo %>%
#'   select(diversity_data) %>%
#'   tidyr::unnest(diversity_data) %>%
#'   count_analyses_variables_used()
count_analyses_variables_used <- function(data) {
  data %>%
    group_by(dataset, id_col) %>%
    mutate(across(everything(), .fns = ~ ifelse(is.na(.x), 0, 1))) %>% # TODO consider moving into earlier data cleaning
    group_by(dataset) %>%
    summarise(across(.cols = -id_col, .fns = sum)) %>%
    pivot_longer(cols = -dataset, names_to = "variable", values_to = "n")
}
