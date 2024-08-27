#' Generate subsets of out-of-sample predictions data by `estimate_type` for multiple analysis datasets.
#'
#' @param yi_analysis A `dataframe` containing multiple datasets of out-of-sample predictions and corresponding diversity data.
#' @details The object `yi_analysis` should contain the character column `dataset`, and list-columns `data` and `diversity_data`. In essence, both `data` and `diversity_data` are nested by `dataset`. `data` should be a list-col of `grouped_df`, which should also contain the list-col `back_transformed_data`.
#' 
#' If the column `back_transformed_data` contains `NA` values, that row will be removed from the list-column `data`.
#' If the column `estimate_type` is present in `yi_analysis`, it will be removed, and replaced with the `estimate_type` derived from the `scenario` and `SurveyID` columns in each `back_transformed_data` dataset.
#' 
#'
#' @family Multi-dataset Wrapper Functions
#' @return A `datraframe` with the character columns `dataset`, `estimate_type` and list-cols `data` and `diversity_data`
#' @export
#' @author Elliot Gould
#' @importFrom purrr map map2
#' @import dplyr
#' @importFrom tidyr unnest
generate_yi_subsets <- function(yi_analysis) {
  cli::cli_h1("Generating out-of-sample prediction subsets.")

  out <- yi_analysis %>%
    group_by(dataset) %>%
    mutate(
      data =
        map(data, ~ dplyr::filter(.x, !is.na(back_transformed_data))) %>% 
          map(split_yi_subsets)
    ) %>%
    select(-contains("estimate_type")) %>% 
    unnest(data) %>%
    mutate(
      diversity_data = 
        map2(
          .x = diversity_data,
          .y = data,
          .f = ~ semi_join(.x, .y, 
                           by = join_by(id_col)) %>% distinct()
        )
    )

  return(out)
}

#' Split a dataset of out-of-sample predictions by `estimate_type`
#'
#' Reorganises the data from nesting based on individual analysis submissions, to nesting based on the type of estimate.
#'
#' @param .data A dataset containing out-of-sample predictions
#' @return A tibble of out-of-sample predictions subset by `estimate_type` and stored in the list-column `data`.
#' @details
#' This function is used to split a dataset of out-of-sample predictions by `estimate_type`, where out-of-sample predictions are stored in a list-column called `back_transformed_data`, with one data frame of data per analysis submission.
#' 
#' The `estimate_type` is derived from either the `scenario` and `SurveyID` columns in each `back_transformed_data` dataset. 
#' The `estimate_type` is then used to nest the data by `estimate_type` in the list-column `data`.
#' 
#' Removes unnecessary data in `.data`: `augmented_data` and `checks`.
#' 
#' @export
#' @import dplyr
#' @importFrom tidyr unnest hoist
#' @importFrom purrr map
#' @seealso This function is called in the wrapper function [generate_yi_subsets()] to split out-of-sample predictions by `estimate_type` across multiple ManyAnalyst datasets.
#' @author Elliot Gould
split_yi_subsets <- function(.data) {

  if (nrow(.data) == 0) {
    cli::cli_alert_danger("{.code nrow(.data)} is {.val {0}} when splitting yi subsets. This is likely because all values for column {.val back_transformed_data} are {.val {NA}}")
    return(NA)
  }
  
 out <-  .data %>%
    select(-augmented_data, -checks) %>%
    filter(!is.na(back_transformed_data)) %>%
    unnest(back_transformed_data) %>%
    dplyr::rename_with(
      .fn = ~ return("estimate_type"),
      .cols = contains(match = c("scenario", "SurveyID"))
    ) %>%
    mutate(estimate_type = case_when(
      estimate_type %in% list("Q1", 1) ~ "y25",
      estimate_type %in% list("Q2", 2) ~ "y50",
      estimate_type %in% list("Q3", 3) ~ "y75", 
      TRUE ~ rlang::na_chr
    )) %>%
    ungroup() %>%
    nest_by(estimate_type, .keep = FALSE) %>%
    hoist(data, .remove = TRUE)
 
 return(out)
}
