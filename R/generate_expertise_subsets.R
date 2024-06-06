#' Generate Expertise Data Subsets
#'
#' @param ManyEcoEvo a ManyEcoEvo dataframe containing formatted raw `data`, formatted `diversity_data`, the `estimate_type`,  `dataset`, `publishable_subset`, and `exclusion_set`. See details.
#' @param expert_subset a dataframe containing the column `response_id` containing response ID's to be included in the expert subset
#'
#' @return A ManyEcoEvo dataframe with added column `expertise_subset` with new subsets of `data` and `diversity_data`
#' @export
#' @details
#' Note that this function needs to be run on `ManyEcoEvo` after the following functions have been run (See examples):
#' - `prepare_response_variables()`
#' - `generate_exclusion_subsets()`
#' - `generate_rating_subsets()`
#' 
#' `generate_rating_subsets()` only creates expertise subsets based on the full dataset where `exclusion_set == "complete"` and `publishable_subset == "All"`.
#' @examples
#' library(ManyEcoEvo)
#' library(tidyverse)
#' library(targets)
#' targets::tar_load(ManyEcoEvo)
#' targets::tar_load(expert_subset)
#' ManyEcoEvo %>%
#' prepare_response_variables(estimate_type = "Zr") |>
#' generate_exclusion_subsets(estimate_type = "Zr") |>
#' generate_rating_subsets() |>
#' generate_expertise_subsets(expert_subset)
generate_expertise_subsets <- function(ManyEcoEvo, expert_subset) {
  #TODO idea, allow ellipses arg in function and pass those expressions to filter.
  # that way isn't hardcoded in the function. Repeat for all other generate / exclude map funs
  # NOTE: should be run *after* computing Zr with compute_MA_inputs() 
  out <- ManyEcoEvo %>% 
    filter(publishable_subset == "All" & exclusion_set == "complete") %>% 
    mutate(data = map(.x = data, .f = dplyr::semi_join, expert_subset )) %>% 
    mutate(diversity_data = 
             map2(.x = diversity_data, 
                  .y = data, 
                  .f = ~ semi_join(.x, .y) %>% distinct)) %>% 
    mutate(expertise_subset = "expert")
  
  # THEN BIND ROWS WITH PREVIOUS DATASETS
  out <- bind_rows(
    ManyEcoEvo %>% 
      mutate(expertise_subset = "All"),
    out
  )
  
  return(out)
}
