#' Summarise Mean Sorensen's Index Estimates
#'
#' @param ManyEcoEvo_results A tibble of `ManyEcoEvo_results`
#' @param ManyEcoEvo_yi_results A tibble of `ManyEcoEvo_yi_results`
#'
#' @return A tibble of aggregate summary statistics (`mean`, `sd`, `min`, `max`) for mean Sorensen's index estimates across each `subset` and `dataset`.
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr distinct
#' @importFrom magrittr "%>%"
#' @importFrom dplyr right_join
#' @importFrom dplyr across
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dfr
#' @importFrom purrr set_names
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @family Multi-dataset Wrapper Functions
#' @examples 
#' summarise_sorensen_index(ManyEcoEvo_results,ManyEcoEvo_yi_results)
summarise_sorensen_index <- function(ManyEcoEvo_results,ManyEcoEvo_yi_results) {
  sorensen_index_zr <- 
    ManyEcoEvo_results %>% 
    filter(exclusion_set == "complete", 
           publishable_subset == "All", 
           expertise_subset == "All") %>%  
    ungroup %>% 
    select(dataset, diversity_indices) %>% 
    unnest(diversity_indices)
  
  sorensen_index_yi <- 
    ManyEcoEvo_yi_results %>%
    filter(exclusion_set == "complete") %>% 
    ungroup %>% 
    select(dataset, diversity_indices) %>% 
    unnest(diversity_indices)
  
  effect_ids <- ManyEcoEvo_results %>% #TODO ensure for other related functions are properly filtering ManyEcoEvo_results
    filter(exclusion_set == "complete", 
           publishable_subset == "All", 
           expertise_subset == "All") %>% 
    select(MA_mod, effects_analysis) %>% 
    group_by(estimate_type, dataset) %>% 
    mutate(tidy_mod = map(MA_mod, 
                          ~ broom::tidy(.x, 
                                        conf.int = TRUE, 
                                        include_studies = TRUE) %>% 
                            rename(study_id = term)), .keep = "none") %>% 
    unnest(tidy_mod) %>% 
    filter(type == "study") %>% 
    ungroup %>% 
    select(study_id) %>% 
    rename(id_col = study_id) %>% #TODO duplicates for "Bell-2-2-1" and "Bonalbo-1-1-1 WHY?
    distinct()
  
  prediction_ids <- ManyEcoEvo_yi_results %>% #TODO Euc mod_data_logged not here!
    filter(exclusion_set == "complete") %>% 
    select(MA_mod, effects_analysis, -exclusion_set) %>% 
    group_by(estimate_type, dataset) %>% 
    mutate(tidy_mod = map(MA_mod, 
                          ~ broom::tidy(.x, conf.int = TRUE, include_studies = TRUE) %>% 
                            rename(study_id = term)), .keep = "none") %>% 
    unnest(tidy_mod) %>% filter(type == "study") %>% 
    ungroup %>% 
    select(study_id) %>% 
    rename(id_col = study_id) %>% 
    distinct()
  
  effects <- sorensen_index_zr %>% #TODO consider generalising, so for each 'estimate_type' group: repeat.
    right_join(effect_ids, by = c("id_col")) # repeat for each
  
  predictions <- sorensen_index_yi %>% 
    right_join(prediction_ids, by = c("id_col"))
  
  summarised_data <- 
    map_dfr(.x = list(predictions, effects) %>% 
              purrr::set_names("predictions", "effects"),
            .f = summarise_sorensen_index_data, 
            .id = "subset")
  
  return(summarised_data)
}

#' Summarise Sorensen's Mean Index Estimates for a dataframe
#' @description
#' Summarises Sorensen's index estimates for a single dataframe of estimates 
#' 
#' @param data A dataframe containing `mean_diversity_index` for the Sorensen's index estimates for each analysis `id_col`, for each `dataset`.
#' @return A dataframe with the `mean`, `sd`, `min`, `max` mean Sorensen's index values for each `dataset`.
#' @export
#'
#' @examples 
#' ManyEcoEvo_results %>%
#' filter(exclusion_set == "complete",
#'       publishable_subset == "All",
#'       expertise_subset == "All") %>%
#'  ungroup %>%
#'  select(dataset, diversity_indices) %>%
#'  unnest(diversity_indices) %>% 
#'  summarise_sorensen_index_data()
summarise_sorensen_index_data <- function(data){
    data %>% 
      group_by(dataset) %>% 
      summarise(mean =  mean(mean_diversity_index, na.rm = TRUE),
                sd = sd(mean_diversity_index, na.rm = TRUE),
                min =  min(mean_diversity_index, na.rm = TRUE),
                max =  max(mean_diversity_index, na.rm = TRUE))
}
