#' Summarise Analysis Types
#' @description
#' Generates a summary of the number of analysis teams, total analyses, models with normal error distributions, mixed effects modls, and models developed using Bayesian statistical methods for a given analysis type.
#' 
#' @param data 
#'
#' @return A summarised tibble with the variables `subset`, `dataset`, `num_teams`, `total_analyses`, `sum_linear`, `sum_mixed`, `sum_Bayesian`.
#' @export
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tidyr unnest
#' @importFrom magrittr "%>%"
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @import metafor
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom dplyr right_join
#' @importFrom dplyr full_join
#' @importFrom purrr map_dfr
#'
#' @examples
#' summarise_analysis_types(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo)
summarise_analysis_types <- function(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo) {
  effect_ids <- ManyEcoEvo_results %>% 
    filter(exclusion_set == "complete", 
           publishable_subset == "All") %>% 
    select(MA_mod, effects_analysis) %>% 
    group_by(estimate_type, dataset) %>% 
    mutate(tidy_mod = map(MA_mod, 
                             ~ broom::tidy(.x, 
                                           conf.int = TRUE, 
                                           include_studies = TRUE) %>% 
                               rename(study_id = term)), .keep = "none") %>% 
    unnest(tidy_mod) %>% 
    filter(type =="study") %>% 
    ungroup %>% 
    select(study_id) %>% 
    rename(id_col = study_id) %>% #TODO duplicates for "Bell-2-2-1" and "Bonalbo-1-1-1 WHY?
    distinct()
  
  prediction_ids <- ManyEcoEvo_yi_results %>% #TODO Euc mod_data_logged not here!
    filter(exclusion_set == "complete", 
           # dataset == "blue tit"
           ) %>% 
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
  
  Master <-
    ManyEcoEvo %>% 
    ungroup %>% 
    select(data) %>% 
    unnest(data) %>% 
    mutate(across(c(num_fixed_variables,
                    num_random_variables,
                    sample_size,
                    num_interactions,
                    Bayesian,
                    mixed_model), 
                  as.numeric),
           lm = ifelse(linear_model == "linear", 1, 0), 
           glm = ifelse(linear_model == "generalised", 1, 0)) #TODO move this into master processing so don't have to repeat else where!!
  
  effects <- Master %>% 
    right_join(effect_ids, by = c("id_col")) # repeat for each
  
  predictions <- Master %>% 
    right_join(prediction_ids, by = c("id_col"))
  
  summarised_data <- full_join(
  map_dfr(.x = list(effects, predictions) %>% 
            set_names("effects", "predictions"),
          count_teams_analyses, 
          .id = "subset"),
      map_dfr(.x = list(effects, predictions) %>% 
            set_names("effects", "predictions"),
          count_binary_coded_features,
          .id = "subset")
  )
  
  return(summarised_data)
  #TODO next: set up so can run on just one object ManyEcoEvo_results, and account for subsets too!
}


#' Summarise number of analyst teams and total analyses per dataset
#'
#' @param data A dataframe containing the variables `TeamIdentifier` and `dataset`
#'
#' @return A dataframe with the columns  `dataset`, `total_teams` and `total_analyses` equal in number of rows to the number of unique values within the `dataset` variable of the input `data`.
#' @export
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr "%>%"
#'
#' @examples
#' ManyEcoEvo %>% 
#' filter(dataset == "blue tit") %>% 
#' ungroup %>% 
#' select(data) %>% 
#' unnest(data) %>% 
#' count_teams_analyses()
count_teams_analyses <- function(data) {
  data %>% 
  count(dataset, TeamIdentifier) %>% #TODO consider renaming col
    group_by(dataset) %>% 
    summarise(total_teams = n(), 
              total_analyses = sum(n))
}

#' Summarise binary coded features of analyses
#'
#' @param data A dataframe containing the variables `dataset`, and `lm`, `mixed_model`, `Bayesian`, which are coded as binary numeric vectors.
#'
#' @return A dataframe with the variables
#' @export
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom magrittr "%>%"
#'
#' @examples
#' ManyEcoEvo %>%
#' filter(dataset == "eucalyptus") %>%
#' ungroup %>%
#'  select(data) %>%
#'  unnest(data) %>%
#'  mutate(lm = ifelse(linear_model == "linear", 1, 0), #TODO move into master processing
#'         glm = ifelse(linear_model == "generalised", 1, 0),
#'         Bayesian = as.numeric(Bayesian)) %>% 
#'  count_binary_coded_features()
count_binary_coded_features <- function(data){
  data %>% 
    group_by(dataset) %>%  
    summarise(sum_linear = sum(lm, na.rm = TRUE), 
              sum_mixed = sum(mixed_model,na.rm = TRUE),
              sum_Bayesian = sum(Bayesian,na.rm = TRUE),
              sum_glm = sum(glm, na.rm = TRUE))
}
