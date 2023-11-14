#' Summarise Model Composition
#' @description
#' Calculate descriptive statistics (mean, sd, min, max) for number of fixed effects, number of random effects, number of interactions and sample size of models for each dataset across both out of sample predictions and standardized corelation coefficients.
#' 
#' @param ManyEcoEvo_results A tibble of `ManyEcoEvo_results`
#' @param ManyEcoEvo_yi_results A tibble of `ManyEcoEvo_yi_results`
#' @param ManyEcoEvo A tibble of `ManyEcoEvo`
#'
#' @return A dataframe
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
#' summarise_model_composition(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo)
summarise_model_composition <- function(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo) {
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
  
  effects <- Master %>% #TODO consider generalising, so for each 'estimate_type' group: repeat.
    right_join(effect_ids, by = c("id_col")) # repeat for each
  
  predictions <- Master %>% 
    right_join(prediction_ids, by = c("id_col"))
  
  summarised_data <- 
    map_dfr(.x = list(effects, predictions, Master) %>% 
              purrr::set_names("effects", "predictions", "all"),
            .f = summarise_model_composition_data, 
            .id = "subset") %>% 
    pivot_longer(cols = c(-subset, -dataset), 
                 names_to = c("variable", "fn"), 
                 names_pattern = "(.*)_(.*)",
                 values_to = "value")
  
  return(summarised_data)
}

#' Summarise model composition for a single dataframe of out of sample predictions or out or effect sizes
#'
#' @param data A dataframe with the variables `dataset`, `num_fixed_effects`, `num_random_effects`, `num_interactions`, `sample_size`
#'
#' @return A dataframe in tidy format yielding descriptive summary statistics (mean, sd, min and max) for the key variables described in `data`, includes the variables `subset`, `dataset`, `variable`, `fn`, `value` 
#' @export
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @examples
#' ManyEcoEvo %>% 
#' ungroup %>% 
#' filter(dataset == "blue tit") %>% 
#' select(data) %>% 
#' unnest(data) %>% 
#' summarise_model_composition_data()
summarise_model_composition_data <- function(data) {
  data %>% 
    group_by(dataset) %>% 
    rename(fixed = num_fixed_effects,
           random = num_random_effects,
           interactions = num_interactions,
           n = sample_size) %>% 
    summarise(across(c(fixed,
                       random,
                       interactions,
                       n
                       ), 
                     .fns = list(mean = ~ mean(.x, na.rm = TRUE),
                                 sd = ~ sd(.x, na.rm = TRUE),
                                 min = ~ min(.x, na.rm = TRUE),
                                 max = ~ max(.x, na.rm = TRUE))))
}
