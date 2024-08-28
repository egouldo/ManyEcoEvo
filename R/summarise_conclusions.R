#' Summarise counts of qualitative conclusions across all datasets
#'
#' @details
#' Data summary is generated with [summarise_conclusions_data()].
#'
#' @param ManyEcoEvo_results A tibble of `ManyEcoEvo_results`
#' @param ManyEcoEvo_yi_results A tibble of `ManyEcoEvo_yi_results`
#' @param ManyEcoEvo A tibble of `ManyEcoEvo`
#' @return A dataframe with count values for each unique `Conclusion` in columns for each `subset` ("effects", "predictions", "all"), for each `dataset`.
#' @export
#' @import dplyr
#' @importFrom purrr map_dfr map
#' @importFrom broom tidy
#' @importFrom tidyr unnest pivot_wider
#' @import metafor
#' @family Multi-dataset Wrapper Functions
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @examples
#' data(ManyEcoEvo_results)
#' data(ManyEcoEvo_yi_results)
#' data(ManyEcoEvo)
#' summarise_conclusions(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo)
summarise_conclusions <- function(ManyEcoEvo_results, ManyEcoEvo_yi_results, ManyEcoEvo) {
  effect_ids <- ManyEcoEvo_results %>%
    filter(
      exclusion_set == "complete",
      publishable_subset == "All"
    ) %>%
    ungroup() %>%
    select(MA_mod, effects_analysis, estimate_type, dataset) %>%
    group_by(estimate_type, dataset) %>%
    mutate(tidy_mod = map(
      MA_mod,
      ~ broom::tidy(.x,
        conf.int = TRUE,
        include_studies = TRUE
      ) %>%
        rename(study_id = term)
    ), .keep = "none") %>%
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
    ungroup() %>%
    select(MA_mod, effects_analysis, -exclusion_set, dataset, estimate_type) %>%
    group_by(estimate_type, dataset) %>%
    mutate(tidy_mod = map(
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

  Master <-
    ManyEcoEvo %>%
    ungroup() %>%
    select(data) %>%
    unnest(data) %>%
    mutate(
      across(
        c(
          num_fixed_variables,
          num_random_variables,
          sample_size,
          num_interactions,
          Bayesian,
          mixed_model
        ),
        as.numeric
      ),
      lm = ifelse(linear_model == "linear", 1, 0),
      glm = ifelse(linear_model == "generalised", 1, 0)
    ) %>% # TODO move this into master processing so don't have to repeat else where!!
    filter(Conclusion != "CHECK", !is.na(Conclusion)) # TODO data cleaning, check these

  effects <- Master %>%
    right_join(effect_ids, by = c("id_col")) # repeat for each

  predictions <- Master %>%
    right_join(prediction_ids, by = c("id_col"))

  summarised_data <-
    map_dfr(
      .x = list(effects, predictions, Master) %>%
        purrr::set_names("effects", "predictions", "all"),
      .f = summarise_conclusions_data,
      .id = "subset"
    ) %>%
    pivot_wider(
      names_from = Conclusion,
      values_from = n,
      values_fill = 0
    ) %>%
    ungroup()

  return(summarised_data)
}


#' Count qualitative conclusions across all analyses for each dataset
#'
#' @param data A dataframe containing the columns `split_id`, `analysis_id`, `dataset`, `Conclusion`
#'
#' @return A dataframe with counts `n` for each unique value of `Conclusion` for each `dataset`
#' @export
#' @import dplyr
#' @author Hannah S. Fraser
#' @author Elliot Gould
#' @examples
#' data(ManyEcoEvo)
#' ManyEcoEvo$data[[1]] %>%
#'   filter(Conclusion != "CHECK") %>%
#'   summarise_conclusions_data()
summarise_conclusions_data <- function(data) {
  data %>%
    ungroup() %>%
    filter(split_id == "1", analysis_id == "1") %>% # TODO how to generalise to data without split_id
    group_by(dataset, Conclusion) %>%
    count() %>%
    ungroup()
}
