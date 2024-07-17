# ----- Define Helper Functions for Calculating Summary Statistics -----

#' Prepare data for summarising descriptive statistics
#' 
#' @description
#' Calculates the number of fixed variables, the number of random variables, 
#' the sample size, the number of interactions, the number of linear models, 
#' the number of generalised models, the number of fixed effects, and the number 
#' of random effects for each analysis in the dataset. 
#' Also codes whether each analysis employs a linear model, a generalised model, 
#' or a Bayesian model.
#' 
#' @param data A tibble containing many-analyst data to be summarised.
#'
#' @return A tibble containing the data prepared for summarising.
#' @export
#'
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>% 
#' select(data) %>% 
#' unnest(everything()) %>% 
#' prepare_df_for_summarising()
#' @import dplyr
prepare_df_for_summarising <- function(data){
  data %>% 
    mutate(across(.cols = c(num_fixed_variables,
                            num_random_variables,
                            sample_size,
                            num_interactions,
                            Bayesian, #NA's coming from CHECK values
                            mixed_model,
                            num_fixed_effects,
                            num_random_effects), 
                  as.numeric),
           lm = ifelse(linear_model == "linear", 1, 0),
           glm = ifelse(linear_model == "generalised", 1, 0))
}

#' Prepare data for summarising Sorensen diversity indices
#' 
#' @description
#' Prepares the data for summarising Sorensen diversity indices across an entire study by unnesting the
#' diversity indices and joining them to the prepared data in preparation for
#' summarising subsets of data with `summarise_study()`.
#' 
#' @param data A ManyAnalyst style tibble containing the data to be analysed.
#' @param data_subset_name A character vector of length 1, the name of the subset of `data`.
#' @param id_subsets A list of tibbles containing the `id_col` for each subset of `data`.
#' @param subset_names A character vector equal to the length of `id_subsets`; the name of data subsets in `id_subsets`.
#' @param filter_expressions A list of expressions to filter the data by.
#' 
#' @return A tibble containing subsets of Sorensen diversity indices `data`.
#' 
#' @export
#' @importFrom cli cli_abort
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom purrr list_flatten
#' @importFrom tibble tibble
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' filter_vars <- rlang::exprs(exclusion_set == "complete", 
#' estimate_type == "Zr", 
#' publishable_subset == "All", 
#' expertise_subset == "All", 
#' collinearity_subset == "All")
#' prepare_sorenson_summary_data(ManyEcoEvo::ManyEcoEvo_results, 
#' "all", 
#' id_subsets, 
#' subset_names, 
#' filter_expressions = filter_vars)
prepare_sorenson_summary_data <- function(data, data_subset_name = "all", id_subsets = list(), subset_names = character(0L), filter_expressions = NULL) {

  if(length(id_subsets) != length(subset_names)){
    cli::cli_abort("Length of `id_subsets` and `subset_names` must be equal")
  }
  
  out <- 
    data %>% 
    ungroup
  
  if(!is.null(filter_expressions)){
    out <- out %>% 
      filter(!!!filter_expressions)
  }
  
  out %>% 
    select(dataset, diversity_indices) %>% 
    unnest(diversity_indices) %>% 
    list(.,
         {map(id_subsets, left_join, ., by = join_by("id_col"))}) %>% 
    list_flatten() %>% 
    tibble(data = ., subset_name = c(data_subset_name, subset_names))

}

#' Prepare data for summarising variable diversity
#' 
#' @description
#' Prepares the data for summarising variable diversity across an entire study by unnesting the
#' diversity data and joining them to the prepared data in preparation for
#' summarising subsets of data with `summarise_study()`.
#' 
#' @param data A ManyAnalyst style tibble containing the data to be analysed.
#' @param data_subset_name A character vector of length 1, the name of the subset of `data`.
#' @param id_subsets A list of tibbles containing the `id_col` for each subset of `data`.
#' @param subset_names A character vector equal to the length of `id_subsets`; the name of data subsets in `id_subsets`.
#' 
#' @return A tibble containing subsets of variable diversity data `data`.
#' 
#' @export
#' @importFrom cli cli_abort
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom purrr list_flatten
#' @importFrom tibble tibble
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' prepare_diversity_summary_data(ManyEcoEvo::ManyEcoEvo,
#' "all",
#' id_subsets,
#' subset_names) #TODO consider adding filter_expressions
prepare_diversity_summary_data <- function(data, data_subset_name = "all", id_subsets = list(), subset_names = character(0L) ) {
  data %>% 
    select(diversity_data) %>% 
    unnest(everything()) %>% 
    mutate(new = id_col) %>% 
    separate_wider_delim(new, "-", 
                         names = c("response_id", "submission_id", "analysis_id", "split_id"), 
                         too_many = "merge") %>%
    mutate_at(c("submission_id", "analysis_id", "split_id"), as.numeric) %>% 
    list(.,
         {map(id_subsets, left_join, ., by = join_by("id_col"))}) %>% 
    list_flatten() %>% 
    tibble(data = ., subset_name = c(data_subset_name, subset_names))
  
}
#'  Calculate the number of teams per dataset for a given subset
#'
#' @param data A tibble containing the data to be analysed.
#' @param subset_name A character vector of length 1, the name of the subset of data being analysed.
#'
#' @return A tibble containing the number of `teams` per `dataset` for a given `subset_name`.
#' @export
#'
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>% 
#' select(data) %>% 
#' unnest(everything()) %>% 
#' prepare_df_for_summarising() %>% 
#' calc_teams_per_dataset("all")
#' @import dplyr
calc_teams_per_dataset <- function(data, subset_name = character(1L)){
  data %>% 
    group_by(dataset) %>% 
    count(TeamIdentifier) %>% 
    tally(name = "teams") %>% 
    mutate(subset = subset_name)
}

#' Calculate total number of analyses per team for a given subset
#' 
#' @description Calculates the number of analyses conducted by each team for each dataset in a given subset.
#'
#' @param data A tibble containing the data to be analysed.
#' @param subset_name A character vector of length 1, the name of the subset of data being analysed.
#'
#' @return A tibble containing the number of `analyses` per `team` for each `dataset` in a given `subset_name`.
#' @export
#'
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>% 
#' select(data) %>% 
#'   unnest(everything()) %>% 
#'   prepare_df_for_summarising() %>% 
#'   calc_analyses_per_team("All")
#' @import dplyr
calc_analyses_per_team <- function(data, subset_name = character(1L)){ #TODO this is calculating number of analyses per dataset not number of analyses per team per dataset ... 
  data %>% 
    count(dataset, name = "totalanalyses") %>% 
    mutate(subset = subset_name)
}

#' Calculate summary statistics for numeric summary variables 
#' 
#' @description
#' Calculates the mean, standard deviation, minimum and maximum for each numeric summary variable (See [prepare_df_for_summarising()]).
#' 
#' for numeric 
#' variables used in analyses of each dataset, for a given subset. 
#' Summary statistics are aggregated across variable type \(Number of fixed variables 
#' within the analysis, number of random variables within the analysis, 
#' analysis samplesize, number of interaction terms within the analysis\).
#' 
#' @param data A tibble containing the data to be analysed.
#' @param subset_name A character vector of length 1, the name of the subset of 
#' data being analysed.
#'
#' @return A tibble containing the mean, standard deviation, minimum and maximum 
#' values for each numeric variable used in analyses of each dataset for a given subset.
#' @export
#'
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>% 
#' select(data) %>% 
#'   unnest(everything()) %>% 
#'   prepare_df_for_summarising() %>% 
#'   calc_summary_stats_numeric("All")
calc_summary_stats_numeric <- function(data, subset_name = character(1L)){
  data %>% 
    group_by(dataset) %>% 
    summarise(across(.cols = c(fixed = num_fixed_effects,
                               random = num_random_effects,
                               samplesize = sample_size,
                               interactions = num_interactions,
    ),
    .fns = list(mean = ~ mean(.x, na.rm = T) %>% round(2),
                sd = ~ sd(.x, na.rm = T) %>% round(2),
                min = ~ min(.x, na.rm = T),
                max = ~ max(.x, na.rm = T)),
    .names = "{.fn}_{.col}"),
    subset = subset_name)
}

#' Calculate summary statistics for binary summary variables 
#'
#' @description
#' Calculates the total number of analyses using linear models, mixed models, 
#' and Bayesian models for each dataset, for a given subset. 
#' See [prepare_df_for_summarising()] for details on the binary variables.
#' 
#'
#' @param data A tibble containing the data to be analysed.
#' @param subset_name A character vector of length 1, the name of the subset of data being analysed.
#'
#' @return A tibble containing the sum of binary variables used in analyses of each dataset for a given subset.
#' @export
#'
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>%
#' select(data) %>%
#' unnest(everything()) %>%
#' prepare_df_for_summarising() %>%
#' calc_summary_stats_binary("All")
calc_summary_stats_binary <- function(data, subset_name = character(1L)) {
  data %>% 
    group_by(dataset) %>% 
    summarise(., 
              sum_linear =sum(lm,na.rm=T), 
              sum_mixed= sum(mixed_model,na.rm=T),
              sum_Bayesian= sum(Bayesian,na.rm=T), subset = subset_name)
}

#' Count the number of times variables are used across analyses
#' 
#' @description
#' This function is used to count the number of times each variable is used across
#' the analyses in the dataset. The output is a tibble with the columns `variable`
#' and `count` which contains the number of times each variable is used across the
#' analyses in the dataset.

#' Count the number of times variables are used across analyses
#'
#' @param data A tibble of variables used in analyses of each dataset, for a given subset.
#' @param subset_name A character vector of length 1, the name of the subset of data being analysed.
#'
#' @return A tibble containing the number of times each variable is used across the analyses in the dataset.
#' @export
#' 
#' @details
#' Takes a tibble of diversity data, i.e. data that is ready for computing Sorensen 
#' diversity indices and computes the number of times each variable is used across 
#' the analyses. Note, that the function does not group by dataset, as the layout of the
#' dataset assumes that each variable within a given dataset does not occur in another dataset.
#' @importFrom tibble enframe
#' @import dplyr
calculate_variable_counts <- function(data, subset_name = character(1L)){
  colSums(!is.na(data)) %>% 
    enframe(name = "variable", value = "count") %>% 
    mutate( subset = subset_name )
}

#' Count the number of different conclusions made by analysts across each dataset.
#'
#' @param data A tibble containing the data to be analysed.
#' @param subset_name A character vector of length 1, the name of the subset of data being analysed.
#'
#' @return A tibble containing counts of each conclusion type made by analysts across each dataset, for a given subset.
#' @description
#' Takes the first analysis per team per dataset, not each analysis that was submitted. 
#' Thus filters for analyses where split_id == 1 and where analysis_id == 1
#' 
#' @export
#' @import dplyr
count_conclusions <- function(data, subset_name = character(1L)){
  data %>% 
    filter(split_id == 1 & analysis_id == 1) %>% 
    group_by(dataset, pick(contains("Conclusion"))) %>% 
    summarise(count = n(), .groups = "drop") %>% 
    filter(if_any(contains("Conclusion"), ~ !is.na(.x)),
           if_any(contains("Conclusion"), ~ .x != "CHECK")) %>% 
    mutate(subset = subset_name)
}
