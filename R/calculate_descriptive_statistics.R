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
#'   select(data) %>%
#'   unnest(everything()) %>%
#'   prepare_df_for_summarising()
#' @import dplyr
prepare_df_for_summarising <- function(data) {
  
  pointblank::expect_col_exists(
    data, 
    columns = c(
      num_fixed_variables,
      num_random_variables,
      sample_size,
      num_interactions,
      Bayesian, 
      mixed_model,
      num_fixed_effects,
      num_random_effects,
      linear_model
    ))
  
  data %>%
    ungroup() %>% 
    mutate(
      across(
        .cols = c(
          num_fixed_variables,
          num_random_variables,
          sample_size,
          num_interactions,
          Bayesian, # NA's coming from CHECK values
          mixed_model,
          num_fixed_effects,
          num_random_effects
        ),
        as.numeric
      ),
      lm = ifelse(linear_model == "linear", 1, 0),
      glm = ifelse(linear_model == "generalised", 1, 0)
    )
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
#' @importFrom pointblank expect_col_exists
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' filter_vars <- rlang::exprs(
#'   exclusion_set == "complete",
#'   estimate_type == "Zr",
#'   publishable_subset == "All",
#'   expertise_subset == "All",
#'   collinearity_subset == "All"
#' )
#' prepare_sorenson_summary_data(ManyEcoEvo::ManyEcoEvo_results,
#'   "all",
#'   id_subsets,
#'   subset_names,
#'   filter_expressions = filter_vars
#' )
prepare_sorenson_summary_data <- function(data, data_subset_name = "all", id_subsets = list(), subset_names = character(0L), filter_expressions = NULL) {
  # ---- Argument Checking ----
  if (length(id_subsets) != length(subset_names)) {
    cli::cli_abort("Length of `id_subsets` and `subset_names` must be equal")
  }
  
  required_colnames <- c("diversity_indices", "data")
  
  if (rlang::is_list(filter_expressions)) {
    if (!all(map_lgl(filter_expressions, rlang::is_call))) {
      cli_abort("{.arg filter_expressions} must be a list of calls")
    } else {
      required_colnames <-  filter_expressions %>% 
        map(rlang::f_lhs) %>% 
        map(rlang::as_string) %>% 
        list_c() %>%
        append(values = required_colnames) %>% 
        unique()
    }
  }
  
  pointblank::expect_col_exists(data, columns = required_colnames)
  
  # ---- Prepare Data ----
  
  out <-
    data %>%
    ungroup()
  
  if (!is.null(filter_expressions)) {
    out <- out %>%
      filter(!!!filter_expressions)
  }
  
  out %>%
    select(dataset, diversity_indices) %>%
    unnest(diversity_indices) %>%
    list(., {
      map(id_subsets, left_join, ., by = join_by("id_col"))
    }) %>%
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
#' @importFrom tidyr unnest separate_wider_delim
#' @importFrom purrr map list_flatten
#' @importFrom tibble tibble
#' @importFrom pointblank expect_col_exists
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' prepare_diversity_summary_data(
#'   ManyEcoEvo::ManyEcoEvo,
#'   "all",
#'   id_subsets,
#'   subset_names
#' ) # TODO consider adding filter_expressions
prepare_diversity_summary_data <- function(data, data_subset_name = "all", id_subsets = list(), subset_names = character(0L)) {
  if (length(id_subsets) != length(subset_names)) {
    cli::cli_abort("Length of `id_subsets` and `subset_names` must be equal")
  }
  
  pointblank::expect_col_exists(data, "diversity_data")
  
  data %>%
    select(diversity_data) %>%
    unnest(everything()) %>%
    mutate(new = id_col) %>%
    separate_wider_delim(new, "-",
                         names = c("response_id", "submission_id", "analysis_id", "split_id"), #TODO generalise
                         too_many = "merge"
    ) %>%
    mutate_at(c("submission_id", "analysis_id", "split_id"), as.numeric) %>%
    list(., {
      map(id_subsets, left_join, ., by = join_by("id_col"))
    }) %>%
    list_flatten() %>%
    tibble(data = ., subset_name = c(data_subset_name, subset_names))
}

#' Prepare data for summarising analyst summary statistics
#'
#' @description
#' Prepares the data for summarising summary statistics across an entire study by unnesting the
#' data and joining them to the prepared data in preparation for
#' summarising subsets of data with `summarise_study()`.
#'
#' @param data A ManyAnalyst style tibble containing the data to be analysed.
#' @param data_subset_name A character vector of length 1, the name of the subset of `data`.
#' @param id_subsets A list of tibbles containing the `id_col` for each subset of `data`.
#' @param subset_names A character vector equal to the length of `id_subsets`; the name of data subsets in `id_subsets`.
#'
#' @return A tibble containing subsets of analyst summary statistics `data`.
#'
#' @export
#' @importFrom cli cli_abort
#' @import dplyr
#' @importFrom tidyr unnest drop_na
#' @importFrom purrr map list_flatten
#' @importFrom tibble tibble
#' @importFrom janitor compare_df_cols
#' @importFrom pointblank expect_col_exists
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' prepare_analyst_summary_data(
#'   ManyEcoEvo::ManyEcoEvo,
#'   "all",
#'   id_subsets,
#'   subset_names
#' )
prepare_analyst_summary_data <- function(data, data_subset_name = "all", id_subsets, subset_names = character(0L)) {
  if (length(id_subsets) != length(subset_names)) {
    cli::cli_abort("Length of `id_subsets` and `subset_names` must be equal")
  }
  
  stopifnot( 
    "data should be a data.frame" = is.data.frame(data),
    "id_subsets must be a list" = 
      is.list(id_subsets),
    "Colnames of id_subsets are not equal" = 
      {janitor::compare_df_cols(id_subsets) %>% 
          drop_na() %>% 
          nrow()} >= 1
  )
  
  pointblank::expect_col_exists(data, columns = "data")
  
  make_subset <- function(x, y) {
    left_join(x, y, by = join_by("id_col")) %>%
      prepare_df_for_summarising()
  }
  
  data %>%
    select("data") %>%
    unnest(everything()) %>%
    prepare_df_for_summarising() %>%
    list(., {
      map(id_subsets, make_subset, .)
    }) %>%
    list_flatten() %>%
    tibble(data = ., subset_name = c(data_subset_name, subset_names))
}

#' Summarise ManyAnalyst study data
#'
#' @description
#' Summarises the ManyAnalyst study data by calculating summary statistics for each subset of data.
#'
#' @param ManyEcoEvo A ManyAnalyst style tibble containing the data to be analysed.
#' @param ManyEcoEvo_results A ManyAnalyst results style tibble containing the results of the data to be analysed.
#' @param id_subsets A list of tibbles containing the `id_col` for each subset of `data`.
#' @param subset_names A character vector equal to the length of `id_subsets`; the name of data subsets in `id_subsets`.
#' @param filter_vars A list of expressions to filter `ManyEcoEvo_results` data
#'
#' @return A tibble containing summary statistics for each subset of data.
#'
#' @export
#' @importFrom cli cli_abort
#' @import dplyr
#' @importFrom purrr map reduce reduce2 pmap list_rbind set_names map_lgl list_c pluck
#' @importFrom tidyr pivot_longer pivot_wider unnest spread drop_na
#' @importFrom tibble enframe
#' @importFrom pointblank expect_col_exists
#' @importFrom rlang is_null is_list
#' @examples
#' id_subsets <- list(ManyEcoEvo:::effect_ids, ManyEcoEvo:::prediction_ids)
#' subset_names <- c("effects", "predictions")
#' filter_vars <- rlang::exprs(
#'   exclusion_set == "complete",
#'   estimate_type == "Zr",
#'   publishable_subset == "All",
#'   expertise_subset == "All",
#'   collinearity_subset == "All"
#' )
#' summarise_study(ManyEcoEvo::ManyEcoEvo, ManyEcoEvo::ManyEcoEvo_results, id_subsets, subset_names, filter_vars = filter_vars)
summarise_study <- function(ManyEcoEvo, ManyEcoEvo_results, id_subsets, subset_names, filter_vars = NULL) {
  
  if (length(id_subsets) != length(subset_names)) {
    cli::cli_abort("Length of `id_subsets` and `subset_names` must be equal")
  }
  
  stopifnot(
    is.data.frame(ManyEcoEvo),
    is.data.frame(ManyEcoEvo_results),
    rlang::is_null(filter_vars) | rlang::is_list(filter_vars)
  )
  
  required_columns_ManyEcoEvo <- c("diversity_data")
  
  pointblank::expect_col_exists(
    ManyEcoEvo, 
    columns = required_columns_ManyEcoEvo
  )
  
  required_colnanmes_ManyEcoEvo_results <- NULL
  
  if (rlang::is_list(filter_vars)) {
    if (!all(map_lgl(filter_vars, rlang::is_call))) {
      cli_abort("{.arg filter_vars} must be a list of calls")
    } else {
      required_colnanmes_ManyEcoEvo_results <-  filter_vars %>% 
        map(rlang::f_lhs) %>% 
        map(rlang::as_string) %>% 
        list_c() %>%
        append(values = required_colnanmes_ManyEcoEvo_results) %>% 
        unique()
    }
  }
  
  pointblank::expect_col_exists(
    ManyEcoEvo_results, 
    columns = required_colnanmes_ManyEcoEvo_results
  )
  
  
  # ------ Prepare Summary Data Subsets ------
  
  subsets_tibble <- ManyEcoEvo %>%
    prepare_analyst_summary_data(
      "all",
      id_subsets,
      subset_names
    )
  
  subsets_tibble_sorensen <- ManyEcoEvo_results %>%
    prepare_sorenson_summary_data("all",
                                  id_subsets,
                                  subset_names,
                                  filter_expressions = filter_vars
    )
  
  subsets_tibble_variables <- ManyEcoEvo %>%
    prepare_diversity_summary_data(
      "all",
      id_subsets,
      subset_names
    )
  
  var_names <-
    ManyEcoEvo %>%
    pull(diversity_data) %>%
    map(~ .x %>%
          select(-id_col, -dataset)
        %>%
          colnames()) %>%
    enframe("dataset", "variable")
  
  # ------ Calculate Summary Statistics ------
  
  ## ----- Descriptive Stats Summary ------
  
  # Analysis Teams Per Dataset, Per Subset
  teams_per_subset <- subsets_tibble %>%
    pmap(~ count(.x, dataset, TeamIdentifier, sort = TRUE) %>%
           mutate(subset = .y)) %>%
    list_rbind()
  
  # actually counting nunmber of analyses per team
  # all_analyst_data %>%
  #     group_by(dataset) %>%
  #     count(TeamIdentifier, sort = TRUE) %>%
  #     tally(n,name = "totalanalyses") %>%
  #     mutate(subset = "all")
  
  # Teams Per Analysis, Per Subset
  Total_Teams_Per_Analysis <-
    pmap(
      subsets_tibble,
      ~ pluck(.x, "TeamIdentifier", n_distinct) %>%
        set_names(.y) %>%
        enframe("subset", "n_teams")
    ) %>%
    list_rbind()
  
  # Number of Teams and Total Analyses per dataset for each subset
  Team_Analyses <- map(
    list(
      calc_teams_per_dataset,
      calc_analyses_per_team
    ),
    ~ pmap(subsets_tibble, .x) %>%
      list_rbind()
  ) %>%
    reduce(full_join,
           by = join_by("dataset", "subset")
    )
  
  
  # Calculate Summary Statistics for Binary and Numeric Variables
  
  Table2 <- subsets_tibble %>%
    pmap(calc_summary_stats_numeric) %>%
    list_rbind() %>%
    group_by(dataset, subset) %>%
    pivot_longer(
      cols = where(is.numeric),
      names_to = c(".value", "variable"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    ungroup()
  
  Table1 <- subsets_tibble %>%
    pmap(calc_summary_stats_binary) %>%
    list_rbind() %>%
    full_join(Team_Analyses,
              by = join_by("dataset", "subset")
    ) %>%
    arrange(dataset, subset) %>%
    relocate(
      dataset,
      subset,
      totalanalyses,
      everything()
    )
  
  ## ----- Coding variable inclusion across analyses -----
  Table3 <- subsets_tibble_variables %>%
    pmap(calculate_variable_counts) %>%
    list_rbind() %>%
    pivot_wider(names_from = subset, values_from = count) %>%
    right_join(
      var_names %>%
        unnest(variable),
      by = join_by(variable)
    ) %>%
    pivot_longer(
      cols = all:predictions,
      names_to = "subset",
      values_to = "value"
    ) %>%
    group_by(subset, dataset) %>%
    summarise(
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      min = min(value, na.rm = T),
      max = max(value, na.rm = T),
      .groups = "drop"
    )
  
  ## ------ Conclusions analysis -----
  Table4 <- subsets_tibble %>%
    pmap(.f = count_conclusions) %>%
    list_rbind() %>%
    spread(Conclusion, count, fill = 0)
  
  ## ----- Sorensen all_diversity_data Index Data -----
  SorensenSummary <-
    subsets_tibble_sorensen %>%
    unnest(cols = "data") %>%
    drop_na(dataset) %>%
    group_by(dataset, subset_name) %>%
    summarise(
      mean = mean(mean_diversity_index, na.rm = TRUE),
      sd = sd(mean_diversity_index, na.rm = TRUE),
      min = min(mean_diversity_index, na.rm = TRUE),
      max = max(mean_diversity_index, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(subset = subset_name)
  
  # ----- Combine Outputs -----
  
  list(subsets_tibble, subsets_tibble_variables, subsets_tibble_sorensen) %>%
    reduce(left_join, by = join_by("data", "subset_name")) %>%
    left_join(Total_Teams_Per_Analysis,
              by = join_by("subset_name" == "subset")
    ) %>%
    reduce2(
      .x = list(SorensenSummary, teams_per_subset, Table4, Table3, Table2, Table1),
      .y = c(
        "sorensen_summary",
        "teams_per_subset",
        "conclusions_summary",
        "variable_count_summary",
        "model_term_summary",
        "model_type_summary"
      ),
      nest_join,
      by = join_by("subset_name" == "subset"),
      copy = FALSE,
      keep = NULL,
      .init = .
    ) %>%
    relocate(data, .after = subset_name)
}

# ----- Summary Statistics Functions -----

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
#'   select(data) %>%
#'   unnest(everything()) %>%
#'   prepare_df_for_summarising() %>%
#'   calc_teams_per_dataset("all")
#' @import dplyr
calc_teams_per_dataset <- function(data, subset_name = character(1L)) {
  
  pointblank::expect_col_exists(data, columns = c("TeamIdentifier", "dataset"))
  
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
#'   select(data) %>%
#'   unnest(everything()) %>%
#'   prepare_df_for_summarising() %>%
#'   calc_analyses_per_team("All")
#' @import dplyr
#' @importFrom pointblank expect_col_exists
calc_analyses_per_team <- function(data, subset_name = character(1L)) { # TODO this is calculating number of analyses per dataset not number of analyses per team per dataset ...
  
  pointblank::expect_col_exists(data, columns = c("dataset"))
  
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
#' @importFrom pointblank expect_col_exists
#' @import dplyr
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>%
#'   select(data) %>%
#'   unnest(everything()) %>%
#'   prepare_df_for_summarising() %>%
#'   calc_summary_stats_numeric("All")
calc_summary_stats_numeric <- function(data, subset_name = character(1L)) {
  
  pointblank::expect_col_exists(data, 
                                columns = c("dataset",
                                            "num_fixed_effects",
                                            "num_random_effects",
                                            "sample_size",
                                            "num_interactions"))
  
  data %>%
    group_by(dataset) %>%
    summarise(
      across(
        .cols = c(
          fixed = num_fixed_effects,
          random = num_random_effects,
          samplesize = sample_size,
          interactions = num_interactions,
        ),
        .fns = list(
          mean = ~ mean(.x, na.rm = TRUE) %>% round(2),
          sd = ~ sd(.x, na.rm = TRUE) %>% round(2),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.fn}_{.col}"
      ),
      subset = subset_name
    )
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
#' @importFrom pointblank expect_col_exists
#' @import dplyr
#' @examples
#' ManyEcoEvo::ManyEcoEvo %>%
#'   select(data) %>%
#'   unnest(everything()) %>%
#'   prepare_df_for_summarising() %>%
#'   calc_summary_stats_binary("All")
calc_summary_stats_binary <- function(data, subset_name = character(1L)) {
  
  pointblank::expect_col_exists(data, 
                                columns = c("dataset",
                                            "lm",
                                            "mixed_model",
                                            "Bayesian"))
  
  data %>%
    group_by(dataset) %>%
    summarise(.,
              sum_linear = sum(lm, na.rm = T),
              sum_mixed = sum(mixed_model, na.rm = T),
              sum_Bayesian = sum(Bayesian, na.rm = T), 
              subset = subset_name
    )
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
calculate_variable_counts <- function(data, subset_name = character(1L)) {
  colSums(!is.na(data)) %>%
    enframe(name = "variable", value = "count") %>%
    mutate(subset = subset_name)
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
#' @importFrom pointblank expect_col_exists
count_conclusions <- function(data, subset_name = character(1L)) {
  # ----- Argument Checking ------
  stopifnot(
    is.data.frame(data),
    is.character(subset_name)
  )
  
  pointblank::expect_col_exists(object = data, 
                                columns =  
                                  c(
                                    contains("Conclusion"), 
                                    "split_id", 
                                    "analysis_id",
                                    "dataset"
                                  )
  )
  
  out <- data %>%
    filter(split_id == 1 & analysis_id == 1) %>% #TODO switch to using `first()` so that we don't require columns split_id and analysis_id to exist
    group_by(dataset, pick(contains("Conclusion"))) %>%
    summarise(count = n(), .groups = "drop") %>%
    filter(
      if_any(contains("Conclusion"), ~ !is.na(.x)),
      if_any(contains("Conclusion"), ~ .x != "CHECK") #TODO rm
    ) %>%
    mutate(subset = subset_name)
  
  return(out)
}
