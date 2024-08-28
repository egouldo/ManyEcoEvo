#' Generate subsets of analyst data based on different exclusion criteria
#' 
#' @description
#' Generates subsets of data with different combinations of outliers removed for different `exclusion_set`s.
#'
#' @param ManyEcoEvo A dataframe containing at minimum the character column `dataset` and list-columns `data` and `diversity_data`.
#' @param estimate_type character vector, one of \code{"Zr", "yi", "y25", "y50", "y75", NULL}.
#' @details 
#' If `estimate_type` is `NULL`, the column `estimate_type` must be in `ManyEcoEvo`. 
#' 
#' This function uses the functions [subset_fns_Zr()] and []subset_fns_yi() to create named lists with elements containing `purrr::`-style lambda functions, whose element name is the name of the function.
#' 
#' If `NULL` is provided to the argument `estimate_type`, then the column `estimate_type` must exist in `ManyEcoEvo`, as this column will be used to filter the exclusion criteria functions.
#' 
#' The value of column `exclusion_set` in the returned object will be the name of the exclusion criteria function, with the prefix "subset_" removed, derived from either [subset_fns_Zr()] or [subset_funs_yi()], depending on the `estimate_type`.
#' 
#' **Note**: This function should be exectued after [prepare_response_variables()] and [generate_yi_subsets()].
#' @return A `dataframe` grouped by `dataset` and `exclusion_set` that contains subsets of `data` and `diversity_data` based on exclusion criteria functions defined in [subset_fns_Zr()] and [subset_funs_yi()], and the `estimate_type` column. 
#' 
#' @export
#' @family Multi-dataset Wrapper Functions
#' @import dplyr
#' @importFrom stringr str_remove
#' @importFrom tidyr unnest_longer
#' @importFrom purrr map2 exec
#' @importFrom pointblank test_col_exists
#' @importFrom rlang is_null
generate_exclusion_subsets <- function(ManyEcoEvo, estimate_type = NULL) {

  match.arg(
    arg = estimate_type,
    choices = c("Zr", "yi", "y25", "y50", "y75", NULL),
    several.ok = FALSE
  )
  cli::cli_h1("Applying exclusion rules and generating exclusion subsets")
  
  subset_fns_df <- tibble::tibble(
    exclusion_set = names(subset_fns_Zr()) %>%
      stringr::str_remove(., "subset_"),
    fns = subset_fns_Zr()
  ) %>%
    dplyr::group_by(exclusion_set) %>%
    dplyr::mutate(estimate_type = "Zr") %>%
    dplyr::bind_rows(
      .,
      tibble::tibble(
        exclusion_set = names(subset_fns_yi()) %>% # TODO expose this as a function argument
          stringr::str_remove(., "subset_"),
        fns = subset_fns_yi()
      ) %>%
        dplyr::mutate(estimate_type = list(c("yi", "y50", "y25", "y75"))) %>%
        tidyr::unnest_longer(estimate_type) %>%
        dplyr::group_by(exclusion_set)
    )

  if (!rlang::is_null(estimate_type)) { # When argument `estimate_type` is not NULL
    if (pointblank::test_col_exists(ManyEcoEvo, "estimate_type")) {
      ManyEcoEvo <- ManyEcoEvo %>%
        select(-"estimate_type")
    }

    df <- subset_fns_df %>%
      pointblank::col_exists("estimate_type") %>%
      dplyr::filter(estimate_type == !!{{ estimate_type }}) %>%
      dplyr::mutate(ManyEcoEvo = list(ManyEcoEvo)) %>%
      tidyr::unnest(ManyEcoEvo) %>%
      ungroup() %>% 
      dplyr::mutate(
        data =
          purrr::map2(
            .x = fns,
            .y = data,
            .f = exec
          ),
        diversity_data = 
          purrr::map2(
            .x = diversity_data,
            .y = data,
            .f = ~ dplyr::semi_join(.x, .y, by = join_by("id_col")) %>% 
              distinct()
          ),
        .keep = "unused"
      ) # TODO duplicate cols for euc R_1LRqq2WHrQaENtM, glasgow?
  } else { # When argument estimate_type is NULL
    df <- ManyEcoEvo %>%
      pointblank::col_exists("estimate_type") %>%
      dplyr::left_join(subset_fns_df, by = join_by("estimate_type")) %>%
      ungroup() %>%
      dplyr::mutate(
        data =
          purrr::map2(
            .x = fns,
            .y = data,
            .f = exec
          ),
        diversity_data =
          map2(
            .x = diversity_data,
            .y = data,
            .f = ~ semi_join(.x, .y, by = join_by("id_col")) %>% 
              distinct()
          ),
        .keep = "unused"
      ) 
  }

  return(df)
}
