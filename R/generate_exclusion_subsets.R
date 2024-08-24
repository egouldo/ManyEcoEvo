#' Generate subsets of analyst data based on different exclusion criteria
#'
#' @param ManyEcoEvo A dataframe
#' @param estimate_type character vector, one of \code{"Zr", "yi", "y25", "y50", "y75", NULL}.
#' @details If `estimate_type` is `NULL`, the column `estimate_type` must be in `ManyEcoEvo`.
#' @note This function uses the functions`subset_fns_Zr`and `subset_fns_yi` to create named lists with elements containing `rlang_lambda_function`s, whose element name is the name of the `rlang_lambda_function`
#' @return A `dataframe` grouped by dataset and exclusion_set that contains subsets of `data` and `diversity_data` based on exclusion criteria functions defined in `subset_fns_Zr` and `subset_funs_yi`, and the `estimate_type` column.
#' @export
#' @note To be run after `generate_exclusion_subsets()`
#' @family Multi-dataset Wrapper Functions
generate_exclusion_subsets <- function(ManyEcoEvo, estimate_type = NULL) {
  # generates datasets according to different exclusion criteria
  # Generates the equivalent datasets with different combinations of outliers removed
  # expects nested data frame grouped by dataset with analysis data stored in list-col variable `data`
  # and raw diversity index data stored in variable `diversity_data`

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

  if (!rlang::is_null(estimate_type)) {
    if (pointblank::test_col_exists(ManyEcoEvo, "estimate_type")) {
      ManyEcoEvo <- ManyEcoEvo %>%
        select(-"estimate_type") # will be replaced with `estimate_type` arg.
    }

    df <- subset_fns_df %>%
      dplyr::filter(estimate_type == !!{{ estimate_type }}) %>%
      dplyr::mutate(ManyEcoEvo = list(ManyEcoEvo)) %>%
      tidyr::unnest(ManyEcoEvo) %>%
      dplyr::group_by(dataset, exclusion_set, estimate_type) %>%
      dplyr::transmute(
        data =
          purrr::map2(
            .x = fns,
            .y = data,
            .f = exec
          ),
        diversity_data = # this step filters diversity_data according to matches in data, is also applied in prepare_yi
          purrr::map2(
            .x = diversity_data,
            .y = data,
            .f = ~ dplyr::semi_join(.x, .y) %>% distinct()
          )
      ) # TODO duplicate cols for euc R_1LRqq2WHrQaENtM, glasgow?
  } else { # Expects estimate_type is stored in column `estimate_type`
    df <- ManyEcoEvo %>% # TODO should we pointblank::stop_if_not(pointblank::test_col_exists(ManyEcoEvo, "estimate_type")) ??
      dplyr::left_join(subset_fns_df) %>%
      dplyr::group_by(dataset, exclusion_set) %>%
      dplyr::mutate(
        data =
          purrr::map2(
            .x = fns,
            .y = data,
            .f = exec
          ),
        diversity_data = # this step filters diversity_data according to matches in data, is also applied in prepare_yi
          map2(
            .x = diversity_data,
            .y = data,
            .f = ~ semi_join(.x, .y) %>% distinct()
          ),
        .keep = "unused"
      )
  }

  return(df)
}
