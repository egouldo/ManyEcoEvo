#' Compute meta-analysis inputs for a nested-dataframe containing different datasets/subsets of analyst data
#' 
#' @description 
#' 
#' Computes the Sorensen diversity indices and joins it to data in preparation for meta-analysing all subsets of data with [meta_analyse_datasets()].
#'
#' @param ManyEcoEvo A dataframe grouped by the character columns `dataset`, `estimate_type`, `exclusion_set`. Each group corresponds to a subset of the full `dataset`, and has the subset analyst data  stored in `data`, with its corresponding subset `diversity_data`.
#' @param estimate_type character string, one of "Zr", "yi", "y25", "y50", "y75".
#' @details 
#'
#' Computes Sorensen diversity indices `diversity_indices` for each subset of data returning them in the list-column `diversity_indices` and joins them to the relevant subset of processed analyst `data` within the list-column `effects_analysis`.
#' 
#' Note that, the name of the subset is derived from the functions within by [subset_fns_yi()] and/or [subset_fns_Zr()] called in the previous step of data processing [generate_exclusion_subsets()]. 
#' Should the user wish to skip the `generate_exclusion_subsets()` step, they can supply arbitrary values for `exclusion_set` and the function will still work. 
#' 
#' @seealso [apply_sorensen_calc()] is used to calculate the Sorensen diversity indices.
#' 
#' @return A dataframe that includes the additional columns in `ManyEcoEvo`, but with added columns `diversity_indices` and `effects_analysis`.
#' @export
#' @family Multi-dataset Wrapper Functions
#' @import dplyr
#' @importFrom purrr map2 map pmap
#' @importFrom rlang is_null
#' @importFrom pointblank col_exists test_col_exists
compute_MA_inputs <- function(ManyEcoEvo, estimate_type = NULL) {
  # TODO should be renamed something to do with diversity indices... that's the
  # only thing happening here!!
  match.arg(estimate_type, choices = c("Zr", "yi", "y50", "y75", "y25", NULL), several.ok = FALSE)
  # TODO insert check to ensure that if estimate_type supplied, there is NO col with estiamte_type in ManyEcoEvo?
  if (!rlang::is_null(estimate_type)) {
    df <- ManyEcoEvo %>%
      mutate(
        diversity_indices = map(diversity_data, apply_sorensen_calc),
        effects_analysis = map2(
          .x = data, 
          .y = diversity_indices, # TODO post-dev, remove data/diversity_data cols??
          .f = ~ left_join(.x, .y) %>%
            rename(study_id = id_col) %>%
            mutate(estimate_type = !!{{ estimate_type }})
        )
      ) %>% #
      dplyr::group_by(exclusion_set, dataset, estimate_type)
  } else {
    df <- ManyEcoEvo %>%
      pointblank::col_exists("estimate_type") %>%
      mutate(
        diversity_indices = map(diversity_data, 
                                apply_sorensen_calc),
        effects_analysis = pmap(
          .l = list(data, diversity_indices, estimate_type),
          .f = ~ left_join(..1, ..2) %>%
            rename(study_id = id_col) %>%
            mutate(estimate_type = ..3)
        )
      ) %>% # TODO can we remove estimate type from this df?
      dplyr::group_by(exclusion_set, dataset, estimate_type)
  }

  return(df)
}
