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
#' Should be run after *all* data subsetting is complete, otherwise the diversity indices will need to be recalculated.
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
#' @importFrom cli cli_alert_warning cli_h1
compute_MA_inputs <- function(ManyEcoEvo, estimate_type = NULL) {
  # TODO should be renamed something to do with diversity indices... that's the
  # only thing happening here!!
  match.arg(estimate_type, choices = c("Zr", "yi", "y50", "y75", "y25", NULL), several.ok = FALSE)
  cli::cli_h1(c("Computing Sorensen diversity indices inputs"))
  if (!rlang::is_null(estimate_type)) {
    if (pointblank::test_col_exists(ManyEcoEvo, "estimate_type")) {
      ManyEcoEvo <- dplyr::select(ManyEcoEvo, -estimate_type)
      cli::cli_alert_warning(text = c(
        "Column {.arg estimate_type} already exists in {.arg ManyEcoEvo},",
        " and will be overwritten by supplied value",
        " of {.arg estimate_type} = {.val {estimate_type}}"))
    }
    
    df <- ManyEcoEvo %>%
      pointblank::col_exists(c("diversity_data", "data")) %>% 
      mutate(
        diversity_indices = map(diversity_data, apply_sorensen_calc),
        effects_analysis = map2(
          .x = data, 
          .y = diversity_indices,
          .f = ~ left_join(.x, .y, by = join_by("id_col")) %>%
            rename(study_id = id_col) %>%
            mutate(estimate_type = !!{{ estimate_type }})
        ),
        estimate_type = !!{{ estimate_type }}
      )
  } else {
    df <- ManyEcoEvo %>%
      pointblank::col_exists(c("estimate_type", "diversity_data", "data")) %>%
      mutate(
        diversity_indices = map(diversity_data, 
                                apply_sorensen_calc),
        effects_analysis = pmap(
          .l = list(data, diversity_indices, estimate_type),
          .f = ~ left_join(..1, ..2, by = join_by("id_col")) %>%
            rename(study_id = id_col) %>%
            mutate(estimate_type = ..3)
        )
      ) 
  }
  
  return(df)
}
