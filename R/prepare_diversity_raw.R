#' Prepare diversity index data
#' @description Prepares data required for calculating Sorensen diversity indices across all analyses. \(Non-targets function\)
#'
#' @param master_data_raw Raw dataset of both eucalyptus and blue tit data
#' @param master_raw_metadata Metadata describing the variable names in `master_data_raw`
#'
#' @return A tibble of raw diversity index data
#' @export
prepare_diversity_raw <- function(master_data_raw, master_raw_metadata) {
  # assumes some preprocessing by prepare_ManyEcoEvo()
  # variable keys for extracting diversity index data
  # returns a nested dataframe with raw diversity data for both
  # datasets  (euc and bt)
  eucalyptus_variable_key <- master_raw_metadata %>%
    dplyr::filter(stringr::str_detect(variable_description, "eucalyptus variable:"))

  blue_tit_variable_key <- master_raw_metadata %>%
    dplyr::filter(stringr::str_detect(variable_description, "Blue tit variable:"))

  diversity_index_data_raw <-
    purrr::map2(
      .x = c("eucalyptus", "blue tit"),
      .y = list(
        eucalyptus_variable_key$variable_name,
        blue_tit_variable_key$variable_name
      ),
      .f = ~ get_diversity_data(master_data_raw, dataset = .x, variables = .y)
    ) %>%
    purrr::set_names(., c("eucalyptus", "blue tit")) %>%
    tibble::tibble(dataset = names(.), diversity_data = .)

  return(diversity_index_data_raw)
}
