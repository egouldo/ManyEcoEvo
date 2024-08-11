#' Get Diversity Data
#'
#' @param raw_data A tibble of raw data
#' @param dataset character string of either "eucalyptus" or "blue tit"
#' @param variables character vector of any length containing names of variables to get diversity data for
#'
#' @return A tibble of diversity data for either 'eucalyptus' or 'blue tit' analyses
#' @export
#' @details Called by `prepare_diversity_raw`
get_diversity_data <- function(raw_data, dataset, variables = character()) {
  match.arg(dataset, choices = c("eucalyptus", "blue tit"), several.ok = FALSE)
  raw_data %>%
    dplyr::select(
      submission_id,
      analysis_id,
      split_id,
      TeamIdentifier,
      dataset,
      {{ variables }}
    ) %>%
    dplyr::filter(dataset == {{ dataset }}) %>%
    tidyr::unite(
      col = "id_col",
      TeamIdentifier, submission_id, analysis_id, split_id,
      sep = "-"
    ) %>%
    naniar::replace_with_na_if(
      .predicate = ~ rlang::is_character(.x),
      condition = ~ .x %in% naniar::common_na_strings
    ) %>%
    naniar::replace_with_na_if(
      .predicate = ~ rlang::is_bare_numeric(.x),
      condition = ~ .x %in% naniar::common_na_numbers
    )
}
