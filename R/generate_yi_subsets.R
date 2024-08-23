#' Generate subsets of out-of-sample predictions data by `estimate_type` for multiple analysis datasets.
#'
#' @param yi_analysis A \code{dataframe} containing multiple datasets of out-of-sample predictions and corresponding diversity data.
#' @details The object \code{yi\_analysis} should contain the character column \code{dataset}, and list-columns \code{data} and \code{diversity\_data}. In essence, both `data` and `diversity_data` are nested by `dataset`. \code{data} should be a list-col of \code{grouped_df}s, which should also contain the list-col `back_transformed_data`
#'
#' @family Multi-dataset Wrapper Functions
#' @return A `datraframe` with the character columns `dataset`, `estimate_type` and list-cols `data` and `diversity_data`
#' @export
#' @author Elliot Gould
generate_yi_subsets <- function(yi_analysis) {
  # So: yi_analysis$data[[1]]$back_transformed data is that obj.
  cli::cli_h1("Generating out-of-sample prediction subsets.")

  # ---- define helper fun ----
  split_yi_subsets <- function(.data) {
    # applied to a single dataset
    # reorganises the data from nesting based on individual analysis submissions, to
    # nesting based on the type of estimate
    # removes unnecessary data: `augmented_data` and `checks`

    if (nrow(.data) == 0) {
      cli::cli_alert_danger("{.code nrow(.data)} is {.val {0}} when splitting yi subsets. This is likely because all values for column {.val back_transformed_data} are {.val {NA}}")
      return(NA)
    }

    .data %>%
      select(-augmented_data, -checks) %>%
      filter(!is.na(back_transformed_data)) %>%
      unnest(back_transformed_data) %>%
      dplyr::rename_with(
        .fn = ~ return("estimate_type"),
        .cols = contains(match = c("scenario", "SurveyID"))
      ) %>%
      mutate(estimate_type = case_when(
        estimate_type %in% list("Q1", 1) ~ "y25",
        estimate_type %in% list("Q2", 2) ~ "y50",
        estimate_type %in% list("Q3", 3) ~ "y75", # TODO: check instructions to analyst for correct ordering of scenarios
        TRUE ~ rlang::na_chr
      )) %>%
      ungroup() %>%
      nest_by(estimate_type, .keep = FALSE) %>%
      hoist(data, .remove = TRUE)
  }

  # ---- apply helper fun ----
  out <- yi_analysis %>%
    group_by(dataset) %>%
    mutate(
      data =
        map(data, ~ filter(.x, !is.na(back_transformed_data))) %>% # TODO is this step replicated in split_yi_subsets() ??
          map(split_yi_subsets)
    ) %>%
    select(-contains("estimate_type")) %>% #delete col if exists: split_yi_subsets creates duplicate estimate_type col
    unnest(data) %>%
    mutate(
      diversity_data = 
        map2(
          .x = diversity_data,
          .y = data,
          .f = ~ semi_join(.x, .y) %>% distinct()
        )
    )
  # ---- rename scenario cols in back transformed data----

  return(out)
}
