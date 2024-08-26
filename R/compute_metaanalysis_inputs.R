#' Compute all metaanalysis inputs for different types of estimates
#'
#' @param .data A dataset containing all necessary values for computing meta-analysis inputs
#' @param estimate_type character string, one of "Zr", "yi", "y25", "y50", "y75".
#'
#' @return A dataframe with processed data ready for meta-analysis
#' @export
compute_metaanalysis_inputs <- function(.data, estimate_type = character(1L)) {
  # TODO insert checks that appropriate columns exist
  match.arg(estimate_type, choices = c("Zr", "yi", "y25", "y50", "y75"), several.ok = FALSE)
  cli::cli_h1(glue::glue("Computing meta-analysis inputs", " for estimate type ", "{estimate_type}"))
  
  if (estimate_type == "Zr") {
    # Convert Effect Sizes to Zr -------
    cli::cli_h2(paste0("Computing standardised effect sizes ", "{.code Zr}", " and variance ", "{.code VZr}"))
    .data <- .data %>%
      # unnest(back_transformed_estimate) %>%
      mutate(Zr_VZr = purrr::pmap(
        .l = list(
          beta_estimate = beta_estimate,
          beta_SE = beta_SE,
          adjusted_df = adjusted_df
        ),
        .f = est_to_zr
      )) %>%
      unnest(cols = c(Zr_VZr))
  } else {
    cli::cli_h2(paste0("Transforming out of sample predictions from link to response scale"))
    .data <- .data %>%
      pointblank::col_exists(
        columns = c(
          "response_id",
          "submission_id",
          "analysis_id",
          "split_id",
          "augmented_data",
          "link_function",
          "response_transformation_description"
        )) %>% # TODO add check for  response transformation
      group_by(response_id, submission_id, analysis_id, split_id) %>%
      mutate(
        back_transformed_data =
          pmap(
            .l = list( #TODO bug, missing argument
              augmented_data,
              link_function,
              response_transformation_description
            ),
            .f = ~ if (!rlang::is_na(..1) | !rlang::is_na(..2)) {
              convert_predictions( #TODO bug, missing argument
                augmented_data = ..1,
                link_fun = ..2,
                response_transformation = ..3
              )
            } else {
              rlang::cpl
            }
          )
      )
  }
  # TODO replace .data, this is a protected 'pronoun' in tidyverse
  return(.data)
}
