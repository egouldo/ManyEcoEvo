#' Preprocess Prediction Files
#'
#' @param predictions_validation a dataframe containing the outputs of pointblank validation for multiple out of sample prediction files
#' @param all_surveys A tibble of analyst-data for all datasets
#'
#' @return pointblank check columns are nested into the list-column `checks`, `submission_id` is computed from `question`, `new_names` for replacement column names are given in `new_names`, `filepath` is appended with "data-raw/analyst_data/S2"
#' @export
preprocess_prediction_files <- function(predictions_validation,
                                        all_surveys) {
  # Load Pointblank and Manually Validated Prediction Data

  new_names <-
    tibble::tibble(
      dataset = "eucalyptus",
      check_id = c(1:5),
      new_name = c("SurveyID", "fit", "se.fit", "ci.low", "ci.hi")
    ) %>%
    dplyr::bind_rows(
      tibble::tibble(
        dataset = "blue tit",
        check_id = c(1:5),
        new_name = c("scenario", "estimate", "se.fit", "ci.low", "ci.hi")
      )
    )

  # Apply Pre-processing Functions
  out <-
    predictions_validation %>%
    dplyr::group_by(response_id, submission_id, dataset, file_name) %>%
    dplyr::distinct() %>%
    tidyr::nest(checks = c(check_id:done, dataset)) %>%
    dplyr::mutate(checks = map(checks, left_join, new_names, by = c("check_id", "dataset"))) %>%
    dplyr::inner_join(all_surveys, by = c("response_id", "submission_id", "analysis_id", "split_id")) %>%
    dplyr::mutate(
      filepath =
        here::here(
          "data-raw/analyst_data/S2", # TODO do not hard-code in
          question,
          file_name
        )
    ) %>%
    dplyr::filter(
      is.na(exclude_csv),
      exclusions_all == "retain",
      is.na(exclusion_predictions)
    ) # Exclude processing of csv's marked with 'exclude' - keep NA's

  return(out)
}
