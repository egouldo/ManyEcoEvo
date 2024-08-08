#' Prepare peer-review data from Qualtrics
#'
#' @param bt_reviews Blue tit review
#' @param euc_reviews
#'
#' @return a tibble of peer-review data for blue tit and eucalyptus analyses
#' @export
#' @family targets-pipeline functions
prepare_review_data <- function(bt_reviews, euc_reviews) {
  all_review_data <- bt_reviews %>%
    bind_rows(euc_reviews) %>%
    rename_with(.fn = ~ gsub("_S2", "", .x)) %>%
    group_by(
      response_id,
      analysis_id,
      submission_id
    ) %>%
    nest(review_data = c(
      ReviewerId,
      PriorBelief,
      RateAnalysis,
      PublishableAsIs
    )) %>%
    filter(!is.na(submission_id))

  return(all_review_data)
}
