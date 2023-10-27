#' Performs QA on re-submitted out-of-sample prediction files
#'
#' @param df A dataframe containing information about new prediction files
#'
#' @return A tibble with list-columns containing the outputs of point-blank checks
#' @export
preprocess_updated_prediction_files <- function(df = data.frame()) {
  cli::cli_h2(text = "Reading and Validating Updated Prediciton Files")
  
  new_names <- 
    tibble(dataset = "eucalyptus", 
           check_id = c(1:5),
           new_name = c("SurveyID", "fit", "se.fit", "ci.low", "ci.hi")) %>% 
    bind_rows(
      tibble(
        dataset = "blue tit",
        check_id = c(1:5),
        new_name = c("scenario", "estimate", "se.fit", "ci.low", "ci.hi")
      )
    )
  
  df %>% 
    drop_na(`new_csv_file_name--file-submissionID-csv_number`) %>% 
    mutate(updated_submission_data = map(.x = `new_csv_file_name--file-submissionID-csv_number`,
                                         .f = ~ read_submission_data(filepath = here::here("data-raw/analyst_data/S2", .x)))) %>% #TODO replace with exported data in pkg / DO NOT hard code in
    group_by(response_id, submission_id, analysis_id, split_id, csv_number) %>% 
    mutate(updated_checks = map2(.x = updated_submission_data, 
                                 .y = dataset,
                                 .f = ~ validate_predictions(data_set = .y,
                                                             input = .x,
                                                             type = "df")),
           updated_checks = map(.x = updated_checks,
                                .f = pointblank::interrogate),
           updated_checks = map(.x = updated_checks,
                                .f = pointblank::get_agent_report, display_table = FALSE),
           updated_checks = map(.x = updated_checks,
                                .f = ~ mutate(.x, column_rename = NA, 
                                              column_exists = NA, 
                                              other_action = NA,
                                              contact_analyst = NA))) %>% 
    drop_na(split_id) %>% #TODO DEV ONLY
    dplyr::mutate(failed_checks = map_int(.x = updated_checks, #TODO DEV ONLY? or alert if dropped??
                                          .f =  ~dplyr::filter(.x, f_pass < 1) %>% 
                                            nrow(.)) %>% 
                    as.logical(.)) %>% 
    dplyr::filter(failed_checks == FALSE) %>% 
    mutate(updated_checks = map2(.x = dataset, 
                                 .y = updated_checks, 
                                 .f = ~ mutate(.y, dataset = paste(.x)) %>% 
                                   rename(check_id = i)),
           updated_checks = map(updated_checks, left_join, new_names, by = c("check_id", "dataset")))
  #TODO DO the updaetd checks have "dataset" col?
}
