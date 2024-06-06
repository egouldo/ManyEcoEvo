# targets take 2

library(targets)
library(tarchetypes)
options(tidyverse.quiet = TRUE)
library(tidyverse)

pkgs <- c("tidyverse", 
          "naniar", 
          "tidyr", 
          "tidylog", 
          "pointblank", 
          "readxl", 
          "betapart", 
          "visNetwork",
          "recipes",
          "ggforestplot",
          "parsnip",
          "workflows",
          "ggplot2",
          "performance",
          "janitor",
          "rsvg",
          "multilevelmod",
          "metafor",
          "ManyEcoEvo") #TODO rm from here and just call in tar_option_set(), but will need to rm all namespacing

tar_option_set(
  packages = pkgs,
  imports = "ManyEcoEvo",
  # debug = c("augmented_data_3efd9941")#, #augmented_data_a4d78efa
  # cue = tar_cue(mode = "always") #because we have silent errors!
)

list(tarchetypes::tar_file_read(name = euc_reviews, 
                                command = "data-raw/anonymised_data/euc_reviews.csv", 
                                read = readr::read_csv(file = !!.x)),
     tarchetypes::tar_file_read(name = bt_reviews,
                                command = "data-raw/anonymised_data/bt_reviews.csv",
                                read = readr::read_csv(file = !!.x)),
     tarchetypes::tar_file_read(name = master_data,
                                # cue = targets::tar_cue(mode = "always"),
                                command = here::here("data-raw/anonymised_data/master_data.csv"),
                                read = readr::read_csv(file = !!.x)),
     tarchetypes::tar_file_read(name = master_metadata,
                                command = here::here("data-raw/anonymised_data/master_metadata.csv"),
                                read = readr::read_csv(file = !!.x)),
     tarchetypes::tar_file_read(name = predictions_validation_coded,
                                command = "data-raw/anonymised_data/predictions_validations_worksheet.csv",
                                read = readr::read_csv(!!.x) %>% 
                                  # dplyr::select(-transformation,
                                  #               -response_transformation_status,
                                  #               -response_variable_type,
                                  #               -error_distribution,
                                  #               -response_variable_name,
                                  #               -response_construction_description,
                                  #               -submission_id) %>% 
                                  # dplyr::rename_with(.cols = ends_with("_new"),
                                  #                    .fn =  stringr::str_remove, 
                                  #                    "_new") %>% 
                                  dplyr::mutate(other_action = 
                                                  case_when(other_action == "columns_not_recognized_as_such_because_seperated_by_\";\"" 
                                                            ~ rlang::na_chr, 
                                                            TRUE ~ other_action))) , 
     tarchetypes::tar_file_read(name = list_of_new_prediction_files,
                                command = "data-raw/analyst_data/S2/list_of_new_csv_files.csv",
                                read = readr::read_csv(!!.x)),
     tarchetypes::tar_file_read(name = expert_subset,
                                command = "data-raw/metadata_and_key_data/Good_Statistician_ResponseIds.csv",
                                read = readr::read_csv(file = !!.x)),
     targets::tar_target(name = all_review_data,
                         command = prepare_review_data(bt_reviews,euc_reviews)),
     targets::tar_target(ManyEcoEvo,
                         command = prepare_ManyEcoEvo(master_data, 
                                                      master_metadata, 
                                                      all_review_data)),
     targets::tar_target(name = ManyEcoEvo_results,
                         command = ManyEcoEvo %>% 
                           prepare_response_variables(estimate_type = "Zr") |>  
                           generate_exclusion_subsets(estimate_type = "Zr") |> 
                           generate_rating_subsets() |> 
                           generate_expertise_subsets(expert_subset) |>
                           generate_collinearity_subset(ManyEcoEvo:::collinearity_subset) |>
                           compute_MA_inputs(estimate_type = "Zr") |> 
                           generate_outlier_subsets() |> # TODO run before MA_inputs? diversity indices need to be recalculated!!
                           filter(expertise_subset != "expert" | exclusion_set != "complete-rm_outliers") |> #TODO mv into generate_outlier_subsets() so aren't created in the first place
                           meta_analyse_datasets()),
     targets::tar_target(updated_prediction_files,
                         preprocess_updated_prediction_files(list_of_new_prediction_files)),
     targets::tar_target(prediction_submissions,
                         preprocess_prediction_files(
                           predictions_validation_coded, 
                           ManyEcoEvo %>% 
                             select(data) %>% 
                             unnest(cols = data)) %>% 
                           left_join(x = ., 
                                     y = updated_prediction_files %>%
                                       select(-transformation,
                                              -response_variable_or_model), 
                                     by = c("response_id", 
                                            "submission_id",
                                            "question" = "survey_question", 
                                            "analysis_id", 
                                            "split_id", 
                                            "dataset")) %>% 
                           ungroup() %>% 
                           mutate(
                             no_updated_checks = map_lgl(updated_checks, is_null),
                             checks = ifelse(no_updated_checks == TRUE, checks, updated_checks),
                             .keep = "unused") %>% 
                           rowwise() %>% #NOTE: file_name and file_path replacement errors fails without rowwise
                           mutate(
                             file_name = case_when(rlang::is_na(`new_csv_file_name--file-submissionID-csv_number`) ~ file_name,
                                                   TRUE ~ `new_csv_file_name--file-submissionID-csv_number`),
                             filepath = case_when(rlang::is_na(`new_csv_file_name--file-submissionID-csv_number`) ~ filepath, 
                                                  TRUE ~ here::here("data-raw/analyst_data/S2", file_name)),
                             exclude_read = case_when(response_id == "R_3dYDpQUfDUXjtDy" & submission_id == 2 ~ "exclude", # see col other_action & misc_notes in predictions_validation_worksheet.csv for reasons
                                                      response_id == "R_3mfyhAj6rakbi5b" & submission_id == 1 ~ "exclude", #TODO: check whether THP's file has addressed these issues, if so we can remove this code as it is redundant
                                                      response_id == "R_1BWpZlSbkmSofe1" & submission_id == 1 ~ "exclude",
                                                      response_id == "R_1d0uRf5iNWOLD8M" & submission_id == 1 ~ "exclude",
                                                      response_id == "R_3NHVKFiOiQBfX9b" & submission_id == 1 ~ "exclude",
                                                      response_id == "R_3NHVKFiOiQBfX9b" & submission_id == 3 ~ "exclude",
                                                      response_id == "R_1LRqq2WHrQaENtM" & submission_id == 1 ~ "exclude",
                                                      response_id == "R_2V7qaLEfdbgUGg3" & submission_id == 1 ~ "exclude", # missing se.fit, but should be filtered out automatically if don't pass in preprocess_updated_prediction_files - might not have warning level set properly to detect f_pass < 1
                                                      TRUE ~ "include")) %>% #TODO seems to be duplicating THP's effort - remove if not already coded as exclude_csv or exclude in ManyEcoEvo
                           ungroup() %>% 
                           dplyr::filter(exclude_read == "include") %>% 
                           select(-`new_csv_file_name--file-submissionID-csv_number`, 
                                  -updated_submission_data) %>% 
                           filter(fs::path_ext(filepath) %nin% "zip")), #TODO check that stuff we haven't already manually unzipped isn't being accidentally removed
     tar_target(grouped_prediction_validation_data,
                command = prediction_submissions %>% #TODO move this code (except group_by() %>%  tar_group()) into preprocess_prediction_files()
                  select(ends_with("_id"),
                         starts_with("response_"),
                         id_col,
                         TeamIdentifier,
                         dataset,
                         question,
                         file_name,
                         filepath, 
                         checks,
                         exclusions_all, 
                         mixed_model,
                         transformation,
                         response_transformation_status,
                         link_function_reported,
                         adjusted_df,
                         review_data
                  ) %>% 
                  drop_na(split_id) %>% #TODO, remove this `drop_na()` once we have fixed missing NA `gh issue view 109 -w`; `gh issue view 102 -w`
                  anti_join(., #TODO remove analyses where there are multiple submissions per split_id (~20) `gh issue view 109 -w`; `gh issue view 102 -w`
                            {count(., response_id, submission_id, analysis_id, split_id) %>% 
                                filter(n>1)}) %>% 
                  group_by(response_id, submission_id, analysis_id, split_id) %>% 
                  tar_group(),
                iteration = "group"),
     tar_target(groups,
                grouped_prediction_validation_data,
                pattern = map(grouped_prediction_validation_data)),
     tar_target(submission_data, 
                command = read_submission_data(groups$filepath), 
                iteration = "list",
                pattern = map(groups),
                error = "continue" #TODO, run without continue and check for problems
     ),
     tar_target(augmented_data,
                command = if(!rlang::is_na(submission_data)){ 
                  augment_prediction_data(.data = submission_data, 
                                          checks = groups$checks, 
                                          dataset = groups$dataset)
                }else{
                  NA
                },
                iteration = "list",
                error = "stop",
                pattern = map(submission_data, groups)),
     tar_target(validated_augmented_data,
                command = if(!rlang::is_na(augmented_data)){
                  validate_predictions(data_set = groups$dataset, 
                                       input = augmented_data %>% 
                                         ungroup(), 
                                       type = "df")
                }else{
                  NA
                },
                iteration = "list",
                error = "continue",
                pattern = map(augmented_data, groups)
     ),
     tar_target(prediction_checks,
                command = if(!rlang::is_na(validated_augmented_data)){
                  pointblank::interrogate(validated_augmented_data) %>% 
                    pointblank::get_agent_report(., display_table = FALSE)
                }else{
                  NA
                },
                iteration = "list",
                error = "continue",
                pattern = map(validated_augmented_data)
     ),
     tar_target(all_prediction_data,
                command = groups %>% #must be groups, not grouped_prediction_validation_data bc the row order changes
                  mutate(augmented_data = augmented_data,
                         checks = prediction_checks,
                         validation_fail = modify_if(.x = checks, 
                                                     .p = negate(rlang::is_na),
                                                     .f = ~ filter(.x, f_pass < 1, 
                                                                   stringr::str_detect(columns, "estimate|fit")) %>% 
                                                       nrow(.)) %>% 
                           flatten_dbl(.) %>% 
                           as.logical(.))),
     targets::tar_target(name = ManyEcoEvo_viz,
                         command =  make_viz(ManyEcoEvo_results)),
     targets::tar_target(name = ManyEcoEvo_yi,
                         command = prepare_ManyEcoEvo_yi(master_data, 
                                                         master_metadata, 
                                                         all_prediction_data)),
     targets::tar_target(name = ManyEcoEvo_yi_results,
                         command =  ManyEcoEvo_yi %>% 
                           dplyr::mutate(data = purrr::map(data, 
                                                           ~ dplyr::filter(.x, stringr::str_detect(response_variable_type, 
                                                                                                   "constructed", 
                                                                                                   negate = TRUE)))) %>% 
                           prepare_response_variables(estimate_type = "yi",
                                                      param_table = ManyEcoEvo:::analysis_data_param_tables) %>%
                           generate_yi_subsets() %>% #TODO: must be run after prepare_response_variables??
                           apply_VZ_exclusions(3) %>%
                           generate_exclusion_subsets() %>% #TODO: runs on ManyEcoEvo that contains Zr and yi results.
                           compute_MA_inputs() %>%  #TODO lone join by "estimate_type" amongst join_by ("id_col") is suspicious!
                           
                           generate_outlier_subsets() %>% #TODO swapped order with previous line, but untested
                           meta_analyse_datasets() #TODO requires col exclusion_set from generate_exclusino_subsets() but don't need that fun in this pipeline anymore
     ),
     targets::tar_target( name = ManyEcoEvo_yi_viz,
                          command = make_viz(ManyEcoEvo_yi_results)),
     tarchetypes::tar_quarto(name = README,
                             path = "README.qmd")
     # tarchetypes::tar_quarto(full_analysis, "analysis/analysis_revised_data.qmd")
)
