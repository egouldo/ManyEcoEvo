devtools::load_all()

#+ message = FALSE, warning = FALSE
out <- targets::tar_read(ManyEcoEvo_yi) %>% 
  mutate(
    data = 
      map_if(data, 
             ~ filter(.x, 
                      stringr::str_detect(
                        response_variable_name, 
                        "average.proportion.of.plots.containing",
                        negate = TRUE)),
             .p = dataset == "eucalyptus")) %>%   
  mutate(
    diversity_data =
      map2(
        .x = diversity_data,
        .y = data,
        .f = ~ semi_join(.x, .y, join_by(id_col)) %>% 
          distinct()
      )
  ) %>% filter(dataset == "eucalyptus") %>% 
  pluck("data", 1)

back_transformed_se <- 
  back_transform_response_vars_yi(out) %>% 
  select("id_col", 
         ends_with("data"), 
         starts_with("transformation"), 
         -review_data) %>% 
  rowwise(id_col) %>% 
  mutate(yi_data = list(left_join(augmented_data, back_transformed_data, 
                                  by = join_by(SurveyID), 
                                  suffix = c(".aug", ".back")))) %>% 
  unnest(yi_data) %>% 
  ungroup %>% 
  select(contains("id"), starts_with("fit"), starts_with("se"), starts_with("sd"), transformation_type) %>% 
  rename("sd.fit.back" = "sd.fit") 

check_transformations_se <- 
  back_transformed_se %>% 
  rowwise(id_col, SurveyID) %>% 
  mutate(log_transformed_se = list(log_transform(estimate = fit.back, 
                                                 std.error = se.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SE"))),
         log_transformed_sd = list(log_transform(estimate = 
                                                   fit.back, 
                                                 std.error = sd.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SD")))
  ) %>% 
  unnest(cols = c(log_transformed_se, log_transformed_sd)) %>% 
  relocate(contains("mean_log"), .after = starts_with("fit")) %>% 
  relocate(contains("se_log"), .after = starts_with("se.")) %>% 
  relocate(contains("sd_log"), .after = starts_with("sd.")) %>% 
  select(-contains("ci"), -contains("lower"), -contains("upper")) 

back_transformed_se_sims <- 
  back_transformed_se %>% 
  rowwise(id_col, SurveyID) %>% 
  mutate(log_transformed_10 = list(log_transform(estimate = fit.back, 
                                                std.error = se.fit.back, 
                                                sim = 10) %>% 
                                    rename_with(~ paste0(.x, "10"))),
         log_transformed_100= list(log_transform(estimate = fit.back, 
                                                 std.error = se.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "100"))),
         log_transformed_1000 = list(log_transform(estimate = fit.back,
                                                  std.error = se.fit.back, 
                                                  sim = 1000) %>% 
                                      rename_with(~ paste0(.x, "1000")))) %>% 
  unnest(cols = starts_with("log_transformed")) %>% 
  relocate(contains("mean_log"), .after = starts_with("fit")) %>% 
  relocate(contains("se_log"), .after = starts_with("se.")) %>% 
  relocate(contains("sd_log"), .after = starts_with("sd.")) %>% 
  select(-contains("ci"), -contains("lower"), -contains("upper")) 

# check_transformations %>% 
#   write_csv("manuscript/euc_back_pipeline_checking.csv")

# convert se to sd before back-transforming
back_transformed_sd <- out %>% 
  drop_na(sample_size) %>% 
  rowwise(id_col) %>% 
  mutate(augmented_data = list(mutate(augmented_data, 
                                      se.fit = se.fit * sqrt(sample_size)))) %>% 
  back_transform_response_vars_yi() %>% 
  select("id_col", 
         ends_with("data"), 
         starts_with("transformation"), 
         -review_data) %>% 
  rowwise(id_col) %>% 
  mutate(yi_data = list(left_join(augmented_data, back_transformed_data, 
                                  by = join_by(SurveyID), 
                                  suffix = c(".aug", ".back")))) %>% 
  unnest(yi_data) %>% 
  ungroup %>% 
  select(contains("id"), 
         starts_with("fit"), 
         starts_with("se"), 
         starts_with("sd"), 
         transformation_type) %>% 
  rename("sd.fit.back" = "sd.fit", "sd.fit.aug" = "se.fit.aug") 

check_transformations_sd <- back_transformed_sd %>% 
  rowwise(id_col, SurveyID) %>% 
  mutate(log_transformed_se = list(log_transform(estimate = fit.back, 
                                                 std.error = se.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SE"))),
         log_transformed_sd = list(log_transform(estimate = 
                                                   fit.back, 
                                                 std.error = sd.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SD")))
  ) %>% 
  unnest(cols = c(log_transformed_se, log_transformed_sd)) %>% 
  relocate(contains("mean_log"), .after = starts_with("fit")) %>% 
  relocate(contains("se_log"), .after = starts_with("se.")) %>% 
  relocate(contains("sd_log"), .after = starts_with("sd.")) %>% 
  select(-contains("ci"), -contains("lower"), -contains("upper")) 

# Convert yi_se to yi_sd before back-transforming, then re-transform
# Using either se or sd from the back-transformed distribution
back_transformed_sd_sims <- 
  back_transformed_sd %>% 
  rowwise(id_col, SurveyID) %>% 
  mutate(log_transformed_100_se = list(log_transform(estimate = fit.back, 
                                                     std.error = se.fit.back, 
                                                     sim = 100) %>% 
                                         rename_with(~ paste0(.x, "100_sd"))),
         log_transformed_1000_se = list(log_transform(estimate = fit.back,
                                                      std.error = se.fit.back, 
                                                      sim = 1000) %>% 
                                          rename_with(~ paste0(.x, "1000_sd")))) %>% 
  mutate(log_transformed_100_sd = list(log_transform(estimate = fit.back, 
                                                     std.error = sd.fit.back, 
                                                     sim = 100) %>% 
                                         rename_with(~ paste0(.x, "100_se"))),
         log_transformed_1000_sd = list(log_transform(estimate = fit.back,
                                                      std.error = sd.fit.back, 
                                                      sim = 1000) %>% 
                                          rename_with(~ paste0(.x, "1000_se")))) %>% 
  unnest(cols = starts_with("log_transformed")) %>% 
  relocate(contains("mean_log"), .after = starts_with("fit")) %>% 
  relocate(contains("se_log"), .after = starts_with("se.")) %>% 
  relocate(contains("sd_log"), .after = starts_with("sd.")) %>% 
  select(-contains("ci"), -contains("lower"), -contains("upper")) 

#' # Euc checking

#+ 
# ---- report euc results ----
check_transformations_se %>% knitr::kable()

back_transformed_se_sims %>%  knitr::kable()

check_transformations_sd %>%  knitr::kable()

back_transformed_sd_sims %>% knitr::kable()

#' # BT checking

#+ message = FALSE, warning = FALSE
# ---- blue tit ---
bt <- targets::tar_read(ManyEcoEvo_yi) %>% filter(dataset == "blue tit") %>% 
  pluck("data", 1)

back_transformed_se_bt <- 
  back_transform_response_vars_yi(bt) %>% 
  select("id_col", 
         ends_with("data"), 
         starts_with("transformation"), 
         -review_data) %>% 
  rowwise(id_col) %>% 
  mutate(yi_data = list(left_join(augmented_data, back_transformed_data, 
                                  by = join_by(scenario), 
                                  suffix = c(".aug", ".back")))) %>% 
  unnest(yi_data) %>% 
  ungroup %>% 
  select(contains("id"), starts_with("estimate"), starts_with("se"), starts_with("sd"), transformation_type) %>% 
  rename("sd.fit.back" = "sd.fit") 

#+ message = FALSE, warning = FALSE
check_transformations_se <- 
  back_transformed_se %>% 
  rowwise(id_col, scenario) %>% 
  mutate(log_transformed_se = list(log_transform(estimate = fit.back, 
                                                 std.error = se.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SE"))),
         log_transformed_sd = list(log_transform(estimate = 
                                                   fit.back, 
                                                 std.error = sd.fit.back, 
                                                 sim = 100) %>% 
                                     rename_with(~ paste0(.x, "SD")))
  ) %>% 
  unnest(cols = c(log_transformed_se, log_transformed_sd)) %>% 
  relocate(contains("mean_log"), .after = starts_with("fit")) %>% 
  relocate(contains("se_log"), .after = starts_with("se.")) %>% 
  relocate(contains("sd_log"), .after = starts_with("sd.")) %>% 
  select(-contains("ci"), -contains("lower"), -contains("upper")) 

#+ 
check_transformations_se %>% knitr::kable()