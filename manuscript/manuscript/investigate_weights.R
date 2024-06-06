#' ---
#' title: "Investigating model weighting issues"
#' author: "Elliot Gould"
#' date: "May 27, 2024"
#' ---

library(ManyEcoEvo)
library(tidyverse)
library(lme4)


#' # Exploring Model Weights

#' Create orchard-style plots for both BT and Euc, without removing analyses with extreme weights:
ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_orchard_plot")) %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name),
       .f = ~ plot_model_means_orchard(..1, 
                                       PublishableAsIs, 
                                       ..2,
                                       new_order = 
                                         c("Unpublishable",
                                           "Major Revision",
                                           "Minor Revision",
                                           "Publishable As Is"),
                                       ..3))
#' From the above it's clear that the general pattern is for the higher weighted cases to pull the model means closer to them.
#' In the case of the eucalyptus data, it's several EXTREME weights that are pulling the model means closer to them.
#' Let's look at the distributions of weights and their values for the euc dataset


#' Weight Distribution Blue Tit Model:
ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "blue tit") %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda, data) %>% pull(box_cox_rating_cat) %>% 
  pluck(1) %>% 
  model.frame() %>% 
  as_tibble() %>% 
  rename(weights = `(weights)`) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  ggplot(aes(x = weights)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Box-Cox Weights",
       x = "Box-Cox Weights",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

#' Weight Distribution Eucalyptus Model
ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda, data) %>% 
  pull(box_cox_rating_cat) %>% 
  pluck(1) %>% 
  model.frame() %>% 
  as_tibble() %>% 
  rename(weights = `(weights)`) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  ggplot(aes(x = weights)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Box-Cox Weights",
       x = "Box-Cox Weights",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

#' Weight Distribution for Eucalyptus model when case with maximum weight removed
ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda, data) %>% 
  pull(box_cox_rating_cat) %>% 
  pluck(1) %>% 
  model.frame() %>% 
  as_tibble() %>% 
  rename(weights = `(weights)`) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  filter(weights != max(weights)) %>% 
  ggplot(aes(x = weights)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Box-Cox Weights",
       x = "Box-Cox Weights",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

#' Let's see all values of weight
ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda, data) %>% 
  pull(box_cox_rating_cat) %>% 
  pluck(1) %>% 
  model.frame() %>% 
  as_tibble() %>% 
  rename(weights = `(weights)`) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  pluck("weights", unique, sort) # all weights

#' What are the cases with extreme weights?
#' Below we identify the cases with the top 2 maximum weights
extreme_weight_observation <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  pluck("effects_analysis", 1) %>% 
  select(study_id, box_cox_abs_deviation_score_estimate, box_cox_var) %>% 
  mutate(weights = 1/box_cox_var) %>%
  # filter(weights == max(weights))
  filter(weights > 10000000000)
extreme_weight_observation

#' # Investigating the impact of extreme weights by removing them

#' Remove cases with extreme weights and refit / plot models
refitted_eucalyptus_model <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  mutate(effects_analysis = map(effects_analysis, ~filter(.x, study_id %nin% extreme_weight_observation$study_id))) %>%
  mutate(box_cox_rating_cat = map(effects_analysis, ~fit_boxcox_ratings_cat(
    .data = .x,
    outcome = box_cox_abs_deviation_score_estimate,
    outcome_var = box_cox_var,
    interceptless = FALSE
  )))

#' Note, not shown here yet. But when only the top most case is removed, lme4 gives singular fit warning, when I run check_singularity() on the model, it seems OK.
#' When the top two cases are removed, singular fit warning is given by lme4, and check_singularity() returns FALSE, meaning the model is singular.
#' Also, when I check convergence on the model with top 2 cases removed, gradient shifts from ~3.79e^-5 with only case with max weight removed to 0. 
#' Seems strange to me that this would occur when both extreme weights are removed... 
#' Anyways, I progress with refitting and plotting after removing cases with weights 10000000000 and above (here, there are two).

#' Recreate the plot data for regenerating plots after refitting models
refitted_plot_data<-
  refitted_eucalyptus_model %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_violin_plot"))

#' Create violin plots for refitted models
refitted_plot_data %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))

#' Regenerate plot but back-transform to original scale (absolute deviation from meta-analytic mean)
refitted_plot_data %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
        .f = ~ plot_model_means_box_cox_cat(..1, 
                                            PublishableAsIs, 
                                            ..2,
                                            new_order = 
                                              c("Unpublishable",
                                                "Major Revision",
                                                "Minor Revision",
                                                "Publishable As Is"),
                                            ..3, ..4, back_transform = TRUE))

#' Create orchard-style plots for refitted models
refitted_plot_data %>% 
  mutate(plot_name = str_replace(plot_name, "_violin_plot", "_orchard_plot")) %>%
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name),
       .f = ~ plot_model_means_orchard(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3))

#' What is causing these extreme values observed in `extreme_weight_observation`?
#' Is the weight calculation ok?

ManyEcoEvo_results %>%
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         dataset == "eucalyptus") %>% 
  select(effects_analysis) %>% 
  unnest(effects_analysis) %>% 
  filter(study_id %in% extreme_weight_observation$study_id) %>%
  select(study_id, 
         Zr,
         VZr,
         abs_deviation_score_estimate,
         box_cox_abs_deviation_score_estimate, 
         folded_mu_val,
         folded_v_val,
         box_cox_var,
         lambda) %>% 
  mutate(weights = 1/box_cox_var)

ManyEcoEvo_results %>%
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  select(effects_analysis) %>% 
  unnest(effects_analysis) %>% 
  unnest(review_data) %>% 
  mutate(weights = 1/box_cox_var) %>% 
  write_csv("extreme_weight_analysis_info.csv")

#' The `weights` are calculated as the inverse of the variance of the box-cox transformed absolute deviation scores.
#' The variance of the box-cox transformed absolute deviation scores `box_cox_var` is calculated as the variance of the folded absolute deviation scores and the folded variance of the absolute deviation scores
#' The folded absolute deviation scores `folded_mu` and the The folded variance of the absolute deviation scores `folded_v` are calculated as follows:
box_cox_transform

# -----

#' # Try using folded_v instead of box_cox_var to calculate weights

refitted_model_folded_v <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  mutate(box_cox_rating_cat = map(effects_analysis, ~fit_boxcox_ratings_cat(
    .data = .x,
    outcome = box_cox_abs_deviation_score_estimate,
    outcome_var = folded_v_val,
    interceptless = FALSE
  )))

refitted_plot_data_folded_v <-
  refitted_model_folded_v %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_violin_plot"))

refitted_plot_data_folded_v %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))


# ------ try update function ------


#' # Update `fit_boxcox_ratings_cat` to use `folded_v` instead of `box_cox_var` to calculate weights


fit_boxcox_ratings_cat_folded_v <- 
  function(.data, outcome, outcome_var, interceptless = FALSE) {
    cli::cli_h2(glue::glue("Fitting lmer with categorical ratings predictor on box_cox_transformed outcomes"))
    # Example Usage:
    # library(tidyverse);library(targets);library(metafor)
    # tar_load(meta_analysis_outputs)
    # meta_analysis_outputs$data[[1]] %>% 
    #   fit_boxcox_ratings_cat(., 
    # outcome = box_cox_abs_deviation_score_estimate,
    #                                   outcome_var = VZr, interceptless = FALSE)
    
    # TODO @egouldo stopifnot data doesn't contain variables named eval(box_cox_outcome_var), eval(sampling_variance_var), review_data
    # TODO @egouldo unnest and then check stopifnot: RateAnalysis, ReviewerId, study_id.
    data_tbl <-
      .data %>% 
      unnest(cols = c(review_data)) %>% 
      select(study_id, 
             ReviewerId, 
             PublishableAsIs, 
             starts_with("box_cox_"), #@TODO - delete this row?
             {{outcome}},
             {{outcome_var}}) %>% 
      ungroup() %>% 
      mutate(PublishableAsIs = forcats::fct_relevel(PublishableAsIs,c("deeply flawed and unpublishable", 
                                                                      "publishable with major revision", 
                                                                      "publishable with minor revision", 
                                                                      "publishable as is")),
             obs_id = 1:n()) 
    
    if (interceptless == FALSE) {
      f <- rlang::new_formula(rlang::ensym(outcome), 
                              expr(PublishableAsIs + 
                                     (1 | ReviewerId)  #+ (1 | study_id ) #RE omitted due to convergence issues
                              ))
      mod <- lme4::lmer(f,
                        data = data_tbl,
                        weights = I(1/pull(data_tbl,{{outcome_var}}))
      )
    } else (#interceptless: for plotting
      mod <- lme4::lmer(rlang::new_formula(rlang::ensym(outcome), 
                                           expr(-1 + PublishableAsIs + (1 | ReviewerId))), #+ (1 | study_id) #problem with the groups
                        data = data_tbl #,
                        # weights = I(1/pull(data_tbl,{{outcome_var}}))
      )
    )
    
    return(mod)
    
  }

# apply

refitted_models_folded_v <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  mutate(box_cox_rating_cat = map(effects_analysis, ~fit_boxcox_ratings_cat_folded_v(
    .data = .x,
    outcome = box_cox_abs_deviation_score_estimate,
    outcome_var = folded_v_val,
    interceptless = FALSE
  )))

refitted_plot_data_folded_v <-
  refitted_models_folded_v %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_violin_plot"))

refitted_plot_data_folded_v %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))


#' now without taking the inverse of the folded_v

fit_boxcox_ratings_cat_folded_v_no_inv <- 
  function(.data, outcome, outcome_var, interceptless = FALSE) {
    cli::cli_h2(glue::glue("Fitting lmer with categorical ratings predictor on box_cox_transformed outcomes"))
    # Example Usage:
    # library(tidyverse);library(targets);library(metafor)
    # tar_load(meta_analysis_outputs)
    # meta_analysis_outputs$data[[1]] %>% 
    #   fit_boxcox_ratings_cat(., 
    # outcome = box_cox_abs_deviation_score_estimate,
    #                                   outcome_var = VZr, interceptless = FALSE)
    
    # TODO @egouldo stopifnot data doesn't contain variables named eval(box_cox_outcome_var), eval(sampling_variance_var), review_data
    # TODO @egouldo unnest and then check stopifnot: RateAnalysis, ReviewerId, study_id.
    data_tbl <-
      .data %>% 
      unnest(cols = c(review_data)) %>% 
      select(study_id, 
             ReviewerId, 
             PublishableAsIs, 
             starts_with("box_cox_"), #@TODO - delete this row?
             {{outcome}},
             {{outcome_var}}) %>% 
      ungroup() %>% 
      mutate(PublishableAsIs = forcats::fct_relevel(PublishableAsIs,c("deeply flawed and unpublishable", 
                                                                      "publishable with major revision", 
                                                                      "publishable with minor revision", 
                                                                      "publishable as is")),
             obs_id = 1:n()) 
    
    if (interceptless == FALSE) {
      f <- rlang::new_formula(rlang::ensym(outcome), 
                              expr(PublishableAsIs + 
                                     (1 | ReviewerId)  #+ (1 | study_id ) #RE omitted due to convergence issues
                              ))
      mod <- lme4::lmer(f,
                        data = data_tbl,
                        weights = I(pull(data_tbl,{{outcome_var}}))
      )
    } else (#interceptless: for plotting
      mod <- lme4::lmer(rlang::new_formula(rlang::ensym(outcome), 
                                           expr(-1 + PublishableAsIs + (1 | ReviewerId))), #+ (1 | study_id) #problem with the groups
                        data = data_tbl #,
                        # weights = I(1/pull(data_tbl,{{outcome_var}}))
      )
    )
    
    return(mod)
    
  }

# apply

refitted_models_folded_v_no_inv <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  mutate(box_cox_rating_cat = map(effects_analysis, ~fit_boxcox_ratings_cat_folded_v_no_inv(
    .data = .x,
    outcome = box_cox_abs_deviation_score_estimate,
    outcome_var = folded_v_val,
    interceptless = FALSE
  )))

refitted_plot_data_folded_v_no_inv <-
  refitted_models_folded_v_no_inv %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_violin_plot"))

refitted_plot_data_folded_v_no_inv %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))

# no weights

#' # Update `fit_boxcox_ratings_cat` to use no weights instead of `box_cox_var` or `folded_v` to calculate weights

fit_boxcox_ratings_cat_no_weights <- 
  function(.data, outcome, outcome_var, interceptless = FALSE) {
    cli::cli_h2(glue::glue("Fitting lmer with categorical ratings predictor on box_cox_transformed outcomes"))
    # Example Usage:
    # library(tidyverse);library(targets);library(metafor)
    # tar_load(meta_analysis_outputs)
    # meta_analysis_outputs$data[[1]] %>% 
    #   fit_boxcox_ratings_cat(., 
    # outcome = box_cox_abs_deviation_score_estimate,
    #                                   outcome_var = VZr, interceptless = FALSE)
    
    # TODO @egouldo stopifnot data doesn't contain variables named eval(box_cox_outcome_var), eval(sampling_variance_var), review_data
    # TODO @egouldo unnest and then check stopifnot: RateAnalysis, ReviewerId, study_id.
    data_tbl <-
      .data %>% 
      unnest(cols = c(review_data)) %>% 
      select(study_id, 
             ReviewerId, 
             PublishableAsIs, 
             starts_with("box_cox_"), #@TODO - delete this row?
             {{outcome}},
             {{outcome_var}}) %>% 
      ungroup() %>% 
      mutate(PublishableAsIs = forcats::fct_relevel(PublishableAsIs,c("deeply flawed and unpublishable", 
                                                                      "publishable with major revision", 
                                                                      "publishable with minor revision", 
                                                                      "publishable as is")),
             obs_id = 1:n()) 
    
    if (interceptless == FALSE) {
      f <- rlang::new_formula(rlang::ensym(outcome), 
                              expr(PublishableAsIs + 
                                     (1 | ReviewerId)  #+ (1 | study_id ) #RE omitted due to convergence issues
                              ))
      mod <- lme4::lmer(f,
                        data = data_tbl
      )
    } else (#interceptless: for plotting
      mod <- lme4::lmer(rlang::new_formula(rlang::ensym(outcome), 
                                           expr(-1 + PublishableAsIs + (1 | ReviewerId))), #+ (1 | study_id) #problem with the groups
                        data = data_tbl #,
                        # weights = I(1/pull(data_tbl,{{outcome_var}}))
      )
    )
    
    return(mod)
    
  }

# apply

refitted_models_no_weights <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All") %>% 
  mutate(box_cox_rating_cat = map(effects_analysis, ~fit_boxcox_ratings_cat_no_weights(
    .data = .x,
    outcome = box_cox_abs_deviation_score_estimate,
    outcome_var = folded_v_val,
    interceptless = FALSE
  )))

refitted_plot_data_no_weights <-
  refitted_models_no_weights %>% 
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  select(exclusion_set, 
         dataset, 
         estimate_type, 
         box_cox_rating_cat, 
         lambda) %>% 
  mutate(predictor_means = 
           map(box_cox_rating_cat, modelbased::estimate_means),
         model_data = map(box_cox_rating_cat, ~pluck(.x, "frame") %>% 
                            drop_na() %>% 
                            as_tibble()),
         plot_name = paste(exclusion_set, dataset, sep = ", ")) %>%
  mutate(model_data = map(model_data, 
                          .f = ~ mutate(.x, PublishableAsIs =
                                          str_replace(PublishableAsIs,
                                                      "publishable with ", "") %>%
                                          str_replace("deeply flawed and ", "") %>% 
                                          capwords())),
         predictor_means = map(predictor_means,
                               .f = ~ mutate(.x, PublishableAsIs =
                                               str_replace(PublishableAsIs,
                                                           "publishable with ", "") %>%
                                               str_replace("deeply flawed and ", "") %>% 
                                               capwords()))) %>% 
  mutate(plot_name = 
           str_remove(plot_name, "complete, ") %>% 
           str_replace_all(., " ", "_") %>% 
           paste0("_violin_plot"))

refitted_plot_data_no_weights %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))

#' comparison of different weight usage

# TODO: not sure if refitted_plot_data_folded_v and refitted_plot_data_no_weights are actually different, check code implementation!!

refitted_plot_data_no_weights %>% slice(1) %>% 
  bind_rows(refitted_plot_data_folded_v %>% 
              slice(1)) %>% 
  mutate(weights_type = c("no_weights", "folded_v")) %>% 
  pmap(.l = list(.$model_data, .$predictor_means, .$plot_name, .$lambda),
       .f = ~ plot_model_means_box_cox_cat(..1, 
                                           PublishableAsIs, 
                                           ..2,
                                           new_order = 
                                             c("Unpublishable",
                                               "Major Revision",
                                               "Minor Revision",
                                               "Publishable As Is"),
                                           ..3, ..4, back_transform = FALSE))

#' model fit stats??
refitted_plot_data_no_weights %>% slice(1) %>% 
  bind_rows(refitted_plot_data_folded_v %>% 
              slice(1)) %>% 
  mutate(weights_type = c("no_weights", "folded_v")) %>% pull(box_cox_rating_cat) %>% map(summary)



#' # Session Info

devtools::session_info()
