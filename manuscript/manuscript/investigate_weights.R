library(ManyEcoEvo)
library(tidyverse)
library(lme4)
library(rlang)
library(performance)
library(see)
library(modelbased)

#' # Exploring Model Weights

#' Create orchard-style plots for both BT and Euc, without removing analyses with extreme weights:
inv_bc_var_weights <- ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(exclusion_set == "complete", 
         publishable_subset == "All",
         expertise_subset == "All",
         collinearity_subset == "All"
  ) %>% 
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
                                               capwords()))) 

inv_bc_var_weights %>% 
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

#' ## Investigating the impact of extreme weights

#' Remove Eucalyptus cases with extreme weights and refit / plot models
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

#' Create orchard-style plots for refitted models:
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
#' Weights seem correlated with the box-cox transformed deviation score too

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
  mutate(weights = 1/box_cox_var) %>% knitr::kable()

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


#' # Systematically Comparing Different Weights & Random Effects

#' So it seems we need to reconsider different weight options, but this might mean we need to reconsider returning back to the planned random effects structure, too

filter_args = rlang::exprs(exclusion_set == "complete", 
                           publishable_subset == "All",
                           expertise_subset == "All",
                           collinearity_subset == "All")

prepare_ratings_data <- function(effects_analysis){
  data_tbl <-
    effects_analysis %>% 
    unnest(cols = c(review_data)) %>% 
    select(study_id, 
           TeamIdentifier,
           starts_with("box_cox_"),
           ReviewerId, 
           PublishableAsIs,
           # lambda,
           folded_v_val) %>% 
    ungroup() %>% 
    mutate(PublishableAsIs = forcats::fct_relevel(PublishableAsIs,c("deeply flawed and unpublishable", 
                                                                    "publishable with major revision", 
                                                                    "publishable with minor revision", 
                                                                    "publishable as is")),
           obs_id = 1:n()) 
  return(data_tbl)
}

# create formulas

base_formula <- rlang::new_formula(expr(box_cox_abs_deviation_score_estimate), 
                                   expr(PublishableAsIs))

calc_inv_bc_var <- as_function(~ 1/pull(.x, box_cox_var))
calc_inv_folded_v <- as_function(~ 1/pull(.x, folded_v_val))
no_weights <- NA

weight_formulas <- list(no_weights,
                        calc_inv_bc_var,
                        calc_inv_folded_v
) %>% 
  set_names("no_weights",
            "inv_bc_var",
            "inv_folded_v")

RE_rev <- expr((1 | ReviewerId))
RE_study <- expr((1 | study_id))
RE_both <- expr(!!RE_rev + !!RE_study)

random_expressions <- list(
  RE_rev,
  RE_study,
  RE_both
) %>% set_names("RE_rev",
                "RE_study",
                "RE_both")

lmer_wrap <- function(data_tbl, random_effect, weight_form, ..., env = caller_env()){
  f <- rlang::new_formula(expr(box_cox_abs_deviation_score_estimate), 
                          expr(PublishableAsIs + !!random_effect), env = env)
  
  weights <- if(is_na(weight_form)) NULL else weight_form(data_tbl)
  
  
  inject(lme4::lmer(!!f,
                    data = data_tbl,
                    weights = weights, ...))
}

all_models <-
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(!!!filter_args) %>% 
  select(dataset, effects_analysis) %>%
  hoist(.col = effects_analysis,
        "lambda",
        .simplify = TRUE,
        .transform = unique) %>% 
  mutate(model_data = map(effects_analysis, prepare_ratings_data),.keep = "unused") %>% 
  expand_grid(expand_grid(weight_formulas, random_expressions) %>% 
                mutate(weight_forms = names(weight_formulas),
                       random_effect = names(random_expressions)) %>% 
                unite("model_spec", weight_forms, random_effect, sep = "_") ) %>% 
  mutate(model = pmap(list(model_data, random_expressions, weight_formulas), lmer_wrap),.keep = "unused") %>% 
  mutate(singularity = map_lgl(model, performance::check_singularity),
         convergence = map_lgl(model, performance::check_convergence))


estimate_means <- 
  all_models %>% 
  filter(singularity == F, convergence == T) %>%
  reframe(model = set_names(model, model_spec), 
          results = map(model, possibly(modelbased::estimate_means, otherwise = NULL), at = "PublishableAsIs"),
          results = set_names(results, dataset), dataset = dataset, model_spec = model_spec) %>% 
  rowwise() %>% 
  drop_na(results) # model means couldn't be estimated due to convergence issues, drop those models

# evaluate and compare performance for remaining models
model_comparison_results <-
  all_models %>% semi_join(estimate_means, by = join_by(dataset, model_spec)) %>% 
  group_by(dataset) %>% 
  summarise(model = set_names(model, model_spec) %>% list, 
            results = map(model, performance::compare_performance, rank = T), 
            results = set_names(results, unique(dataset)), .groups = "keep")

#+ results='asis'
model_comparison_results %>% pull(results) %>% map(knitr::kable)

model_comparison_plots <- 
  model_comparison_results %>% 
  pull(results, "dataset") %>% map(plot)


model_comparison_plots[[1]] + ggtitle(names(model_comparison_plots)[[1]])
model_comparison_plots[[2]] + ggtitle(names(model_comparison_plots)[[2]])

#' note that the Performance scores are relative to best performing model for each dataset

model_means_results <- 
  estimate_means %>% 
  left_join(model_comparison_results %>% select(-model) %>% unnest(results), by = join_by("dataset", "model_spec" == "Name")) %>%
  mutate(label = paste(dataset, model_spec, sep = ".")) %>% 
  arrange(dataset, desc(Performance_Score)) %>% 
  select(Performance_Score, dataset, model_spec, results) %>% 
  mutate(label = paste(dataset, model_spec, sep = ".")) 

#'
#' ### BT model means
#' 

model_means_results %>% 
  filter(dataset == "blue tit") %>% 
  pull(results, name = model_spec) %>% 
  map(., plot, at = "PublishableAsIs") %>% 
  map2(., names(.), ~ .x + labs(subtitle = as_label(.y), title = NULL) +  
         see::theme_lucid() + theme(axis.text.x = element_text(angle = 50, hjust = 1))) 

#+ results='asis'
model_means_results %>% 
  pull(results, name = label) %>% 
  map(knitr::kable)

#' - inv_bc_var model mean getting pulled towards 0
#' - inv_folded_v, better, but model mean getting pulled more negatively
#' - no weights seems like most appropriate option
#' 
#' ### Eucalyptus model means

model_means_results %>% 
  filter(dataset == "eucalyptus") %>% 
  pull(results, name = model_spec) %>% 
  map(., plot, at = "PublishableAsIs") %>% 
  map2(., names(.), ~ .x + labs(subtitle = as_label(.y), title = NULL) +  
         see::theme_lucid() + theme(axis.text.x = element_text(angle = 50, hjust = 1))) 

#' - inv_bc_var model mean getting pulled towards 0, quite extreme for *Eucalyptus*, especially for poorly rated analyses. Only in case when random effect included for ReviewerID
#' - when RE for study id substituted, model means seem more fitting to data, but uncertainty seems artificially small given spread of data.
#' - inv_folded_v, better, but model mean getting pulled negatively, just ilke for BT analyses
#' - no weights seems like most appropriate option here, but hard to say since could not include desired random effects structure (+ study ID)
#'
#' ## And how does accounting for rm highly collinear analyses affect the model performance?

filter_args <- discard_at(filter_args, length(filter_args)) %>% 
  list(expr(dataset == "blue tit")) %>% list_flatten()

bt_co_v_all <- 
  ManyEcoEvo_results %>% 
  ungroup %>% 
  filter(!!!filter_args) %>% 
  select(dataset, collinearity_subset, effects_analysis) %>%
  mutate(model_data = map(effects_analysis, prepare_ratings_data),.keep = "unused") %>% 
  expand_grid(expand_grid(weight_formulas, random_expressions) %>% 
                mutate(weight_forms = names(weight_formulas),
                       random_effect = names(random_expressions)) %>% 
                unite("model_spec", weight_forms, random_effect, sep = "_") ) %>% 
  mutate(model = pmap(list(model_data, random_expressions, weight_formulas), lmer_wrap),.keep = "unused") %>% 
  mutate(singularity = map_lgl(model, performance::check_singularity),
         convergence = map_lgl(model, performance::check_convergence))

bt_co_v_reduced <- 
  bt_co_v_all %>% 
  filter(singularity == F, convergence == T) %>%
  group_by(collinearity_subset, model_spec) %>% 
  reframe(model = set_names(model, model_spec), 
          results = map(model, possibly(modelbased::estimate_means, otherwise = NULL), at = "PublishableAsIs"),
          results = set_names(results, collinearity_subset), collinearity_subset,  model_spec) %>% 
  rowwise() %>% 
  drop_na(results)

bt_perf <- 
  bt_co_v_all %>% semi_join(bt_co_v_reduced, by = join_by(collinearity_subset, model_spec)) %>% 
  group_by(collinearity_subset) %>% 
  summarise(model = set_names(model, model_spec) %>% list, 
            .groups = "keep") %>% 
  mutate( results = map(model, performance::compare_performance, rank = T), 
          results = set_names(results, unique(collinearity_subset)))

bt_perf %>% 
  pull(results, "collinearity_subset") %>% 
  map(plot)

#+ results='asis'
bt_perf %>% pull(results) %>% map(knitr::kable)
#'
bt_mod_means_res <- 
  bt_co_v_reduced  %>% 
  left_join(bt_perf %>% select(-model) %>% unnest(results), by = join_by("collinearity_subset", "model_spec" == "Name")) %>%
  mutate(label = paste(collinearity_subset, model_spec, sep = ".")) %>% 
  arrange(collinearity_subset, desc(Performance_Score)) %>% 
  select(Performance_Score, collinearity_subset, model_spec, results) %>% 
  mutate(label = paste(collinearity_subset, model_spec, sep = ".")) 
#'
#+ results='asis'
bt_mod_means_res %>% 
  pull(results, name = label) %>% 
  map(knitr::kable)
#'
bt_mod_means_res %>% 
  pull(results, name = label) %>% 
  map(., plot, at = "PublishableAsIs") %>% 
  map2(., names(.), ~ .x + labs(subtitle = as_label(.y), title = NULL) +  
         see::theme_lucid() + theme(axis.text.x = element_text(angle = 50, hjust = 1)))
