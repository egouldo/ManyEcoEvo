#' Meta-analyses multiple datasets or subsets of datasets of analyst data
#' @description Runs all meta-analyses and regression models for the ManyEcoEvo project analysis, including:
#' - Fitting univariate / fixed-effects meta-analysis
#' - Calculating the deviation of every effect size / point-estimate from the meta-analytic mean for all data subsetes
#' - The absolute, box-cox transformed deviation scores
#' - A univariate GLM regression of the transformed deviation scores on the sorensen diversity indices
#' - A univariate GLM regression of the transformed deviation scores on the continuous peer-review ratings
#' - A univariate GLM regression of the transformed deviation scores on the categorical peer-review ratings
#' - A univariate GLM regression of the transformed deviation scores on a binary variable corresponding to whether the analysis was a mixed-effects model (i.e. GLM with random-effects) or not.
#' - To be implemented: a multivariate regression #TODO
#' - The deviation scores on transformed categorical ratings but with no intercept (for nice plotting / ecploration).
#'
#' @param MA_data A nested-dataframe grouped by `dataset` and / or `exclusion_set`, `estimate_type`, containing the list-column of prepared analyst subset data `effects_analysis` ready for meta-analysis.
#'
#' @return A nested dataframe with all columns of object parsed to arg `MA_data`, but with additional columns for the results of each analysis: `MA_mod`, `sorensen_glm`, `box_cox_ratings_cont`, `box_cox_ratings_cat`, `box_cox_rating_cat_no_int`, `uni_mixed_effects`
#' @export
#' @family Multi-dataset Wrapper Functions
#'
#' @examples
#'   # meta_analyse_datasets(targets::tar_read(round_2_survey_meta_analysis))
#'   # remove_problematic_responses <- 
#'   #   preprocessed_dataset %>% 
#'   #   dplyr::filter(dataset == "eucalyptus",
#'   #          (max(VZr, na.rm = TRUE) == VZr)) TODO, do we need to include now that INF's removed?
meta_analyse_datasets <- function(MA_data){
  #example:
  
  poss_fit_metafor_mv <- purrr::possibly(fit_metafor_mv,
                                         otherwise = NA,
                                         quiet = FALSE)
  
  cli::cli_h1(text = "Meta-analysing Datasets")
  
  fit_MA_mv <- function(effects_analysis, Z_colname, VZ_colname, estimate_type){
    Zr <- effects_analysis %>%  pull({{Z_colname}})
    VZr <- effects_analysis %>%  pull({{VZ_colname}})
    mod <- fit_metafor_mv(estimate = Zr, 
                          variance = VZr, 
                          estimate_type = estimate_type, 
                          data = effects_analysis)
    return(mod)
  }
  
  if( any(str_detect(unique(MA_data$estimate_type), pattern = "Zr")) ){
    # Must group by cols else multiple "effects_analysis" elements
    # get passed to fit_MA_mv()
    MA_data <- MA_data %>% 
      group_by(estimate_type, dataset, exclusion_set, publishable_subset, expertise_subset)
  } else {
    MA_data <- MA_data %>% 
      group_by(estimate_type, dataset, exclusion_set)
  }
  
  # --- Fit Meta-Models & Create Plots ---
  out <- 
    MA_data %>% 
    dplyr::mutate(Z_colname = 
                    map_chr(effects_analysis,
                            ~ if(!is_null(.x)){
                              dplyr::select(.x, 
                                            starts_with("Z")) %>% 
                                colnames()
                              } else {
                                rlang::na_chr
                              } ),
                  VZ_colname = 
                    map_chr(effects_analysis,
                            ~if(!is_null(.x)){
                              dplyr::select(.x, 
                                            starts_with("VZ")) %>% 
                                colnames()
                            } else {
                              rlang::na_chr
                            })) %>% 
    dplyr::mutate(effects_analysis = 
                    purrr::pmap(.l = list(effects_analysis, 
                                          Z_colname, 
                                          VZ_colname),
                                .f = rm_inf_na)) %>% 
    dplyr::mutate(MA_mod = 
                    purrr::pmap(.l = list(effects_analysis, Z_colname, VZ_colname, estimate_type),
                                .f = fit_MA_mv),
                  effects_analysis = 
                    ifelse(is.na(MA_mod),
                           NA,
                           purrr::map2(.x = effects_analysis,
                                       .y = MA_mod,
                                       .f = ~ calculate_deviation_score(.x, .y))),
                  effects_analysis = 
                    ifelse(rlang::is_na(effects_analysis),
                           NA,
                           purrr::map2(.x = effects_analysis,
                                       .y = dataset,
                                       .f = ~ box_cox_transform(.x, .y))),
                  sorensen_glm = 
                    purrr::map(.x = effects_analysis,
                               .f = ~ poss_fit_sorensen_glm(
                                 data = .x)),
                  box_cox_rating_cont = 
                    purrr::map(.x = effects_analysis, 
                               .f = ~ fit_boxcox_ratings_cont(
                                 .data = .x,
                                 outcome = box_cox_abs_deviation_score_estimate,
                                 outcome_var = box_cox_var
                               )),
                  box_cox_rating_cat = 
                    purrr::map(.x = effects_analysis, 
                               .f = ~ poss_fit_boxcox_ratings_cat(
                                 .data = .x,
                                 outcome = box_cox_abs_deviation_score_estimate,
                                 outcome_var = box_cox_var,
                                 interceptless = FALSE
                                 
                               )),
                  box_cox_rating_cat_no_int = 
                    purrr::map(.x = effects_analysis, 
                               .f = ~ poss_fit_boxcox_ratings_cat(
                                 .data = .x,
                                 outcome = box_cox_abs_deviation_score_estimate,
                                 outcome_var = box_cox_var,
                                 interceptless = TRUE
                               )),
                  uni_mixed_effects = 
                    purrr::map(.x = effects_analysis,
                               .f = ~ fit_uni_mixed_effects(
                                 data = .x
                               )
                               
                    )
    ) %>% 
    select(-ends_with("_colname"))
  
  
  return(out)
  
}