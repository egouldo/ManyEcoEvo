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
#' @param filter_vars A list of expressions to filter the `MA_data` dataframe by. E.g. `rlang::exprs(exclusion_set == "complete", expertise_subset == "All", publishable_subset == "All", collinearity_subset == "All")`
#'
#' @return A nested dataframe with all columns of object parsed to arg `MA_data`, but with additional columns for the results of each analysis: `MA_mod`, `sorensen_glm`, `box_cox_ratings_cont`, `box_cox_ratings_cat`, `box_cox_rating_cat_no_int`, `uni_mixed_effects`
#' @export
#' @importFrom purrr map_chr map2 map possibly pmap
#' @import dplyr
#' @import cli
#' @importFrom rlang na_chr is_null na_chr
#' @family Multi-dataset Wrapper Functions
#'
#' @examples
#'     filter_vars <- rlang::exprs(exclusion_set == "complete",
#'                                 expertise_subset == "All",
#'                                 publishable_subset == "All",
#'                                 collinearity_subset == "All")
meta_analyse_datasets <- function(MA_data, filter_vars = NULL){

  # poss_fit_metafor_mv <- purrr::possibly(fit_metafor_mv,
  #                                        otherwise = NA,
  #                                        quiet = FALSE)
  # 
  cli::cli_h1(text = "Meta-analysing Datasets")
  
  if( any(str_detect(unique(MA_data$estimate_type), pattern = "Zr")) ){
    # Must group by cols else multiple "effects_analysis" elements
    # get passed to fit_MA_mv()
    MA_data <- MA_data %>% 
      group_by(estimate_type, dataset, exclusion_set, publishable_subset, expertise_subset, collinearity_subset)
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
    ) 
  
  # --- Fit Multivariate Models --- 
  #TODO apply same strategy below for filtering any other models out that we might not want
  if (!is.null(filter_vars)) { #TODO check if this is the correct way to check for NULL 
    filter_var_syms <- map(filter_vars, ~ rlang::call_args(.x) %>% pluck(1)) 
    
    filter_var_names <- filter_var_syms %>% 
      map(rlang::quo_name) %>% 
      purrr::list_c()
    
    multivar_mods <- 
      out %>% 
      dplyr::filter(!!!filter_vars) %>% 
      group_by(dataset, !!!filter_var_syms) %>% #retain grouping, but add additional dataset grouping
      mutate(effects_analysis = map(effects_analysis, ~ .x %>% 
                                      unnest(review_data))) %>% 
      mutate(MA_mod_mv = map(effects_analysis, fit_multivar_MA), 
             .keep = "none") #retain output and grouping cols only
    
    by <- join_by("dataset", !!!filter_var_names)
    
    out <- nest_join(out, multivar_mods, by) %>% 
      select(-ends_with("_colname"))
      
  } else {
    out <- out %>% 
      mutate(MA_mod_mv = map(effects_analysis, fit_multivar_MA)) %>% 
      select(-ends_with("_colname"))
  }
  
  return(out)
  
}
