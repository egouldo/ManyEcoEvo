#' Meta-analyses multiple datasets or subsets of datasets of analyst data
#' 
#' @description Runs all meta-analyses and regression models for the ManyEcoEvo project analysis, including:
#' - Fitting univariate / fixed-effects meta-analysis
#' - Calculating the deviation of every effect size / point-estimate from the meta-analytic mean for all data subsetes
#' - The absolute, box-cox transformed deviation scores
#' - A univariate GLM regression of the transformed deviation scores on the sorensen diversity indices
#' - A univariate GLM regression of the transformed deviation scores on the continuous peer-review ratings
#' - A univariate GLM regression of the transformed deviation scores on the categorical peer-review ratings
#' - A univariate GLM regression of the transformed deviation scores on a binary variable corresponding to whether the analysis was a mixed-effects model (i.e. GLM with random-effects) or not.
#' - To be implemented: a multivariate regression #TODO
#' - The deviation scores on transformed categorical ratings but with no intercept (for nice plotting / exploration).
#'
#' @param data A nested- dataframe grouped by `dataset` and / or `exclusion_set`, `estimate_type`, containing the list-column of prepared analyst subset data `effects_analysis` ready for meta-analysis.
#' @param filter_vars A list of expressions to filter the `data` dataframe by. E.g. `rlang::exprs(exclusion_set == "complete", expertise_subset == "All", publishable_subset == "All", collinearity_subset == "All")`
#' #' @param outcome_variable A named list containing either/and a list of `dataset`s and their corresponding outcome variables for each value of `dataset`, a list of `estimate_type`s and their corresponding outcome variables for each value of `estimate_type`.
#' @return A nested dataframe with all columns of object parsed to arg `data`, but with additional columns for the results of each analysis: `MA_mod`, `sorensen_glm`, `box_cox_ratings_cont`, `box_cox_ratings_cat`, `box_cox_rating_cat_no_int`, `uni_mixed_effects`
#' @export
#' @importFrom stringr str_detect
#' @importFrom purrr map_chr map2 map possibly pmap list_c map_lgl 
#' @import dplyr
#' @importFrom cli cli_h1 cli_abort
#' @importFrom rlang na_chr is_null na_chr is_list is_call exprs f_lhs as_string
#' @importFrom pointblank expect_col_exists
#' @importFrom tidyr unnest drop_na
#' @family Multi-dataset Wrapper Functions
#' @examples
#' filter_vars <- rlang::exprs(
#'   exclusion_set == "complete",
#'   expertise_subset == "All",
#'   publishable_subset == "All",
#'   collinearity_subset == "All"
#' )
#' @details
#' 
#' When `filter_vars` are supplied the function will filter the `data` dataframe by the expressions in the list, any data subsets excluded by filtering will not have multivariate met-analysis models fitted with [fit_multivar_MA()].
#' 
#' When the arguments `outcome_variable` and/or `outcome_variable` are not supplied, the function defaults to:
#' 
#' - using `"Zr"` as the standardised effect size and `"VZr"` as the standard error when `estimate_type` is `"Zr"`.
#' - using `"Z"` as the standardised out-of-sample estimate and `"VZ" `as the standardised out-of-sample estimate error when `estimate_type` is one of `c("yi", "y25", "y50", "y75")`.
#' 
#' The function will check if the `data` dataframe contains the required columns for meta-analysis, including any variable names specified in calls to the `filter_vars` argument. If the required functions do not exist then the function will stop with an error.
#'  
#' Function assumes that if argument `outcome_variable` is supplied, then `outcome_SE` is also supplied, and conversely, if `outcome_SE` is not supplied, then neither is `outcome_variable` (*TODO* not yet checked in function).
#' 
meta_analyse_datasets <- function(data, outcome_variable = NULL, outcome_SE, filter_vars = NULL) {
  
  # ----- Argument Checks -----
  stopifnot(
    is.data.frame(data),
    rlang::is_null(filter_vars) | rlang::is_list(filter_vars)
  )
  
  required_columns <- c("effects_analysis", 
                        "estimate_type", 
                        "dataset")
  
  if (rlang::is_list(filter_vars)) {
    if (!all(map_lgl(filter_vars, rlang::is_call))) {
      cli_abort("{.arg filter_vars} must be a list of calls")
    } else {
      required_columns <-  filter_vars %>% 
        map(rlang::f_lhs) %>% 
        map(rlang::as_string) %>% 
        list_c() %>%
        append(values = required_columns) %>% 
        unique()
    }
  }
  
  pointblank::expect_col_exists(
    data, 
    columns = required_columns
  )
  
  # ---- Data Preparation & Conditional Object Assignment ---
  
  # if (any(str_detect(unique(data$estimate_type), pattern = "Zr"))) {
  #   # Must group by cols else multiple "effects_analysis" elements
  #   # get passed to fit_MA_mv()
  #   data <- data %>%
  #     group_by(estimate_type, #TODO can we just `ungroup()`?
  #              dataset, 
  #              exclusion_set, 
  #              publishable_subset, 
  #              expertise_subset, 
  #              collinearity_subset)
  #   
  # } else {
  #   data <- data %>%
  #     group_by(estimate_type, 
  #              dataset, 
  #              exclusion_set) #TODO we need to merge the yi together for eucalyptus
  # }
  
  # Assign outcome_variable values
  if (rlang::is_null(outcome_variable)) {
    # Defaults
    if (any(str_detect(unique(data$estimate_type), pattern = "Zr"))) {
      outcome_variable <- "Zr"
      outcome_SE <- "VZr"
    } else {
      outcome_variable <- "Z"
      outcome_SE <- "VZ"
    } 
    data <- data %>% 
      ungroup %>% 
      mutate(outcome_colname = outcome_variable,
             outcome_SE_colname = outcome_SE)
  } else {
    # Argument supplied
    matched_formulae_outcome <- 
      map(outcome_variable,
          ~ formulae_match(x = names(.x), y = .x))
    
    matched_formulae_outcome_SE <- 
      map(outcome_SE,
          ~ formulae_match(x = names(.x), y = .x))
    
    data <- map2(names(matched_formulae_outcome),
                 matched_formulae_outcome,
                 .f = ~ map_match_formulae(ungroup(data), .x, .y)) %>% 
      bind_rows() %>% 
      drop_na(outcome_colname) %>% 
      left_join(
        {
          map2(names(matched_formulae_outcome_SE),
               matched_formulae_outcome_SE, 
               .f = ~ map_match_formulae(
                 {data %>% 
                   ungroup %>% 
                   select(names(outcome_SE))}, 
                   .x, 
                   .y, 
                   col_name = "outcome_SE_colname")) %>% 
            bind_rows() %>% 
            drop_na(outcome_SE_colname) %>% 
            distinct()
        },
        by = unique(names(outcome_SE))
      )
      
  }
  
  # ----- Fit Meta-Models & Create Plots -----
  cli::cli_h1(text = "Meta-analysing Datasets")
  
  out <-
    data %>%
    dplyr::mutate(
      effects_analysis =
        purrr::pmap(
          .l = list(
            effects_analysis,
            outcome_colname,
            outcome_SE_colname
          ),
          .f = rm_inf_na
        )
    ) %>%
    dplyr::mutate(
      MA_mod =
        purrr::pmap(
          .l = list(effects_analysis, 
                    outcome_colname, 
                    outcome_SE_colname, 
                    estimate_type),
          .f = fit_MA_mv
        ),
      effects_analysis =
        ifelse(is.na(MA_mod),
               NA,
               purrr::map2(
                 .x = effects_analysis,
                 .y = MA_mod,
                 .f = ~ calculate_deviation_score(.x, .y)
               )
        ),
      effects_analysis =
        ifelse(rlang::is_na(effects_analysis),
               NA,
               purrr::map2(
                 .x = effects_analysis,
                 .y = dataset,
                 .f = ~ box_cox_transform(.x, .y)
               )
        ),
      sorensen_glm =
        purrr::map(
          .x = effects_analysis,
          .f = ~ poss_fit_sorensen_glm(
            data = .x
          )
        ),
      box_cox_rating_cont =
        purrr::map(
          .x = effects_analysis,
          .f = ~ fit_boxcox_ratings_cont(
            .data = .x,
            outcome = box_cox_abs_deviation_score_estimate,
            outcome_var = box_cox_var
          )
        ),
      box_cox_rating_cat =
        purrr::map(
          .x = effects_analysis,
          .f = ~ poss_fit_boxcox_ratings_cat(
            .data = .x,
            outcome = box_cox_abs_deviation_score_estimate,
            outcome_var = box_cox_var,
            interceptless = FALSE
          )
        ),
      box_cox_rating_cat_no_int =
        purrr::map(
          .x = effects_analysis,
          .f = ~ poss_fit_boxcox_ratings_cat(
            .data = .x,
            outcome = box_cox_abs_deviation_score_estimate,
            outcome_var = box_cox_var,
            interceptless = TRUE
          )
        ),
      uni_mixed_effects =
        purrr::map(
          .x = effects_analysis,
          .f = ~ fit_uni_mixed_effects(
            data = .x
          )
        )
    )
  
  # --- Fit Multivariate Models ---
  
  # TODO apply same strategy below for filtering any other models out that we might not want to fit
  if (!rlang::is_null(filter_vars)) {
    filter_var_syms <- map(filter_vars, ~ rlang::call_args(.x) %>% pluck(1))
    
    filter_var_names <- filter_var_syms %>%
      map(rlang::quo_name) %>%
      purrr::list_c()
    
    multivar_mods <-
      out %>%
      dplyr::filter(!!!filter_vars) %>%
      group_by(dataset, !!!filter_var_syms) %>% # retain grouping, but add additional dataset grouping
      mutate(effects_analysis = map(effects_analysis, ~ .x %>%
                                      unnest(review_data))) %>%
      mutate(
        MA_mod_mv = map(effects_analysis, fit_multivar_MA),
        .keep = "none"
      ) # retain output and grouping cols only
    
    by <- join_by("dataset", !!!filter_var_names)
    
    out <- left_join(out, multivar_mods, by) %>%
      select(-ends_with("_colname"))
  } else {
    
    out <- out %>%
      mutate(effects_analysis = map(effects_analysis, ~ .x %>%
                                      unnest(review_data))) %>%
      mutate(MA_mod_mv = map(effects_analysis, fit_multivar_MA)) %>%
      select(-ends_with("_colname"))
  }
  
  out <-
    out %>% # replace any NULL values with NA
    mutate(across(everything(),
                  .fns = ~ coalesce(.x, list(NA))
    ))
  
  return(out)
}
