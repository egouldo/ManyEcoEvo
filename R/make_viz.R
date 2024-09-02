#' Make visualisations wrapper function
#' @description Compute model summaries, tidy model summaries, model fit statistics, funnel plots and forest plots for a tibble of multiple fitted models
#'
#' @param data a nested tibble with processed and standardised data stored in list-column `data`, grouped by variables `exclusion_set`, `dataset`, `estimate_type`, `publishable_subset`, `expertise_subset`, `collinearity_subset`. Each group contains a list-column `model` containing fitted models of class `rma.uni`, `rma.mv` or `merMod`.
#'
#' @return a nested tibble containing model summaries, fit statistics, plots, model parameters and other components of fitted model objects, see details.
#' @details
#' [make_viz()] is a wrapper function that takes a nested tibble of fitted models and computes model summaries, tidy model summaries, model fit statistics, funnel plots and forest plots for each model. The function is designed to be used in conjunction with `ManyEcoEvo_results`-type datasets, which contains multiple fitted models for each dataset and estimate type.
#' 
#' The following functions are applied to each model:
#' 
#' - `mod_summary`:  [base::summary()] to extract model summaries
#' - `tidy_mod_summary`: [broom.mixed::tidy()] to extract tidy model summaries
#' - `mod_fit_stats` / `mod_glance`:  [performance::performance()] and [broom::glance()] to extract model fit statistics
#' - `funnel_plot`:  [metaviz::viz_funnel()] to create funnel plots for `rma.uni` models
#' - `forest_plot`:  [gg_forest()] to create forest plots for `rma.mv` models
#' - `MA_fit_stats`:  [get_MA_fit_stats()] to extract model fit statistics for `rma.mv` models
#' - `model_params`: [parameters::parameters()] to extract model parameters
#' 
#' Note that where the fitted model object doesn't exist, i.e. is a `NA` or `NULL` value, the function will return `NA` for all components.
#' 
#' @export
#' @family Multi-dataset Wrapper Functions
#' @import dplyr
#' @importFrom purrr map_if map2 pmap possibly
#' @importFrom stringr str_detect
#' @importFrom broom.mixed tidy
#' @importFrom broom glance
#' @importFrom performance performance
#' @importFrom metaviz viz_funnel
#' @importFrom ggplot2 ggplot
#' @importFrom parameters parameters
#' @import metafor
#' @import lme4
#' @importFrom tidyr unnest pivot_longer
#' @importFrom rlang is_na
#' @examples
#' make_viz(ManyEcoEvo_results)
#' @seealso [get_MA_fit_stats()], [gg_forest()]
make_viz <- function(data) {
  
  # ---- Define Helper Functions ----
  
  tidy_mod <- function(mod) {
    broom.mixed::tidy(mod, 
                conf.int = TRUE,
                include_studies = TRUE)
  }
  
  viz_funnel_2 <- function(x) {
    metaviz::viz_funnel(x, y_axis = "precision")
  }
  
  poss_viz_funnel <- possibly(viz_funnel_2, otherwise = NA)
  
  # ---- Prepare Data for Visualisation ----
  if (any(str_detect(unique(data$estimate_type), pattern = "Zr"))) {
    data_Zr <- data %>%
      filter(estimate_type == "Zr") %>%
      select(
        -data,
        -diversity_data,
        -diversity_indices,
        -effects_analysis
      ) %>% 
      pivot_longer(names_to = "model_name", values_to = "model",
                   cols = c(starts_with("MA_mod"), 
                            "sorensen_glm", 
                            starts_with("box_cox_rating"), 
                            "uni_mixed_effects")
      )
  }
  
  if (any(str_detect(unique(data$estimate_type), "y"))) {
    data_yi <- data %>%
      filter(estimate_type %in% c("yi", "y25", "y50", "y75")) %>%
      select(
        -data,
        -diversity_data,
        -diversity_indices,
        -effects_analysis
      ) %>% 
      pivot_longer(names_to = "model_name", values_to = "model",
                   cols = c(starts_with("MA_mod"), 
                            "sorensen_glm", 
                            starts_with("box_cox_rating"), 
                            "uni_mixed_effects")
      )
  }
  
  if (exists("data_Zr") & exists("data_yi")) {
    all_data <- bind_rows(data_Zr, data_yi)
  } else if (exists("data_Zr")) {
    all_data <- data_Zr
  } else {
    all_data <- data_yi
  }
  
  # ----- Extract Model Metrics and Apply Visualisation Functions -----
  
  viz_out <-
    all_data %>%
    mutate(
      mod_summary = 
        map_if(
          .x = model,
          .p = ~ !rlang::is_na(.x),
          .f = summary,
          .else = ~ return(NA)
        ),
      tidy_mod_summary = 
        map_if(
          .x = model,
          .p = ~ !rlang::is_na(.x),
          .f = purrr::possibly(tidy_mod,
                               otherwise = NA,
                               quiet = TRUE
          ),
          .else = ~ return(NA)
        ),
      mod_fit_stats = 
        map_if(
          .x = model,
          .p = ~ !rlang::is_na(.x),
          .f = purrr::possibly(performance::performance,
                               otherwise = NA,
                               quiet = FALSE
          ),
          .else = ~ return(NA)
        ),
      mod_glance = 
        map_if(
          .x = model,
          .p = ~ !rlang::is_na(.x),
          .f = purrr::possibly(broom::glance, 
                               otherwise = NA,
                               quiet = FALSE
          ),
          .else = ~ return(NA)
        ),
      funnel_plot = 
        purrr::map_if(
          .x = model,
          .p = ~ any(class(.x) == "rma.uni"),
          .f = poss_viz_funnel,
          # metafor::funnel(., yaxis = "seinv") #alternative plot fun
          .else = ~NA
        ),
      forest_plot = 
        ifelse(!rlang::is_na(model) & model_name == "MA_mod",
               purrr::pmap(
                 .l =
                   list(model, estimate_type, dataset),
                 .f = purrr::possibly(gg_forest, otherwise = NA)
               ),
               NA
        ),
      MA_fit_stats = 
        ifelse(model_name == "MA_mod" & !rlang::is_na(model),
               map_if(
                 .x = model,
                 .p = ~ "rma.mv" %in% class(.x),
                 .f = purrr::possibly(get_MA_fit_stats, otherwise = NA),
                 .else = ~ return(NA)
               ),
               NA
        ),
      model_params = map_if(
        .x = model,
        .p = possibly(\(x) class(x) %in% parameters::supported_models() %>% 
                        any(), 
                      otherwise = NA),
        .f = possibly(parameters::parameters, NA),
        .else = ~ return(NA),
      )
    )
  
  return(viz_out)
}
