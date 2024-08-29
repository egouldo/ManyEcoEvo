#' Make visualisations wrapper function
#' @description Computes model summaries, tidy model summaries, model fit stats, funnel plots and forest plots for a dataframe of multiple fitted models
#'
#' @param data a nested dataframe with processed and standardised data stored in list-column `data`, grouped by variables `exclusion_set`, `dataset`, `estimate_type`, `publishable_subset`, `expertise_subset`, `collinearity_subset`. Each group contains a list-column `model` containing fitted models of class `rma.uni`, `rma.mv` or `merMod`.
#'
#' @return a nested dataframe grouped by variables `exclusion_set`, `dataset`, `estimate_type`, `publishable_subset`, `expertise_subset`, `collinearity_subset` containing model summaries, tidy model summaries, model fit stats, funnel plots and forest plots
#' @export
#' @family Multi-dataset Wrapper Functions
#' @import dplyr
#' @importFrom purrr map_if map2 pmap possibly
#' @importFrom stringr str_detect
#' @importFrom broom tidy
#' @importFrom performance performance
#' @importFrom metaviz viz_funnel
#' @importFrom ggplot2 ggplot
#' @importFrom parameters parameters
#' @import metafor
#' @import lme4
#' @importFrom tidyr unnest pivot_longer
#' @importFrom rlang is_na
make_viz <- function(data) {
  
  # ---- Define Helper Functions ----
  
  tidy_mod <- function(mod) {
    broom::tidy(mod, 
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
      group_by(exclusion_set, dataset, estimate_type, publishable_subset, expertise_subset, collinearity_subset, data) %>%
      pivot_longer(
        names_to = "model_name",
        values_to = "model",
        cols = c(
          -exclusion_set,
          -dataset,
          -estimate_type,
          -data,
          -diversity_data,
          -diversity_indices,
          -effects_analysis,
          -publishable_subset,
          -expertise_subset,
          -collinearity_subset
        )
      ) %>%
      ungroup() %>%
      select(
        -data,
        -diversity_data,
        -diversity_indices,
        -effects_analysis
      )
  }
  
  if (any(str_detect(unique(data$estimate_type), "y"))) {
    data_yi <- data %>%
      filter(estimate_type %in% c("yi", "y25", "y50", "y75")) %>%
      group_by(exclusion_set, dataset, estimate_type, data) %>%
      pivot_longer(
        names_to = "model_name",
        values_to = "model",
        cols = c(
          -exclusion_set,
          -dataset,
          -estimate_type,
          -data,
          -diversity_data,
          -diversity_indices,
          -effects_analysis
        )
      ) %>%
      ungroup() %>%
      select(
        -data,
        -diversity_data,
        -diversity_indices,
        -effects_analysis
      ) %>%
      mutate(publishable_subset = NA)
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
          .f = purrr::possibly(performance::performance, # switch to performance from glance
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
